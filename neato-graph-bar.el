;; neato-graph-bar.el - Neat-o graph bars for Emacs
;; Copyright (C) 2016 Robert Cochran <robert-git@cochranmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defgroup neato-graph-bar nil
  "Neat-o graph bars for various things"
  :group 'applications)

(defcustom neato-graph-bar/memory-info-file
  "/proc/meminfo"
  "File to grab the memory usage info from.

Currently expecting Linux meminfo format."
  :type '(file :must-match t)
  :group 'neato-graph-bar)

(defcustom neato-graph-bar/cpu-stat-file
  "/proc/stat"
  "File to grab the CPU usage statistics from.

Currently expecting Linux /proc/stat format"
  :type '(file :must-match t)
  :group 'neato-graph-bar)

(defcustom neato-graph-bar/unified-cpu-graph
  nil
  "When nil, draw separate graphs for each CPU.
When non-nil, draw a single unified CPU graph.")

(defcustom neato-graph-bar/label-padding
  4
  "Number of character to pad graph labels to"
  :type 'wholenum
  :group 'neato-graph-bar)

(defcustom neato-graph-bar/refresh-time
  1
  "Number of seconds to wait before refreshing"
  :type 'number
  :group 'neato-graph-bar)

(defface neato-graph-bar/memory-used
  '((t
     :foreground "green"
     :inherit default))
  "Face for the portion of the memory graph representing userspace memory"
  :group 'neato-graph-bar)

(defface neato-graph-bar/memory-buffer
  '((t
     :foreground "blue"
     :inherit default))
  "Face for the portion of the memory graph representing buffers"
  :group 'neato-graph-bar)

(defface neato-graph-bar/memory-cache
  '((t
     :foreground "yellow"
     :inherit default))
  "Face for the portion of the memory graph representing caches"
  :group 'neato-graph-bar)

(defface neato-graph-bar/cpu-user
  '((t
     :foreground "green"
     :inherit default))
  "Face for the portion of the CPU graph representing user programs"
  :group 'neato-graph-bar)

(defface neato-graph-bar/cpu-system
  '((t
     :foreground "red"
     :inherit default))
  "Face for the portion of the CPU graph representing the kernel"
  :group 'neato-graph-bar)

(defface neato-graph-bar/cpu-interrupt
  '((t
     :foreground "gray"
     :inherit default))
  "Face for the portion of the CPU graph representing interrupts"
  :group 'neato-graph-bar)

(defface neato-graph-bar/cpu-vm
  '((t
     :foreground "purple"
     :inherit default))
  "Face for the portion of the CPU graph representing virtual machines"
  :group 'neato-graph-bar)

(defvar-local neato-graph-bar/memory-fields-to-keep
  '("MemTotal"
    "MemFree"
    "MemAvailable"
    "Buffers"
    "Cached"
    "SwapTotal"
    "SwapFree"
    "SwapCached")
  "Fields to keep when retrieving memory information, to keep
memory use low and not have a bunch of unused info in the
alist.")

(defvar-local neato-graph-bar/cpu-field-names
  '(user
    nice
    system
    idle
    iowait
    irq
    softirq
    steal
    guest
    guest-nice)
  "Field names for CPU statistics")

(defvar-local neato-graph-bar/cpu-stats-previous
  nil
  "Previous statistics of CPU usage information")

(defvar-local neato-graph-bar/update-timer
  nil
  "Update timer for graphs")

(defvar-local neato-graph-bar/current-window
  nil
  "Current window")

(defun neato-graph-bar ()
  "Displays system information graph bars"
  (interactive)
  (let ((buffer (get-buffer-create "*Neato Graph Bar*")))
    (set-buffer buffer)
    (buffer-disable-undo buffer)
    (when (zerop (buffer-size))
      (neato-graph-bar-mode)
      (neato-graph-bar/update))
    (pop-to-buffer buffer)
    (if (null neato-graph-bar/update-timer)
	(setq neato-graph-bar/update-timer
	      (run-at-time 0
			   neato-graph-bar/refresh-time
			   'neato-graph-bar/update)))))

(define-derived-mode neato-graph-bar-mode
  special-mode
  "Neato Graph Bar"
  (set (make-local-variable 'revert-buffer-function)
       'neato-graph-bar/update)
  (setq cursor-type nil)
  (add-hook 'kill-buffer-hook
	    (lambda ()
	      (when (timerp neato-graph-bar/update-timer)
		(cancel-timer neato-graph-bar/update-timer)))))

(defun neato-graph-bar/update ()
  "Update the graphs."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'neato-graph-bar-mode)
	(setq neato-graph-bar/current-window (get-buffer-window nil t))
	(unless (null neato-graph-bar/current-window)
	  (neato-graph-bar/redraw))))))

(defun neato-graph-bar/redraw (&rest x)
  "Neato Graph Bar revert function"
  (if (= (window-width neato-graph-bar/current-window)
	 0)
      (return-from neato-graph-bar/update))
  (let ((buffer-read-only nil))
    (erase-buffer)
    (neato-graph-bar/draw-all-graphs)))

(defun neato-graph-bar/draw-graph (label portions &optional end-text)
  "Draw a bar graph.

LABEL is the label in front of the graph. PORTIONS is an alist
of font-percent value pairs, where percent is a float between 0
and 1. Al unused space will automatically be marked as empty. For
example, to specify that you want a graph drawn with face1 as the
first 30%, and face2 the second 20%, with the rest empty, you
would pass

((face1 . 0.3)
 (face2 . 0.2))

for PORTIONS. END-TEXT is placed within the graph at the
end. Unspecified, it defaults to a percentage, but can be any
arbitrary string (good for doing things such as providing a
\"30MB/100MB\" type counter for storage graphs)"
  (let* ((padded-label (concat (make-string (- neato-graph-bar/label-padding
                                               (length label)) ?\s)
                               label))
         ;; 3 -> Space after label + '[' + ']'
         (bar-width (- (window-body-width neato-graph-bar/current-window)
                       ;; Seems that Emacs will wrap the line if it extends
                       ;; all the way to the end of the window in a terminal...
                       ;; not what we want... so adjust for it.
                       (if (display-graphic-p) 0 1)
                       (length padded-label)
                       3))
	 (filled-percent 0.0))
    (insert padded-label " [")
    (dolist (pair portions)
      (let ((face (car pair))
	    (percent (cdr pair)))
	(insert (propertize
		 (make-string (round (* percent bar-width)) ?|)
		'font-lock-face face))
	(setq filled-percent (+ percent filled-percent))))
    (insert (make-string (- (+ bar-width (length padded-label) 2)
			    (current-column))
	    ?\s))
    (insert "]")
    (if (null end-text)
	(setq end-text (format "%.1f%%" (* filled-percent 100))))
    (save-excursion
      (backward-char (+ (length end-text) 1))
      (insert end-text)
      (delete-char (length end-text)))))

(defun neato-graph-bar/create-storage-status-text (used total)
  "Create an end-text suitable for storage bars (ie \"10M/200M\").

USED and TOTAL should both be in kilobytes."
  (let ((suffix-table '(?K ?M ?G ?T))
	(log-used (if (= used 0) 0 (floor (log used 1024))))
	(log-total (if (= total 0) 0 (floor (log total 1024)))))
    (format "%.1f%c/%.1f%c"
	    (/ (float used) (expt 1024 log-used))
	    (elt suffix-table log-used)
	    (/ (float total) (expt 1024 log-total))
	    (elt suffix-table log-total))))

(defun neato-graph-bar/get-memory-info ()
  "Retrieve the system memory information.

This is obtained from `neato-graph-bar/memory-info-file', and
filtered down to entries listed in `neato-graph-bar/memory-fields-to-keep'."
  (let ((mem-info-list
	 (mapc (lambda (x) (split-string x ":" t))
	       (with-temp-buffer
		 (insert-file-contents neato-graph-bar/memory-info-file)
		 (split-string (buffer-string) "\n" t)))))
    (delete-if-not (lambda (x) (member x neato-graph-bar/memory-fields-to-keep))
		   mem-info-list
		   :key #'car)
    (mapc (lambda (x)
	    (rplacd x
		    (string-to-number (car (split-string (cadr x))))))
	  mem-info-list)))

(defun neato-graph-bar/get-memory-attribute (alist attribute)
  "Get the ATTRIBUTE from the memory info ALIST."
  (cdr (find attribute alist :key #'car :test #'string=)))

(defun neato-graph-bar/draw-memory-graph ()
  "Draw memory graph."
  (let* ((memory-info (neato-graph-bar/get-memory-info))
	 (memory-total
	  (neato-graph-bar/get-memory-attribute memory-info "MemTotal"))
	 (memory-free
	  (neato-graph-bar/get-memory-attribute memory-info "MemFree"))
	 (memory-buffers
	  (neato-graph-bar/get-memory-attribute memory-info "Buffers"))
	 (memory-cached
	  (neato-graph-bar/get-memory-attribute memory-info "Cached"))
	 ;; Side note - it appears that there is a difference of 20MB (when I
	 ;; tested) between `(- memory-total memory-free memory-buffers-cache)`
	 ;; and the result of `(- memory-total memory-available)`... no idea
	 ;; *why*. This 20MB is likely kernel memory, I bet. Anyways, my
	 ;; reference, the file 'proc/sysinfo.c' from procps-ng uses the first
	 ;; form, so I do here as well.
	 (memory-used
	  (- memory-total memory-free memory-buffers memory-cached))
	 (memory-graph-alist
	  `((neato-graph-bar/memory-used . ,(/ (float memory-used)
					       memory-total))
	    (neato-graph-bar/memory-buffer . ,(/ (float memory-buffers)
						 memory-total))
	    (neato-graph-bar/memory-cache . ,(/ (float memory-cached)
						memory-total))))
	 (memory-end-text (neato-graph-bar/create-storage-status-text
			   memory-used
			   memory-total)))
    (neato-graph-bar/draw-graph "Mem" memory-graph-alist memory-end-text)))

(defun neato-graph-bar/draw-swap-graph ()
  "Draw swap graph."
  (let* ((memory-info (neato-graph-bar/get-memory-info))
	 (swap-total
	  (neato-graph-bar/get-memory-attribute memory-info "SwapTotal"))
	 (swap-free
	  (neato-graph-bar/get-memory-attribute memory-info "SwapFree"))
	 (swap-cached
	  (neato-graph-bar/get-memory-attribute memory-info "SwapCached"))
	 (swap-used (- swap-total swap-free swap-cached))
	 (swap-graph-alist
	  `((neato-graph-bar/memory-used . ,(/ (float swap-used) swap-total))
	    (neato-graph-bar/memory-cache . ,(/ (float swap-cached)
						swap-total))))
	 (swap-end-text (neato-graph-bar/create-storage-status-text
			 swap-used
			 swap-total)))
    (neato-graph-bar/draw-graph "Swap" swap-graph-alist swap-end-text)))

(defun neato-graph-bar/get-cpu-stats ()
  "Get the difference in the system CPU statistics since last update.

The old information snapshot is moved to `neato-graph-bar/cpu-stats-previous'.

This is obtained from `neato-graph-bar/cpu-stat-file', and made
into key-value pairs as defined by `neato-graph-bar/cpu-field-names'."
  (let* ((cpu-stat-list-strings
	  (mapcar (lambda (x) (split-string x " " t))
		  (with-temp-buffer
		    (insert-file-contents neato-graph-bar/cpu-stat-file)
		    (delete-if-not
		     (lambda (x) (string= (substring x 0 3) "cpu"))
		     (split-string (buffer-string) "\n" t)))))
	 (cpu-stat-list
	  (mapcar (lambda (x)
		    (cons (car x)
			  (pairlis neato-graph-bar/cpu-field-names
				   (mapcar #'string-to-number (cdr x)))))
		  cpu-stat-list-strings))
	 (cpu-diff (copy-tree cpu-stat-list)))
    ;; Account for empty first run
    (if	(null neato-graph-bar/cpu-stats-previous)
	(setq neato-graph-bar/cpu-stats-previous cpu-stat-list))
    (map 'list (lambda (n o)
		 (map 'list (lambda (x y)
			      (rplacd x (- (cdr x) (cdr y))))
		      (cdr n) (cdr o)))
	 cpu-diff neato-graph-bar/cpu-stats-previous)
    (setq neato-graph-bar/cpu-stats-previous cpu-stat-list)
    cpu-diff))

(defun neato-graph-bar/get-cpu-stat-total (cpu)
  "Get the time total for CPU"
  (reduce #'+ (mapcar #'cdr (cdr cpu))))

(defun neato-graph-bar/get-cpu-attribute (cpu attribute)
  "Get ATTRIBUTE from CPU.

CPU is a string identifier, as found in the originating statistics file.
ATTRIBUTE is a symbol as defined in `neato-graph-bar/cpu-field-names'."
  (cdr (assoc attribute (cdr cpu))))

(defun neato-graph-bar/draw-cpu-graph-helper (cpu)
  "Helper to draw the CPU graph(s). Does the actual work."
  (let* ((cpu-name (upcase (car cpu)))
	 (cpu-total (neato-graph-bar/get-cpu-stat-total cpu))
	 (cpu-user (+
		    (neato-graph-bar/get-cpu-attribute cpu 'user)
		    (neato-graph-bar/get-cpu-attribute cpu 'nice)))
	 (cpu-system (neato-graph-bar/get-cpu-attribute cpu 'system))
	 (cpu-irq (+
		   (neato-graph-bar/get-cpu-attribute cpu 'irq)
		   (neato-graph-bar/get-cpu-attribute cpu 'softirq)))
	 (cpu-vm (+
		  (neato-graph-bar/get-cpu-attribute cpu 'steal)
		  (neato-graph-bar/get-cpu-attribute cpu 'guest)
		  (neato-graph-bar/get-cpu-attribute cpu 'guest-nice)))
	 (cpu-graph-alist
	  `((neato-graph-bar/cpu-user . ,(/ (float cpu-user) cpu-total))
	    (neato-graph-bar/cpu-system . ,(/ (float cpu-system) cpu-total))
	    (neato-graph-bar/cpu-interrupt . ,(/ (float cpu-irq) cpu-total))
	    (neato-graph-bar/cpu-vm . ,(/ (float cpu-vm) cpu-total)))))
    ;; First run has cpu-total at 0, which will cause a div-by-0.
    ;; If we find any NaNs, skip drawing
    (if (not (find -0.0e+NaN cpu-graph-alist :key #'cdr))
	(neato-graph-bar/draw-graph cpu-name cpu-graph-alist))))

(defun neato-graph-bar/draw-cpu-graph ()
  "Draw the CPU graph."
  (let ((cpu-info (neato-graph-bar/get-cpu-stats)))
    (if neato-graph-bar/unified-cpu-graph
	(neato-graph-bar/draw-cpu-graph-helper (car cpu-info))
      (progn
	(dolist (cpu (cdr cpu-info))
	  (neato-graph-bar/draw-cpu-graph-helper cpu)
	  (insert "\n"))
	(backward-delete-char 1)))))

(defun neato-graph-bar/draw-all-graphs ()
  (neato-graph-bar/draw-cpu-graph)
  (insert "\n")
  (neato-graph-bar/draw-memory-graph)
  (insert "\n")
  (neato-graph-bar/draw-swap-graph)
  (insert "\n"))

(provide 'neato-graph-bar)
