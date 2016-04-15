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
  "Face for the portion of the memory graph repsenting buffers"
  :group 'neato-graph-bar)

(defface neato-graph-bar/memory-cache
  '((t
     :foreground "yellow"
     :inherit default))
  "Face for the portion of the memory graph repsenting caches"
  :group 'neato-graph-bar)

(defun neato-graph-bar/draw-graph (label portions &optional end-text)
  "Draw a bar graph.

LABEL is the label in front of the graph. PORTIONS is an alist
of font-percent value pairs, where percent is a float between 0
and 1. Al unused space will automatically be marked as empty. For
example, to specify that you want a graph drawn with face1 as the
first 30%, and face2 the second 20%, with the rest empty, you
would pass

(('face1 . 0.3)
 ('face2 . 0.2))

for PORTIONS. END-TEXT is placed within the graph at the
end. Unspecified, it defaults to a percentage, but can be any
arbitrary string (good for doing things such as providing a
\"30MB/100MB\" type counter for storage graphs)"
  ;; 3 -> Space after label + '[' + ']'
  (let ((bar-width (- (window-body-width) 3 (length label)))
	(filled-percent 0.0))
    (insert label " [")
    (dolist (pair portions)
      (let ((face (car pair))
	    (percent (cdr pair)))
	(insert (propertize
		 (make-string (round (* percent bar-width)) ?|)
		'font-lock-face face))
	(setq filled-percent (+ percent filled-percent))))
    (insert (make-string (- (+ bar-width (length label) 2)
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
	 (mapcar (lambda (x) (split-string x ":" t))
		 (with-temp-buffer
		   (insert-file-contents neato-graph-bar/memory-info-file)
		   (split-string (buffer-string) "\n" t)))))
    (mapcar (lambda (x)
	      (rplacd x
		      (string-to-number (car (split-string (cadr x))))))
	    mem-info-list)
    (remove-if-not (lambda (x) (member x neato-graph-bar/memory-fields-to-keep))
		   mem-info-list :key #'car)))

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
	  `(('neato-graph-bar/memory-used . ,(/ (float memory-used)
						memory-total))
	    ('neato-graph-bar/memory-buffer . ,(/ (float memory-buffers)
						 memory-total))
	    ('neato-graph-bar/memory-cache . ,(/ (float memory-cached)
						 memory-total))))
	 (memory-end-text (neato-graph-bar/create-storage-status-text
				  memory-used
				  memory-total)))
    (neato-graph-bar/draw-graph " Mem" memory-graph-alist memory-end-text)))

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
	  `(('neato-graph-bar/memory-used . ,(/ (float swap-used) swap-total))
	    ('neato-graph-bar/memory-cache . ,(/ (float swap-cached)
							swap-total))))
	 (swap-end-text (neato-graph-bar/create-storage-status-text
			 swap-used
			 swap-total)))
    (neato-graph-bar/draw-graph "Swap" swap-graph-alist swap-end-text)))
