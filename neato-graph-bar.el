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
  (let* ((memory-info (neato-graph-bar/get-memory-info))
	 (memory-total
	  (neato-graph-bar/get-memory-attribute memory-info "MemTotal"))
	 (memory-free
	  (neato-graph-bar/get-memory-attribute memory-info "MemFree"))
	 (memory-buffers-cache
	  (+ (neato-graph-bar/get-memory-attribute memory-info "Buffers")
	     (neato-graph-bar/get-memory-attribute memory-info "Cached")))
	 ;; Side note - it appears that there is a difference of 20MB (when I
	 ;; tested) between `(- memory-total memory-free memory-buffers-cache)`
	 ;; and the result of `(- memory-total memory-available)`... no idea
	 ;; *why*. This 20MB is likely kernel memory, I bet. Anyways, my
	 ;; reference, the file 'proc/sysinfo.c' from procps-ng uses the first
	 ;; form, so I do here as well.
	 (memory-used
	  (- memory-total memory-free memory-buffers-cache))
	 (win-width (- (window-body-width) 2))
	 (graph-fill-used
	  (round (* (/ (float memory-used) memory-total) win-width)))
	 (graph-fill-buffers-cache
	  (round (* (/ (float memory-buffers-cache) memory-total) win-width))))
    (insert "[")
    (insert (make-string graph-fill-used ?|))
    (insert (make-string graph-fill-buffers-cache ?|))
    (insert (make-string (- win-width
			    graph-fill-used
			    graph-fill-buffers-cache) ?\s))
    (insert "]\n")
    ))
