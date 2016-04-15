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
