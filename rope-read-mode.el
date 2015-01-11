;;; rope-read-mode.el --- Rearrange lines to read text smoothly

;; Copyright 2015 Marco Wahl

;; Author: Marco Wahl <marcowahlsoft@gmail.com>
;; Maintainer: Marco Wahl <marcowahlsoft@gmail.com>
;; Created: 4 Jan 2015
;; Version: 0.0
;; Keywords: convenience
;; URL: https://github.com/marcowahl/rope-read-mode

;; This file is not part of Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;; ** Usage

;;  Type M-x rope-read-mode in a buffer and wait until the
;;  transformation of the buffer has been performed.
;;  
;;  You can use:

;;  SPC/<backspace> to scroll a screen.
;;  v,<return>/V y to scroll one line.
;;  q to quit.
;;  g to refresh rope read for the current visible part of the buffer.
;;  ? open the help buffer.

;;  For convenience command rope-read-mode can be associated to a key
;;  sequence.  For example to activate or deactivate rope-read-mode by
;;  pressing scroll lock two times use the line

;;  (global-set-key (kbd "<Scroll_Lock> <Scroll_Lock>") 'rope-read-mode)

;; ** Effect

;;  rope-read-mode reverses every other line in the visible part of a
;;  buffer.  When every other line has been reversed reading is like
;;  following a rope.

;; ** Benefit

;;  - Find the next line easily.
;;    - In particular for long lines.
;;  - Have an alternative view on text.
;;  - Have fun.

;; ** Price

;;  - Effort to learn read upside-down lines easily.

;; ** Links

;;  See elpa package 'spray' for another alternative view mode.

;;  See package 'fliptext' for an
;;      ˙ʇxǝʇ pǝddılɟ ɹoɟ poɥʇǝɯ-ʇnduı

;; ** Development

;; *** Bugs

;; - rope-read-mode fails when truncated lines occur.

;; *** Features

;; - Make the transformation quicker.

;; *** Lentic Literate Style

;; This program is in literal style based on lentic-mode.  The
;; lentic-views are emacs lisp and Org.

;; A possible initialization of lentic is this:

;; (load-library "lentic-org")
;; (global-lentic-start-mode)

;; The el file is the source.  It is an emacs lisp file.

;; The el file fits to the lentic-mode, hopefully.  Use
;; lentic-mode-split-window-below to get a view on the program as Orgmode
;; file.

;; ** Communication

;; Contact the author for feedback, bug reports, feature requests,
;; enhancements, ideas.
;; ** Contributors

;; Syohei YOSHIDA
;; Marco WAHL

;;; Code:
;; ** Variables
;; #+BEGIN_SRC emacs-lisp
(defcustom rope-read-indication-string-for-reversed-line
  ""
  "Suffix to indicate a reversed line.  E.g. '<' or '⟵'."
  :group 'rope-read)
(defvar rope-read-overlays nil
  "List of rope-read-overlays.")

(defvar rope-read-olimid-next-unused 0
  "Overlay-image-id that has not been used yet.

  The program must reset this variable reasonably when an id gets
  used.")

(defvar rope-read-image-overlay-path "~/.emacs.d/rope-reading/"
  "Path where the overlay images get stored.")

(defvar rope-read-image-overlay-filename-format-string
  (concat (file-name-directory rope-read-image-overlay-path) "%d.png")
  "Template for the filenames to be written to disk.")
(defvar rope-read-mode nil)
(make-variable-buffer-local 'rope-read-mode)

(defvar rope-read-old-buffer-read-only)
(make-variable-buffer-local 'rope-read-old-buffer-read-only)
;; #+END_SRC
;; ** Keys
;; #+BEGIN_SRC emacs-lisp
(defvar rope-read-mode-hook nil)
(defvar rope-read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'rope-read-next-page)
    (define-key map (kbd "<backspace>") 'rope-read-prev-page)
    (define-key map (kbd "<return>") 'rope-read-scroll-up-line)
    (define-key map "v" 'rope-read-scroll-up-line)
    (define-key map "y" 'rope-read-scroll-down-line)
    (define-key map "V" 'rope-read-scroll-down-line)
    (define-key map "g" 'rope-read-refresh)
    (define-key map "q" 'rope-read-quit)
    (define-key map "?" 'describe-mode)
    map)
  "Keymap for rope-read-mode.")
;; #+END_SRC
;; ** Mode rope-read
;; #+BEGIN_SRC emacs-lisp
;;;###autoload
(define-minor-mode rope-read-mode
  "Rope Reading mode.

In rope-read-mode every other line gets reversed.  rope-read-mode is a
view only mode.

\\{rope-read-mode-map}

This mode can help to save eye movements.

By reversing every other line the reader often just can dip the
gaze at the end of a line to read on instead of doing the
annoying search for the next line at the other side of the text."
  :lighter " rope-read" :keymap rope-read-mode-map
  (if rope-read-mode (rope-read-mode-enable) (rope-read-mode-disable)))

(defun rope-read-mode-enable ()
  (unless (file-exists-p rope-read-image-overlay-path)
    (make-directory rope-read-image-overlay-path))
  (setq rope-read-mode t
        rope-read-old-buffer-read-only buffer-read-only
        buffer-read-only t)
  (rope-read-reol-in-visible-buffer-part-with-images)
  (run-hooks 'rope-read-mode-hook))

(defun rope-read-mode-disable ()
  (rope-read-delete-overlays)
  (setq rope-read-mode nil
        buffer-read-only rope-read-old-buffer-read-only))
;; #+END_SRC
;; ** Management and Navigation in Org
;; #+BEGIN_SRC emacs-lisp
(defun rope-read-delete-overlays ()
  "Delete all overlays currently used with the rope-read-feature."
  (interactive)
  (mapc #'delete-overlay rope-read-overlays)
  (setq rope-read-overlays nil))

(defun rope-read-next-page ()
  (interactive)
  (rope-read-delete-overlays)
  (scroll-up-command)
  (redisplay t)
  (rope-read-reol-in-visible-buffer-part-with-images))

(defun rope-read-prev-page ()
  (interactive)
  (rope-read-delete-overlays)
  (scroll-down-command)
  (redisplay t)
  (rope-read-reol-in-visible-buffer-part-with-images))

(defun rope-read-scroll-line (n)
  "Scroll the buffer N lines and reverse every other visible line."
  (rope-read-delete-overlays)
  (scroll-up-line n)
  (redisplay t)
  (rope-read-reol-in-visible-buffer-part-with-images))

(defun rope-read-scroll-up-line (n)
  "Scroll the buffer up N lines and reverse every other visible line.

  E.g.  for N = 1 the second-line becomes first."
  (interactive "p")
  (unless n (setq n 1))
  (rope-read-scroll-line n))

(defun rope-read-scroll-down-line (n)
  "Scroll the buffer down N lines and reverse every other line.

  E.g.  for N = 1 the first-line becomes second."
  (interactive "p")
  (unless n (setq n 1))
  (rope-read-scroll-line (- n)))

(defun rope-read-refresh ()
  "Refresh the rope-read-representation for the given window."
  (interactive)
  (rope-read-delete-overlays)
  (redisplay t)
  (rope-read-reol-in-visible-buffer-part-with-images))

(defun rope-read-quit ()
  (interactive)
  (rope-read-mode 'toggle))  
;; #+END_SRC

;; ** Snap the line which containts point

;; For testing: (local-set-key (kbd "<f8>")
;; 'rope-read-snap-a-line-under-olimid-filename)
;; #+BEGIN_SRC emacs-lisp
(defun rope-read-height-of-a-line ()
  "Return the height of the line that contains `(point)'."
  (save-excursion
    (let* ((end (progn (end-of-line) (point)))
           (height (progn
                     (beginning-of-line)
                     (cdr (nth 9 (posn-at-point (point))))) ; empty line default
                   ))
      (while (progn (forward-char)
                    (< (point) end))
        (setq height (max height (cdr (nth 9 (posn-at-point (point)))))))
      height)))

(defun rope-read-snap-a-line-under-olimid-filename ()
  "Snapshot the line that contains `(point)'.

Also consider the line above the line containing `(point)'.  If
the line above is longer then extend the snapshot to use the
length of the line above.

Rationale: This often eases continuation of reading for short lines.

The file name for the snapshot containing the number
`rope-read-olimid-next-unused' as index."
  (interactive "P")
  (save-excursion
    (let* ((beg (progn (beginning-of-line) (point)))
           (end (progn (end-of-line) (point)))
           (end-above (progn  (end-of-line 0) (point)))
           (width (- (max
                      (car (posn-x-y (posn-at-point end)))
                      (car (posn-x-y (posn-at-point end-above))))
                     (car (posn-x-y (posn-at-point beg)))))
           (height (cdr (nth 9 (posn-at-point beg))))
           (x-win-left (nth 0 (window-inside-pixel-edges)))
           (y-win-top (nth 1 (window-inside-pixel-edges)))
           (y-pos-line (cdr (posn-x-y (posn-at-point end))))
           (x-anchor (+ x-win-left))
           (y-anchor (+ y-win-top y-pos-line)))
      (call-process
       "convert" nil nil nil
       (format "x:%s[%dx%d+%d+%d]"
               (frame-parameter nil 'window-id)
               width height x-anchor y-anchor)
       "-flip"
       "-flop"
       (expand-file-name
        (format
         rope-read-image-overlay-filename-format-string
         ((lambda ()
            (1-
             (setq
              rope-read-olimid-next-unused
              (1+ rope-read-olimid-next-unused)))))))))))
;; #+END_SRC
;; ** Revers every other line

;; #+BEGIN_SRC emacs-lisp
(defun rope-read-advance-one-line ()
  (forward-line 1))

(defun rope-read-reol-in-visible-buffer-part-with-images ()
  "Reversal of every other line (reol) in the visible part.

  Results are quite good for font Courier and ProFont.

   BUG: [2014-01-24 Fri 12:43] There is an additional
   one-pixel-line for each image for several fonts, but not
   Courier.  It makes the text longer when using the
   rope-read-feature.  No idea, how to get rid of it."
  (interactive)
  (save-excursion
    (let* ((first-line
            (progn (move-to-window-line 0) 
                   (point)))
           (last-line
            (progn (move-to-window-line -1)
                   (beginning-of-line)
                   (point)))
           (toggle t)
           (olimid-start rope-read-olimid-next-unused)
           (olimid-current olimid-start))
      ;; pass 1: create the images.
      (goto-char first-line)
      (rope-read-advance-one-line)
      (while (and (<= (point) last-line) (< (point) (point-max)))
        (if toggle
            (rope-read-snap-a-line-under-olimid-filename))
        (setq toggle (not toggle))
        (rope-read-advance-one-line))
      ;; pass 2: insert the images as overlays.
      (goto-char first-line)
      (rope-read-advance-one-line)
      (setq toggle t)
      (while (and (<= (point) last-line) (< (point) (point-max)))
        (if toggle
            (let ((l-beg (progn (beginning-of-line) (point)))
                  (l-end (progn (end-of-line) (point))))
              (setq rope-read-overlays
                    (cons (make-overlay l-beg l-end)
                          rope-read-overlays))
              (overlay-put
               (car rope-read-overlays) 'display
               (create-image
                (expand-file-name
                 (format 
                  rope-read-image-overlay-filename-format-string
                  olimid-current))
                nil nil
                :ascent 'center
                ;; TODO: try to refine.  hint: try
                ;; understand.  is this a font-dependent
                ;; thing?  e.g. :ascent 83 is possible.
                ;; there are further attributes...
                ))
              (setq olimid-current (1+ olimid-current))
              (overlay-put
               (car rope-read-overlays)
               'after-string rope-read-indication-string-for-reversed-line)))
        (setq toggle (not toggle))
        (rope-read-advance-one-line)))))
;; #+END_SRC

;; ** Provide the file as library

;; #+BEGIN_SRC emacs-lisp
(provide 'rope-read-mode)
;; #+END_SRC
;;; Tail:

;; # Local Variables:
;; # lentic-init: lentic-orgel-org-init
;; # End:

;;; rope-read-mode.el ends here
