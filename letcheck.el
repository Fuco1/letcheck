;;; letcheck.el --- Check the erroneous assignments in let forms

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 22 Jan 2013
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/Fuco1/letcheck

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
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

;; Toggle checking of let assignments.  If point is inside a let form,
;; the variables in this let block are checked and if you reference a
;; previously defined variable in this let binding, it is highlight
;; with warning face, because you can't reference it.  You then need
;; to change the let into let*.

;; See github readme at https://github.com/Fuco1/letcheck

;;; Code:

(require 'thingatpt)

(defvar letcheck-overlays-list nil
  "List of overlays used to highlight erroneous assignments
  inside let.")

(defvar letcheck-idle-timer nil
  "Timer used to run the letcheck function.")

(defun letcheck-function ()
  "Test if point is inside let form."
  ;; remove any overlay that has letcheck property
  (dolist (ov letcheck-overlays-list) (delete-overlay ov))
  (setq letcheck-overlays-list nil)
  (save-excursion
    (let (s sexp (ok t))
      ;; first, get to the let form
      (while (and ok (setq s (syntax-ppss)))
        (if (= 0 (car s))
            (setq ok nil)
          (goto-char (cadr s))
          (setq sexp (sexp-at-point))
          (when (and sexp
                     (eq (car sexp) 'let))
            (setq ok nil))))
      (when (eq (car sexp) 'let)
        (let ((varlist (cadr sexp))
              (variables nil))
          ;; get all the variables this let defines
          (while varlist
            (if (listp (car varlist))
                (setq variables (cons (caar varlist) variables))
              (setq variables (cons (car varlist) variables)))
            (setq varlist (cdr varlist)))
          (setq variables (mapcar #'symbol-name variables))
          ;; now check all the forms inside the let to see if they use
          ;; any of the variables.  If so, we should signal an error
          (down-list 2)
          (backward-up-list)
          (save-restriction
            (widen)
            (narrow-to-region (point) (progn (forward-sexp) (point)))
            (beginning-of-buffer)
            (while (forward-symbol 1)
              ;; we don't want to highlight the first definition
              (when (and (save-excursion
                           (goto-char (match-beginning 0))
                           (save-match-data
                             (not (looking-back "("))))
                         (member (match-string 0) variables))
                (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                  (push ov letcheck-overlays-list)
                  (overlay-put ov 'face font-lock-warning-face))
                ))))))))

(define-minor-mode letcheck-mode
  "Toggle checking of let assignments.  If point is inside a let
  form, the variables in this let block are checked and if you
  reference a previously defined variable in this let binding, it
  is highlight with warning face, because you can't reference it.
  You then need to change the let into let*."
  :init-value nil
  (if letcheck-mode
      (unless letcheck-idle-timer
        (setq letcheck-idle-timer
              (run-with-idle-timer 0.125 t
                                   'letcheck-function)))
    (when letcheck-idle-timer
      (cancel-timer letcheck-idle-timer)
      (setq letcheck-idle-timer nil))
    (when letcheck-overlays-list
      (dolist (ov letcheck-overlays-list) (delete-overlay ov)))))

(provide 'letcheck)

;;; letcheck.el ends here
