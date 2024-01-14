;;; my_move_mode.el --- a package for remapping movement keys           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Yurkanin

;; Author: Justin Yurkanin <yurkanin321@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

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

;;; Commentary: Damn why would you do this?

;; This is a minor to what make even go more like, sorta like vim

;;; Keymap
(defvar my-move-mode-map (make-sparse-keymap) "my-move-mode keymap")
(define-key my-move-mode-map (kbd "M-h") 'left-char)
(define-key my-move-mode-map (kbd "M-j") 'next-line)
(define-key my-move-mode-map (kbd "M-k") 'previous-line)
(define-key my-move-mode-map (kbd "M-l") 'right-char)

(define-key my-move-mode-map (kbd "M-H") 'left-word)
(define-key my-move-mode-map (kbd "M-J") 'forward-paragraph)
(define-key my-move-mode-map (kbd "M-K") 'backward-paragraph)
(define-key my-move-mode-map (kbd "M-L") 'right-word)



(define-minor-mode my-move-mode
  "Toggle My Movement mode.
This enables vim style hjkl movement in emacs."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter "My Move"
  ;; The minor mode bindings.
  :keymap my-move-mode-map
 )

;;; (provide 'my-move-mode)

;;; my_move_mode.el ends here
