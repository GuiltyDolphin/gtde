;;; org-gtd-oo.el --- org-gtd object-oriented interface.

;; Copyright (C) 2021 Ben Moon
;; Author: Ben Moon <software@guiltydolphin.com>

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

;;; Commentary:
;;;
;;; Defines org-gtd classes.
;;;
;;; Code:

(require 'eieio)
(require 'org)

(defclass org-gtd--base ()
  ()
  :abstract t
  :documentation "Abstract base class for org-gtd.")

(defclass org-gtd--item (org-gtd--base)
  ((title :initarg :title
          :documentation "Title of the item.")
   (id :initarg :id
       :documentation "Unique identifier for the item. Don't change this manually."))
  :abstract t
  :documentation "Abstract class for item-like classes.")

(defclass org-gtd--has-parent-projects (org-gtd--base)
  ((superior-projects :initarg :projects
                      :documentation "Projects that contain this item."))
  :abstract t
  :documentation "Abstract class for entries that can have associated parent projects.")

(provide 'org-gtd-oo)
;;; org-gtd-oo.el ends here
