;;; gtde.el --- Getting Things Done in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Ben Moon
;; Author: Ben Moon <software@guiltydolphin.com>
;; URL: https://github.com/GuiltyDolphin/gtde
;; Git-Repository: git://github.com/GuiltyDolphin/gtde.git
;; Created: 2020-05-03
;; Version: 0.0.0
;; Keywords: outlines, tools
;; Package-Requires: ((dash "2.18.1") (emacs "25.1") (org "9.3") (transient "0.3.0"))

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

;; gtde provides tools for Getting Things Done in Emacs.
;;
;; For more information see the README.

;;; Code:


(require 'gtde-transient)

(require 'gtde-oo)

(require 'gtde-ui)


(provide 'gtde)
;;; gtde.el ends here
