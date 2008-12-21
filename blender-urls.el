;;; ========================================================
;;; blender-urls.el      Dietrich Bollmann, Tokyo 2007/05/29
;;; 
;;; URLs useful when programming Blender with Python.
;;; --------------------------------------------------------
;;; 
;;; Copyright (C) Dietrich Bollmann
;;; 
;;; Author:       Dietrich Bollmann
;;; Maintainer:   Dietrich Bollmann
;;; Keywords:     emacs, python, blender.
;;; 
;;; This file is part of the Blender Python Mode for emacs.
;;; 
;;; The Blender Python Mode is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;; 
;;; The Blender Python Mode is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.
;;; 
;;; --------------------------------------------------------

(require 'cl)
(require 'bpm-utilities)

;;; ========================================================
;;; customizable variables:
;;; --------------------------------------------------------

(defcustom blender-python-api-reference-url
  "http://www.blender.org/documentation/244PythonDoc/index.html"
  "Url of the Blender Python API Reference."
  :type 'string
  :group 'blender)

(defcustom blender-user-forums-url
  "http://www.blender.org/forum/index.php"
  "Url of the Blender User Forums."
  :type 'string
  :group 'blender)

(defcustom blender-python-forum-url
  "http://www.blender.org/forum/viewforum.php?f=9"
  "Url of the Blender Python Forum."
  :type 'string
  :group 'blender)

(defcustom blender-coding-blender-forum-url
  "http://www.blender.org/forum/viewforum.php?f=5"
  "Url of the `Coding Blender' Forum."
  :type 'string
  :group 'blender)

(defcustom blenderartists-forum-url
  "http://blenderartists.org/forum/"
  "Url of the `Blender Artist' Forums index page."
  :type 'string
  :group 'blender)

(defcustom blenderartists-works-in-progress-forum-url
  "http://blenderartists.org/forum/forumdisplay.php?f=16"
  "Url of the `Blender Artist - Works in Progress' Forum."
  :type 'string
  :group 'blender)

;;; ========================================================
;;; functions:
;;; --------------------------------------------------------

(defun blender-browse-python-api-reference ()
  "Open The Blender Python API Reference URL."
  (interactive)
  (browse-url blender-python-api-reference-url))

;; (blender-browse-python-api-reference)

;;; --------------------------------------------------------

(defun blender-browse-user-forums ()
  "Open The Blender User Forums URL."
  (interactive)
  (browse-url blender-user-forums-url))

;; (blender-browse-user-forums)

;;; --------------------------------------------------------

(defun blender-browse-python-forum ()
  "Open The Blender Python Forum URL."
  (interactive)
  (browse-url blender-python-forum-url))

;; (blender-browse-python-forum)

;;; --------------------------------------------------------

(defun blender-browse-coding-blender-forum ()
  "Open The URL of the `Coding Blender' Forum."
  (interactive)
  (browse-url blender-coding-blender-forum-url))

;; (blender-browse-coding-blender-forum)

;;; --------------------------------------------------------

(defun browse-blenderartists-forums ()
  "Open The URL of the `Blender Artist' Forums index page."
  (interactive)
  (browse-url blenderartists-forum-url))

;;; --------------------------------------------------------

(defun browse-blenderartists-works-in-progress-forum ()
  "Open The URL of the `Blender Artist - Works in Progress' Forum."
  (interactive)
  (browse-url blenderartists-works-in-progress-forum-url))

;; (browse-blenderartists-works-in-progress-forum)

;;; ========================================================

(provide 'blender-urls)

;;; ========================================================
;;; ========================================================

;;; fin.
