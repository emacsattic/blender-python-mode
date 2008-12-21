;;; ========================================================
;;; bpm-utilities.el     Dietrich Bollmann, Tokyo 2008/12/21
;;; 
;;; Utility functions used by the Blender Python Mode.
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

(defun* bpm:message (&rest args)
  "Print a message prefixed with [Blender Python Mode]."
  (let ((message (apply 'format args)))
    (message (concat "[Blender Python Mode] " message))))

;;; (bpm:message "Hello %s, how are %s?" "world" "you")

;; =========================================================

(provide 'bpm-utilities)

;;; ========================================================
;;; ========================================================

;;; fin.

