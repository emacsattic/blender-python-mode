;;; ========================================================
;;;                   Dietrich Bollmann, Kamakura 2009/05/15
;;; 
;;; blender-geometry.el
;;; 
;;; Dynamic menu with blender geometry values to choose from...
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

;;; ========================================================
;;; Variables
;;; --------------------------------------------------------

(defcustom blender:blender-geometry-values
  '("300x400+0+100"
    "370x500+0+450"
    "1015x740+0+0"
    "1200x800+0+100"
    )
  "*Different geometry values for the blender server."
  :type '(repeat
          (string :tag "Geometry"))
  :group 'blender)

;;| (setq blender:blender-geometry-values
;;|   '("370x500+0+450"
;;|     "1015x740+0+0"
;;|     ))

;;; blender:blender-geometry-values
;;; (customize-variable 'blender:blender-geometry-values)

;;; --------------------------------------------------------

(defcustom blender:blender-geometry-default (first blender:blender-geometry-values) ;; default is the first one defined
  "*The default blender geometry to use when starting up."
  ;;| :type '(radio
  ;;|         (string :tag "370x500+0+450" "370x500+0+450")
  ;;|         (string :tag "1015x740+0+0"  "1015x740+0+0")
  ;;|         (function :tag "Other"))
  :type `(radio
          ,@(loop for variant in blender:blender-geometry-values
                  for name  = variant
                  for value = variant
                  collect `(string :tag ,name ,value))
          (function :tag "Other"))
  :group 'blender)

;;; (customize-variable 'blender:blender-geometry-default)

;;; ========================================================
;;; Variables
;;; --------------------------------------------------------

(defvar blender:blender-geometry nil
  ;; dynamically set from `blender:blender-geometry-default' whenever the file is loaded
  "*The blender geometry used when starting up.")

;;; ========================================================
;;; Set blender geometry
;;; --------------------------------------------------------

(defun blender:set-blender-geometry (geometry)
  "Set the blender geometry to use when starting the blender server."
  (setq blender:blender-geometry geometry))

;;; ========================================================
;;; Setting the default geometry
;;; --------------------------------------------------------

;; setting it to the default
;; every time when reading the file
(blender:set-blender-geometry blender:blender-geometry-default)

;;; ========================================================
;;; Get blender geometry
;;; --------------------------------------------------------

(defun blender:get-blender-geometry ()
  "Get the blender geometry to use when starting the blender server."
  blender:blender-geometry)
  
;;| (blender:get-blender-geometry)
;;| (blender:set-blender-geometry "370x500+0+450")
;;| (blender:get-blender-geometry)
;;| (blender:set-blender-geometry "1015x740+0+0")
;;| (blender:get-blender-geometry)

;;; ========================================================
;;; Functions
;;; --------------------------------------------------------

(defun blender:format-blender-geometry-submenu ()
  "Format the blender geometry entries in variable
`blender-geometry' for the blender menu."
  `("Blender Geometry"
    ,@(loop for geometry in blender:blender-geometry-values
            collect `[,geometry (blender:set-blender-geometry ,geometry)
                                :style radio :selected (string-equal (blender:get-blender-geometry) ,geometry)])
    "---"
    ["Customize the list of Blender geometry values" (customize-variable 'blender:blender-geometry-values) t]
    ["Customize the default geometry value"          (customize-variable 'blender:blender-geometry-default) t]))

;; (blender:format-blender-geometry-submenu)

;;; ========================================================

(provide 'blender-geometry)

;;; ========================================================
;;; ========================================================

;;; fin.
