;;; ========================================================
;;;                   Dietrich Bollmann, Kamakura 2008/12/09
;;; 
;;; blender-profiles.el
;;; 
;;; Dynamic menu with blender profiles to choose from...
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
;;; Variables
;;; --------------------------------------------------------

(defcustom blender:blender-profiles 
  `(("Default" ;; name
     "")       ;; profile path - when no profile is given, Blender creates the default scene
    ("3x3+1 Perspective and Button Window"
     ,(concat blender-python-mode-installation-dir "/blender-profiles/blender-profile.3x3+1.perspective.button-window"))
    ("3x3+0 Perspective"
     ,(concat blender-python-mode-installation-dir "/blender-profiles/blender-profile.3x3+0.perspective"))
    ("3x3+1 Top Button Window"
     ,(concat blender-python-mode-installation-dir "/blender-profiles/blender-profile.3x3+1.top.button-window"))
    )
  "*Different blender profiles to use at the place of the standard blender..."
  :type '(repeat
          (list
           (string  :tag "Name")
           (string  :tag "Profile Path")))
  :group 'blender)

;;| (setq blender:blender-profiles
;;|       '(("Default" ;; name
;;|          "")       ;; profile path - when no profile is given, Blender creates the default scene
;;|         ("3x3+1 Perspective and Button Window"
;;|          "~/fg/blender/profiles/blender-profile.3x3+1.perspective.button-window")
;;|         ("3x3+0 Perspective"
;;|          "~/fg/blender/profiles/blender-profile.3x3+0.perspective")
;;|         ("3x3+1 Top Button Window"
;;|          "~/fg/blender/profiles/blender-profile.3x3+1.top.button-window")
;;|         ))

;;; blender:blender-profiles
;;; (customize-variable 'blender:blender-profiles)
;;; for customize :type examples try: grep-emacs-sources :type | g list

;;; --------------------------------------------------------

(defcustom blender:blender-profile-default (first (first blender:blender-profiles)) ;; default is the first one defined
  "*The default blender profile to use when starting up."
  ;;| :type '(radio
  ;;|         (string :tag "Default"                             "Default")
  ;;|         (string :tag "3x3+1 Perspective and Button Window" "3x3+1 Perspective and Button Window")
  ;;|         (string :tag "3x3+0 Perspective"                   "3x3+0 Perspective")
  ;;|         (string :tag "3x3+1 Top Button Window"             "3x3+1 Top Button Window")
  ;;|         (function :tag "Other"))
  :type `(radio
          ,@(loop for variant in blender:blender-profiles
                  for name  = (first variant)
                  for value = name
                  collect `(string :tag ,name ,value))
          (function :tag "Other"))
  :group 'blender)

;;; (customize-variable 'blender:blender-profile-default)

;;; ========================================================
;;; Variables
;;; --------------------------------------------------------

(defvar blender:blender-profile nil
  ;; dynamically set from `blender:blender-profile-default' whenever the file is loaded
  "*The blender profile used when starting up.")

;;; ========================================================
;;; Set blender profile
;;; --------------------------------------------------------

(defun blender:set-blender-profile (profile-name)
  "Set the blender profile to use when starting the blender server."
  (setq blender:blender-profile profile-name))

;;; ========================================================
;;; Setting the default profile
;;; --------------------------------------------------------

;; setting it to the derault
;; every time when reading the file
(blender:set-blender-profile blender:blender-profile-default)

;;; ========================================================
;;; Get blender profile
;;; --------------------------------------------------------

(defun blender:get-blender-profile ()
  "Get the blender profile to use when starting the blender server."
  blender:blender-profile)
  
;;| (blender:get-blender-profile)
;;| (blender:set-blender-profile "3x3+1 Perspective and Button Window")
;;| (blender:get-blender-profile)
;;| (blender:set-blender-profile "3x3+0 Perspective")
;;| (blender:get-blender-profile)
;;| (blender:set-blender-profile "3x3+1 Top Button Window")
;;| (blender:get-blender-profile)
;;| (blender:set-blender-profile "Default")
;;| (blender:get-blender-profile)

;;; ========================================================
;;; Getters
;;; --------------------------------------------------------

(defun blender:get-blender-profile-settings ()
  "Get the settings for the currently active blender profile."
  (let ((profile-name blender:blender-profile))
        (loop for profile in blender:blender-profiles
              when (string-equal profile-name (first profile)) return profile)))

;;; (blender:get-blender-profile-settings)

;;; --------------------------------------------------------

(defun blender:get-blender-profile-name ()
  "Returns the name of the currently active blender profile."
  (nth 0 (blender:get-blender-profile-settings)))

;;| (blender:get-blender-profile-name)
;;| (blender:set-blender-profile "3x3+1 Perspective and Button Window")
;;| (blender:get-blender-profile-name)
;;| (blender:set-blender-profile "3x3+0 Perspective")
;;| (blender:get-blender-profile-name)
;;| (blender:set-blender-profile "3x3+1 Top Button Window")
;;| (blender:get-blender-profile-name)
;;| (blender:set-blender-profile "Default")
;;| (blender:get-blender-profile-name)

;;; --------------------------------------------------------

(defun blender:get-blender-profile-path ()
  "Returns the bin dir of the currently active blender profile."
  (nth 1 (blender:get-blender-profile-settings)))

;;| (blender:get-blender-profile-path)
;;| (blender:set-blender-profile "3x3+1 Perspective and Button Window")
;;| (blender:get-blender-profile-path)
;;| (blender:set-blender-profile "3x3+0 Perspective")
;;| (blender:get-blender-profile-path)
;;| (blender:set-blender-profile "3x3+1 Top Button Window")
;;| (blender:get-blender-profile-path)
;;| (blender:set-blender-profile "Default")
;;| (blender:get-blender-profile-path)

;;; ========================================================
;;; Get the currently active provile argument
;;; --------------------------------------------------------

(defun blender:get-blender-profile-argument-list ()
  "Returns the currently active blender profile.

The result is returned as a list which can be inserted into the
blender server startup argument list.

When the path of the currently active profile is \"\" this
results in an empty list returned which makes blender to open the
default blender cube scene."
  (let ((profile (blender:get-blender-profile-path)))
    (if (string-equal profile "")
        nil
      (list (expand-file-name profile)))))

;;| (blender:get-blender-profile-argument-list)
;;| (blender:set-blender-profile "3x3+1 Perspective and Button Window")
;;| (blender:get-blender-profile-argument-list)
;;| (blender:set-blender-profile "Default")
;;| (blender:get-blender-profile-argument-list)

;;; ========================================================
;;; Functions
;;; --------------------------------------------------------

(defun blender:format-blender-profiles-submenu ()
  "Format the blender profile entries in variable
`blender-profiles' for the blender menu."
  `("Blender Profiles"
    ,@(loop for (name profile-path) in blender:blender-profiles
            collect `[,name (blender:set-blender-profile ,name)
                            :style radio :selected (string-equal (blender:get-blender-profile) ,name)])))

;; (blender:format-blender-profiles-submenu)

;;; ========================================================

(provide 'blender-profiles)

;;; ========================================================
;;; ========================================================

;;; fin.
