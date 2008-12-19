;;; ========================================================
;;; blender-python-mode.el      Dietrich Bollmann, Tokyo 2007/01/09
;;; 
;;; Dealing with different Blender variants...
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

(defcustom blender:blender-variants
  '(;;
    ;; current default blender (see: which blender)
    ;;
    ("Blender Command Port" ;; name
     ""         ;; empty bin-dir => use blender version found on the path
     t          ;; blash defined
     nil        ;; --geometry option defined
     t          ;; --command-port (t) vs. --bcp (nil) option
     nil)       ;; --debug (t) vs. --debug-command-port (nil) option

    ("Some Other Blender Variant :)" ;; name
     "~/path/to/blender/variants/bin/dir" ;; bin-dir
     nil        ;; blash defined
     nil        ;; --geometry option defined
     nil        ;; --command-port (t) vs. --bcp (nil) option
     nil)       ;; --debug (t) vs. --debug-command-port (nil) option

    ;;| ("Formgames Blender" ;; name
    ;;|  "~/blendev/bazaar/working/branches/blender-formgames/install/linux2" ;; bin-dir
    ;;|  t          ;; blash defined
    ;;|  nil        ;; --geometry option defined
    ;;|  t          ;; --command-port (t) vs. --bcp (nil) option
    ;;|  nil)

    )
  "*Different blender variants to use at the place of the standard blender..."
  :type '(repeat
          (list
           (string  :tag "Name")
           (string  :tag "Bin dir")
           (boolean :tag "blash")
           (boolean :tag "--geometry option")
           (boolean :tag "--command-port option")
           (boolean :tag "--debug option")))
  :group 'blender)

;;; (customize-variable 'blender:blender-variants)
;;; for customize :type examples try: grep-emacs-sources :type | g list

(defcustom blender:blender-variant-default (first (first blender:blender-variants)) ;; get name of first variant
  "*The default blender variant to use when starting up."
  ;;| :type '(radio
  ;;|         (string :tag "Blender"                  "Blender")
  ;;|         (string :tag "Blender Command Port"     "Blender Command Port")
  ;;|         ...
  ;;|         (function :tag "Other"))
  :type `(radio
          ,@(loop for variant in blender:blender-variants
                  for name  = (first variant)
                  for value = name
                  collect `(string :tag ,name ,value))
          (function :tag "Other"))
  :group 'blender)

;;; (customize-variable 'blender:blender-variant-default)

;;; ========================================================
;;; Variables
;;; --------------------------------------------------------

(defvar blender:blender-variant blender:blender-variant-default
  "*The blender variant used when starting up.")

;;; setting it every time when reading the file
(setq blender:blender-variant blender:blender-variant-default)

(defvar blender:blender-command nil ;; dynamically set by `blender:set-blender-variant'
  "*Command used to start the Blender Server.")

(defvar blender:blash-command nil ;; dynamically set by `blender:set-blender-variant'
  "*Command used to start the Blash Client.")

(defvar blender:blender-variant-setting nil ;; dynamically set by `blender:set-blender-variant'
  "*The settings of the currently active blender variant.")

;;; ========================================================
;;; Functions
;;; --------------------------------------------------------

(defun blender:format-blender-variants-submenu ()
  "Format the blender variants entries in variable
`blender-variants' for the blender menu."
  `("Blender Variants"
    ,@(loop for (name bin-dir blurbs) in blender:blender-variants
            collect `[,name (blender:set-blender-variant ,name)
                            :style radio :selected (string-equal blender:blender-variant ,name)])))

;;; (blender:format-blender-variants-submenu)

;;; ========================================================
;;; Getters
;;; --------------------------------------------------------

(defun blender:get-blender-variant-settings (variant-name)
  "Get the settings for the currently active blender variant."

  (loop for variant in blender:blender-variants
        when (string-equal variant-name (first variant)) return variant))

;;; (blender:get-blender-variant-settings "Blender")
;;; (blender:get-blender-variant-settings "Blender Command Port")
;;; (blender:get-blender-variant-settings "Blender Formgames Python")
;;; (blender:get-blender-variant-settings "Blender Geometry Option")

;;; --------------------------------------------------------

(defun blender:get-blender-variant-name ()
  "Returns the name of the currently active blender variant."
  (nth 0 blender:blender-variant-setting))

;; (blender:get-blender-variant-name)

;;; --------------------------------------------------------

(defun blender:get-blender-variant-bin-dir ()
  "Returns the bin dir of the currently active blender variant."
  (nth 1 blender:blender-variant-setting))

;; (blender:get-blender-variant-bin-dir)

;;; --------------------------------------------------------

(defun blender:blender-variant-blash-implemented-p ()
  "Returns true if the `--geometry' option is defined for the currently active blender variant."
  (nth 2 blender:blender-variant-setting))

;; (blender:blender-variant-blash-implemented-p)

;;; --------------------------------------------------------

(defun blender:blender-variant-geometry-option-defined-p ()
  "Returns true if the `--geometry' option is defined for the currently active blender variant."
  (nth 3 blender:blender-variant-setting))

;; (blender:blender-variant-geometry-option-defined-p)

;;; --------------------------------------------------------

(defun blender:blender-variant-command-port-option-defined-p ()
  "Returns true if the `--command-port' option is defined for the currently active blender variant."
  (nth 4 blender:blender-variant-setting))

;; (blender:blender-variant-command-port-option-defined-p)

;;; --------------------------------------------------------

(defun blender:blender-variant-debug-option-defined-p ()
  "Returns true if the `--debug' option is defined for the currently active blender variant."
  (nth 5 blender:blender-variant-setting))

;; (blender:blender-variant-debug-option-defined-p)

;;; ========================================================
;;; Functions
;;; --------------------------------------------------------

(defun blender:set-blender-variant (variant-name)
  "Search `blender:blender-variants' for the blender variant with name `variant-name'
and set the corresponding values."

  (interactive)

  ;; get name, bin dir and blurbs of blender variant `variant-name'
  (let ((settings (blender:get-blender-variant-settings variant-name)))

    ;; set the blender variant settings
    (setq  blender:blender-variant-setting settings)

    (let ((name    (blender:get-blender-variant-name))
          (bin-dir (blender:get-blender-variant-bin-dir)))

      ;; setting the global blender variant variable
      (setq blender:blender-variant name)
      
      ;; set the blender command
      (setq blender:blender-command
            (if (string-equal bin-dir "")
                "blender" 
              (concat (expand-file-name bin-dir) "/blender")))
      
      ;; set the blash command
      (setq blender:blash-command
            (if (string-equal bin-dir "")
                "blash" 
              (concat (expand-file-name bin-dir) "/blash")))

      ;; inform the user
      (message "Using Blender variant `%s'  (cmd: %s)" name blender:blender-command))))

;;; (blender:set-blender-variant "Blender Formgames Python")
 
;;; ========================================================
;;; Setting the default variant
;;; --------------------------------------------------------

(blender:set-blender-variant blender:blender-variant)

;;; ========================================================
;;; format the geometry option - 
;;; depending on the `--geometry option' value of
;;; the respective `blender:blender-variants' entry
;;; --------------------------------------------------------

(defun blender:parse-geometry (geometry)
  "Parse a geometry option value.

Examples:

  (blender:parse-geometry \"1000x800+0+100\")
  (blender:parse-geometry \"1000x800-0-100\")
  (blender:parse-geometry \"1000x800\")
  (blender:parse-geometry         \"+0+100\")
"
  (string-match "^\\(\\([0-9]*\\)x\\([0-9]*\\)\\)?\\(\\([+-][0-9]*\\)\\([+-][0-9]*\\)\\)?$" geometry)
  (let ((dimension-x (or (match-string 2 geometry) "800"))
        (dimension-y (or (match-string 3 geometry) "600"))
        (offset-x    (or (match-string 5 geometry) "100"))
        (offset-y    (or (match-string 6 geometry) "100")))

    (list dimension-x dimension-y offset-x offset-y)))

;;| (blender:parse-geometry "1000x800+0+100")
;;| (blender:parse-geometry "1000x800-0-100")
;;| (blender:parse-geometry "1000x800")
;;| (blender:parse-geometry         "+0+100")

;;; --------------------------------------------------------

(defun blender:format-geometry-option (geometry)
  "Format the geometry option depending on the currently active blender variant."
  ;; does the currently active blender variant understand the geometry option?
  (if (blender:blender-variant-geometry-option-defined-p)

      (list "--geometry" geometry)

    (destructuring-bind (dimension-x dimension-y offset-x offset-y) 
        (blender:parse-geometry geometry)

      (list "-p" offset-x offset-y dimension-x dimension-y))))

;;; (blender:set-blender-variant "Blender Command Port")
;;; (blender:format-geometry-option "1000x800+50+100")
;;; => ("-p" "+50" "+100" "1000" "800")
;;; (blender:set-blender-variant "Blender Geometry Option")
;;; (blender:format-geometry-option "1000x800+50+100")
;;; => ("--geometry" "1000x800+50+100")
 
;;; ========================================================
;;; format the command-port option - 
;;; depending on the `--command-port option' value of
;;; the respective `blender:blender-variants' entry
;;; --------------------------------------------------------

(defun blender:format-command-port-option (port-number)
  "Format the command-port option depending on the currently active blender variant."
  ;; does the currently active blender variant understand the command-port option?
  (if (blender:blender-variant-command-port-option-defined-p)
      (list "--command-port" (int-to-string port-number))
    (list "--bcp" (int-to-string port-number))))

;;; (let ((blender:blender-variant-setting '(dummy dummy dummy dummy   t dummy))) (blender:format-command-port-option 9))
;;; => ("--command-port" "9")
;;; (let ((blender:blender-variant-setting '(dummy dummy dummy dummy nil dummy))) (blender:format-command-port-option 9))
;;; => ("--bcp" "9")
 
;;; ========================================================
;;; format the debug option - 
;;; depending on the `--debug option' value of
;;; the respective `blender:blender-variants' entry
;;; --------------------------------------------------------

(defun blender:format-debug-option (debug-level)
  "Format the debug option depending on the currently active blender variant."
  ;; does the currently active blender variant understand the debug option?
  (if (blender:blender-variant-debug-option-defined-p)
      (list "--debug" (format "bcp:%d" debug-level))
    (list "--debug-command-port" (int-to-string debug-level))))

;;; (let ((blender:blender-variant-setting '(dummy dummy dummy dummy dummy   t))) (blender:format-debug-option 4))
;;; => ("--debug" "bcp:4")
;;; (let ((blender:blender-variant-setting '(dummy dummy dummy dummy dummy nil))) (blender:format-debug-option 4))
;;; => ("--debug-command-port" "4")
 
;;; ========================================================

(provide 'blender-variants)

;;; ========================================================
;;; ========================================================

;;; fin.
