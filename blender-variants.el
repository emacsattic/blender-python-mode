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
(require 'bpm-utilities)

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

;;| ;; for testing purpose:
;;| (setq blender:blender-variants
;;|   '(("Blender Variant 1" ;; name
;;|      ""   ;; empty bin-dir => use blender version found on the path
;;|      t    ;; blash defined
;;|      t    ;; --geometry option defined
;;|      t    ;; --command-port (t) vs. --bcp (nil) option
;;|      t)   ;; --debug (t) vs. --debug-command-port (nil) option
;;|     ("Blender Variant 2" ;; name
;;|      "path/blender/variant/2" ;; bin-dir
;;|      nil  ;; blash defined
;;|      nil  ;; --geometry option defined
;;|      nil  ;; --command-port (t) vs. --bcp (nil) option
;;|      nil) ;; --debug (t) vs. --debug-command-port (nil) option
;;|     ))

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

;;; ========================================================
;;; Functions
;;; --------------------------------------------------------

(defun blender:format-blender-variants-submenu ()
  "Format the blender variants entries in variable
`blender-variants' for the blender menu."
  `("Blender Variants"
    ,@(loop for (name bin-dir blurbs) in blender:blender-variants
            collect `[,name (blender:set-blender-variant ,name)
                            :style radio :selected (string-equal blender:blender-variant ,name)])
    "---"
    ["Customize the list of Blender variants"        (customize-variable 'blender:blender-variants) t]
    ["Customize the default Blender variant"         (customize-variable 'blender:blender-variant-default) t]))

;;; (blender:format-blender-variants-submenu)

;;; ========================================================
;;; Getters
;;; --------------------------------------------------------

(defun blender:set-blender-variant (variant-name)
  "Set the blender variant to use when starting the blender server."
  (interactive) 
  ;; setting the global blender variant variable
  (setq blender:blender-variant variant-name))

;; (blender:set-blender-variant "Blender Variant 1")

;;; --------------------------------------------------------

(defun blender:get-blender-variant ()
  "Returns the name of the currently active blender variant."
  blender:blender-variant)

;;;                                                      (blender:get-blender-variant)
;;; (let ((blender:blender-variant "Blender Variant 1")) (blender:get-blender-variant))
;;; (let ((blender:blender-variant "Blender Variant 2")) (blender:get-blender-variant))

;;; --------------------------------------------------------

(defun blender:get-blender-variant-settings ()
  "Get the settings for the currently active blender variant."
  
  (let ((variant-name (blender:get-blender-variant)))

    (loop for variant in blender:blender-variants
          when (string-equal variant-name (first variant)) return variant)))

;;; (let ((blender:blender-variant "Blender"))                  (blender:get-blender-variant-settings))
;;; (let ((blender:blender-variant "Blender Variant 1"))     (blender:get-blender-variant-settings))
;;; (let ((blender:blender-variant "Blender Formgames Python")) (blender:get-blender-variant-settings))
;;; (let ((blender:blender-variant "Blender Geometry Option"))  (blender:get-blender-variant-settings))

;;; --------------------------------------------------------

(defun blender:get-blender-variant-bin-dir ()
  "Returns the bin dir of the currently active blender variant."
  (let ((variant-settings (blender:get-blender-variant-settings)))
    (nth 1 variant-settings)))

;;;                                                      (blender:get-blender-variant-bin-dir)
;;; (let ((blender:blender-variant "Blender Variant 1")) (blender:get-blender-variant-bin-dir))
;;; (let ((blender:blender-variant "Blender Variant 2")) (blender:get-blender-variant-bin-dir))

;;; --------------------------------------------------------

(defun blender:blender-variant-blash-implemented-p ()
  "Returns true if the currently active blender variant
implements the command port and blash."
  (let ((variant-settings (blender:get-blender-variant-settings)))
    (nth 2 variant-settings)))

;;;                                                      (blender:blender-variant-blash-implemented-p)
;;; (let ((blender:blender-variant "Blender Variant 1")) (blender:blender-variant-blash-implemented-p))
;;; (let ((blender:blender-variant "Blender Variant 2")) (blender:blender-variant-blash-implemented-p))

;;; --------------------------------------------------------

(defun blender:blender-variant-geometry-option-defined-p ()
  "Returns true if the `--geometry' option is defined for the currently active blender variant."
  (let* ((variant-settings (blender:get-blender-variant-settings)))
    (nth 3 variant-settings)))

;;;                                                      (blender:blender-variant-geometry-option-defined-p)
;;; (let ((blender:blender-variant "Blender Variant 1")) (blender:blender-variant-geometry-option-defined-p))
;;; (let ((blender:blender-variant "Blender Variant 2")) (blender:blender-variant-geometry-option-defined-p))

;;; --------------------------------------------------------

(defun blender:blender-variant-command-port-option-defined-p ()
  "Returns true if the `--command-port' option is defined for the currently active blender variant."
  (let* ((variant-settings (blender:get-blender-variant-settings)))
    (nth 4 variant-settings)))

;;;                                                      (blender:blender-variant-command-port-option-defined-p)
;;; (let ((blender:blender-variant "Blender Variant 1")) (blender:blender-variant-command-port-option-defined-p))
;;; (let ((blender:blender-variant "Blender Variant 2")) (blender:blender-variant-command-port-option-defined-p))

;;; --------------------------------------------------------

(defun blender:blender-variant-debug-option-defined-p ()
  "Returns true if the `--debug' option is defined for the currently active blender variant."
  (let* ((variant-settings (blender:get-blender-variant-settings)))
    (nth 5 variant-settings)))

;;;                                                      (blender:blender-variant-debug-option-defined-p)
;;; (let ((blender:blender-variant "Blender Variant 1")) (blender:blender-variant-debug-option-defined-p))
;;; (let ((blender:blender-variant "Blender Variant 2")) (blender:blender-variant-debug-option-defined-p))

;;; ========================================================
;;; get paths to blender server and blash binaries
;;; --------------------------------------------------------

(defun blender:get-blender-command ()
  "Returns the path of the currently active blender variant command - 
or simpley `blender' if no bin dir is defined for the current blender variant."
  (let ((bin-dir (blender:get-blender-variant-bin-dir)))
    (if (string-equal bin-dir "")
        "blender"
      (concat (expand-file-name bin-dir) "/blender"))))

;;;                                                      (blender:get-blender-command)
;;; (let ((blender:blender-variant "Blender Variant 1")) (blender:get-blender-command))
;;; (let ((blender:blender-variant "Blender Variant 2")) (blender:get-blender-command))

;;; --------------------------------------------------------

(defun blender:get-blash-command ()
  "Returns the path of the blash command which corresponds to
the currently active blender variant - or simpley `blash' if no bin dir is
  defined for the current blender variant."
  (let ((bin-dir (blender:get-blender-variant-bin-dir)))
    (if (string-equal bin-dir "")
        "blash"
      (concat (expand-file-name bin-dir) "/blash"))))

;;;                                                      (blender:get-blash-command)
;;; (let ((blender:blender-variant "Blender Variant 1")) (blender:get-blash-command))
;;; (let ((blender:blender-variant "Blender Variant 2")) (blender:get-blash-command))

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
      
      ;; it does
      (list "--geometry" geometry)
    
    ;; it does not
    (destructuring-bind (dimension-x dimension-y offset-x offset-y) 
          (blender:parse-geometry geometry)
      
      (list "-p" offset-x offset-y dimension-x dimension-y))))

;;;                                                      (blender:format-geometry-option "1000x800+50+100")
;;; (let ((blender:blender-variant "Blender Variant 1")) (blender:format-geometry-option "1000x800+50+100"))
;;; => ("--geometry" "1000x800+50+100")
;;; (let ((blender:blender-variant "Blender Variant 2")) (blender:format-geometry-option "1000x800+50+100"))
;;; => ("-p" "+50" "+100" "1000" "800")
 
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

;;;                                                      (blender:format-command-port-option 1)
;;; (let ((blender:blender-variant "Blender Variant 1")) (blender:format-command-port-option 2))
;;; (let ((blender:blender-variant "Blender Variant 2")) (blender:format-command-port-option 3))

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

;;;                                                      (blender:format-command-port-option 1)
;;; (let ((blender:blender-variant "Blender Variant 1")) (blender:format-command-port-option 2))
;;; (let ((blender:blender-variant "Blender Variant 2")) (blender:format-command-port-option 3))

;;; ========================================================

(provide 'blender-variants)

;;; ========================================================
;;; ========================================================

;;; fin.
