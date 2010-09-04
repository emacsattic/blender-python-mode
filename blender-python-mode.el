;;; ========================================================
;;; blender-python-mode.el       Dietrich Bollmann, Tokyo 2007/01/09
;;; 
;;; Emacs mode for Blender Python development.
;;; --------------------------------------------------------
;;; 
;;; Copyright (C) Dietrich Bollmann
;;; 
;;; Author:         Dietrich Bollmann
;;; Maintainer:     Dietrich Bollmann
;;; Keywords:       emacs, python, blender.
;;; Contributor(s): Marc Weber
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
(require 'python)
(require 'comint)
(require 'bpm-utilities)
(require 'bpm-shell-command)
(require 'blender-urls)
(require 'blender-variants)
(require 'blender-profiles)
(require 'blender-geometry)

;;; ========================================================
;;; customizable variables:
;;; -------------------------------------------------------- 

(defgroup blender nil
  "Support for the Blender's Python scripting."
  :group 'languages
  :prefix "blender-")

(defcustom blender-debug-level 2
  "*The Blender debug level. The value of this variable
determines the amount of debug messages printed by the Blender
server."
  :type  'integer
  :group 'blender)

(defcustom blender-ip-address "127.0.0.1"
  "*IP-address of the Blender server."
  :type  'string
  :group 'blender)
  
(defcustom blender-server-port-range '(6767 . 6776)
  "*Range of possible values for the Blender server port."
  :type '(cons (integer :tag "Minimum port number")
               (integer :tag "Maximum port number"))
  :group 'blender)

;;; the following doesn't work with 'python.el'
;;; but worked with 'python-mode.el'
;;; using python.el's 'python-buffer' now...
;;; 
;;;   | (defcustom blash-buffer "*Blender*"
;;;   |   "*The Blender shell interaction buffer."
;;;   |   :type 'string
;;;   |   :group 'blender)

(defcustom blender-output-buffer "*Blender Server*"
  "*Output buffer of the Blender Server."
  :type  'string
  :group 'blender)

(defcustom blash-debug-level 0
  "*The Blash debug level. The value of this variable determines
the amount of debug messages printed by blash."
  :type  'integer
  :group 'blender)

(defcustom blash-retries 60
  "Number of times to retry when connecting to the Blender server."
  :type  'integer
  :group 'blender)

(defcustom blash-retry-sleeptime 0.3
  "Time to sleep between connection retries in seconde."
  :type  'number
  :group 'blender)

(defcustom blender-python-mode-installation-dir "~/emacs/lisp/blender-python-mode"
  "Directory where to find the blender mode emacs-lisp sources."
  :type  'string
  :group 'blender)

(defcustom blender-icon-dir (concat blender-python-mode-installation-dir "/icons/22x22")
  "Directory where to find the icons used in the toolbar etc."
  :type  'string
  :group 'blender)
  
(defcustom blender-python-init-file-dir nil
  "Directory where to find the blender version of the file `emacs.py'
which inits the Blender python server with some the module `emacs'
containing python functions `python-mode' is built on.

The directory is supposed to contain a subdirectory for every
used python version.  In each of these subdirs a version of
`emacs.py' for the relative python version has to be located.

Example:

If `blender-python-init-file-dir' is `python' the directory should
contain the following files:

  python/2.6/emacs.py
  python/2.5/emacs.py
  python/3.1/emacs.py

The `emacs.py' files are a modified versions of the file with the
same name supplied with the `python-mode' source file
`python.el'.  And should be compared to the original file from
time to time for the case the latter has been updated.

Note:
`blender-python-init-file-dir' relies on `blender-python-mode-installation-dir'!"
  :type  'string
  :group 'blender)
(setq blender-python-init-file-dir (expand-file-name (concat blender-python-mode-installation-dir "/python")))
  
(defcustom blender:blash-buffer-height .25 ;; .15
  "Height of the blash window relative to the original window it is
split of from.

Example:

 0.33 makes the new window height 1/3 of the height of the original window."

  :type  'number
  :group 'blender)

;;; ========================================================
;;; variables:
;;; --------------------------------------------------------

(defvar blender-process-name nil
  "The name of the running blender process.
The variable is set to the process name by the funcion `start-blender-server'.")

(defvar blender-process-buffer nil
  "The output buffer of the blender process.
The variable is set to the process name by the funcion `start-blender-server'.")

(defvar blender-server-port nil
  "The port number used by the blender server currently running.
The port number is constrained by the `blender-server-port-range'
and calculated and set by the function
`get-next-blender-port-number'.")

;; =========================================================
;; Utilities
;; ---------------------------------------------------------

(defun blender:get-blender-python-init-file-dir ()
  "Get the versioned directory where to look for the `emacs.py' initialisation file.
The directory depends on the python version specified for the
current blender variant."
  (let ((python-dir  blender-python-init-file-dir)
        (python-vers (blender:get-blender-variant-python-version)))
    (concat python-dir "/" python-vers)))

;; =========================================================
;; Initialization code
;; for loading the server-side `emacs' module
;; on which the `python-mode' relies.
;; ---------------------------------------------------------
;;
;; Ensure that
;; - `blender-python-mode's `emacs.el' is on the path - and
;; - `blender-python-mode's `emacs.el' overwrites the `python-mode's `emacs.el' file
;; by making the `blender-python-mode's `emacs.el' directory
;; the first on the python system path
;; 
;; Note:
;; `blender-python-mode's `emacs.el' differs from the original one 
;; only by the possibility, to set the main dictionary 
;; used for evaluating.  Every blender client uses this
;; possibility to set the main dictionary
;; immediately after (re)connecting.
;; This is necessary as 
;; every blender client uses it's own `__main__' dictionary
;; and not the default one obtained by `import __main__'.
;;
;; !!! from time to time it should be checked,
;; if the `emacs.el' in the emacs distribution has changed.
;; And if so, blender's emacs.el should be upgraded!
;;
;; -----------------------------------------------
;;
;; Note:
;; When debugging this code it is helpful to be able to 
;; send python command's to the client bypassing 
;; `python-mode's mechanisms.  This can be done using
;; `python-send-command'.  
;; 
;; Examples - code used to develop the following init commands:
;; 
;; (python-send-command "print \"hello :)\"")
;; (python-send-command "import sys\nprint \"sys.path:\", sys.path")
;; 
;; (python-send-command 
;;  (let ((dir (expand-file-name "~/blendev/emacs/blender-python-mode/bazaar/working/trunk/blender-python-mode")))
;;    (concat "print \"# before manipulating sys.path:\", sys.path\n"
;;            "import sys\n"
;;            "if not sys.path[0] == '" dir "':\n"
;;            "    sys.path = ['" dir "'] + [d for d in sys.path if d != '" dir "']\n"
;;            ;; here is an emty line necessary
;;            ;; as `python-send-command' uses eval mode.
;;            ;; in the code used with `--init', which uses file mode,
;;            ;; this line can be scipped.
;;            "\n"
;;            "print \"# after manipulating sys.path: \", sys.path\n")))
;; 
;; (python-send-command "import emacs")
;; 
;; (python-send-command
;;  (concat "print \"# before setting python-mode's main dict:\"\n"
;;          "print \"# - python main dict:     \", emacs.__main__.__dict__.keys()\n"
;;          "print \"# - client main dict:     \", globals().keys()\n"
;;          "print \"# - python-mode main dict:\", emacs._main_dict.keys()\n"
;;          "emacs.set_main_dict(globals())\n"
;;          "print \"# after setting python-mode's main dict:\"\n"
;;          "print \"# - python main dict:     \", emacs.__main__.__dict__.keys()\n"
;;          "print \"# - client main dict:     \", globals().keys()\n"
;;          "print \"# - python-mode main dict:\", emacs._main_dict.keys()\n"))
;; 
;; -----------------------------------------------

(defun get-blender-python-init-cmd nil
  "Assembles and returns the command used to load the blender-mode version of the
python-mode initialization file `emacs.py'.

Note:

`get-blender-python-init-cmd' relies on
`blender-python-init-file-dir' which relies itself on
`blender-python-mode-installation-dir' and on the python version
the current blender variant has been specified to work with."
  (let ((dir (blender:get-blender-python-init-file-dir)))
    (concat
     ;; ensure that `blender-python-mode's `emacs.py' is the first one found by python
     "import sys\n"
     "if not sys.path[0] == '" dir "':\n"
     "    sys.path = ['" dir "'] + [d for d in sys.path if d != '" dir "']\n"
     ;; import `blender-python-mode's `emacs.py'
     "import emacs\n"
     ;; Use the blender client's main dictionary 
     "emacs.set_main_dict(globals())\n"
     ;; print an `ok' message :)
     "print(\"# Blender client initialized.\")\n"
     ;; fin.
     )))

;; (get-blender-python-init-cmd)

;; =========================================================
;; A function for inserting dynamically generated menus
;; ---------------------------------------------------------

(defun blender:blender-menu-filter (menu)
  "A function with allowes to generate submenus dynamically:

Menu items of the form:

  (:dynamic-menu ,#'<function which generates submenu>)

allow to insert submenus generated by <function which generates submenu>."

  (loop for item in menu
        ;; search for submenus to calculate dynamically
        when (and
              (listp item)
              (equal (first item) :dynamic-menu))
        do (let ((dynamic-menu-func (second item)))
             (setq item (funcall dynamic-menu-func)))
        ;; collect the other items
        collect item))

;; (blender:blender-menu-filter `(1 (:dynamic-menu ,#'blender:format-blender-profiles-submenu) 2))

;; =========================================================
;; Blender menu - requires the easymenu.el package:
;; ---------------------------------------------------------

(defvar blender-menu nil
  "Menu for the Blender version of the Python Mode.")

(defun blender:make-blender-menu ()
  "Make \"Blender\"-menu..."
  ;; (interactive)
  (let* ((name "Blender")
	 (menu (list name)))
    (require 'easymenu)
    (easy-menu-define
      blender-menu
      python-mode-map
      "Blender Mode menu"
      `(,name
        ;; :filter FUNCTION
        ;; FUNCTION is a function with one argument, the rest of menu items.
        ;; It returns the remaining items of the displayed menu.
        ;; from: (describe-function 'easy-menu-define)
        :filter blender:blender-menu-filter
        ["Execute current line"        blender-execute-current-line t]
        ["Execute region"              blender-send-region t]
        ["Execute region between `region-begin' and `region-end'"
         blender-execute-blender-region t]
        ["Execute first region between `region-begin' and `region-end' in the current file."
         blender-execute-first-blender-region t]
        ["Execute file"                blender-execute-file-menu t]
        "---"
        ["Start"                       blender:start t]
        ["Restart"                     blender:restart t]
        ["Stop"                        blender:stop t]
        "---"
        ["Start Blash"                 blash t]
        ["Reconnect to Blender server" blash-reconnect t]
        ["Restart Blash"               blash:restart-blash-client t]
        ["Stop Blash"                  blash:stop-blash-client t]
        "---"
        ["Start Blender Server"        start-blender-server t]
        ["Restart Blender Server"      blender:restart-blender-server t]
        ["Stop Blender Server"         stop-blender-server t]
        "---"
        ["Stop Blender Server and Client and kill their buffers" blender:kill-blender-buffers t]
        "---"
        ;; insert blender variants submenu
        (:dynamic-menu ,#'blender:format-blender-variants-submenu)
        ;; insert blender profiles submenu
        (:dynamic-menu ,#'blender:format-blender-profiles-submenu)
        ;; insert blender geometry submenu
        (:dynamic-menu ,#'blender:format-blender-geometry-submenu)
        "---"
        ("www"
         ["Blender Python API Reference"   blender-browse-python-api-reference t]
         "---"
         ["Blender User Forums index page" blender-browse-user-forums t]
         "---"
         ["Blender Python Forum"           blender-browse-python-forum t]
         ["`Coding Blender' Forum"         blender-browse-coding-blender-forum t]
         "---"
         ["`Blender Artist' Forums"        browse-blenderartists-forums t]
         "---"
         ["`Works in Progress' Forum"      browse-blenderartists-works-in-progress-forum t]
         )
        "---"
        ("Customization"
         ["Customize the Blender Mode settings"         customize-blender-python-mode t]
         "---"
         ["Customize the Blender Mode installation dir"   (customize-variable 'blender-python-mode-installation-dir) t]
         ["Customize the list of Blender variants"        (customize-variable 'blender:blender-variants) t]
         ["Customize the default Blender variant"         (customize-variable 'blender:blender-variant-default) t]
         ["Customize the list of Blender profiles"        (customize-variable 'blender:blender-profiles) t]
         ["Customize the default Blender profile"         (customize-variable 'blender:blender-profile-default) t]
         ["Customize the list of Blender geometry values" (customize-variable 'blender:blender-geometry-values) t]
         ["Customize the default geometry value"          (customize-variable 'blender:blender-geometry-default) t]
         ["Customize which Blender profile to use"        (customize-variable 'blender:blender-profile-default) t]
         )
        "---"
        ["Versuch" versuch t]
        "---"))
    ;; (d-diedel-add-menus menu)
    ))

;; =========================================================
;; Blender Toolbar
;; ---------------------------------------------------------

(defun blender:make-toolbar (template-map)
  "Copy `template-map', append the d-lisp mode tools to the local toolbar
and finally return the new toolbar."
  (if (display-graphic-p)
      (let ((map      (copy-keymap template-map))
            (icon-dir (concat blender-icon-dir "/")))
        ;;; 
        (define-key-after map [blender:start]
          `(menu-item "blender" blender:start
                     :help "Start Blender"
                     :image (image :type png :file ,(concat icon-dir "blender.png"))))
        ;;; 
        (define-key-after map [blender:restart]
          `(menu-item "restart-blender" blender:restart
                     :help "Restart Blender"
                     :image (image :type png :file ,(concat icon-dir "blender-restart.png"))))
        ;;; 
        (define-key-after map [blender:restart-blender-server]
          `(menu-item "restart-blender-server" blender:restart-blender-server
                     :help "Restart Blender Server"
                     :image (image :type png :file ,(concat icon-dir "blender-restart-server.png"))))
        ;;; 
        (define-key-after map [blash:restart-blash-client]
          `(menu-item "restart-blash-client" blash:restart-blash-client
                     :help "Restart Blash Client"
                     :image (image :type xpm :file ,(concat icon-dir "blash-restart.xpm"))))
        ;;| ;;; 
             ["Execute first region between `region-begin' and `region-end'"
                                            blender-execute-first-blender-region t]
        ;;; 
        (define-key-after map [blender-execute-first-blender-region]
          `(menu-item "blender-execute-first-blender-region" blender-execute-first-blender-region
                     :help "Execute first region between `region-begin' and `region-end' in the current file."
                     :image (image :type png :file ,(concat icon-dir "walking-man-green.png"))))
        ;;; 
        ;;| (define-key-after map [d-lisp-load-init-file]
        ;;|   `(menu-item "d-lisp-load-init-file" d-lisp-load-init-file 
        ;;|              :help "load the `d-lisp-init-file'."
        ;;|              :image (image :type png :file ,(concat icon-dir "walking-man-green.png"))))
        ;;| ;;; 
        ;;| (define-key-after map [d-lisp-load-asdf-system-file]
        ;;|   `(menu-item "d-lisp-load-asdf-system-file" d-lisp-load-asdf-system-file 
        ;;|              :help "load the `d-lisp-asdf-system-file'."
        ;;|              :image (image :type png :file ,(concat icon-dir "walking-man-blue.png"))))
        ;;| ;;;
        ;;| (define-key-after map [d-slime-eval-init-region]
        ;;|   `(menu-item "d-slime-eval-init-region" d-slime-eval-init-region 
        ;;|              :help "eval init region (between 'init-begin' and 'init-end'"
        ;;|              :image (image :type png :file ,(concat icon-dir "walking-man-yellow.png"))))
        ;;; 
        (define-key-after map [clear-blash-buffer]
          `(menu-item "clear-blash-buffer" clear-blash-buffer 
                     :help "Erase everything beyond the last line in the blash buffer."
                     :image (image :type png :file ,(concat icon-dir "walking-man-blue-cyan.png"))))
        ;;| ;;; 
        ;;| (define-key-after map [d-slime-remove-edit-hilights]
        ;;|   `(menu-item "d-slime-remove-edit-hilights" d-slime-remove-edit-hilights 
        ;;|              :help "Delete the existing Slime edit hilights in the current buffer."
        ;;|              :image (image :type png :file ,(concat icon-dir "walking-man-red.png"))))
        ;;; 
	map)))

;; =========================================================
;; blender-python-mode:

(define-derived-mode blender-python-mode python-mode  "Blender"
  "Major mode for editing Blender python files.
Like `python-mode', but sets up parameters for Blender python subprocesses.
Runs `blender-python-mode-hook' after `python-mode-hook'."
  :group 'python
  (make-local-variable 'python-command)
  ;; scroll no/all windows showing python buffer.
  ;; neither of the following does anything :(
  (set (make-local-variable 'comint-scroll-to-bottom-on-input)  nil) ;; 'all)
  (set (make-local-variable 'comint-scroll-show-maximum-output) nil)
  
  ;; key bindings
  (make-local-variable 'python-mode-map)
  (define-key python-mode-map [C-f12]    'blender-send-region)
  (define-key python-mode-map "\C-x\C-r" 'blender-send-region)
  (define-key python-mode-map "\C-x\C-e" 'blender-execute-blender-region)
  (define-key python-mode-map "\C-c\C-e" 'blender-execute-blender-region)
  (define-key python-mode-map [M-return] 'blender-execute-blender-region)
  (define-key python-mode-map "\C-c\C-c" 'blender-send-buffer)
  (define-key python-mode-map [C-return] 'blender-execute-current-line)
  (define-key python-mode-map "\C-c\C-n" 'blender:restart-blender-and-reevaluate-first-blender-region)
  (define-key python-mode-map [f8]       'blender:restart-blender-and-reevaluate-first-blender-region)
  ;; tool bar
  ;;| (make-local-variable 'tool-bar-map)
  ;;| (blender-init-toolbar tool-bar-map)
  (set (make-local-variable 'tool-bar-map) (blender:make-toolbar tool-bar-map))
  ;; menu
  (blender:make-blender-menu))
  
;; =========================================================
;; key bindings:

(define-key global-map [f12] 'blender-execute-blender-region)

;; =========================================================
;; blender functions:

(defun customize-blender-python-mode ()
  "Customize the Blender mode settings."
  (interactive)
  (customize-group 'blender))

;;| (customize-group 'blender)

;; ---------------------------------------------------------

(defun* start-blender-server (&key debug)
  "Start the Blender Server."

  (interactive)
  (let ((bcp-debug-level blender-debug-level)
        (geometry        (blender:get-blender-geometry))
        (port-number     (get-next-blender-port-number))
        (blender-profile (blender:get-blender-profile-path)))

    (let ((blender-command `(blender
                             ,@(blender:format-debug-option        bcp-debug-level)
                             ,@(blender:format-geometry-option     geometry)
                             ,@(blender:format-command-port-option port-number)
                             )))

      ;; when a profile has been specified...
      (when (and blender-profile (not (equal blender-profile "")))

        ;; assure its existence
        (unless (file-exists-p blender-profile)
          (error (format "Profile does not exist: %s" blender-profile)))

        ;; and append it to the command
        (setq blender-command (append blender-command (list (expand-file-name blender-profile)))))

      ;; DEBUG
      ;; when the debug key-argument is set
      ;; only return the blender server startup command list
      ;; without executing it
      (if debug

          ;; in DEBUG mode
          ;; return the blender server startup command list
          ;; but do not execute it
          blender-command

        ;; NOT in DEBUG mode
        ;; start blender server
        (multiple-value-bind (process-name process-buffer) (eval blender-command)

          ;; store process name and buffer
          ;; in the global variables `blender-process-name' and `blender-process-buffer'
          (setq blender-process-name   process-name)
          (setq blender-process-buffer process-buffer)
          
          (bpm:message "Started the Blender Server..."))))))

;;; (start-blender-server)
;;; 
;;; Append a blender profile to the argument list:
;;; 
;;; (blender:set-blender-profile "3x3+1 Perspective and Button Window")
;;; (start-blender-server :debug t)
;;; (blender:set-blender-profile "3x3+0 Perspective")
;;; (start-blender-server :debug t)
;;; (blender:set-blender-profile "3x3+1 Top Button Window")
;;; (start-blender-server :debug t)
;;; 
;;; Do NOT append a blender profile to the argument list and use Blenders default cube scene
;;; (blender:set-blender-profile "Default")
;;; (start-blender-server :debug t)

;;; Look into the `*Messages*' buffer to check the use command!
;;; 
;;; (progn (blender:set-blender-variant "Blender")                  (start-blender-server))
;;; (stop-blender-server)
;;; 
;;; (progn (blender:set-blender-variant "Blender Command Port")     (start-blender-server))
;;; (stop-blender-server)
;;; 
;;; (progn (blender:set-blender-variant "Blender Formgames Python") (start-blender-server))
;;; (stop-blender-server)
;;; 
;;; (progn (blender:set-blender-variant "Blender Geometry Option")  (start-blender-server))
;;; (stop-blender-server)
;;; 
;;; (progn (blender:set-blender-variant "Blender 2.5")  (start-blender-server))
;;; (stop-blender-server)
;;; 
;;; (progn (blender:set-blender-variant "Blender 2.5")  (start-blender-server :debug t))
;;; (stop-blender-server)

;; ---------------------------------------------------------

(defun blender (&rest args)
  "Execute the Blender server command with arguments `args'.
Stdout and stderr are printed to the `blender-output-buffer'
buffer.  The name of the blender process and its process buffer
are returned."

  (let* ((command       (blender:get-blender-command))
         (output-buffer blender-output-buffer))

    ;; DEBUG:
    (let ((command-string command))
      ;; append parameters
      (loop for arg in args do (setq command-string (concat command-string " " arg)))
      ;; print a message
      (bpm:message "Starting the Blender Server: %s (output buffer: %s)" command-string output-buffer))

    ;; start blender
    (multiple-value-bind (process-name process-buffer)
        (bpm-shell-command command :output-buffer output-buffer :args args
                           ;; try to get back the focus back to emacs
                           ;; ...which is given to the started blender by gnome?
                           :get-back-focus t)

      ;; return process name and process buffer
      (values process-name process-buffer))))

;; (blender "--debug" "bcp:2" "--geometry" "500x400+0+500" "--bcp" "5678")
;; 
;; (blender "--debug-command-port" "2" "-p" "+0" "+0" "1015" "740" "--command-port" "6771" "/home/dietrich/blender/blender-profiles/blender-profile.1x1+1.perspective.button-window010.blend")
;; 
;; (blender "--debug-command-port" "2" "-p" "+0" "+0" "1015" "740" "--command-port" "6771" "/home/dietrich/blender/blender-profiles/blender-profile.1x1+1.perspective.button-window010.blend")

;;; -------------------------------------------------------- 

(defun get-blender-server-port ()
  "Get the blender server port."

  blender-server-port)

;; (get-blender-server-port)

;; ---------------------------------------------------------

(defun get-next-blender-port-number ()
  "Calculate the next port number for the blender command port,
store it in the global variable `blender-server-port' and return
it.

This is done as it might take some time after terminating the
server until the same port number might be used again if the port
socket is still used by some client."

  (let ((min (car blender-server-port-range))
        (max (cdr blender-server-port-range)))

    (let ((new-port-number

           ;; calculate the next port number
           (cond
            
            ;; when `blender-server-port' is nil use the lower port range
            ((not blender-server-port) min)
            
            ;; when `blender-server-port' is out of range also use the lower port range
            ((< blender-server-port min) min)
            ((> blender-server-port max) min)
            
            ;; when `blender-server-port' is equal to the upper port range also use the lower port range
            ((equal blender-server-port max) min)
            
            ;; in all other cases use the successor of the last port number
            (t (1+ blender-server-port)))))

      ;; store the new port number in the global variable `blender-server-port'
      (setq blender-server-port new-port-number)

      ;; and return it
      new-port-number)))

;; blender-server-port
;; (get-next-blender-port-number)
;; blender-server-port
;; (setq blender-server-port 1000000)
;; (get-next-blender-port-number)
;; blender-server-port
;; (setq blender-server-port 0)
;; (get-next-blender-port-number)
;; blender-server-port
;; ...

;; ---------------------------------------------------------

(defun stop-blender-server ()
  "Stop the Blender Server."

  ;; see GNU Emacs Lisp Reference Manual
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Processes.html
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Deleting-Processes.html
  
  (interactive)
  (if (blender-server-running)
      (progn
        (delete-process blender-process-name)
        (bpm:message "Stopped the Blender Server..."))
    (bpm:message "Blender Server not running...")))

;; (start-blender-server)
;; (blender-server-process-status)
;; (blender-server-running)
;; (stop-blender-server)
;; (blender-server-process-status)
;; (blender-server-running)

;; ---------------------------------------------------------

(defun blender-server-process-status ()
  "Returns the status of the blender server process as a symbol.

possible return values:

    run    - for a process that is running.
    stop   - for a process that is stopped but continuable.
    exit   - for a process that has exited.
    signal - for a process that has received a fatal signal.
    nil    - if process-name stored in the global variable `blender-process-name'
             is not the name of an existing process."

  ;; see GNU Emacs Lisp Reference Manual
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Processes.html
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Process-Information.html

  ;; (process-status blender-process-name)

  (and
   ;; return nil if `blender-process-name' is nil (blender process not started yet)
   blender-process-name 
   (let ((process (get-process blender-process-name)))
     (and
      ;; return nil if there is no blender process
      process 
      ;; return process status of blender process
      (process-status process)))))

;; (start-blender-server)
;; (blender-server-process-status)
;; (blender-server-running)
;; (stop-blender-server)
;; (blender-server-process-status)
;; (blender-server-running)

;; ---------------------------------------------------------

(defun blender-server-running ()
  "Returns t if the blender server process is running."

  (equal (blender-server-process-status) 'run))

;; (start-blender-server)
;; (blender-server-process-status)
;; (blender-server-running)
;; (stop-blender-server)
;; (blender-server-process-status)
;; (blender-server-running)

;; ---------------------------------------------------------

(defun blender:restart-blender-server ()
  "Stop the running blender server and start a new one.

todo:

  - test this function for a while
  - correct the comments!!!
"
  ;; Hack:
  ;; In order to make the Blender shell realize that the old Blender
  ;; server is not running anymore, and that it has to connect to a
  ;; new Blender instance I send an arbitrary command ("'Hi :)'") via
  ;; `comint' to Blender.  By doing so the `comint' functions realize
  ;; that the old shell doesn't work anymore and can be restarted by
  ;; executing `py-shell' via `blash'.  When not doing so the
  ;; `py-shell' command is simply ignored...
  (interactive)

  ;; restarting the blender server
  (save-excursion ;; with and without the blender server buffer is shown :(
    (stop-blender-server)
    (start-blender-server))

  ;; trying to send an arbitrary command to blender 
  ;; to make comint realize, that the former blender server
  ;; is not running anymore
  ;; erase all content from the blender shell buffer `*Blender*'
  ;;| (clear-blash-buffer)
  ;; wait for blender to start up
  ;;| (loop for i from 1 to 30  ;; wait maximal for ~ 50 * 0.2 = 10 seconds
  ;;|       do
  ;;|       (blash-buffer-message ".")
  ;;|       (sleep-for 0.3))
  ;;| (loop for i from 1 to 50  ;; wait maximal for ~ 50 * 0.2 = 10 seconds
  ;;|       until (equal (blender-server-process-status) 'running) ;; is the blender server running?
  ;;|       do
  ;;|       (blash-buffer-message ".")
  ;;|       (sleep-for 0.33)) ;; sleep for 0.33 seconds and try again
  ;;| (blash-buffer-message "\n# Blender is up - reconnecting...")
  ;;| 
  ;; give blender anorther .5 seconds to start the command port
  ;;| (sleep-for .3)
  ;; restarting the comint blender shell
  ;;| (blash)
  ;;| (blash :port port)
  ;;| (blash:stop-blash-client)  ;; blender-send-command "'Hi :)'")
  (if (blash-running)
      (let ((port (get-blender-server-port)))
        (blash-reconnect :port port))
    (blash))

  ;; hack
  ;; as most of the time no promt is shown in the blash buffer - and
  ;; the connection does not work
  ;; I'll sleep for a moment and restart blash...
  ;; ... at least the `start blash' code ensures that the server is
  (sleep-for .3)
  (blash:restart-blash-client)
  
  ;; done :)
  (bpm:message "Restarted the Blender Server..."))

;; (if (equal (blender-server-process-status) 'running) t nil)

;; (blender:start)
;; (blender:restart-blender-server)
;; (blender:stop)

;;; ========================================================
;;; general functions 
;;; --------------------------------------------------------

(defun blender:start ()
  "Start blender server and blender shell."
  
  (interactive)

  ;; print an error if blender server is already running
  (if (blender-server-running)
      (bpm:message "Blender is already running...")

    ;; start blender server
    (save-excursion ;; with and without the blender server buffer is shown :(
      (start-blender-server))

    ;; start blash
    ;; if blash is already running, reconnect
    (if (blash-running)
        (let ((port (get-blender-server-port)))
          (blash-reconnect :port port))
      (blash))

    ;; hack
    ;; as most of the time no promt is shown in the blash buffer - and
    ;; the connection does not work
    ;; I'll sleep for a moment and restart blash...
    ;; ... at least the `start blash' code ensures that the server is
    (sleep-for .3)
    (blash:restart-blash-client)

  ;; done :)
  (bpm:message "Started blash and blender server...")))

;; (blender:start)
;; (blender:stop)

;;; --------------------------------------------------------

(defun blender:restart ()
  "Retart blender server and blender shell."
  
  (interactive)

  ;; ...same as:
  (blender:restart-blender-server)

  ;; done :)
  (bpm:message "Restarted blash and blender server..."))

;; (blender:start)
;; (blender:restart)
;; (blender:stop)

;;; --------------------------------------------------------

(defun blender:stop ()
  "Start blender server and blender shell."
  
  (interactive)

  ;; stop blash and blender server
  (blash:stop-blash-client)
  (stop-blender-server)

  ;; done :)
  (bpm:message "Stopped blash client and blender server..."))

;; (blender:start)
;; (blender:stop)

;;; ========================================================
;;; blash
;;; --------------------------------------------------------

(defun* blash (&key ip-address port)
  "todo..."

  (interactive)

  ;; init parameters
  (let* ((ip-address (or ip-address blender-ip-address))
         (port       (or port (get-blender-server-port))))

    ;; start blash and connect to blender server
    (bpm:message "Connecting to Blender server...   (IP-address: %s, port: %i)" ip-address port)
    (run-blash :debug           blash-debug-level
               :ip-address      ip-address
               :port            port
               :retries         blash-retries
               :retry-sleeptime blash-retry-sleeptime
               :noshow          t ;; don't pop to blash window - doing this when evaluating / executing some code...
               )))

;; blender --debug bcp:2 --geometry 1200x800+0+100 --bcp 7784
;; (blash :port 7784)
;; or:
;; (stop-blender-server)
;; (start-blender-server)
;; (blash)
;; (blash-process-status)
;; (blash-running)
;; (blash:stop-blash-client)
;; (blash-process-status)
;; (blash-running)

;; ---------------------------------------------------------

(defun* run-blash (&key debug ip-address port retries retry-sleeptime noshow new)
  "Variation of `run-python' in python.el.

TODO - rewrite the following:

Run an inferior Blash process, input and output via buffer *blash*.
CMD is the Blash command to run.  NOSHOW non-nil means don't show the
buffer automatically.

Normally, if there is a process already running in
`python-buffer', switch to that buffer.  Interactively, a prefix
arg allows you to edit the initial command line; `-i' etc. args
will be added to this as appropriate.  A new process is started
if: one isn't running attached to `python-buffer', or
interactively the default blash command, or argument
NEW is non-nil.  See also the documentation for `python-buffer'.

Runs the hook `inferior-blash-mode-hook' \(after the
`comint-mode-hook' is run).  \(Type \\[describe-mode] in the process
buffer for a list of commands.)"
  (interactive)
  (when (or new (not (blash-running)))
    (with-current-buffer
        (let* ((cmd (append
                     (list (blender:get-blash-command))
                     (if debug           (list (concat "--debug="  (number-to-string debug))))
                     (if ip-address      (list "--IP-address"                        ip-address))
                     (if port            (list "--port"            (number-to-string port)))
                     (if retries         (list "--retries"         (number-to-string retries)))
                     (if retry-sleeptime (list "--retry-sleeptime" (number-to-string retry-sleeptime)))
                     ;; 
                     ;; * Initializing the Blender for blash mode
                     ;; 
                     ;; !!! Note: emacs `python-mode' relies on
                     ;; !!! python module `emacs'!
                     ;; 
                     ;; Blash allows to provide initialization
                     ;; code via the option `--init'.  This code
                     ;; is executed immediately after connecting
                     ;; or reconnecting to the Blender server.
                     ;; 
                     (let* ((dir blender-python-init-file-dir)
                            (cmd (get-blender-python-init-cmd)))
                       (list "--init" cmd)))))
          (bpm:message "Starting Blash - connecting to Blender server at host %S, port %i\n" ip-address port)
          ;;| (bpm:message "[DEBUG blender-python-mode] Starting blash with command: %S" cmd)
          (apply 'make-comint-in-buffer "*Blash Client*"
                 (if new (generate-new-buffer "*Blash*") "*Blash*")
                 (car cmd) nil (cdr cmd)))
      (setq-default python-buffer (current-buffer))
      (setq python-buffer (current-buffer))
      (accept-process-output (get-buffer-process python-buffer) 5)
      (inferior-python-mode)))
  (sit-for 1 t)
  (unless noshow (pop-to-buffer python-buffer t)))
  
;; test:
;; 
;; (run-blash)
;; (run-blash                                  :port 1234)
;; (run-blash          :ip-address "127.0.0.1" :port 1234)
;; (run-blash :debug 0 :ip-address "127.0.0.1" :port 1234 :retries 77 :retry-sleeptime 0.33 :noshow t   :new t)
;; (run-blash :debug 1 :ip-address "127.0.0.1" :port 1234 :retries 77 :retry-sleeptime 0.33 :noshow nil :new nil)

;; ---------------------------------------------------------

;; returns t even after killing blender - not really of use for my purpose

(defun blash-process-status ()
  "Returns the status of the blash process as a symbol.

possible return values:

    run    - for a process that is running.
    stop   - for a process that is stopped but continuable.
    exit   - for a process that has exited.
    signal - for a process that has received a fatal signal.
    nil    - if process-name stored in the global variable `blender-process-name'
             is not the name of an existing process.

The Blash process is the process associated to the `*Blender*' buffer."

  ;; see GNU Emacs Lisp Reference Manual
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Processes.html
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Process-Information.html
  ;; compare `comint-check-proc' in `/usr/share/emacs/21.4/lisp/comint.el'.

  (let ((blash-process (get-buffer-process python-buffer)))
    (and blash-process (process-status blash-process))))

;; (stop-blender-server)
;; (start-blender-server)
;; (blash)
;; (blash-process-status)
;; (blash-running)
;; (blash:stop-blash-client)
;; (blash-process-status)
;; (blash-running)

;; ---------------------------------------------------------

(defun blash-running ()
  "Returns t if the blash process is running."

  (equal (blash-process-status) 'run))

;; (stop-blender-server)
;; (start-blender-server)
;; (blash)
;; (blash-process-status)
;; (blash-running)
;; (blash:stop-blash-client)
;; (blash-process-status)
;; (blash-running)

;; ---------------------------------------------------------

(defun blash:stop-blash-client ()
  "Stop the blash client."

  ;; see GNU Emacs Lisp Reference Manual
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Processes.html
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Deleting-Processes.html
  
  (interactive)
  (if (blash-running)
      (let ((blash-process (get-buffer-process python-buffer)))
        (delete-process blash-process)
        (bpm:message "Stopped the blash client..."))
    (bpm:message "Blash not running...")))

;;| (blash)
;;| (blash-process-status)
;;| (blash-running)
;;| (blash:stop-blash-client)
;;| (blash-process-status)
;;| (blash-running)

;; ---------------------------------------------------------

(defun* blash:restart-blash-client ()
  "Stop and restart the blash client..."

  (interactive)
  (blash:stop-blash-client)
  (blash))

;; (blash:restart-blash-client)

;; =========================================================
;; blender region
;; ---------------------------------------------------------

;; region-begin
;; region-begin
;; (search-backward "region-begin" nil 'noerror)

(defun get-blender-region ()
  "Get the region between the markers `region-begin' and `region-end'"
  (save-excursion
    (let ((begin-marker "region-begin")
          (end-marker   "region-end"))
      (let ((begin (when
                       ;; search for begin marker
                       (or (search-backward begin-marker nil 'noerror)
                           (search-forward begin-marker nil 'noerror))
                     ;; if found - return beginning of next line
                     (next-line 1)
                     (beginning-of-line)
                     (point)))
            (end   (when
                       ;; search for begin marker
                       (search-forward end-marker nil 'noerror)
                     ;; if found - return beginning of the marker's line
                     (beginning-of-line)
                     (point))))
        (if (and begin end)
            ;; both markers found
            (cons begin end)
          ;; couldn't find region
          (bpm:message "Couldn't find a region limited by `%s' and `%s' in the current buffer."
                   begin-marker end-marker))))))

;; example:
;; 
;; region-begin
;; this is the region :)
;; region-end
;; 
;; (let ((region (get-blender-region))) (when region (buffer-substring (car region) (cdr region))))

;; ---------------------------------------------------------

;; region-begin
;; region-begin
;; (search-backward "region-begin" nil 'noerror)

(defun get-first-blender-region ()
  "Get the first region between the markers `region-begin' and `region-end' in the current file."
  (save-excursion
    (goto-char (point-min))
    (get-blender-region)))

;; ---------------------------------------------------------

(defun blender-send-region (begin end)
  "Send python current region - wrapper for `'
which also recenters the python buffer."
  (interactive "r")
  (blender:frame-show-blash-window-and-prompt)
  (python-send-region begin end))
  ;; (blender-recenter-python-window))
  
;; ---------------------------------------------------------

(defun blender-execute-blender-region ()
  "Call `blender-send-region' on the region limited by `region-begin' and `region-end'"
  (interactive)
  (blender:frame-show-blash-window-and-prompt)
  (let ((region (get-blender-region)))
    (when region
        (let ((begin  (car region))
              (end    (cdr region)))
          ;; (bpm:message "; region (%d,%d): %s" begin end (buffer-substring begin end))
          (blender-send-region begin end)))))
  
;; ---------------------------------------------------------

(defun blender-execute-file (filename)
  "load .py file in blender shell \(using imp.load_source\)"
  ;; Author: Marc Weber
  (interactive
   (find-file-read-args "blender execute file: "
                        (confirm-nonexistent-file-or-buffer)))
  (blender-send-command
   (format "import imp; imp.load_source(\"blender_execute_file\",\"%s\")"
           (expand-file-name filename)))
  (blender:frame-show-blash-window-and-prompt))

;; ---------------------------------------------------------

(defun blender-execute-file-menu ()
  "load .py file in blender shell \(using imp.load_source\)"
  ;; edited copy of emacs function `menu-find-file-existing'
  (interactive)
  (let* ((mustmatch (not (and (fboundp 'x-uses-old-gtk-dialog)
			      (x-uses-old-gtk-dialog))))
	 (filename (car (find-file-read-args "Find file: " mustmatch))))
    (if mustmatch
	(find-file-existing filename)
      (find-file filename))
    ;; execute file in blender
    (blender-execute-file filename)))

;; ---------------------------------------------------------

(defun blender-send-buffer ()
  "Send the current buffer to the inferior Python process.

When the buffer is bound to a file and the current state of the
buffer has been saved, the corresponding file itself is loaded
using `imp.load_source'; when the buffer is not bound to a file
or has been modified since it has been saved last, the buffer
content is sent."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (if (and
         filename                   ;; buffer is bound to some file
         (not (buffer-modified-p))) ;; buffer has not been modified since last save
        ;; use `blender-execute-file'
        (blender-execute-file filename)
      ;; buffer is not bound to file or modified
      ;; use `python-send-buffer'
      (python-send-buffer))))

;; ---------------------------------------------------------

(defun blender-execute-first-blender-region ()
  "Call `blender-send-region' on the first region limited by
`region-begin' and `region-end' in this file.  The first region
can be used to store initialization functionality needed by all
functions later in the file."
  (interactive)
  (let ((region (get-first-blender-region)))
    (when region
        (let ((begin  (car region))
              (end    (cdr region)))
          ;; (bpm:message "; region (%d,%d): %s" begin end (buffer-substring begin end))
          (blender-send-region begin end)))))

;; ---------------------------------------------------------

;;| (defun blender-execute-current-line ()
;;|   "Send blash the current line."
;;|   (interactive)
;;|   (save-excursion
;;|     (let* ((beg      (progn (beginning-of-line) (point)))
;;|            (end      (progn (end-of-line)       (point))))
;;|       (blender-send-region beg end))))

(defun blender-execute-current-line ()
  "Send blash the current line."
  (interactive)
  (blender:frame-show-blash-window-and-prompt)
  (save-excursion
    (let* ((beg      (progn (beginning-of-line) (point)))
           (end      (progn (end-of-line)       (point)))
           (line     (buffer-substring beg end))
           (command  (blender-erase-eol-comment line)))
      (blender-send-command command))))

;; ---------------------------------------------------------

(defun blender-erase-eol-comment (line)
  "Erase a python end-of-line comment starting with `#...' form line."
  (bcp:replace-regexp "[ 	]*#.*" "" line))

;; (blender-erase-eol-comment "eins zwei # drei vier")
;; => "eins zwei"

;; ---------------------------------------------------------

(defun blender-recenter-python-window ()
  "Recenter the python shell window"
  (interactive)
  (save-selected-window
    (let ((python-window (get-buffer-window (blender:get-blash-buffer))))
      (select-window python-window)
      (goto-char (point-max))
      (recenter (/ (window-height) 4)))))

;; ---------------------------------------------------------

(defun blender-send-command (command)
  "Insert `command' into the `*Blender*' buffern and execute it :)."
  (save-excursion
    (let ((buffer (blender:get-blash-buffer)))
      (set-buffer buffer)
      (goto-char (blender-get-process-mark))
      ;;| ;; if not at the end of a python prompt - insert a new one :) -
      ;;| ;; not necessary - just for making things easyer to read...
      ;;| (when (not (looking-back "\n>>> "))
      ;;|   (blash-buffer-message "\n>>> ")
      ;;|   (goto-char (blender-get-process-mark)))
      (insert command)
      (end-of-line)
      (comint-send-input)))
  ;; recenter the window to always see command and output
  ;; I set `comint-scroll-to-bottom-on-input' to `all'
  ;; so scrolling occurs anyway - but only if the
  ;; current line is not the last one, which is prevented
  ;; by the recenter function
  ;;| (blender-recenter-python-window)
  )

;; (blender-send-command "print 'Hi Die :)'")

;; ---------------------------------------------------------

(defun* blash-reconnect (&key port)
  "Reconnect to Blender server.
If a port is given, use it to connect to the server."

  (interactive)

  ;; format blash reconnect command
  (let ((cmd (if port
                 (format "reconnect %d" port)
               "reconnect")))

    ;; reconnect
    (blender-send-command cmd)
    ;; Load function definitions needed by python-mode.
    ;; See python.el
    ;;| (python-send-receive "import emacs; print '_emacs_out ()'")
    ;;| (blender-send-command "import emacs; print '_emacs_out ()'")
    ;;| ;; hack: send an empty input
    ;;| ;; to make blash print out a new prompt '>>> ':
    ;;| (blender-send-command "")
    ))

;; (blash-reconnect)

;; ---------------------------------------------------------

(defun clear-blash-buffer ()
  "Clear the blash buffer."
  (interactive)
  (save-excursion
    (let ((buffer (blender:get-blash-buffer)))
      (set-buffer buffer)
      (erase-buffer))))

;; (clear-blash-buffer)

;;; --------------------------------------------------------

(defun blender:get-blash-buffer ()
  "Get the blash buffer..."
  (get-buffer python-buffer))

;; (blender:get-blash-buffer)
;; (buffer-name (blender:get-blash-buffer))

;;; --------------------------------------------------------

(defun blender:get-blender-output-buffer ()
  "Get the blender output buffer..."
  blender-process-buffer)

;; (blender:get-blender-output-buffer)
;; (buffer-name (blender:get-blender-output-buffer))

;;; --------------------------------------------------------

(defun blender-get-process-mark ()
  "Get the blash process mark"
  (let* ((proc     (get-buffer-process (blender:get-blash-buffer)))
         (pmark    (process-mark proc))
         (position (marker-position pmark)))
    position))

;; (blender-get-process-mark)

;; ---------------------------------------------------------

(defun blender-set-process-mark (position)
  "Set the blash process mark"
  (let* ((proc     (get-buffer-process (blender:get-blash-buffer)))
         (pmark    (process-mark proc)))
    (set-marker pmark position)))

;; test:
;;| 
;;| (blender-get-process-mark)
;;| (blender-set-process-mark 123)
;;| (let* ((orig     (blender-get-process-mark))
;;|        (new      (progn
;;|                    (blender-set-process-mark 10)
;;|                    (blender-get-process-mark)))
;;|        (restored (progn
;;|                    (blender-set-process-mark orig)
;;|                    (blender-get-process-mark))))
;;|   (bpm:message ">>> %d -> %d -> %d." orig new restored))
  
;; ---------------------------------------------------------

(defun blash-buffer-message (message)
  "Write a message to the blash buffer."
  (save-excursion
    (let ((buffer (blender:get-blash-buffer)))
      (set-buffer buffer)
      ;; goto last position in shell buffer
      (goto-char (blender-get-process-mark))
      ;; insert message
      (insert message)
      ;; set process mark to the end of the inserted message
      (blender-set-process-mark (point)))))

;; (blash-buffer-message "hi :)")

;; ---------------------------------------------------------
;;| 
;;| Experiments to defun a `blender-is-alive' func
;;|
;;| using:
;;|   - /usr/share/emacs/21.4/lisp/comint.el
;;|   - /usr/share/emacs/site-lisp/python-mode/python-mode.el
;;| 
;;| (py-execute-string "Hi :)")
;;| (save-excursion
;;| (save-window-excursion 
;;|   (py-execute-string "print Blender.__name__"))
;;| (blender-send-command "print Blender.__doc__")
;;| 
;;| 
;;| (defun comint-simple-send (proc string)
;;|   "Default function for sending to PROC input STRING.
;;| This just sends STRING plus a newline. To override this,
;;| set the hook `comint-input-sender'."
;;|   (comint-send-string proc string)
;;|   (comint-send-string proc "\n"))
;;| 
;;| 
;;| (defun blender-send-command (command)
;;|   "Insert `command' into the `*Blender*' buffern and execute it :)."
;;| 
;;| )))
;;| 
;;| (let ((comint-output-filter #'(lambda (process string) (format "%S %S" process string))))
;;| (save-excursion
;;|   (let ((buffer (get-buffer "*Blender*")))
;;|     (set-buffer buffer)
;;|     (goto-char (point-max))
;;|     (let ((proc (get-buffer-process (current-buffer))))
;;|       (if (not proc) (error "Current buffer has no process")
;;|         ;; (process-filter proc))))))
;;|         (comint-simple-send proc "print 'Hi Die :)'"))))))
;;| 
;;| 
;;| (let comint-output-filter (process string)
;;| (let comint-output-filter (process string)

;;; ========================================================
;;; blash window related functions
;;; --------------------------------------------------------

(defun blender:frame-show-blash-window-and-prompt ()
  "Assures that the blash-buffer is shown in the current frame and
displayes its last line - where the blash prompt should be located
- on the last window line."

  ;; save the window that the cursor now appears
  ;; and also save the point position
  ;; to restore the same state later
  (let ((initial-window (selected-window))
        (initial-point-pos (point)))

    ;; if displayed in the current frame - get
    ;; if not displayed in the current frame - create
    ;; a blash-window in the current fram
    (let ((blash-window (blender:frame-get-blash-window-create)))

      ;; switch to the blash-window,
      ;; go to the last position in its buffer
      ;; and recenter window in a way to make the last buffer line
      ;; (in which the blash prompt should be located)
      ;; the last displayed line in the window
      (select-window blash-window)
      (goto-char (point-max))
      (blender:recenter)

      ;; switch back to the window
      ;; and set the cursor to its initial position
      (select-window initial-window)
      (goto-char initial-point-pos))))

;; (blender:frame-show-blash-window-and-prompt)

;;; --------------------------------------------------------

(defun blender:recenter ()
  "Put point on last line."

  (recenter -1))

;; (blender:recenter)

;;; --------------------------------------------------------

(defun blender:frame-get-blash-window-create ()
  "Returns the window of the blash buffer if displayed in
the current frame.  If the buffer currently is not displayed it
is created by the function."

  (let ((blash-window (blender:frame-get-blash-window)))

    (if blash-window

        ;; window exists
        ;; return it
        blash-window

      ;; window does not exist
      ;; create and return it
      (blender:frame-create-blash-window))))
        
;; (blender:frame-get-blash-window-create)

;;; --------------------------------------------------------

(defun blender:frame-create-blash-window ()
  "Creates a blash buffer window in the current frame."

  ;; save the window that the cursor now appears
  ;; to remake it as the current window later
  ;; and create a blash buffer window in the current fram
  (let ((initial-window (selected-window))
        (blash-window   (blender:split-window blender:blash-buffer-height))
        (blash-buffer   (blender:get-blash-buffer)))

    ;; switch to the blash-buffer in the new window
    (select-window blash-window)
    (switch-to-buffer blash-buffer)

    ;; switch back to the window the cursor was in before
    (select-window initial-window)

    ;; and finally return the newly created blash window
    blash-window))

;; (blender:frame-create-blash-window)

;;; --------------------------------------------------------

(defun blender:split-window (height)
  "Splits the current window.

The parameter `height' specifies the size of the new window relative to the original one.

Example:

 0.33 makes the new window height 1/3 of the height of the original window."

  (let* (;; get height of the current window
         (window-height (window-height))
         ;; calculate height in lines from the parameter `height' and the
         ;; height of the current window
         (height2 (* window-height height))
         ;; Assert that the height value to be given to `split-window-vertically'
         ;; is in the allowed range:
         ;; 
         ;; - only integers are allow
         ;;   numeric conversion functions:  `truncate', `floor', `ceiling' and `round'.
         ;;   see: http://www.gnu.org/software/emacs/elisp/html_node/Numeric-Conversions.html
         (height3 (round height2))
         ;; - the minimal number of lines is 3
         (height3 (if (>= height3 3) height3 3))
         ;; - for some reason the height of the created window is 
         ;;   one smaller than the given number
         ;;   => adding 1
         (height3 (1+ height3))
         ;; - a negative number means the size of the lowermost window.
         (height3 (- height3)))
    ;; DEBUG
    (bpm:message ">>> window height: %s, height: %s, height2: %s, height3: %s."
             window-height height height2 height3)
    (split-window-vertically height3)))

;; (blender:split-window .8)
;; (blender:split-window .5)
;; (blender:split-window .2)
;; (blender:split-window .15)
;; (blender:split-window .1)
;; (blender:split-window 0)

;;; --------------------------------------------------------

(defun blender:frame-get-blash-window ()
  "Returns the window of the slime-blash buffer if displayed in
the current frame and `nil' otherwise."

  (let ((frame-buffer-names (blender:frame-buffer-names))
        (blash-buffer-name  (buffer-name (blender:get-blash-buffer))))
    (if (member blash-buffer-name frame-buffer-names)

        ;; current frame displays blash-buffer
        ;; get window and return it
        (get-buffer-window blash-buffer-name (selected-frame))

      ;; current frame doesn't display blash-buffer - return nil
      nil)))

;; (blender:frame-get-blash-window)

;;; --------------------------------------------------------

(defun blender:frame-buffer-names (&optional frame)
  "Return the list with the names of the buffers displayed in windows in FRAME."

  (let ((frame-buffers (blender:frame-buffers frame)))

    (loop for buffer in frame-buffers
          for buffer-name = (buffer-name buffer)
          collect buffer-name)))

;; (blender:frame-buffer-names)

;;; --------------------------------------------------------

(defun blender:frame-buffers (&optional frame)
  "Return the list of buffers displayed in windows in FRAME."

  (let ((frame-windows (blender:frame-windows frame)))

    (loop for window in frame-windows
          for buffer = (window-buffer window)
          collect buffer)))

;; (blender:frame-buffers)
;; (insert (format "%s\n" (blender:frame-buffers)))

;;; --------------------------------------------------------

(defun blender:frame-windows (&optional frame)
  "Return the list of windows in FRAME.

This function is a copy of `slime-frame-windows'."

  ;; Notes:
  ;; 
  ;; - `frame-first-window' is a built-in function in `C source code'.
  ;;   (frame-first-window &optional FRAME)
  ;;   Returns the topmost, leftmost window of FRAME.
  ;;   If omitted, FRAME defaults to the currently selected frame.
  ;; - `selected-frame'
  ;;    Return the frame that is now selected.
  ;;    (not used as its functionality is part of `frame-first-window')
;; (round 

  (loop with last-window = (previous-window (frame-first-window frame))
        for window = (frame-first-window frame) then (next-window window)
        collect window
        until (eq window last-window)))

;;; --------------------------------------------------------

(defun blender:kill-blender-buffers (&optional frame)
  "Stop the blender server and blash and kill their buffers."

  (interactive)

  ;; stop client and server
  (blash:stop-blash-client)
  (stop-blender-server)

  ;; kill the buffers
  (kill-buffer (blender:get-blash-buffer))
  (kill-buffer (blender:get-blender-output-buffer)))

;; (blender:kill-blender-buffers)

;;; ========================================================
;;; 
;;; --------------------------------------------------------

(defun blender:restart-blender-and-reevaluate-first-blender-region ()
  "Restart blender and evaluate the first region in the file limited by `region-begin' and `region-end'."
  (interactive)
  (blender:restart)
  (clear-blash-buffer)
  (blash:restart-blash-client)
  (blender-execute-first-blender-region)
  )
  
;; =========================================================

(provide 'blender-python-mode)

;; =========================================================
;; =========================================================

;; fin.
