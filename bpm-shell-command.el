;;; -*- Mode: Emacs-Lisp -*- ===============================
;;;                  Dietrich Bollmann, Kamakura, 2008/12/19
;;;   
;;; bpm-shell-command.el
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
(require 'bpm-regexp)

;;; ========================================================
;;; function definitions:
;;; --------------------------------------------------------

;;| (defun* bpm-shell-command (command &key args output-buffer get-back-focus)
;;|   "Wie \`shell-command\', jedoch koennen beliebig viele Kommandos gleichzeitig
;;| gestartet werden..."
;;|   (let* ((shell-buf-name (if output-buffer
;;|                              output-buffer
;;|                          (concat "Bpm-Shell-Command" (blender-new-postfix))))
;;| 	 (proc-name shell-buf-name)
;;| 	 (shell-buf (get-buffer-create shell-buf-name))
;;| 	 (origbuf (current-buffer)))
;;|     (pop-to-buffer shell-buf)
;;|     (pop-to-buffer origbuf)
;;|     ;; (save-excursion
;;|     (save-window-excursion
;;|       (set-buffer shell-buf)
;;|       (insert (format "(start-process proc-name: %S, shell-buf: %S, args: %S)"
;;|                       proc-name shell-buf args))
;;|       (let ((process-connection-type t)) ;; use a PTY to communicate with asynchronous subprocesses
;;|         (let ((proc (eval (append '(start-process proc-name shell-buf command) args))))
;;|           ;; (set-process-coding-system proc *euc-japan* *euc-japan*)     ;; d
;;|           (set-process-filter proc 'bpm-shell-command-filter)
;;|           (set-marker (process-mark proc) (point-max) shell-buf)))
;;| 
;;|       (when get-back-focus
;;|         ;; try to get back the focus to emacs
;;|         (get-focus-back-to-emacs)))
;;| 
;;|     ;; return name of process
;;|     proc-name))

(defun* bpm-shell-command (command &key args output-buffer get-back-focus)
  "Wie \`shell-command\', jedoch koennen beliebig viele Kommandos gleichzeitig
gestartet werden..."
  (let* ((shell-buf-name (if output-buffer
                             output-buffer
                         (concat "Bpm-Shell-Command" (blender-new-postfix))))
	 (proc-name shell-buf-name)
	 (shell-buf (get-buffer-create shell-buf-name))
	 (origbuf (current-buffer)))
    ;;| (pop-to-buffer shell-buf)
    ;;| (pop-to-buffer origbuf)
    ;; (save-excursion

    ;; An ugly hack to get the focus back to blender...
    ;; ... which probably only works for linux as os and gnome as window manager...
    (d-restore-window-focus 

     (save-window-excursion
       (set-buffer shell-buf)
       (insert (format "(start-process proc-name: %S, shell-buf: %S, args: %S)"
                       proc-name shell-buf args))
       (let ((process-connection-type t)) ;; use a PTY to communicate with asynchronous subprocesses
         (let ((proc (eval (append '(start-process proc-name shell-buf command) args))))
           ;; (set-process-coding-system proc *euc-japan* *euc-japan*)     ;; d
           (set-process-filter proc 'bpm-shell-command-filter)
           (set-marker (process-mark proc) (point-max) shell-buf)))))

    ;; return name of process
    proc-name))

;;| with and without getting back the focus to emacs
;;| 
;;| (bpm-shell-command "blender" :output-buffer "*out*" :args '("--geometry" "500x400+0+500"))
;;| (bpm-shell-command "blender" :output-buffer "*out*" :args '("--geometry" "500x400+0+500") :get-back-focus t)

;;; tests:010-circle.py
;;| 
;;| (bpm-shell-command "ls")
;;| (bpm-shell-command "ls" :args '("-la"))
;;| (bpm-shell-command "ls" :args '("-l" "-a"))
;;| (bpm-shell-command "ls" :output-buffer "out.buf" :args '("-la"))
;;| 
;;| (bpm-shell-command "blender-server" :args '("start"))
;;| (bpm-shell-command "blender-server" :output-buffer "*out*" :args '("-debug" "2" "--geometry" "300x400+0+100" "start"))
;;| (bpm-shell-command "blender-server" :args '("restart"))
;;| (bpm-shell-command "blender-server" :args '("stop"))
;;| 
;;| (setq *process-name* (bpm-shell-command "blender" :output-buffer "*out*" :args '("--debug" "bcp:2" "--geometry" "500x400+0+500" "--bcp" "5678")))
;;| (delete-process *process-name*)

;;; ========================================================
;;; *** hack to get back the focus to emacs ***
;;; --------------------------------------------------------

(defmacro d-restore-window-focus (&rest body)
  "An ugly hack to get back the focus to emacs under gnome
after starting some application using a window.

The hack probably only works when using linux/gnome and
the tool `wmctrl' is installed...

Note that `wmctrl' finds the window by its name - which has
to be \"emacs\" in the case of emacs to make this work.
As the corresponding emacs command 

  \(setq frame-title-format \"emacs\")

is ignored in my emacs most of the time I use 
`wmctrl' also for this purpose...

In case of my usage here the window name is most of the time /
sometimes reset immediately after back to 'mule[<no of
frame>]:(<name of buffer>)' so that the emacs actually is hardly
visible :)"
  
  (if (string-match "wmctrl" (shell-command-to-string "which wmctrl"))

      ;; ok, `wmctrl' is installed -
      ;; use it to get the focus back...
      `(progn

         ;; set the name of the window to emacs
         ;; as `wmctrl' uses the name to find a window
         ;; (the emacs command for the same purpose didn't work...)

         (shell-command "wmctrl -r :ACTIVE: -T 'emacs'")

         ;; execute the body
         ,@body

         ;; sleep a moment and try to get the focus back
         ;; !!! Note that the window is found by its name, not the name of the command!
         (sleep-for 0.1)
         (shell-command "wmctrl -a 'emacs'"))

    ;; `wmctrl' is not installed -
    ;; just execute the body as it is

    ;; ok, `wmctrl' is installed -
    ;; use it to get the focus back...
    `(progn

       ;; execute the body
       ,@body)))

;; experiments:

;;| without this hack the focus is given to the starting blender window
;;| (start-process "+++name+++" "+++buffer+++" "xterm")
;;| (progn
;;|   (start-process "+++name+++" "+++buffer+++" "xterm")
;;|   (sleep-for 0.1)
;;|   (shell-command "wmctrl -a emacs"))
;;| 
;;| (start-process "+++name+++" "+++buffer+++" "blender" "--geometry" "500x400+0+500")
;;| (progn
;;|   (start-process "+++name+++" "+++buffer+++" "blender" "--geometry" "500x400+0+500")
;;|   (sleep-for 0.1)
;;|   (shell-command "wmctrl -a emacs"))
;;| 
;;| (progn
;;|   (start-process "+++name+++" "+++buffer+++" "blender" "--geometry" "500x400+0+500")
;;|   ;; if `wmctrl' is installed, use it to get back the focus
;;|   ;; from the started application to emacs
;;|   (when (string-match "wmctrl" (shell-command-to-string "which wmctrl"))
;;|     (sleep-for 0.1)
;;|     (shell-command "wmctrl -a emacs")))
;;| 
;;| (progn
;;|   (start-process "+++name+++" "+++buffer+++" "blender" "--geometry" "500x400+0+500")
;;|   ;; if `wmctrl' is installed, use it to get back the focus
;;|   ;; from the started application to emacs
;;|   (get-focus-back-to-emacs))

;;; --------------------------------------------------------

(defun bpm-shell-command-filter (proc str)
  "Filter for \`bpm-shell-command\' process."
  (let* ((buf (process-buffer proc))
         (win (get-buffer-window buf)))
    ;; Can I assume the current-buffer is the process's buffer?  I don't know.
    (save-excursion
      (set-buffer buf)
      (goto-char (process-mark proc))
      ;; erase C-Ms
      (insert (bcp:replace-regexp "" "" str))
      (move-marker (process-mark proc) (point)))))

;;; test:
;;;
;;; (bcp:replace-regexp "" "" "einszweidrei" :all t)


;;       (if win    ;; If buffer not visible, do nothing.
;;           (let* ((curstart (window-start win))
;;                  (h (1- (window-height win)))
;;                  (sl (count-lines trans-mark (point)))
;;                  (bl (max h sl)))
;;             ;; Make the last bl lines visible in the window.
;;             (forward-line (- bl))
;;             (set-window-start win (point)))))))

;;; --------------------------------------------------------

(defvar blender-new-postfix-number 0
  "Next number used by `blender-new-postfix'.")

(defun blender-new-postfix ()
  "Returns a new postfix every time called: `-0', `-1', `-2', ..."
  (let ((postfix (format "-%S" blender-new-postfix-number)))
    (setq blender-new-postfix-number (+ 1 blender-new-postfix-number))
    postfix))

;; test:
;; (blender-new-postfix)

;;; ========================================================
;;; ========================================================

(provide 'bpm-shell-command)

;;; ========================================================
;;; ========================================================

;;; fin.
