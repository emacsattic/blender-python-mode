;;; ========================================================
;;;                  Dietrich Bollmann, Kamakura, 2008/12/19
;;; 
;;; bpm-regexp.el
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
;;;    
;;; Some wrappers for using regular expressions...
;;;    
;;; --------------------------------------------------------

(require 'cl)    ;; common lisp `&key' arguments only supported with `defun*'...
(require 'dired) ;; using `dired-string-replace-match'

;;; ========================================================
;;; functions
;;; --------------------------------------------------------

(defun* bcp:replace-regexp (from to string &key all literal)
  "Replace first match of regexp `from' in `string' with `to'.
If `regexp' doesn\'t match, the unchanged `string' is returned;
If `:all' is true, all matches are replaced;
If `:literal' is true, to is taken literally."
  (let ((result (dired-string-replace-match from string to literal all)))
    (if result
        result
      string)))

;;; test:
;;| 
;;| (insert (format "\n;;; => %S" (bcp:replace-regexp "\\([ab]+\\)" "(\\1)" "abcaabbccaaabbbccc")))
;;; => "(ab)caabbccaaabbbccc"
;;| (insert (format "\n;;; => %S" (bcp:replace-regexp "\\([ab]+\\)" "(\\1)" "abcaabbccaaabbbccc" :all t)))
;;; => "(ab)c(aabb)cc(aaabbb)ccc"
;;| (insert (format "\n;;; => %S" (bcp:replace-regexp "\\([ab]+\\)" "(\\1)" "abcaabbccaaabbbccc" :literal t)))
;;; => "(\\1)caabbccaaabbbccc"
;;| (insert (format "\n;;; => %S" (bcp:replace-regexp "\\([ab]+\\)" "(\\1)" "abcaabbccaaabbbccc" :all t :literal t)))
;;; => "(\\1)c(\\1)cc(\\1)ccc"
;;| (insert (format "\n;;; => %S" (bcp:replace-regexp "a" "?" "abc")))
;;; => "?bc"
;;| (insert (format "\n;;; => %S" (bcp:replace-regexp "a" "?" "bc")))
;;; => "bc"

;;; the first 4 tests in 1 :)
;;| 
;;| (loop for all in '(nil t)
;;|       do (loop for literal in '(nil t)
;;|                do (insert
;;|                    (format "\n;;; (:all %-4s :literal %-4s => %S"
;;|                            (format "%S," all)
;;|                            (format "%S)" literal)
;;|                            (bcp:replace-regexp "\\([ab]+\\)" "(\\1)"
;;|                                                    "abcaabbccaaabbbccc"
;;|                                                    :all all :literal literal)))))
;;|
;;; (:all nil, :literal nil) => "(ab)caabbccaaabbbccc"
;;; (:all nil, :literal t)   => "(\\1)caabbccaaabbbccc"
;;; (:all t,   :literal nil) => "(ab)c(aabb)cc(aaabbb)ccc"
;;; (:all t,   :literal t)   => "(\\1)c(\\1)cc(\\1)ccc"

;;; ========================================================
;;; ========================================================

(provide 'bpm-regexp)

;;; ========================================================
;;; ========================================================

;;; fin.

