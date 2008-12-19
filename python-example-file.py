### -*- mode: blender-python -*- ===========================
###                   Dietrich Bollmann, Kamakura 2008/12/19
### 
### python-example-file.py
### --------------------------------------------------------
### 
### Copyright (C) Dietrich Bollmann
### 
### Author:       Dietrich Bollmann
### Maintainer:   Dietrich Bollmann
### Keywords:     emacs, python, blender.
### 
### This file is part of the Blender Python Mode for emacs.
### 
### The Blender Python Mode is free software; you can redistribute it
### and/or modify it under the terms of the GNU General Public License
### as published by the Free Software Foundation; either version 3, or
### (at your option) any later version.
### 
### The Blender Python Mode is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied warranty
### of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
### 
### You should have received a copy of the GNU General Public License
### along with GNU Emacs; see the file COPYING.  If not, write to the
### Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
### Boston, MA 02110-1301, USA.
### 
### --------------------------------------------------------

usage = '''

*** NOTE ***

The following HowTo is for Unix / Linux user!
If you use some other operating system you have to adapt the commands.


* Preparation

Open some shell and define an environment variable defining the directory where
you want to install the Blender Python Mode:

  BLENDER_PYTHON_MODE_INSTALLARION_DIR=~/some/dir/where/you/want/to/install/the/blender-python-mode

Make the directory

  mkdir -p $BLENDER_PYTHON_MODE_INSTALLARION_DIR


* Download the sources of the Blender Python Mode

  cd $BLENDER_PYTHON_MODE_INSTALLARION_DIR

  ...todo
  ...todo
  ...todo

  
* Installation
  
The following should start an emacs ignoring all your emacs customizations
but loading the Blender Python Mode together with a simple example file :)

...If it does not work please let me know!

  emacs \
    --no-init-file --no-site-file --no-splash \
    --debug-init \
    --eval "
        (progn
          (define-key global-map \"\\C-h\" (quote delete-backward-char)) ;; mapping Control-h to delete-backward-char
          (defvar blender-python-mode-installation-dir \"$BLENDER_PYTHON_MODE_INSTALLARION_DIR\")
          (setq load-path (add-to-list 'load-path blender-python-mode-installation-dir))
          (require 'blender-python-mode)
          (find-file (concat blender-python-mode-installation-dir \"/python-example-file.py\")))
      "

If you decide that the Blender Python Mode is something usefull, you should
consider to add the following lines to your .emacs file:

  (defvar blender-python-mode-installation-dir \"$BLENDER_PYTHON_MODE_INSTALLARION_DIR\")
  (setq load-path (add-to-list 'load-path blender-python-mode-installation-dir))
  (require 'blender-python-mode)


* Customization

You can customize the Blender Python mode by selecting the following menu item
from the menu visible when editing a buffer in `blender-python' mode:

  Emacs Menu: Blender -> Customization -> Customize the Blender mode settings


Set `blender-python-mode-installation-dir' to the directory where
you have installed the sources of the blender python mode
by entering:
##
  M-x customize-variable [ENTER] blender-python-mode-installation-dir

or by evaluating the following line in some emacs lisp buffer - for example buffer `*scratch*'

  (customize-variable 'blender-python-mode-installation-dir)

You can define different blender installations / variants and switch between them 
by entering:
##
  M-x customize-variable [ENTER] blender:blender-variants

or by evaluating the following line in some emacs lisp buffer - for example buffer `*scratch*'

  (customize-variable 'blender:blender-variants)


'''

# simple test string

print 'Hello %s :)' % 'Blender Python Mode'

# region-begin

from Blender import *

vertexes = [[1, 1, 0], [-1, 1, 0], [-1, -1, 0], [1, -1, 0], [0, 0, 1.27]]
faces    = [[3, 2, 1, 0], [0, 1, 4], [1, 2, 4], [2, 3, 4], [3, 0, 4]]

mesh = Mesh.New('mesh')
mesh.verts.extend(vertexes)
mesh.faces.extend(faces)

scene  = Scene.GetCurrent()
object = scene.objects.new(mesh, 'object')
Redraw()

# region-end


### ========================================================
### ========================================================

### fin.
