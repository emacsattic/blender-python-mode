### -*- mode: blender-python -*- ===========================
###                   Dietrich Bollmann, Kamakura 2008/12/19
### 
### python-example-file.py
### --------------------------------------------------------

usage = '''

  emacs \
    --no-init-file --no-site-file --no-splash \
    --debug-init \
    --eval "
        (progn
          (define-key global-map \"\\C-h\" (quote delete-backward-char)) ;; mapping Control-h to delete-backward-char
          (defvar blender-python-mode-installation-dir \"$BPM/blender-python-mode\")
          (setq load-path (add-to-list 'load-path blender-python-mode-installation-dir))
          (require 'blender-python-mode)
          (find-file (concat blender-python-mode-installation-dir \"/python-example-file.py\")))
      "
'''

# a one-line command

print 'Hello %s :)' % 'Blender Python Mode'

# region-begin - a little script to generate a simple pyramid

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
