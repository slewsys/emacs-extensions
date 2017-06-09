;;; script-set-magic.el --- set script magic number   -*- lexical-binding: t -*-
;;
;; -*- coding: utf-8 -*-
;;
;; Copyright (C) 2017 Andrew L. Moore

;; Author: Andrew L. Moore <alm@gnu.org>
;; Keywords: editing, languages, script
;; URL: https://github.com/slewsys/emacs-extensions

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; `script-set-magic-mode' is a minor mode that sets a script's magic
;; number, if any, and makes it executable based on the current major
;; mode. An alist of major-modes and associated script interpreters is
;; defined in `script-set-magic-alist'.
;;
;; The form of the magic number (or "shebang") is controlled by the
;; variables `executable-interpreter-path-absolute' and `executable-prefix'.
;; In particular, setting `executable-interpreter-path-absolute' to
;; nil and `executable-prefix' to "#!/usr/bin/env " produces a magic
;; number of the form "#!/usr/bin/env interpreter".
;;
;; Set also the function `executable-set-magic'.

;;; Installation:
;;
;;   (require 'script-set-magic)
;;
;; To enable `script-set-magic-mode' globally:
;;
;;   (script-set-magic-mode)
;;
(defcustom script-set-magic-alist
  '(
   (awk-mode . "awk")
   (enh-ruby-mode . "ruby")
   (go-mode . "go")
   (javascript-mode . "node")
   (js2-mode . "node")
   (lua-mode . "lua")
   (perl-mode . "perl")
   (php-mode . "php")
   (python-mode . "python")
   (python-mode . "python")
   (ruby-mode . "ruby")
   (shell-mode . "bash")
   (tcl-mode . "tcl")
    )
  "Alist of major modes and associated script interpreters."
  :tag "Mode-interpreter alist."
  :version "26.0"
  :group 'executable
  :type '(alist :key-type symbol value-type: string))


(defun script-set-magic ()
  "Look up interpreter associated with current major mode in
`script-set-magic-alist' and call `executable-set-magic'."
  (let ((interpreter (alist-get major-mode script-set-magic-alist)))
    (if interpreter (executable-set-magic interpreter)))
  )

;;;###autoload
(define-minor-mode script-set-magic-mode
  "Toggles variable `script-set-magic-mode'.

 Interactively with no argument, this command toggles the mode.
With a positive prefix argument, enables the mode. With any other
prefix argument, disables it.

From Lisp (i.e., non-interactively), argument omitted or nil
enables the mode, `toggle' toggles the state.

`script-set-magic-mode' is a minor mode that sets a script's
magic number, if any, and makes the script executable based on
the current major mode. An alist of major-modes and associated
script interpreters is defined in `script-set-magic-alist'.

The form of the magic number (or \"shebang\") is controlled by the
variables `executable-interpreter-path-absolute' and `executable-prefix'.
In particular, setting `executable-interpreter-path-absolute' to
nil and `executable-prefix' to \"#!/usr/bin/env \" produces a magic
number of the form \"#!/usr/bin/env interpreter\".

See also the function `executable-set-magic'."

  ;;; No initial value.
  :init-value nil

  ;;; No mode line indicator.
  :lighter nil

  ;;; Minor mode keymap
  :keymap nil

  ;;; Gobal variable `script-set-magic-mode'.
  :global t

  ;;; Customization group
  :group 'executable

  (if script-set-magic-mode
      (add-hook 'find-file-hook 'script-set-magic)
    (remove-hook 'find-file-hook 'script-set-magic)))

(provide 'script-set-magic)

;;; script-set-magic.el ends here
