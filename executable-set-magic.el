;;; script-set-magic.el --- set script magic number   -*- lexical-binding: t -*-
;;
;; -*- coding: utf-8 -*-
;;
;; Copyright © 2017 Andrew L. Moore

;; Author: Andrew L. Moore <alm@gnu.org>
;; Keywords: editing, languages, script
;; URL: https://github.com/slewsys/emacs-extensions

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; `executable-set-magic-mode' is a minor mode that makes a script
;; executable and sets its magic number, if any, based on the current
;; major mode. An alist of major-modes and associated script
;; interpreters is defined in `executable-set-magic-alist'.
;;
;; If variable `executable-prefix-env' is non-nil, then the magic
;; number inserted by function `executable-set-magic' takes the form
;; "#!/usr/bin/env interpreter", otherwise "#!/path/to/interpreter".
;;
;; If variable `executable-exclude-project-files' is non-nil, then
;; files contained in project directories - as defined by the function
;; `executable-project-directory' - are not processed.

;;; Installation:
;;
;;   (require 'executable-set-magic)
;;
;; To enable `executable-set-magic-mode' globally:
;;
;;   (executable-set-magic-mode)
;;
(eval-when-compile
  (require 'cl))

(defcustom executable-set-magic-alist
  '((awk-mode        ("awk"    . "-f"))
    (enh-ruby-mode   ("ruby"   . nil))
    (go-mode         ("go"     . nil))
    (javascript-mode ("node"   . nil))
    (js2-mode        ("node"   . nil))
    (julia-mode      ("julia"  . nil))
    (lua-mode        ("lua"    . nil))
    (perl-mode       ("perl"   . nil))
    (php-mode        ("php"    . nil))
    (python-mode     ("python" . nil))
    (ruby-mode       ("ruby"   . nil))
    (sed-mode        ("sed"    . "-f"))
    (shell-mode      ("bash"   . nil))
    (tcl-mode        ("tclsh"    . nil)))
  "Alist of major modes and associated script interpreters used
by `executable-set-magic' to produce a magic number for a script."
  :tag "Mode-interpreter alist."
  :version "26.1"
  :group 'executable
  :type '(repeat (list symbol (cons string sexp))))

(defcustom executable-exclude-project-files t
  "If non-nil, `executable-set-magic-hook' is not applied to files
contained in project directories as identified by the function
`executable-project-directory'."

  :tag "Do not apply `executable-set-magic' to project files."
  :version "26.1"
  :group 'executable
  :type '(boolean))

(defcustom executable-project-root-files
  '(".git" ".svn" ".hgtags" ".bzr" "CVS" "_darcs" "GRTAGS")
  "List of files used by function `executable-project-root-path'
to identify root of project hierarchy."
  :tag "Project root files."
  :version "26.1"
  :group 'executable
  :type '(repeat string))

(defvar executable-root-path nil)

(defun executable-project-root-path (project-directory)
  "Searches PROJECT-DIRECTORY for one of the files in list
`executable-project-root-files'. If found, returns the pathname,
otherwise nil."
  (block project-block
      (dolist (root-file executable-project-root-files)
        (setq executable-root-path
              (concat project-directory root-file))
        (if (file-exists-p executable-root-path)
            (return-from project-block
              (file-name-directory executable-root-path))))
      nil))

(defun executable-project-directory (&optional project-directory)
  "Searches hierarchy of PROJECT-DIRECTORY for top-most project
root, as defined by function `executable-project-root-path'. If
found, returns the pathname, other nil."
  (block root-block
    (let* ((canonical-dir
            (expand-file-name (file-name-directory (or project-directory
                                                       default-directory))))
          (top-level-path "")
          (separator "/")
          (path-components (split-string canonical-dir separator)))
      (dolist (directory path-components executable-root-path)
        (setq top-level-path (concat top-level-path directory separator))
        (setq executable-root-path
                (executable-project-root-path top-level-path))
        (if executable-root-path
            (return-from root-block executable-root-path))))))

(defun executable-set-magic-hook ()
"Calls `executable-set-magic' with arguments returned by look up
in `executable-set-magic-alist' of interpreter associated with
current major mode."
  (let ((interpreter (car (alist-get major-mode executable-set-magic-alist))))
    (if (and interpreter
             (not (and executable-exclude-project-files
                       (executable-project-directory))))
        (executable-set-magic (car interpreter) (cdr interpreter)))))

;;;###autoload
(define-minor-mode executable-set-magic-mode
  "Toggles variable `executable-set-magic-mode'.

 Interactively with no argument, this command toggles the mode.
With a positive prefix argument, enables the mode. With any other
prefix argument, disables it.

From Lisp (i.e., non-interactively), argument omitted or nil
enables the mode, `toggle' toggles the state.

`executable-set-magic-mode' is a minor mode that makes a script
executable and sets its magic number, if any, based on the current
major mode. An alist of major-modes and associated script
interpreters is defined in `executable-set-magic-alist'.

If variable `executable-prefix-env' is non-nil, then the magic
number inserted by function `executable-set-magic' takes the form
\"#!/usr/bin/env interpreter\", otherwise \"#!/path/to/interpreter\".

If variable `executable-exclude-project-files' is non-nil, then
files contained in project directories - as defined by the function
`executable-project-directory' - are not processed."

  ;;; No initial value.
  :init-value nil

  ;;; No mode line indicator.
  :lighter nil

  ;;; Minor mode keymap
  :keymap nil

  ;;; Gobal variable `executable-set-magic-mode'.
  :global t

  ;;; Customization group
  :group 'executable

  (if executable-set-magic-mode
      (add-hook 'find-file-hook 'executable-set-magic-hook)
    (remove-hook 'find-file-hook 'executable-set-magic-hook)))

(provide 'executable-set-magic)

;;; executable-set-magic.el ends here
