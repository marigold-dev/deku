;;; ocaml-config.el -*- lexical-binding: t; -*-
;;
;;
;; Author:  alexjiverson@gmail.com
;; Maintainer:  alexjiverson@gmail.com
;; Created: May 28, 2020
;; Modified: May 28, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage:
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(setq ligo-developer/merlin-site-elisp (getenv "MERLIN_SITE_LISP"))
(setq ligo-developer/utop-site-elisp (getenv "UTOP_SITE_LISP"))
(setq ligo-developer/ocp-site-elisp (getenv "OCP_INDENT_SITE_LISP"))

(use-package tuareg
  :mode ("\\.ml[ily]?$" . tuareg-mode))

(use-package merlin
  :if ligo-developer/merlin-site-elisp
  :load-path ligo-developer/merlin-site-elisp
  :hook
  (tuareg-mode . merlin-mode)
  (merlin-mode . company-mode)
  :custom
  (merlin-command "ocamlmerlin"))

(use-package utop
  :if ligo-developer/utop-site-elisp
  :load-path ligo-developer/utop-site-elisp
  :hook
  (tuareg-mode . utop-minor-mode))

(use-package ocp-indent
  :if ligo-developer/ocp-site-elisp
  :load-path ligo-developer/ocp-site-elisp)


(provide 'ocaml-config)
;;; ocaml-config.el ends here
