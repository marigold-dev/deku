# Emacs plugin for LIGO

This plugin features syntax highlighting and `lsp-mode` support for PascaLIGO, CameLIGO, and ReasonLIGO.

For the LSP to work, you need to install `lsp-mode` and put `ligo-squirrel` executable in PATH.

## Automatic installation

Install the `ligo-mode` package from [MELPA](https://melpa.org). This is the recommended installation method.

## Manual installation

Put `ligo-mode.el` to the emacs load path, and add the following lines to your `init.el`:

```el
(add-to-list 'load-path "<LIGO_MODE_DIR>")
(add-to-list 'auto-mode-alist '("\\.ligo\\'" . ligo-pascal-mode))
(add-to-list 'auto-mode-alist '("\\.mligo\\'" . ligo-caml-mode))
(add-to-list 'auto-mode-alist '("\\.religo\\'" . ligo-reason-mode))
(autoload ligo-pascal-mode "ligo-mode" "LIGO pascal mode" t)
(autoload 'ligo-caml-mode "ligo-mode" "LIGO caml mode" t)
(autoload 'ligo-reason-mode "ligo-mode" "LIGO reason mode" t)
```

Alternatively, run `M-x update-directory-autoloads` against `<LIGO_MODE_DIR>`, outputting to `<LIGO_MODE_DIR>/ligo-mode-autoloads.el`, and then your config becomes:
```el
(add-to-list 'load-path "<LIGO_MODE_DIR>")
(load "<LIGO_MODE_DIR>/ligo-mode-autoloads.el")
```

# LSP support

For users of `lsp-mode`, setup can be performed automatically by using
`M-x ligo-setup-lsp`, or with the following snippet in an init file:

```el
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'ligo-mode
    (ligo-setup-lsp)))
```
