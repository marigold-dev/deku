# LIGO language support for Vim

This plugin adds support for PascaLIGO, CameLIGO, and ReasonLIGO to Vim.

Syntax highlighting is supported out-of-the box. Language features are provided
via LSP. The language server (`ligo-squirrel`) should be installed separately.
See the [README](../../../../lsp/README.md) for `ligo-squirrel` for instructions.

## Installation
Simply copy the plugin to the `~/.vim/pack` directory: 
```
mkdir -p ~/.vim/pack
cp -rv <path-to-ligo-repo>/tools/vim/ligo ~/.vim/pack/ligo
```

## Language server configuration

You can use the following language client plugins:
* `vim-lsp`: preconfigured, ensure that ligo-squirrel is on your vim $PATH,
   which you can check with `:echo $PATH`.
* `coc.nvim`:
```
"languageserver": {
  "Ligo": {
    "command": "path/to/ligo-squirrel",
    "filetypes": ["ligo", "mligo", "religo"]
  }
}
```

* `LanguageClient-neovim`:
```
let g:LanguageClient_serverCommands = {
    \ 'ligo': ['path/to/ligo-squirrel'],
    \ 'mligo': ['path/to/ligo-squirrel'],
    \ 'religo': ['path/to/ligo-squirrel'],
    \ }
```
