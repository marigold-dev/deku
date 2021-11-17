if executable('ligo-squirrel')
  if !exists("autocommands_loaded")
    let autocommands_loaded=1
    augroup ligoRegisterLanguageServer
      autocmd User lsp_setup
          \ call lsp#register_server({
          \   'name': 'ligo_lsp',
          \   'cmd': {server_info->['ligo-squirrel']},
          \   'allowlist': ['ligo', 'mligo', 'religo'],
          \ })
    augroup END
  endif
endif
