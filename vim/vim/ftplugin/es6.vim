source ~/.vim/ftplugin/javascript.vim

exe ':EmmetInstall'

if exists('+omnifunc')
  augroup Autocompletion
    autocmd!
    setl omnifunc=javascriptcomplete#CompleteJS
  augroup end
else
  echom "Warning this instalation of VIM doesn't support 'omnifunc'."
endif

if exists('*TsuDefinition')
  let g:tsuquyomi_completion_detail = 1
  let g:tsuquyomi_definition_split = 0
  let g:tsuquyomi_disable_quickfix = 1
  let g:tsuquyomi_javascript_support = 1
  let g:tsuquyomi_disable_default_mappings = 1
  nnoremap <buffer> <C-]> :TsuDefinition<cr>
endif
