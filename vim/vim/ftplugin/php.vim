source ~/.vim/ftplugin/devel.vim

exe ':EmmetInstall'

" ================================================
" Tabularize
" ================================================

if exists(":Tabularize")
  noremap <buffer> <leader>a=> :Tabularize /=><cr>
  " vnoremap <leader>a=> :Tabularize /=><cr>
endif

