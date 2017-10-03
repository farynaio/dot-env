exe ':EmmetInstall'

" ================================================
" Tabularize
" ================================================

if exists(":Tabularize")
  nnoremap <leader>a=> :Tabularize /=><cr>
  vnoremap <leader>a=> :Tabularize /=><cr>
endif

