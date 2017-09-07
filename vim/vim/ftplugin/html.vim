" ================================================
" Emmet Support
" ================================================

" let g:user_emmet_install_global = 0
" let g:user_emmet_mode='a'
exe "EmmetInstall"

if executable('js-beautify')
  noremap <buffer> <leader><c-f> :call RangeHtmlBeautify()<cr>
endif

