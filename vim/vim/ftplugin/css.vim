" CSS - fix vim-css3-syntax highlighiting
setlocal iskeyword+=-

" let g:cssColorVimDoNotMessMyUpdatetime = 1
" augroup VimCSS3Syntax
" autocmd!
"   autocmd FileType css setlocal iskeyword+=-
" augroup END
"
exe ':EmmetInstall'

" ================================================
" CSSComb
" ================================================
" autocmd FileType css,scss noremap <buffer> <leader>bc :CSScomb<cr>
"
if executable('js-beautify')
  noremap <buffer> <leader><c-f> :call RangeCSSBeautify()<cr>
endif

