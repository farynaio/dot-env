" CSS - fix vim-css3-syntax highlighiting
setlocal iskeyword+=-
" augroup VimCSS3Syntax
" autocmd!
"   autocmd FileType css setlocal iskeyword+=-
" augroup END
"
exe ':EmmetInstall'

if executable('js-beautify')
  noremap <buffer> <leader><c-f> :call RangeCSSBeautify()<cr>
endif

