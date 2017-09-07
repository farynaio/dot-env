" Fix for JSON syntax highlighting (vim-json plugin)
hi! def link jsonKeyword Identifier

if executable('js-beautify')
  nnoremap <buffer> <leader><c-f> :call RangeJsonBeautify()<cr>
endif

