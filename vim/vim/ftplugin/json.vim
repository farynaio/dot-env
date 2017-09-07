" Fix for JSON syntax highlighting (vim-json plugin)
hi! def link jsonKeyword Identifier

let g:vim_json_syntax_conceal = 0

if executable('js-beautify')
  nnoremap <buffer> <leader><c-f> :call RangeJsonBeautify()<cr>
endif

