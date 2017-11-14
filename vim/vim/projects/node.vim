if has('autocmd')
  augroup NODE_AUTO_CMD
    autocmd!
    autocmd BufNewFile,BufRead *.js setl filetype=node | setl syntax=javascript
    autocmd BufRead,BufNewFile *rc setl syntax=json
  augroup END
endif

let g:coverage_json_report_path = './coverage/coverage-final.json'
