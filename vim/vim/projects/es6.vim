if has('autocmd')
  augroup ES6_AUTO_CMD
    autocmd!
    autocmd BufNewFile,BufRead *.js set filetype=es6 | set syntax=javascript
    autocmd BufRead,BufNewFile *rc set syntax=json
  augroup END
endif

let g:coverage_json_report_path = './coverage/coverage-final.json'
