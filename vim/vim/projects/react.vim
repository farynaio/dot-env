if has('autocmd')
  augroup REACT_AUTO_CMD
    autocmd!
    autocmd BufNewFile,BufRead *.js set filetype=react | set syntax=javascript
    autocmd BufRead,BufNewFile *rc set syntax=json
  augroup END
endif

let g:coverage_json_report_path = './coverage/coverage-final.json'
