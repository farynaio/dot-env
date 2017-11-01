
" augroup JSX_Autocmd
"   autocmd!
"   autocmd BufNewFile,BufReadPost * if &ft == 'javascript.jsx' | call <SID>TLoad() | endif
" augroup END

source ~/.vim/ftplugin/es6.vim

let g:jsx_ext_required = 0 " Allow JSX in normal JS files

function! LintBuffer()
  if executable('jshint')
    if filereadable(glob('~/.jshintrc'))
      call JSHintUpdate()
      execute '!jshint --config ~/.jshintrc'
    else
      echo 'jshint config "~./.jshintrc" does not exists.'
    endif
  else
    echo 'jshint is not installed.'
  endif
endfunction

" Lint JS buffers on save.
" au BufWritePost *.js,*.json,*.jsx,*.tsx :call LintBuffer()

