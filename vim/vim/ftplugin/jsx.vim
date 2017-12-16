source ~/.vim/ftplugin/es6.vim

if !exists("s:isJsxGlobalsInitiated")
  let s:isJsxGlobalsInitiated = 1

  " Allow JSX in normal JS files
  let g:jsx_ext_required = 0
endif

let b:jsx_ext_found = 1

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
