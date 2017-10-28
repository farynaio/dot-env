setlocal completeopt+=preview

if executable('js-beautify')
  noremap <buffer> <leader><c-f> :call RangeJsBeautify()<cr>
  " noremap <buffer> <leader><c-f> :call RangeJsxBeautify()<cr>
else
  echom "Warrning 'js-beautify' is not installed."
endif

let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1
let g:vim_json_syntax_conceal = 0

" Override eslint with local version where necessary.
let local_eslint = finddir('node_modules', '.;') . '/.bin/eslint'
if matchstr(local_eslint, "^\/\\w") == ''
  let local_eslint = getcwd() . "/" . local_eslint
endif

if executable(local_eslint)
  let g:syntastic_javascript_checkers = ['eslint']
  let g:syntastic_javascript_eslint_exec = local_eslint
endif

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
