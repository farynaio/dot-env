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

" ================================================
" Linting and formatting
" ================================================

" Override eslint with local version where necessary.
" let local_eslint = finddir('node_modules', '.;') . '/.bin/eslint'
" if matchstr(local_eslint, "^\/\\w") == ''
"   let local_eslint = getcwd() . "/" . local_eslint
" elseif executable('eslint')
"   let local_eslint = 'eslint'
" endif

if executable('eslint')
  if g:loaded_ale
    if v:version >= 800
      let b:ale_linters = {}
      let b:ale_linters[&ft] = ['eslint']

      let b:ale_fixers = {}
      let b:ale_fixers[&ft] = ['eslint']

      let b:ale_linter_aliases = {}
      let b:ale_linter_aliases[&ft] = 'javascript'
    endif
  endif
endif

" let g:prettier#autoformat = 0
" let g:prettier#config#print_width = 127
" let g:prettier#config#single_quote = 'false'
" let g:prettier#exec_cmd_async = 1

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

if exists('g:loaded_istanbul')
  let b:istanbulShown = 0

  function! IstanbulToggle()
    if b:istanbulShown
      let b:istanbulShown = 0
      exe "IstanbulHide"
    else
      let b:istanbulShown = 1
      exe "IstanbulShow"
    endif
  endfunction

  command! IstanbulToggle call IstanbulToggle()
  nnoremap <leader>i :call IstanbulToggle()<cr>
endif
