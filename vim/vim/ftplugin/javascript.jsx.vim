
" augroup JSX_Autocmd
"   autocmd!
"   autocmd BufNewFile,BufReadPost * if &ft == 'javascript.jsx' | call <SID>TLoad() | endif
" augroup END

setlocal completeopt+=preview

if exists('*TsuDefinition')
  let g:tsuquyomi_completion_detail = 1
  let g:tsuquyomi_definition_split = 0
  let g:tsuquyomi_disable_quickfix = 1
  let g:tsuquyomi_javascript_support = 1
  let g:tsuquyomi_disable_default_mappings = 1
  nnoremap <buffer> <C-]> :TsuDefinition<cr>
endif

if exists('+omnifunc')
  augroup Autocompletion
    autocmd!
    setl omnifunc=javascriptcomplete#CompleteJS
  augroup end
else
  echom "Warning this instalation of VIM doesn't support 'omnifunc'."
endif

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

let g:jsx_ext_required = 0 " Allow JSX in normal JS files

function LintBuffer()
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

