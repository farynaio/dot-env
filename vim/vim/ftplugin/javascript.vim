setlocal completeopt+=preview

exe ':EmmetInstall'

if executable('js-beautify')
  noremap <buffer> <leader><c-f> :call RangeJsBeautify()<cr>
else
  echom "'js-beautify' is not installed."
endif

let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1
let g:vim_json_syntax_conceal = 0
let g:coverage_json_report_path = './coverage/coverage-final.json'

if exists('+omnifunc')
  if has('autocmd')
    augroup Autocompletion
      autocmd!
      setl omnifunc=javascriptcomplete#CompleteJS
    augroup end
  endif
else
  echom "Warning this instalation of VIM doesn't support 'omnifunc'."
endif

if exists('g:project_type') && (g:project_type ==? 'react' || g:project_type ==? 'react-native' || g:project_type ==? 'node')
  if exists('*TsuDefinition') 
    let g:tsuquyomi_completion_detail = 1
    let g:tsuquyomi_definition_split = 0
    let g:tsuquyomi_disable_quickfix = 1
    let g:tsuquyomi_javascript_support = 1
    let g:tsuquyomi_disable_default_mappings = 1
    nnoremap <buffer> <C-]> :TsuDefinition<cr>
  endif

  " Allow JSX in normal JS files
  let g:jsx_ext_required = 0
  let b:jsx_ext_found = 1
endif

" ================================================
" Linting and formatting
" ================================================

if g:loaded_ale && v:version >= 800 && ThisFileLintingSupported()
  let g:ale_linter_aliases[&ft] = 'javascript'

  if executable('eslint') 
    let g:ale_linters[&ft] = ['eslint']
    let g:ale_fixers[&ft] = ['eslint']
  endif
endif

if exists('g:loaded_istanbul')
  let b:istanbulShown = 0

  function! IstanbulToggle() abort
    if exists('g:coverage_json_report_path')
      echom 'Variable g:coverage_json_report_path is not set.'
      return 0
    endif

    if !filereadable(expand(g:coverage_json_report_path))
      echom 'Coverate file ' . g:coverage_json_report_path . ' dont exists!'
      return 0
    endif

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
