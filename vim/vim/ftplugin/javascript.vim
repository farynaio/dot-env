source ~/.vim/ftplugin/devel.vim

setlocal completeopt+=preview

if executable('js-beautify')
  noremap <buffer> <leader><c-f> :call RangeJsBeautify()<cr>
else
  echom "'js-beautify' is not installed."
endif

let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1
let g:vim_json_syntax_conceal = 0

" ================================================
" Linting and formatting
" ================================================

if g:loaded_ale && v:version >= 800 && ThisFileLintingSupported()
  let b:ale_linters = {}
  let b:ale_fixers = {}
  let b:ale_linter_aliases = {}
  let b:ale_linter_aliases[&ft] = 'javascript'

  if executable('eslint')
    let b:ale_linters[&ft] = ['eslint']
    let b:ale_fixers[&ft] = ['eslint']
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
