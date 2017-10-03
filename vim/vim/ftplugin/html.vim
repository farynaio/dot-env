exe ':EmmetInstall'

if executable('js-beautify')
  noremap <buffer> <leader><c-f> :call RangeHtmlBeautify()<cr>
endif

