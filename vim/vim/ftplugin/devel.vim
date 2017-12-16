function! ThisFileLintingSupported() abort
  let l:currentFileName = expand('%:t')

  if exists('g:linterIgnoredSuffixes')
    for item in g:linterIgnoredSuffixes
      if l:currentFileName =~ item
        return false
      endif
    endfor
  endif

  return 1
endfunction

if exists('g:loaded_ale') && ThisFileLintingSupported()
  function! LinterStatusbar() abort
      let l:message = '['
      let l:isLinterDefined = !empty(b:ale_linters[&ft])

      if l:isLinterDefined
        let l:count = ale#statusline#Count(bufnr(''))

        let l:errors = 0
        let l:warnings = 0
        let l:styleError = 0
        let l:totalErrors = 0

        if type(l:count) == type({})
          let l:errors = l:count.error
          let l:warnings = l:count.warning
          let l:styleError = l:count.style_error
          let l:totalErrors = l:count.total

        elseif type(l:count) == type([])
          let [ l:errors, l:warnings ] = l:count
          let l:totalErrors = l:errors
        endif

        let l:totalIssues = l:errors + l:warnings + l:styleError
        let l:message .= 'Linter: '

        if l:totalIssues
          let l:all_errors = l:errors + l:styleError
          let l:all_non_errors = l:totalErrors - l:all_errors
          let l:message .= printf('%dW %dE', all_non_errors, all_errors)
        else
          let l:message .= 'OK'
        endif

      else
        let l:messages .= 'No Linter'
      endif

      return l:message . ']'
    endif
  endfunction

  setlocal statusline=%<%f\ %y%h%m%r%{fugitive#statusline()}\ %{LinterStatusbar()}%=%-14.(%l,%c%V%)\ %P
endif

