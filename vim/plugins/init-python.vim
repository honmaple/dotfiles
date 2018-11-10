Plug 'klen/python-mode'

" PyMode {
    " Disable if python support not present
    if !has('python') && !has('python3')
        let g:pymode = 0
    endif

    let g:pymode_python = 'python3'
    let g:pymode_lint = 0
    let g:pymode_lint_checkers = ['pyflakes']
    let g:pymode_trim_whitespaces = 1
    let g:pymode_options = 0
    let g:pymode_rope = 0
" }
" python语法实时检查 {
    " python fly check, 弥补syntastic只能打开和保存才检查语法的不足
    "let g:pyflakes_use_quickfix = 0

    " for python.vim syntax highlight
    " let python_highlight_all = 1
" }
