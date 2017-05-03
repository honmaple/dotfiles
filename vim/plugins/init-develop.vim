Plugin 'w0rp/ale'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
" Plugin 'Shougo/neocomplete.vim'
Plugin 'Valloric/YouCompleteMe'

" ale {
    let g:ale_linters = {
                    \   'sh' : ['shellcheck'],
                    \   'html' : ['tidy'],
                    \   'python' : ['flake8'],
                    \   'markdown' : ['mdl'],
                    \   'javascript' : ['eslint'],
                    \}
    let g:ale_sign_column_always = 1
    let g:ale_set_highlights = 0
    let g:ale_sign_error = '>>'
    let g:ale_sign_warning = '--'
    let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']
    let g:ale_echo_msg_error_str = 'Error'
    let g:ale_echo_msg_warning_str = 'Warning'
" }

" neocomplete.vim {
    set shortmess+=c
    let g:clang_user_options='|| exit 0'
    let g:ycm_key_list_select_completion = ['<Down>']
    let g:ycm_key_list_previous_completion = ['<Up>']
    let g:ycm_complete_in_comments = 1  "在注释输入中也能补全
    let g:ycm_complete_in_strings = 1   "在字符串输入中也能补全
    let g:ycm_use_ultisnips_completer = 1 "提示UltiSnips
    let g:ycm_collect_identifiers_from_comments_and_strings = 1   "注释和字符串中的文字也会被收入补全
    let g:ycm_collect_identifiers_from_tags_files = 1
    let g:ycm_seed_identifiers_with_syntax=1   "语言关键字补全, 不过python关键字都很短，所以，需要的自己打开
    let g:ycm_cache_omnifunc=0 " 禁止缓存匹配项,每次都重新生成匹配项
    " 跳转到定义处, 分屏打开
    let g:ycm_goto_buffer_command = 'horizontal-split'
    " nnoremap <leader>jd :YcmCompleter GoToDefinition<CR>
    nnoremap <leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>
    nnoremap <leader>gd :YcmCompleter GoToDeclaration<CR>

    if !empty(glob("~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py"))
        let g:ycm_global_ycm_extra_conf = "~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py"
    endif

    " YCM 补全菜单配色
    " 菜单
    " highlight Pmenu ctermfg=2 ctermbg=0 guifg=#005f87 guibg=#EEE8D5
    " 选中项
    " highlight PmenuSel ctermfg=2 ctermbg=3 guifg=#AFD700 guibg=#106900
    " 直接触发自动补全 insert模式下
    " let g:ycm_key_invoke_completion = '<C-Space>'
    " 黑名单,不启用
    highlight Pmenu guifg=#C269FE guibg=#34323e
    highlight PmenuSbar guifg=#C269FE guibg=#303030
    highlight PmenuSel guifg=#C269FE guibg=#303030

    let g:ycm_filetype_blacklist = {
                \ 'tagbar' : 1,
                \ 'gitcommit' : 1,
                \ 'nerdtree' : 1
                \}
    "set completeopt-=preview
" }

" ultisnips {
    if isdirectory(expand("~/.vim/bundle/ultisnips/"))
        "let g:UltiSnipsExpandTrigger = '<C-space>'
        "let g:UltiSnipsJumpForwardTrigger = '<Down>'
        "let g:UltiSnipsJumpBackwardTrigger = '<Up>'
        "定义存放代码片段的文件夹 .vim/snippets下，使用自定义和默认的，将会的到全局，有冲突的会提示
        "let g:UltiSnipsSnippetDirectories=['bundle/vim-snippets', 'bundle/ultisnips']
        let g:UltiSnipsSnippetDirectories=['bundle/vim-snippets']

        function! g:UltiSnips_Complete()
            call UltiSnips#ExpandSnippet()
            if g:ulti_expand_res == 0
                if pumvisible()
                    return "\<Down>"
                else
                    call UltiSnips#JumpForwards()
                    if g:ulti_jump_forwards_res == 0
                        return "\<TAB>"
                    endif
                endif
            endif
            return ""
        endfunction

        autocmd BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
        let g:UltiSnipsJumpForwardTrigger="<tab>"
        "let g:UltiSnipsListSnippets="<c-e>"
        " let g:ulti_expand_res = 1
        " function! Ulti_ExpandOrEnter()
            " call UltiSnips#ExpandSnippet()
            " if g:ulti_expand_res
                " return ''
            " else
                " return "\<return>"
        " endfunction

        " " Set <space> as primary trigger
        " inoremap <return> <C-R>=Ulti_ExpandOrEnter()<CR>
        "回车即选中当前项
        inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
        if has('conceal')
            set conceallevel=2 concealcursor=i
        endif
    endif
" }
