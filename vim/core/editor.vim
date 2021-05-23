Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'vim-scripts/matchit.zip'
Plug 'gcmt/wildfire.vim'
Plug 'godlygeek/tabular'
Plug 'majutsushi/tagbar'
Plug 'kristijanhusak/vim-multiple-cursors'
Plug 'tpope/vim-commentary'
Plug 'Chiel92/vim-autoformat'
Plug 'thinca/vim-quickrun'

Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" tagbar {
    "nmap tb :TlistClose<CR>:TagbarToggle<CR>
    let g:tagbar_width=20                       "设置窗口宽度
    let g:tagbar_left=0                         "在左侧窗口中显示
    let g:tagbar_compact=1                      "不显示帮助信息
    "tagbar_map_togglefold 打开tagbar变量  "o"
    " set tags=./tags;
    " 自动切换目录为当前编辑文件所在目录
    " au BufRead,BufNewFile,BufEnter * cd %:p:h
" }
" wildfire {
    let g:wildfire_objects = {
            \ "*" : ["i'", 'i"', "i\)", "i\]", "i\}", "ip","it"],
            \ "html,xml" : ["at","i>",'i"',"i\)"]}
" }
" vim-multiple-cursors {
    " default mapping
    let g:multi_cursor_next_key='<c-n>'
    let g:multi_cursor_prev_key='<c-p>'
    let g:multi_cursor_skip_key='<c-x>'
    let g:multi_cursor_quit_key='<esc>'
" }
" Tabularize {
    " 对齐
    " nmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
    " vmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
" }
" quickrun {
    let g:quickrun_config = {
    \   "_" : {
    \       "outputter" : "message",
    \   },
    \}
    let g:quickrun_no_default_key_mappings = 1
" }
" vim-fugitive {
    nnoremap <silent> <leader>gs :Gstatus<CR>
    nnoremap <silent> <leader>gd :Gdiff<CR>
    nnoremap <silent> <leader>gc :Gcommit<CR>
    nnoremap <silent> <leader>gb :Gblame<CR>
    nnoremap <silent> <leader>gl :Glog<CR>
    nnoremap <silent> <leader>gp :Git push<CR>
    nnoremap <silent> <leader>gr :Gread<CR>
    nnoremap <silent> <leader>gw :Gwrite<CR>
    nnoremap <silent> <leader>ge :Gedit<CR>
    " Mnemonic _i_nteractive
    nnoremap <silent> <leader>gi :Git add -p %<CR>
    nnoremap <silent> <leader>gg :SignifyToggle<CR>
"}
"vim-gitgutter {
    let g:gitgutter_sign_added = '+'
    let g:gitgutter_sign_modified = '~'
    let g:gitgutter_sign_removed = '-'

    " let g:gitgutter_sign_removed_first_line = '^^'
    " let g:gitgutter_sign_modified_removed = 'ww'
    let g:gitgutter_override_sign_column_highlight = 0
" }
" file-header {
    let g:maple_header_template = [
        \repeat('*', 79),
        \'Copyright © '.strftime('%Y'),
        \'File Name: '.expand('%'),
        \'Author: '.g:username,
        \'Email: '.g:email,
        \'Created: '..strftime('%F %T (%Z)'),
        \'Last Update:',
        \'         By:',
        \'Description:',
        \repeat('*', 79),
        \]

    let g:maple_header_alist = {
        \'py': {
        \    'block': 0,
        \    'prefix': ['#!/usr/bin/env python', '# -*- coding=UTF-8 -*-'],
        \},
        \'go': {
        \    'block': 1,
        \},
        \'c': {
        \    'block': 1,
        \    'suffix': ['#include<stdio.h>', '#include<string.h>'],
        \},
        \'sh': {
        \    'block': 0,
        \    'prefix': ['#!/bin/bash'],
        \},
    \}

    let g:maple_header_update_alist = {
    \'File Name': 's@:.*$@\=": ".expand("%:t")@',
    \'Last Update': 's@:.*$@\=": ".strftime("%A %T (%Z)")@',
    \'By': 's@:.*$@\=": ".g:username@',
    \}

    function! MatchFileType()
        return has_key(g:maple_header_alist, expand('%:e'))
    endfunction

    function! InsertHeader()
        let l:ext = expand('%:e')
        if has_key(g:maple_header_alist, ext) == 0
            return
        endif

        let l:index = 0
        let l:alist = g:maple_header_alist[ext]
        let l:block = get(alist, 'block', 0)
        let l:lines = get(alist, 'prefix', [])
        let l:comment = get(alist, 'comment', '# ')
        let l:template = get(alist, 'template', g:maple_header_template)

        for m in l:template
            if l:block == 1
                if l:index == 0
                    let l:lines = add(l:lines, '/*'.m)
                elseif l:index == len(l:template) - 1
                    let l:lines = add(l:lines, m.'*/')
                else
                    let l:lines = add(l:lines, ' '.m)
                endif
            else
                let l:lines = add(l:lines, l:comment . m)
            endif
            let l:index += 1
        endfor
        call append(0, l:lines + get(l:alist, 'suffix', []))
    endfunction

    function! UpdateHeader()
        let n=1
        while n < 11
            let line = getline(n)
            for m in items(g:maple_header_update_alist)
                if line =~ '^.*'.m[0].':.*$'
                    execute '/'.m[0].'/'.m[1]
                endif
            endfor
            let n = n + 1
        endwhile
    endfunction

    autocmd BufNewFile * if MatchFileType() | exec ":call InsertHeader()" | endif
    "新建文件后，自动定位到文件末尾
    autocmd BufNewFile * normal G
    autocmd BufWrite * if MatchFileType() | exec ":call UpdateHeader()" | endif
" }
