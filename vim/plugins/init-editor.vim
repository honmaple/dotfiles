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
