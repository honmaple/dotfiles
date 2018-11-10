Plug 'groenewege/vim-less'
Plug 'pangloss/vim-javascript'
Plug 'briancollins/vim-jst'
Plug 'kchmck/vim-coffee-script'
" Plug 'amirh/HTML-AutoCloseTag'
Plug 'mattn/emmet-vim'
"Plug 'ap/vim-css-color'
Plug 'hail2u/vim-css3-syntax'
Plug 'gorodinskiy/vim-coloresque'
Plug 'tpope/vim-haml'

" javascript {
    let g:html_indent_inctags = "html,body,head,tbody"
    let g:html_indent_style1 = "inc"
" }
" JSON {
    nmap <leader>jt <Esc>:%!python -m json.tool<CR><Esc>:set filetype=json<CR>
    let g:vim_json_syntax_conceal = 0
" }
" AutoCloseTag {
    " Make it so AutoCloseTag works for xml and xhtml files as well
    au FileType xhtml,xml ru ftplugin/html/autoclosetag.vim
    nmap <Leader>hh <Plug>ToggleAutoCloseMappings
" }
" emmet {
    " let g:user_emmet_expandabbr_key = '<Tab>'
    " Enable just for html/css
    let g:user_emmet_install_global = 0
    " autocmd FileType html,css EmmetInstall
" }
au BufRead,BufNewFile *.{html,janja,htmljanja} set filetype=html
