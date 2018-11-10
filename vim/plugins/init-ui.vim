Plug 'altercation/vim-colors-solarized'
Plug 'tomasr/molokai'
" Plug 'spf13/vim-colors'
Plug 'bling/vim-airline'
Plug 'powerline/fonts'
Plug 'luochen1990/rainbow'
Plug 'Yggdroot/indentLine'
Plug 'liuchengxu/space-vim-dark'
Plug 'mhinz/vim-startify'



if $TERM=~'cons25'
    colorscheme default
elseif g:is_gui
    set background=dark
    " let g:solarized_termcolors=256
    " let g:solarized_termtrans = 0
    " colorscheme solarized
    let g:molokai_original = 1
    colorscheme molokai
else
    set background=dark
    let g:solarized_termcolors=256
    let g:solarized_termtrans = 0
    " colorscheme molokai
    colorscheme solarized
endif
" indentLine {
    " 用于显示对齐线
    let g:indentLine_char = "┊"
    let g:indentLine_first_char = "┊"
    " 色块宽度
    "let g:indent_guides_guide_size=1
    " 设置终端对齐线颜色，如果不喜欢可以将其注释掉采用默认颜色
    let g:indentLine_color_term = 256
" }
" vim-airline {
    " let g:airline#extensions#tabline#enabled = 1
    let g:airline_powerline_fonts=1
    if !exists('g:airline_theme')
        let g:airline_theme = 'dark'
    endif
" }
" rainbow {
    let g:rainbow_active = 1
" }
