" 修改leader键
let mapleader = ','
let g:mapleader = ','

filetype on                    "启用文件类型侦测
filetype plugin on             "针对不同的文件类型加载对应的插件
filetype plugin indent on      "启用缩进
syntax on                      "代码高亮
syntax enable                      "代码高亮
set t_Co=256

set mouse=a                    "任何模式下启用鼠标
set mousehide                  "Hide the mouse cursor while typing
scriptencoding utf-8
" 设置 退出vim后，内容显示在终端屏幕, 可以用于查看和复制, 不需要可以去掉
" set t_ti= t_te=

if has('clipboard')
    if has('unnamedplus')  " When possible use + register for copy-paste
        set clipboard=unnamed,unnamedplus
    else         " On mac and Windows, use * register for copy-paste
        set clipboard=unnamed
    endif
endif

set shortmess+=filmnrxoOtT          "去掉欢迎界面
set guifont=Monospace\ 12
set viewoptions=folds,options,cursor,unix,slash " Better Unix / Windows compatibility
set virtualedit=onemore             " Allow for cursor beyond last character
set history=1000                    " Store a ton of history (default is 20)
"set spell                           "启用拼写检查
set hidden                          " Allow buffer switching without saving
set iskeyword-=.                    " '.' is an end of word designator
set iskeyword-=#                    " '#' is an end of word designator
set iskeyword-=-                    " '-' is an end of word designator

"set backup                          "设置备份文件
"if has('persistent_undo')
"    set undofile                " So is persistent undo ...
"    set undolevels=1000         " Maximum number of changes that can be undone
"    set undoreload=10000        " Maximum number lines to save for undo on a buffer reload
"endif

set tabpagemax=15               " Only show 15 tabs
set showmode                    " Display the current mode

set cursorline                  "高亮光标所在行
set cuc                         "高亮光标所在列
set colorcolumn=80
highlight clear SignColumn      " SignColumn should match background
highlight clear LineNr          " Current line number row will have same background color in relative mode
"highlight clear CursorLineNr    " Remove highlight color from current line number

if has('cmdline_info')
    set ruler                   " Show the ruler
    set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " A ruler on steroids
    set showcmd                 " Show partial commands in status line and
" Selected characters/lines in visual mode
endif

if has('statusline')
        set laststatus=2                         "启用状态栏信息
        set statusline=%<%f\                     " Filename
        set statusline+=%w%h%m%r                 " Options
    "  if !exists('g:override_bundles')
        "set statusline+=%{fugitive#statusline()} " Git Hotness
        "endif
        set statusline+=\ [%{&ff}/%Y]            " Filetype
        set statusline+=\ [%{getcwd()}]          " Current dir
        set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
endif

set backspace=indent,eol,start  " Backspace for dummies
set linespace=0                 " No extra spaces between rows
set number                      "显示行号
set relativenumber number       "设置相对行号
au FocusLost * :set norelativenumber number
au FocusGained * :set relativenumber
" 插入模式下用绝对行号, 普通模式下用相对
autocmd InsertEnter * :set norelativenumber number
autocmd InsertLeave * :set relativenumber
function! NumberToggle()
    if(&relativenumber == 1)
        set norelativenumber number
    else
        set relativenumber
    endif
endfunc
set scrolloff=10                "在上下移动光标时，光标的上方或下方至少会保留显示的行数
set showmatch                   "高亮显示匹配的括号
set incsearch                   "在输入要搜索的文字时，实时匹配
set hlsearch                    "高亮搜索
"set winminheight=0              " Windows can be 0 line high
set ignorecase                  "搜索模式里忽略大小写
set smartcase                   "如果搜索模式包含大写字符，不使用'ignorecase' 选项，只有在输入搜索模式并且打开 'ignorecase' 选项时才会使用
set wildmenu
" 增强模式中的命令行自动完成操作
set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.
set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too
"让Vim的补全菜单行为与一般IDE一致(参考VimTip1228)
set completeopt=longest,menu
set wildignore=*.o,*~,*.pyc,*.class
"set scrolljump=5                " Lines to scroll when cursor leaves screen
"set scrolloff=3                 " Minimum lines to keep above and below cursor
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace
set showcmd                       "在状态栏显示正在输入的命令

set nowrap                      "设置不自动换行
"set autoindent                  "打开自动缩进
set shiftwidth=4                "换行时自动缩进宽度，可更改（宽度同tabstop）
set expandtab                   "将Tab键转换为空格
set tabstop=4                   "设置Tab键的宽度，可以更改，如：宽度为2
"autocmd FileType haskell,rust setlocal nospell

let g:FoldMethod = 0
fun! ToggleFold()
    if g:FoldMethod == 0
        exe "normal! zM"
        let g:FoldMethod = 1
    else
        exe "normal! zR"
        let g:FoldMethod = 0
    endif
endfun

function! ToggleBG()
    let s:tbg = &background
    " Inversion
    if s:tbg == "dark"
        set background=light
    else
        set background=dark
    endif
endfunction


"set smartindent                "启用智能对齐方式
"set shiftround                 "缩进时，取整
"set showtabline=1              "显示标签
set smarttab                   "指定按一次backspace就删除shiftwidth宽度
"set foldmethod=indent          "indent 折叠方式
"set foldmethod=syntax
"set foldenable                  "启用折叠
set foldmethod=marker
" 启动 vim 时关闭折叠代码
set nofoldenable
"set matchtime=5                "匹配括号高亮的时间（单位是十分之一秒）
"set autoread                   "当文件在外部被修改，自动更新该文件
"set autowrite                  "自动保存
" set vb t_vb=                   "关闭提示音

" 显示/隐藏菜单栏、工具栏、滚动条，可用 Ctrl + F11 切换
if (g:is_gui)
    winpos 100 10                 "指定窗口出现的位置，坐标原点在屏幕左上角
    set guiheadroom=0
    set lines=38 columns=120
    set guioptions-=m
    set guioptions-=T
    set guioptions-=r
    set guioptions-=L
    nmap <silent> <c-F11> :if &guioptions =~# 'm' <Bar>
                \set guioptions-=m <Bar>
                \set guioptions-=T <Bar>
                \set guioptions-=r <Bar>
                \set guioptions-=L <Bar>
                \else <Bar>
                \set guioptions+=m <Bar>
                \set guioptions+=T <Bar>
                \set guioptions+=r <Bar>
                \set guioptions+=L <Bar>
                \endif<CR>
endif
