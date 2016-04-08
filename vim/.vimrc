" vim: set sw=4 ts=4 sts=4 et tw=78 foldmarker={,} foldlevel=0 foldmethod=marker spell:
" windows linux {
    let g:iswindows = 0
    let g:islinux = 0
    if(has("win32") || has("win64") || has("win95") || has("win16"))
        let g:iswindows = 1
    else
        let g:islinux = 1
    endif

    " -----------------------------------------------------------------------------
    "  < 判断是终端还是 Gvim >
    " -----------------------------------------------------------------------------
    if has("gui_running")
        let g:isGUI = 1
    else
        let g:isGUI = 0
    endif

    " -----------------------------------------------------------------------------
    "  < Windows Gvim 配置>
    " -----------------------------------------------------------------------------
    if (g:iswindows && g:isGUI)
        source $VIMRUNTIME/vimrc_example.vim
        source $VIMRUNTIME/mswin.vim
        behave mswin
        set diffexpr=MyDiff()

        function MyDiff()
            let opt = '-a --binary '
            if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
            if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
            let arg1 = v:fname_in
            if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
            let arg2 = v:fname_new
            if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
            let arg3 = v:fname_out
            if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
            let eq = ''
            if $VIMRUNTIME =~ ' '
                if &sh =~ '\<cmd'
                    let cmd = '""' . $VIMRUNTIME . '\diff"'
                    let eq = '"'
                else
                    let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
                endif
            else
                let cmd = $VIMRUNTIME . '\diff'
            endif
            silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3 . eq
        endfunction
    endif
" }

" PluginEnvironment {

    " Basics {
        set nocompatible        " Must be first line
        " set background=dark     " Assume a dark background
    " }

    " Windows Compatible {
        if has('win32') || has('win64')
          set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after

          if has("multi_byte")
            set termencoding=cp850
            set encoding=utf-8
            setglobal fileencoding=utf-8
            set fileencodings=ucs-bom,utf-8,utf-16le,cp1252,iso-8859-15
          endif
        endif
    " }

    " Setup Plugin Support {
        filetype off
        set rtp+=~/.vim/bundle/Vundle.vim
    " }

" }

" Plugins插件 {

    call vundle#begin()
    " Deps {
        Plugin 'VundleVim/Vundle.vim'
        Plugin 'MarcWeber/vim-addon-mw-utils'
        Plugin 'tomtom/tlib_vim'
    " }

    if !exists('g:bundle_groups')
        let g:bundle_groups=['general', 'writing', 'youcompleteme', 'programming', 'php', 'ruby', 'python', 'javascript', 'html', 'misc',]
    endif

    "   let g:override_bundles = 1
    if !exists("g:override_bundles")

    " General {
        if count(g:bundle_groups, 'general')
            Plugin 'scrooloose/nerdtree'
            Plugin 'altercation/vim-colors-solarized'
            " Plugin 'tomasr/molokai'
            Plugin 'spf13/vim-colors'
            " 快速增加括号
            Plugin 'tpope/vim-surround'
            " 重复上次操作
            Plugin 'tpope/vim-repeat'
            Plugin 'jiangmiao/auto-pairs'
            " 搜索文件
            Plugin 'ctrlpvim/ctrlp.vim'
            Plugin 'tacahiroy/ctrlp-funky'
            " 搜索文件内容
            Plugin 'dyng/ctrlsf.vim'   
            " 多光标编辑
            Plugin 'kristijanhusak/vim-multiple-cursors'
            " 括号间%跳转
            Plugin 'vim-scripts/matchit.zip'
            " 状态栏
            Plugin 'bling/vim-airline'
            Plugin 'powerline/fonts'
           " Plugin 'fholgado/minibufexpl.vim'
           " 显示打开的文件
            Plugin 'bling/vim-bufferline'
            "Plugin 'Lokaltog/vim-easymotion'
            Plugin 'jistr/vim-nerdtree-tabs'
            "Plugin 'flazz/vim-colorschemes'
            " vim 历史状态
            Plugin 'sjl/gundo.vim'
            " Plugin 'mbbill/undotree'
            Plugin 'Yggdroot/indentLine'
            "if !exists('g:no_views')
                "Plugin 'vim-scripts/restore_view.vim'
            "endif
            Plugin 'mhinz/vim-signify'
            Plugin 'tpope/vim-abolish.git'
            "Plugin 'osyo-manga/vim-over'
            "Plugin 'kana/vim-textobj-user'
            "Plugin 'kana/vim-textobj-indent'
            " 快速选中
            Plugin 'gcmt/wildfire.vim'
        endif
    " }

    " Writing {
"        if count(g:bundle_groups, 'writing')
            "Plugin 'reedes/vim-litecorrect'
            "Plugin 'reedes/vim-textobj-sentence'
            "Plugin 'reedes/vim-textobj-quote'
            "Plugin 'reedes/vim-wordy'
        "endif
    " }

    " General Programming {
        if count(g:bundle_groups, 'programming')
            " Pick one of the checksyntax, jslint, or syntastic
            Plugin 'scrooloose/syntastic'
            " git
            Plugin 'tpope/vim-fugitive'
            "Plugin 'mattn/webapi-vim'
            "Plugin 'mattn/gist-vim'
            " 括号高亮
            Plugin 'luochen1990/rainbow'
            " 注释
            Plugin 'scrooloose/nerdcommenter'
            " 排版等号之类的对齐
            Plugin 'godlygeek/tabular'
            if executable('ctags')
                Plugin 'majutsushi/tagbar'
            endif
        endif
    " }

    " Snippets & AutoComplete {
        if count(g:bundle_groups, 'youcompleteme')
            Plugin 'Valloric/YouCompleteMe'
            Plugin 'SirVer/ultisnips'
            Plugin 'honza/vim-snippets'
        endif
    " }

    " PHP {
        "if count(g:bundle_groups, 'php')
            "Plugin 'spf13/PIV'
            "Plugin 'arnaud-lb/vim-php-namespace'
            "Plugin 'beyondwords/vim-twig'
        "endif
    " }

    " Python {
        if count(g:bundle_groups, 'python')
            " Pick either python-mode or pyflakes & pydoc
            Plugin 'klen/python-mode'
        endif
    " }

    " Javascript {
        if count(g:bundle_groups, 'javascript')
            " Plugin 'elzr/vim-json'
            Plugin 'groenewege/vim-less'
            Plugin 'pangloss/vim-javascript'
            Plugin 'briancollins/vim-jst'
            Plugin 'kchmck/vim-coffee-script'
        endif
    " }

    " Scala {
"        if count(g:bundle_groups, 'scala')
            "Plugin 'derekwyatt/vim-scala'
            "Plugin 'derekwyatt/vim-sbt'
            "Plugin 'xptemplate'
        "endif
    " }

    " Haskell {
 "       if count(g:bundle_groups, 'haskell')
            "Plugin 'travitch/hasksyn'
            "Plugin 'dag/vim2hs'
            "Plugin 'Twinside/vim-haskellConceal'
            "Plugin 'Twinside/vim-haskellFold'
            "Plugin 'lukerandall/haskellmode-vim'
            "Plugin 'eagletmt/neco-ghc'
            "Plugin 'eagletmt/ghcmod-vim'
            "Plugin 'Shougo/vimproc.vim'
            "Plugin 'adinapoli/cumino'
            "Plugin 'bitc/vim-hdevtools'
        "endif
    " }

    " HTML {
        if count(g:bundle_groups, 'html')
            Plugin 'amirh/HTML-AutoCloseTag'
            Plugin 'mattn/emmet-vim'
            "Plugin 'ap/vim-css-color'
            Plugin 'hail2u/vim-css3-syntax'
            Plugin 'gorodinskiy/vim-coloresque'
            Plugin 'tpope/vim-haml'
            "Plugin 'Glench/Vim-Jinja2-Syntax'
        endif
    " }

    " Ruby {
  "      if count(g:bundle_groups, 'ruby')
            "Plugin 'tpope/vim-rails'
            "let g:rubycomplete_buffer_loading = 1
            "let g:rubycomplete_classes_in_global = 1
            "let g:rubycomplete_rails = 1
   "     endif
    " }

    " Puppet {
    "    if count(g:bundle_groups, 'puppet')
            "Plugin 'rodjek/vim-puppet'
        "endif
    " }

    " Go Lang {
     "   if count(g:bundle_groups, 'go')
            ""Plugin 'Blackrush/vim-gocode'
            "Plugin 'fatih/vim-go'
        "endif
    " }

    " Elixir {
      "  if count(g:bundle_groups, 'elixir')
            "Plugin 'elixir-lang/vim-elixir'
            "Plugin 'carlosgaldino/elixir-snippets'
            "Plugin 'mattreduce/vim-mix'
        "endif
    " }

    " Misc {
        if count(g:bundle_groups, 'misc')
       "     Plugin 'rust-lang/rust.vim'
            "Plugin 'plasticboy/vim-markdown'
            Plugin 'tpope/vim-markdown'
            "Plugin 'spf13/vim-preview'
            "Plugin 'tpope/vim-cucumber'
            "Plugin 'cespare/vim-toml'
            "Plugin 'quentindecock/vim-cucumber-align-pipes'
            "Plugin 'saltstack/salt-vim'
            Plugin 'iamcco/markdown-preview.vim'
        endif
    " }

    endif
    call vundle#end()            " required
    filetype plugin indent on    " required
" }

" Plugins {
" viewoptions {
    " indentLine {
        if isdirectory(expand("~/.vim/bundle/indentLine/"))
            " 用于显示对齐线
            let g:indentLine_char = "┊"
            let g:indentLine_first_char = "┊"
            " 色块宽度
            "let g:indent_guides_guide_size=1
            " 设置终端对齐线颜色，如果不喜欢可以将其注释掉采用默认颜色
            let g:indentLine_color_term = 256
        endif
    " }
    " vim-airline {
        " let g:airline#extensions#tabline#enabled = 1
        if isdirectory(expand("~/.vim/bundle/vim-airline/"))
            let g:airline_powerline_fonts=1
            if !exists('g:airline_theme')
                let g:airline_theme = 'dark'
            endif
        endif
    " }
" }
" 文件操作 {
    " syntastic {
        set statusline+=%#warningmsg#
        set statusline+=%{SyntasticStatuslineFlag()}
        set statusline+=%*
        let g:syntastic_error_symbol='>>'
        let g:syntastic_warning_symbol='>'
        "let g:syntastic_always_populate_loc_list = 1
        "let g:syntastic_auto_loc_list = 1
        let g:syntastic_check_on_open=1
        let g:syntastic_check_on_wq=0
        let g:syntastic_enable_highlighting=1
        let g:syntastic_ignore_files=[".*\.py$"]
        " let g:syntastic_python_checkers=['pyflakes'] " 使用pyflakes
        "let g:syntastic_python_pep8_args='--ignore=E501,E225'
        "let g:syntastic_python_checkers=['pyflakes']
        "let g:syntastic_python_checkers=['pylint']
         "let g:syntastic_python_pylint_args='--disable=C0111,R0903,C0301'


        let g:syntastic_javascript_checkers = ['jsl', 'jshint']
        let g:syntastic_html_checkers=['tidy', 'jshint']

        " 修改高亮的背景色, 适应主题
        highlight SyntasticErrorSign guifg=white guibg=black

        " to see error location list
        let g:syntastic_always_populate_loc_list = 0
        let g:syntastic_auto_loc_list = 0
        let g:syntastic_loc_list_height = 5
    " }
    " YouCompleteMe {
        if isdirectory(expand("~/.vim/bundle/YouCompleteMe/"))
            "youcompleteme  默认tab  s-tab 和自动补全冲突
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
            let g:ycm_filetype_blacklist = {
                        \ 'tagbar' : 1,
                        \ 'gitcommit' : 1,
                        \ 'nerdtree' : 1
                        \}
            "set completeopt-=preview
        endif
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
    " tagbar {

    if isdirectory(expand("~/.vim/bundle/tagbar/"))
        "nmap tb :TlistClose<CR>:TagbarToggle<CR>

        let g:tagbar_width=20                       "设置窗口宽度
        let g:tagbar_left=0                         "在左侧窗口中显示
        let g:tagbar_compact=1                      "不显示帮助信息
        "tagbar_map_togglefold 打开tagbar变量  "o"

        let g:tagbar_type_python = {
            \ 'kinds' : [
                \ 'i:imports:1:0',
                \ 'c:classes',
                \ 'f:functions',
                \ 'm:members',
                \ 'v:variables:0:0',
            \ ],
        \ }
        let g:tagbar_type_c = {
            \ 'kinds' : [
                \ 'd:macros:1:0',
                \ 'p:prototypes:1:0',
                \ 'g:enums',
                \ 'e:enumerators:0:0',
                \ 't:typedefs:0:0',
                \ 's:structs',
                \ 'u:unions',
                \ 'm:members:0:0',
                \ 'v:variables:0:0',
                \ 'f:functions',
            \ ],
        \ }
            set tags=./tags;
            " 自动切换目录为当前编辑文件所在目录
            au BufRead,BufNewFile,BufEnter * cd %:p:h
    endif
    " }
    " NerdTree {
        if isdirectory(expand("~/.vim/bundle/nerdtree"))
            "autocmd BufEnter * :syntax sync fromstart
            "set hid             " 可以在没有保存的情况下切换buffer
            " 自动开启nerdtree
            "let g:nerdtree_tabs_open_on_console_startup=1
            "当打开vim且没有文件时自动打开NERDTree
            " autocmd StdinReadPre * let s:std_in=1
            " autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
            autocmd vimenter * if !argc() | NERDTree | endif
            " 只剩 NERDTree时自动关闭
            autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
            "显示文件
            let NERDTreeShowFiles=1
            let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
            "显示隐藏文件
            let NERDTreeShowHidden=0
            let NERDTreeChDirMode=0
            let NERDTreeQuitOnOpen=1
            let NERDTreeMouseMode=2
            let NERDTreeKeepTreeInNewTab=1
            let NERDTreeWinSize=22
            let NERDTreeWinPos="left"
            "高亮显示当前文件或目录
            let NERDTreeHightCursorline=1
            "不显示'Bookmarks' label 'Press ? for help'
            let NERDTreeMinimalUI=1
            " s/v 分屏打开文件
            let g:NERDTreeMapOpenSplit = 's'
            let g:NERDTreeMapOpenVSplit = 'v'
        endif
    " }
    " nerdcommenter {
            " 注释增加空格
         let g:NERDSpaceDelims=1
        " <leader>cc，注释当前选中文本，如果选中的是整行则在每行首添加 //，如果选中一行的部分内容则在选中部分前后添加分别 / 、 /；
        "<leader>cu，取消选中文本块的注释
    " }
  " Tabularize {
        " 对齐
        if isdirectory(expand("~/.vim/bundle/tabular"))
            nmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
            vmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
        endif
    " }
    " ctrlp {
        if isdirectory(expand("~/.vim/bundle/ctrlp.vim/"))
            let g:ctrlp_working_path_mode = 'ra'
            let g:ctrlp_map = '<c-p>'
            let g:ctrlp_cmd = 'CtrlP'

            let g:ctrlp_custom_ignore = {
                \ 'dir':  '\v[\/]\.(git|hg|svn|rvm)$',
                \ 'file': '\v\.(exe|so|dll|zip|tar|tar.gz|pyc)$',
                \ }

            if executable('ag')
                let s:ctrlp_fallback = 'ag %s --nocolor -l -g ""'
            elseif executable('ack-grep')
                let s:ctrlp_fallback = 'ack-grep %s --nocolor -f'
            elseif executable('ack')
                let s:ctrlp_fallback = 'ack %s --nocolor -f'
            else
                let s:ctrlp_fallback = 'find %s -type f'
            endif
            if exists("g:ctrlp_user_command")
                unlet g:ctrlp_user_command
            endif
            let g:ctrlp_user_command = {
                        \ 'types': {
                        \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
                        \ 2: ['.hg', 'hg --cwd %s locate -I .'],
                        \ },
                        \ 'fallback': s:ctrlp_fallback
                        \ }

        endif
    " }
    " ctrlp-funky {
            if isdirectory(expand("~/.vim/bundle/ctrlp-funky/"))
                " CtrlP extensions
                let g:ctrlp_extensions = ['funky']
                let g:ctrlp_funky_syntax_highlight = 1
                nnoremap <Leader>fu :CtrlPFunky<Cr>
            endif
    " }
    " ctrlsf {
            " 工程内查找文件内容,先安装ag /the_silver_searcher
            let g:ctrlsf_ackprg = 'ag'
            nmap     <C-F>f <Plug>CtrlSFPrompt
            vmap     <C-F>f <Plug>CtrlSFVwordPath
            vmap     <C-F>F <Plug>CtrlSFVwordExec
            nmap     <C-F>n <Plug>CtrlSFCwordPath
            nmap     <C-F>p <Plug>CtrlSFPwordPath
    " }
    " Wildfire {
        let g:wildfire_objects = {
                \ "*" : ["i'", 'i"', "i)", "i]", "i}", "ip","it"],
                \ "html,xml" : ["at","i>",'i"',"i)"]
                \ }
    " vim-multiple-cursors {
        " Default mapping
        let g:multi_cursor_next_key='<C-n>'
        let g:multi_cursor_prev_key='<C-p>'
        let g:multi_cursor_skip_key='<C-x>'
        let g:multi_cursor_quit_key='<Esc>'
     "   }
    " gundo {
        if isdirectory(expand("~/.vim/bundle/gundo.vim/"))
            set undodir=~/.vim/undo
            set undofile
            set undolevels=10         " Maximum number of changes that can be undone
            set undoreload=10        " Maximum number lines to save for undo on a buffer reload
            nnoremap <Leader>U :GundoToggle<CR>
        endif
    " }
    " Session List {
        set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize
        if isdirectory(expand("~/.vim/bundle/sessionman.vim/"))
            nmap <leader>sl :SessionList<CR>
            nmap <leader>ss :SessionSave<CR>
            nmap <leader>sc :SessionClose<CR>
        endif
    " }
    " rainbow {
        let g:rainbow_active = 1
         let g:rainbow_conf = {
            \   'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick'],
            \   'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
            \   'operators': '_,_',
            \   'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
            \   'separately': {
            \       '*': {},
            \       'tex': {
            \           'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
            \       },
            \       'lisp': {
            \           'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick', 'darkorchid3'],
            \       },
            \       'vim': {
            \           'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
            \       },
            \       'html': {
            \           'parentheses': ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold'],
            \       },
            \       'css': 0,
            \   }
            \}
    " }

" }
" HTML/CSS/javascript {
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
" }
" Python {
    " PyMode {
        " Disable if python support not present
        if !has('python')
            let g:pymode = 0
        endif

        if isdirectory(expand("~/.vim/bundle/python-mode"))
            let g:pymode_python = 'python3'
            "let g:pymode_run = 0
            let g:pymode_lint_checkers = ['pyflakes']
            "let g:pymode_indent = 1
            "let g:pymode_lint_on_write = 1
            "let g:pymode_lint_message = 1
            let g:pymode_trim_whitespaces = 0
            "let g:pymode_lint_todo_symbol = 'WW'
            "let g:pymode_lint_comment_symbol = 'CC'
            "let g:pymode_lint_visual_symbol = 'RR'
            "let g:pymode_lint_error_symbol = 'EE'
            "let g:pymode_lint_info_symbol = 'II'
            "let g:pymode_lint_pyflakes_symbol = 'FF'
            "let g:pymode_run_bind = '<leader>r'
"            let g:pymode_lint_on_fly = 1
            "let g:pymode_options_max_line_length = 79
            "let g:pymode_quickfix_minheight = 3
            "let g:pymode_quickfix_maxheight = 6
            let g:pymode_options = 0
            let g:pymode_rope = 0
        endif
    " }
    " python语法实时检查 {
        " python fly check, 弥补syntastic只能打开和保存才检查语法的不足
        "let g:pyflakes_use_quickfix = 0

        " for python.vim syntax highlight
        " let python_highlight_all = 1
    " }
    au BufRead,BufNewFile *.{html,janja,htmljanja} set filetype=html
" }
" PHP {
    " PIV {
        " if isdirectory(expand("~/.vim/bundle/PIV"))
            " let g:DisableAutoPHPFolding = 0
            " let g:PIVAutoClose = 0
        " endif
    " }
" }
" MarkDown {
    " vim-markdown {
        au BufRead,BufNewFile *.{md,mdown,mkd,mkdn,markdown,mdwn} set filetype=markdown
        let g:vim_markdown_folding_disabled=1
        let g:vim_markdown_no_default_key_mappings=1
    " }
" MarkDownPreview {
    let g:mkdp_path_to_chrome = "google-chrome-stable"
    let g:mkdp_auto_start = 0
        " 设置为 1 可以在打开 markdown 文件的时候自动打开浏览器预览，只在打开
        " markdown 文件的时候打开一次

    let g:mkdp_auto_open = 0
        " 设置为 1 在编辑 markdown 的时候检查预览窗口是否已经打开，否则自动打开预
        " 览窗口

    let g:mkdp_auto_close = 1
        " 在切换 buffer 的时候自动关闭预览窗口，设置为 0 则在切换 buffer 的时候不
        " 自动关闭预览窗口

    let g:mkdp_refresh_slow = 0
        " 设置为 1 则只有在保存文件，或退出插入模式的时候更新预览，默认为 0，实时
        " 更新预览   
" }
    " Fugitive(git) {
        if isdirectory(expand("~/.vim/bundle/vim-fugitive/"))
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
        endif
    "}
" }
" }
"
" general settting {
" general settting {
    " 修改leader键
    let mapleader = ','
    let g:mapleader = ','

    filetype on                    "启用文件类型侦测
    filetype plugin on             "针对不同的文件类型加载对应的插件
    filetype plugin indent on      "启用缩进
    syntax on                      "代码高亮
    syntax enable                      "代码高亮
    set t_Co=256



    if $TERM=~'cons25'
        colorscheme default
    else
        if isdirectory(expand("~/.vim/bundle/vim-colors-solarized/"))
            set background=dark
            " let g:solarized_termcolors=256
            " let g:solarized_termtrans = 0
            " colorscheme solarized
            let g:molokai_original = 1
            colorscheme molokai
        else
            colorscheme default
        endif
    endif
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
    set scrolloff=10                  "在上下移动光标时，光标的上方或下方至少会保留显示的行数
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

    " 启用每行超过80列的字符提示（字体变蓝并加下划线），不启用就注释掉
    "au BufWinEnter * let w:m2=matchadd('Underlined', '\%>' . 80 . 'v.\+', -1)
" }

" 界面设置 {
    " ===============================
    "       < 界面配置 >
    " ===============================

    " 显示/隐藏菜单栏、工具栏、滚动条，可用 Ctrl + F11 切换
    if has("gui_running")
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
" }

" others 设置 {
    "==========================================
    " others 其它设置
    "==========================================
    let g:clang_user_options='|| exit 0'
    autocmd! bufwritepost _vimrc source % " vimrc文件修改之后自动加载。 windows。
    autocmd! bufwritepost .vimrc source % " vimrc文件修改之后自动加载。 linux。

    "离开插入模式后自动关闭预览窗口
    autocmd InsertLeave * if pumvisible() == 0|pclose|endif

    if has("autocmd")
        au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
    endif

    " Python 文件的一般设置，比如不要 tab 等
    "autocmd FileType python set tabstop=4 shiftwidth=4 expandtab ai
    "autocmd FileType ruby set tabstop=2 shiftwidth=2 softtabstop=2 expandtab ai

    " 保存python文件时删除多余空格
    fun! <SID>StripTrailingWhitespaces()
        let l = line(".")
        let c = col(".")
        %s/\s\+$//e
        call cursor(l, c)
    endfun
    autocmd filetype c,cpp,java,go,php,javascript,puppet,python,rust,twig,xml,yml,perl autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()

    "设置标记一列的背景颜色和数字一行颜色一致
    hi! link SignColumn   LineNr
    hi! link ShowMarksHLl DiffAdd
    hi! link ShowMarksHLu DiffChange

    " for error highlight，防止错误整行标红导致看不清
    highlight clear SpellBad
    highlight SpellBad term=standout ctermfg=1 term=underline cterm=underline
    highlight clear SpellCap
    highlight SpellCap term=underline cterm=underline
    highlight clear SpellRare
    highlight SpellRare term=underline cterm=underline
    highlight clear SpellLocal
    highlight SpellLocal term=underline cterm=underline
" }

" 替换操作 {
    " 替换函数。参数说明：
    " confirm：是否替换前逐一确认
    " wholeword：是否整词匹配
    " replace：被替换字符串
    function! Replace(confirm, wholeword, replace)
      wa
      let flag = ''
      if a:confirm
        let flag .= 'gec'
      else
        let flag .= 'ge'
      endif
      let search = ''
      if a:wholeword
        let search .= '\<' . escape(expand('<cword>'), '/\.*$^~[') . '\>'
      else
        let search .= expand('<cword>')
      endif
      let replace = escape(a:replace, '/\&~')
      execute 'argdo %s/' . search . '/' . replace . '/' . flag . '| update'
    endfunction
    " 不确认、非整词
    nnoremap <Leader>rr :call Replace(0, 0, input('Replace '.expand('<cword>').' with: '))<CR>
    " 不确认、整词
    nnoremap <Leader>rw :call Replace(0, 1, input('Replace '.expand('<cword>').' with: '))<CR>
    " 确认、非整词
    nnoremap <Leader>rc :call Replace(1, 0, input('Replace '.expand('<cword>').' with: '))<CR>
    " 确认、整词
    nnoremap <Leader>rcw :call Replace(1, 1, input('Replace '.expand('<cword>').' with: '))<CR>
    nnoremap <Leader>rwc :call Replace(1, 1, input('Replace '.expand('<cword>').' with: '))<CR>
" }

" 插入文件标题 {
    " ======================================================================================
    "                插入文件标题
    " ======================================================================================
    autocmd BufNewFile *.cpp,*.[ch],*.sh,*.rb,*.py,*.asm,*md exec ":call SetTitle()" 
    func! SetTitle() 
        "如果文件类型为.sh文件 
        if &filetype == 'sh' 
            call setline(1,"\#!/bin/bash") 
            call append(line("."), "") 
        elseif &filetype == 'markdown'
            call setline(1,          "Title: ".expand("%"))
            call append(line("."),   "Author: honmaple ") 
            call append(line(".")+1, "Date: ".strftime("%F")) 
            call append(line(".")+2, "Category: ") 
            call append(line(".")+3, "Tags: []") 
            call append(line(".")+4, "Slug: ".expand("%")) 
            call append(line(".")+5, "Summary: ") 
            call append(line(".")+6, "")
        elseif &filetype == 'python' || &filetype == 'ruby'
            if &filetype == 'python'
                call setline(1,     "#!/usr/bin/env python")
                call append(line("."),"# -*- coding=UTF-8 -*-")
            elseif &filetype == 'ruby'
                call setline(1,      "#!/usr/bin/env ruby")
                call append(line("."),"# encoding: utf-8")
            endif
            call append(line(".")+1,  "#*************************************************************************") 
            call append(line(".")+2,  "# Copyright © 2015 JiangLin. All rights reserved.") 
            call append(line(".")+3,  "# File Name: ".expand("%")) 
            call append(line(".")+4,  "# Author:JiangLin ") 
            call append(line(".")+5,  "# Mail:xiyang0807@gmail.com ") 
            call append(line(".")+6,  "# Created Time: ".strftime("%F %T")) 
            call append(line(".")+7,  "# Last Update: ") 
            call append(line(".")+8,  "#          By: ") 
            call append(line(".")+9,  "# Description: ") 
            call append(line(".")+10,  "#*************************************************************************")
            call append(line(".")+11, "") 
        endif

        if expand("%:e") == 'cpp' ||expand("%:e") == 'c' ||expand("%:e") == "h" ||expand("%:e") == 'asm'
            call setline(1,          "/**************************************************************************") 
            call append(line("."),   "  Copyright © 2015 JiangLin. All rights reserved.") 
            call append(line(".")+1, "  File Name: ".expand("%")) 
            call append(line(".")+2, "  Author:JiangLin ") 
            call append(line(".")+3, "  Mail:xiyang0807@gmail.com ") 
            call append(line(".")+4, "  Created Time: ".strftime("%F %T")) 
            call append(line(".")+7, "  Last Update: ") 
            call append(line(".")+8, "           By: ") 
            call append(line(".")+9, "  Description: ") 
            call append(line(".")+10, "**************************************************************************/")
            if expand("%:e") == 'h'
                call append(line(".")+6, "#ifndef _".toupper(expand("%:r"))."_H")
                call append(line(".")+7, "#define _".toupper(expand("%:r"))."_H")
                call append(line(".")+8, "#endif")
                call append(line(".")+9, "")
            elseif expand("%:e") == 'c'
                call append(line(".")+6, "#include<stdio.h>")
                call append(line(".")+7, "#include<string.h>")
                call append(line(".")+8, "")
            elseif expand("%:e") == 'cpp'
                call append(line(".")+6, "#include<iostream>")
                call append(line(".")+7, "using namespace std;")
                call append(line(".")+8, "")
            elseif expand("%:e") == 'asm'
                call append(line(".")+6, "")
            endif
        "elseif expand("%:e") == 'html' || expand("%:e") == 'xml' ||  expand("%:e") == 'xhtml'
            "call setline(1, "<!--")
            "call append(line("."),   "  Copyright © 2015 JiangLin. All rights reserved.") 
            "call append(line(".")+1, "  File Name: ".expand("%")) 
            "call append(line(".")+2, "  Author:JiangLin ") 
            "call append(line(".")+3, "  Mail:xiyang0807@gmail.com ") 
            "call append(line(".")+4, "  Created Time: ".strftime("%F %T")) 
            "call append(line(".")+5, "-->")
            "call append(line(".")+6, "")
        endif
                "新建文件后，自动定位到文件末尾
    endfunc 
    autocmd BufNewFile * normal G
    function! SetLastModifiedTime()  
        let n=8
        while n < 11
            let line = getline(n)
            if line =~ '^\#\s*\S*File\sName:\S*.*$'
                normal m'
                execute '/# *File Name:/s@:.*$@\=": ".expand("%:t")@'
                normal ''
            endif
            if line =~ '^\#\s*\S*Last\sUpdate:\S*.*$'
                normal m'
                execute '/# *Last Update:/s@:.*$@\=strftime(": %F %T")@'
                normal ''
            endif
            if line =~ '^\#\s*\S*By:\S*.*$'
                normal m'
                execute '/# *By:/s@:.*$@\=": ".$USER@'
                normal ''
                return
            endif
            let n = n + 1
        endwhile
    endfunction  
    au BufWrite *.py,*.java call SetLastModifiedTime()          
" }

" 编译配置 {
    func! Run()
        exec ":w"
        if &filetype == 'html'
            exe ":!google-chrome-stable %"
        elseif &filetype == 'python'
            exe ":!python %"
        elseif &filetype == 'c'
            exe ":!gcc\ -Wall\ -g\ -O0\ -c\ %\ -o\ %<.o"
        elseif &filetype == 'mkd'
            exec ":MarkdownPreview"
        endif
    endfunc
" }

" 代码格式优化化 {
    "定义FormartSrc()
    func! FormartSrc()
        exec ":w"
        normal m'
        "if &filetype == 'c'
            "exec "!astyle --style=ansi -a --suffix=none %"
        if &filetype == 'cpp' || &filetype == 'hpp'
            exec "r !astyle --style=ansi --one-line=keep-statements -a --suffix=none %> /dev/null 2>&1"
        elseif &filetype == 'perl'
            exec "!astyle --style=gnu --suffix=none %"
        elseif &filetype == 'py'||&filetype == 'python'
            exec ":PymodeLintAuto"
        elseif &filetype == 'java'
            exec "!astyle --style=java --suffix=none %"
        elseif &filetype == 'jsp'
            exec "!astyle --style=gnu --suffix=none %"
        elseif &filetype == 'xml'
            exec "!astyle --style=gnu --suffix=none %"
        else
            exec "normal gg=G"
        endif
        normal ''
    endfunc
    "结束定义FormartSrc
" }

" 快捷键 {
    " 常规模式下用空格键来开关光标行所在折叠（注：zR 展开所有折叠，zM 关闭所有折叠）
    nnoremap <space> @=((foldclosed(line('.')) < 0) ? 'zc' : 'zo')<CR>
    " 常规模式下输入 cS 清除行尾空格
    nnoremap cS :%s/\s\+$//g<CR>:noh<CR>
    " 常规模式下输入 cM 清除行尾 ^M 符号
    nnoremap cM :%s/\r$//g<CR>:noh<CR>

    "插入模式下的方向键
    inoremap <c-k> <Up>
    inoremap <c-j> <Down>
    inoremap <c-h> <Left>
    inoremap <c-l> <Right>

    " 调整缩进后自动选中，方便再次操作
    " vnoremap < <gv
    " vnoremap > >gv
    " 在不使用 MiniBufExplorer 插件时也可用<C-k,j,h,l>切换到上下左右的窗口中去
    noremap <c-k> <c-w>k
    noremap <c-j> <c-w>j
    noremap <c-h> <c-w>h
    noremap <c-l> <c-w>l
    "行首行尾
    noremap H ^
    noremap L $

    "设置esc键
    imap jj <Esc>
    nmap ;; <Esc>
    vmap ;; <Esc>

    nmap <C-Z> <Esc>u
    nmap <C-A> ggVG$"+y
    vmap <leader>y "+y
    nmap <leader>p "+p

    " 开启/关闭对齐线
    nmap <leader>il :IndentLinesToggle<CR>

    " 设置NerdTree
    nmap <F2> :NERDTreeToggle<CR>

    nmap tb :TagbarToggle<CR>

    "切换buffer
    nmap <Tab> :bn<CR>
    nmap <S-Tab> :tabn<CR>

    "按F5编译运行
    nmap <F5> :call Run()<CR>


    "代码格式优化化
    nmap <F6> :call FormartSrc()<CR>
    noremap <F1> <Esc> "废弃F1键以防调出系统帮助
    nmap <leader>zz :call ToggleFold()<cr>     "代码折叠快捷键
    nnoremap <C-n> :call NumberToggle()<cr>   "显示/关闭相对行号
    " 去掉搜索高亮
    noremap <silent><leader>/ :nohls<CR>
    "鼠标粘贴
    noremap <silent><leader>vb :set mouse=v<CR>
    "切换背景
    noremap <leader>bg :call ToggleBG()<CR>
    "CtrlPFunky快捷键
    nnoremap <Leader>fu :CtrlPFunky<Cr>
    " narrow the list down with a word under cursor
    nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
    "ctrlpwen文件模糊查找快捷键 ctrl+p
    nnoremap <silent> <D-t> :CtrlP<CR>
    nnoremap <silent> <D-r> :CtrlPMRU<CR>
    "粘贴快捷键
    set pastetoggle=<F12>
    "搜索文件内容
    nnoremap <Leader>ff :CtrlSF <CR>

    "<C-y>,  emmet快捷键
    "<leader><leader>fa 快速移动
    "<leader>cc，注释当前选中文本，如果选中的是整行则在每行首添加 //，如果选中一行的部分内容则在选中部分前后添加分别 / 、 /；
    "<leader>cu，取消选中文本块的注释
    "
    "<Leader>be　　全屏方式打来 buffer 列表
    "<Leader>bs　　水平窗口打来 buffer 列表
    "<Leader>bv　　垂直窗口打开 buffer 列表
" }
aug QFClose
  au!
  au WinEnter * if winnr('$') == 1 && getbufvar(winbufnr(winnr()), "&buftype") == "quickfix"|q|endif
aug END
