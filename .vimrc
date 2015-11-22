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

" BundleEnvironment {

    " Basics {
        set nocompatible        " Must be first line
        set background=dark     " Assume a dark background
    " }

    " Windows Compatible {
        " On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
        " across (heterogeneous) systems easier.
        if has('win32') || has('win64')
          set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after

          " Be nice and check for multi_byte even if the config requires
          " multi_byte support most of the time
          if has("multi_byte")
            " Windows cmd.exe still uses cp850. If Windows ever moved to
            " Powershell as the primary terminal, this would be utf-8
            set termencoding=cp850
            " Let Vim use utf-8 internally, because many scripts require this
            set encoding=utf-8
            setglobal fileencoding=utf-8
            " Windows has traditionally used cp1252, so it's probably wise to
            " fallback into cp1252 instead of eg. iso-8859-15.
            " Newer Windows files might contain utf-8 or utf-16 LE so we might
            " want to try them first.
            set fileencodings=ucs-bom,utf-8,utf-16le,cp1252,iso-8859-15
          endif
        endif
    " }

    " Setup Bundle Support {
        " The next three lines ensure that the ~/.vim/bundle/ system works
        filetype off
        set rtp+=~/.vim/bundle/vundle
        call vundle#rc()
    " }

    " Add an UnBundle command {
    function! UnBundle(arg, ...)
      let bundle = vundle#config#init_bundle(a:arg, a:000)
      call filter(g:vundle#bundles, 'v:val["name_spec"] != "' . a:arg . '"')
    endfunction

    com! -nargs=+         UnBundle
    \ call UnBundle(<args>)
    " }

" }

" Bundles插件 {

    " Deps {
        Bundle 'gmarik/vundle'
        Bundle 'MarcWeber/vim-addon-mw-utils'
        Bundle 'tomtom/tlib_vim'
        Bundle 'dyng/ctrlsf.vim'   
    " }

    if !exists('g:bundle_groups')
        let g:bundle_groups=['general', 'writing', 'youcompleteme', 'programming', 'php', 'ruby', 'python', 'javascript', 'html', 'misc',]
    endif

    "   let g:override_bundles = 1
    if !exists("g:override_bundles")

    " General {
        if count(g:bundle_groups, 'general')
            Bundle 'scrooloose/nerdtree'
            Bundle 'altercation/vim-colors-solarized'
            Bundle 'spf13/vim-colors'
            Bundle 'tpope/vim-surround'
            Bundle 'tpope/vim-repeat'
            Bundle 'jiangmiao/auto-pairs'
            Bundle 'ctrlpvim/ctrlp.vim'
            Bundle 'tacahiroy/ctrlp-funky'
            Bundle 'kristijanhusak/vim-multiple-cursors'
            Bundle 'vim-scripts/sessionman.vim'
            Bundle 'matchit.zip'
            if (has("python") || has("python3")) && exists('g:use_powerline') && !exists('g:use_old_powerline')
                Bundle 'Lokaltog/powerline', {'rtp':'/powerline/bindings/vim'}
            elseif exists('g:use_powerline') && exists('g:use_old_powerline')
                Bundle 'Lokaltog/vim-powerline'
            else
                Bundle 'bling/vim-airline'
            endif
            Bundle 'powerline/fonts'
           " Bundle 'fholgado/minibufexpl.vim'
            Bundle 'bling/vim-bufferline'
            Bundle 'Lokaltog/vim-easymotion'
            Bundle 'jistr/vim-nerdtree-tabs'
            Bundle 'flazz/vim-colorschemes'
            Bundle 'mbbill/undotree'
            Bundle 'Yggdroot/indentLine'
            "if !exists('g:no_views')
                "Bundle 'vim-scripts/restore_view.vim'
            "endif
            Bundle 'mhinz/vim-signify'
            Bundle 'tpope/vim-abolish.git'
            Bundle 'osyo-manga/vim-over'
            Bundle 'kana/vim-textobj-user'
            Bundle 'kana/vim-textobj-indent'
            Bundle 'gcmt/wildfire.vim'
        endif
    " }

    " Writing {
"        if count(g:bundle_groups, 'writing')
            "Bundle 'reedes/vim-litecorrect'
            "Bundle 'reedes/vim-textobj-sentence'
            "Bundle 'reedes/vim-textobj-quote'
            "Bundle 'reedes/vim-wordy'
        "endif
    " }

    " General Programming {
        if count(g:bundle_groups, 'programming')
            " Pick one of the checksyntax, jslint, or syntastic
            Bundle 'scrooloose/syntastic'
            Bundle 'tpope/vim-fugitive'
            Bundle 'mattn/webapi-vim'
            Bundle 'mattn/gist-vim'
            Bundle 'scrooloose/nerdcommenter'
            Bundle 'tpope/vim-commentary'
            Bundle 'godlygeek/tabular'
            if executable('ctags')
                Bundle 'majutsushi/tagbar'
            endif
        endif
    " }

    " Snippets & AutoComplete {
        if count(g:bundle_groups, 'youcompleteme')
            Bundle 'Valloric/YouCompleteMe'
            Bundle 'SirVer/ultisnips'
            Bundle 'honza/vim-snippets'
        endif
    " }

    " PHP {
        if count(g:bundle_groups, 'php')
            Bundle 'spf13/PIV'
            Bundle 'arnaud-lb/vim-php-namespace'
            Bundle 'beyondwords/vim-twig'
        endif
    " }

    " Python {
        if count(g:bundle_groups, 'python')
            " Pick either python-mode or pyflakes & pydoc
            Bundle 'klen/python-mode'
            Bundle 'yssource/python.vim'
            Bundle 'python_match.vim'
 "           Bundle 'pythoncomplete'
            "Bundle 'hdima/python-syntax'
"            Bundle 'kevinw/pyflakes-vim'
        endif
    " }

    " Javascript {
        if count(g:bundle_groups, 'javascript')
            Bundle 'elzr/vim-json'
            Bundle 'groenewege/vim-less'
            Bundle 'pangloss/vim-javascript'
            Bundle 'briancollins/vim-jst'
            Bundle 'kchmck/vim-coffee-script'
        endif
    " }

    " Scala {
"        if count(g:bundle_groups, 'scala')
            "Bundle 'derekwyatt/vim-scala'
            "Bundle 'derekwyatt/vim-sbt'
            "Bundle 'xptemplate'
        "endif
    " }

    " Haskell {
 "       if count(g:bundle_groups, 'haskell')
            "Bundle 'travitch/hasksyn'
            "Bundle 'dag/vim2hs'
            "Bundle 'Twinside/vim-haskellConceal'
            "Bundle 'Twinside/vim-haskellFold'
            "Bundle 'lukerandall/haskellmode-vim'
            "Bundle 'eagletmt/neco-ghc'
            "Bundle 'eagletmt/ghcmod-vim'
            "Bundle 'Shougo/vimproc.vim'
            "Bundle 'adinapoli/cumino'
            "Bundle 'bitc/vim-hdevtools'
        "endif
    " }

    " HTML {
        if count(g:bundle_groups, 'html')
            Bundle 'amirh/HTML-AutoCloseTag'
            Bundle 'mattn/emmet-vim'
            "Bundle 'ap/vim-css-color'
            Bundle 'hail2u/vim-css3-syntax'
            Bundle 'gorodinskiy/vim-coloresque'
            Bundle 'tpope/vim-haml'
            "Bundle 'Glench/Vim-Jinja2-Syntax'
        endif
    " }

    " Ruby {
  "      if count(g:bundle_groups, 'ruby')
            "Bundle 'tpope/vim-rails'
            "let g:rubycomplete_buffer_loading = 1
            "let g:rubycomplete_classes_in_global = 1
            "let g:rubycomplete_rails = 1
   "     endif
    " }

    " Puppet {
    "    if count(g:bundle_groups, 'puppet')
            "Bundle 'rodjek/vim-puppet'
        "endif
    " }

    " Go Lang {
     "   if count(g:bundle_groups, 'go')
            ""Bundle 'Blackrush/vim-gocode'
            "Bundle 'fatih/vim-go'
        "endif
    " }

    " Elixir {
      "  if count(g:bundle_groups, 'elixir')
            "Bundle 'elixir-lang/vim-elixir'
            "Bundle 'carlosgaldino/elixir-snippets'
            "Bundle 'mattreduce/vim-mix'
        "endif
    " }

    " Misc {
        if count(g:bundle_groups, 'misc')
       "     Bundle 'rust-lang/rust.vim'
           Bundle 'tpope/vim-markdown'
            "Bundle 'spf13/vim-preview'
            "Bundle 'tpope/vim-cucumber'
            "Bundle 'cespare/vim-toml'
            "Bundle 'quentindecock/vim-cucumber-align-pipes'
            "Bundle 'saltstack/salt-vim'
            Bundle 'iamcco/markdown-preview.vim'
        endif
    " }

    endif

" }

" Bundles {
" viewoptions {
    " indentLine {
        " 用于显示对齐线，与 indent_guides 在显示方式上不同，根据自己喜好选择了
        " 在终端上会有屏幕刷新的问题，这个问题能解决有更好了
        " 开启/关闭对齐线

        let g:indentLine_char = "┊"
        let g:indentLine_first_char = "┊"
        " 色块宽度
        "let g:indent_guides_guide_size=1
        " 设置终端对齐线颜色，如果不喜欢可以将其注释掉采用默认颜色
        let g:indentLine_color_term = 256
    " }
    " vim-airline {
        " Set configuration options for the statusline plugin vim-airline.
        " Use the powerline theme and optionally enable powerline symbols.
        " To use the symbols , , , , , , and .in the statusline
        " segments add the following to your .vimrc.before.local file:
        let g:airline_powerline_fonts=1
        " let g:airline#extensions#tabline#enabled = 1
        " If the previous symbols do not render for you then install a
        " powerline enabled font.

        " See `:echo g:airline_theme_map` for some more choices
        " Default in terminal vim is 'dark'
        if isdirectory(expand("~/.vim/bundle/vim-airline/"))
            if !exists('g:airline_theme')
                let g:airline_theme = 'dark'
            endif
"            if !exists('g:airline_powerline_fonts')
                "" Use the default set of separators with a few customizations
                "let g:airline_left_sep=''  " Slightly fancier than '>'
                "let g:airline_right_sep='' " Slightly fancier than '<'
            "endif
        endif
    " }
" }
" 文件操作 {
    " syntastic {
        let g:syntastic_error_symbol='>>'
        let g:syntastic_warning_symbol='>'
        let g:syntastic_check_on_open=1
        let g:syntastic_check_on_wq=0
        let g:syntastic_enable_highlighting=1
        "let g:syntastic_always_populate_loc_list = 1
        " let g:syntastic_auto_loc_list = 1
        let g:syntastic_ignore_files=[".*\.py$"]
        "let g:syntastic_python_checkers=['pyflakes'] " 使用pyflakes
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
            "youcompleteme  默认tab  s-tab 和自动补全冲突
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
            "highlight Pmenu ctermfg=2 ctermbg=0 guifg=#005f87 guibg=#EEE8D5
            " 选中项
            "highlight PmenuSel ctermfg=2 ctermbg=3 guifg=#AFD700 guibg=#106900
            " 直接触发自动补全 insert模式下
            " let g:ycm_key_invoke_completion = '<C-Space>'
            " 黑名单,不启用
            "let g:ycm_filetype_blacklist = {
            "            \ 'tagbar' : 1,
            "            \ 'gitcommit' : 1,
            "            \}
            "set completeopt-=preview


            "autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
            "autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
            "autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
            "autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
            "autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
            "autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
            "autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
    " }
    " ultisnips {
        "let g:UltiSnipsExpandTrigger = '<C-space>'
        "let g:UltiSnipsJumpForwardTrigger = '<Down>'
        "let g:UltiSnipsJumpBackwardTrigger = '<Up>'
        "定义存放代码片段的文件夹 .vim/snippets下，使用自定义和默认的，将会的到全局，有冲突的会提示
        let g:UltiSnipsSnippetDirectories=['bundle/vim-snippets', 'bundle/ultisnips']

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

        au BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
        let g:UltiSnipsJumpForwardTrigger="<tab>"
        "let g:UltiSnipsListSnippets="<c-e>"
        "回车即选中当前项
        inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
        if has('conceal')
            set conceallevel=2 concealcursor=i
        endif
    " }
    " BufExplorer {
        "<Leader>be　　全屏方式打来 buffer 列表
        "<Leader>bs　　水平窗口打来 buffer 列表
        "<Leader>bv　　垂直窗口打开 buffer 列表
        "let g:bufExplorerDefaultHelp=0       " Do not show default help.
        "let g:bufExplorerShowRelativePath=1  " Show relative paths.
        "let g:bufExplorerSortBy='mru'        " Sort by most recently used.
        "let g:bufExplorerSplitRight=0        " Split left.
        "let g:bufExplorerSplitVertical=1     " Split vertically.
        "let g:bufExplorerSplitVertSize = 30  " Split width
        "autocmd BufWinEnter \[Buf\ List\] setl nonumber
    " }
    " tagbar {

    if isdirectory(expand("~/.vim/bundle/tagbar/"))
        " 相对 TagList 能更好的支持面向对象

        " 常规模式下输入 tb 调用插件，如果有打开 TagList 窗口则先将其关闭
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
        " let g:winManagerWindowLayout = "TagList|FileExplorer,BufExplorer"
        "let g:winManagerWindowLayout = "TagList|Tagbar"
    endif
    " }
    " NerdTree {
        if isdirectory(expand("~/.vim/bundle/nerdtree"))
            "autocmd BufEnter * :syntax sync fromstart
            "set hid             " 可以在没有保存的情况下切换buffer
            " 自动开启nerdtree
            "let g:nerdtree_tabs_open_on_console_startup=1
            "当打开vim且没有文件时自动打开NERDTree
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
        " <leader>cc，注释当前选中文本，如果选中的是整行则在每行首添加 //，如果选中一行的部分内容则在选中部分前后添加分别 / 、 /；
        "<leader>cu，取消选中文本块的注释
    " }
    " Ctags {
        " 对浏览代码非常的方便,可以在函数,变量之间跳转等
        if isdirectory(expand("~/.vim/bundle/tagbar/"))
            set tags=./tags;                            "向上级目录递归查找tags文件（好像只有在Windows下才有用）

            " 自动切换目录为当前编辑文件所在目录
            au BufRead,BufNewFile,BufEnter * cd %:p:h
        endif
    " }
    " cscope {
        " 用Cscope自己的话说 你可以把它当做是超过频的ctags
    "    if has("cscope")
            ""设定可以使用 quickfix 窗口来查看 cscope 结果
            "set cscopequickfix=s-,c-,d-,i-,t-,e-
            ""使支持用 Ctrl+]  和 Ctrl+t 快捷键在代码间跳转
            "set cscopetag
            ""如果你想反向搜索顺序设置为1
            "set csto=0
            ""在当前目录中添加任何数据库
            "if filereadable("cscope.out")
                "cs add cscope.out
                ""否则添加数据库环境中所指出的
            "elseif $CSCOPE_DB != ""
                "cs add $CSCOPE_DB
            "endif
            "set cscopeverbose
            ""快捷键设置
            "nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR><CR>
            "nmap <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
            "nmap <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
            "nmap <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
            "nmap <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
            "nmap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
            "nmap <C-\>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
            "nmap <C-\>d :cs find d <C-R>=expand("<cword>")<CR><CR>
        "endif
    " }
  " Tabularize {
        if isdirectory(expand("~/.vim/bundle/tabular"))
            nmap <Leader>a& :Tabularize /&<CR>
            vmap <Leader>a& :Tabularize /&<CR>
            nmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
            vmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
            nmap <Leader>a=> :Tabularize /=><CR>
            vmap <Leader>a=> :Tabularize /=><CR>
            nmap <Leader>a: :Tabularize /:<CR>
            vmap <Leader>a: :Tabularize /:<CR>
            nmap <Leader>a:: :Tabularize /:\zs<CR>
            vmap <Leader>a:: :Tabularize /:\zs<CR>
            nmap <Leader>a, :Tabularize /,<CR>
            vmap <Leader>a, :Tabularize /,<CR>
            nmap <Leader>a,, :Tabularize /,\zs<CR>
            vmap <Leader>a,, :Tabularize /,\zs<CR>
            nmap <Leader>a<Bar> :Tabularize /<Bar><CR>
            vmap <Leader>a<Bar> :Tabularize /<Bar><CR>
        endif
    " }
    " MiniBufExplorer {
"        " 显示/隐藏 MiniBufExplorer 窗口
        "map <Leader>bl :MBEToggle<cr>
        "" buffer 切换快捷键
        "map <C-Tab> :MBEbn<cr>
        "map <C-S-Tab> :MBEbp<cr>
    " }
    " ctrlp {
        if isdirectory(expand("~/.vim/bundle/ctrlp.vim/"))
            let g:ctrlp_working_path_mode = 'ra'
            nnoremap <silent> <D-t> :CtrlP<CR>
            nnoremap <silent> <D-r> :CtrlPMRU<CR>

            let g:ctrlp_custom_ignore = {
                        \ 'dir':  '\.git$\|\.hg$\|\.svn$',
                        \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }

            if executable('ag')
                let s:ctrlp_fallback = 'ag %s --nocolor -l -g ""'
            elseif executable('ack-grep')
                let s:ctrlp_fallback = 'ack-grep %s --nocolor -f'
            elseif executable('ack')
                let s:ctrlp_fallback = 'ack %s --nocolor -f'
                " On Windows use "dir" as fallback command.
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

            if isdirectory(expand("~/.vim/bundle/ctrlp-funky/"))
                " CtrlP extensions
                let g:ctrlp_extensions = ['funky']

                "funky
                nnoremap <Leader>fu :CtrlPFunky<Cr>
            endif
        endif
    " }
    " ctrlp-funky {
"        let g:ctrlp_extensions = ['funky']
"        let g:ctrlp_funky_syntax_highlight = 1
    " }
    " ctrlsf {
            " 工程内查找文件内容,先安装ack
            nnoremap <Leader>sp :CtrlSF<CR>
    " }
    " Wildfire {
        let g:wildfire_objects = {
                \ "*" : ["i'", 'i"', "i)", "i]", "i}", "ip"],
                \ "html,xml" : ["at"],
                \ }
    " vim-multiple-cursors {
        " Default mapping
        let g:multi_cursor_next_key='<C-n>'
        let g:multi_cursor_prev_key='<C-p>'
        let g:multi_cursor_skip_key='<C-x>'
        let g:multi_cursor_quit_key='<Esc>'
     "   }
    " UndoTree {
        if isdirectory(expand("~/.vim/bundle/undotree/"))
            nnoremap <Leader>u :UndotreeToggle<CR>
            " If undotree is opened, it is likely one wants to interact with it.
            let g:undotree_SetFocusWhenToggle=1
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
        nmap <Leader>ac <Plug>ToggleAutoCloseMappings
    " }
" }
" Python {
    " PyMode {
        " Disable if python support not present
        if !has('python')
            let g:pymode = 0
        endif

        if isdirectory(expand("~/.vim/bundle/python-mode"))
            let g:pymode_lint_checkers = ['pyflakes']
            let g:pymode_trim_whitespaces = 0
"            let g:pymode_lint_on_fly = 1
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
        if isdirectory(expand("~/.vim/bundle/PIV"))
            let g:DisableAutoPHPFolding = 0
            let g:PIVAutoClose = 0
        endif
    " }
" }
" MarkDown {
    " vim-markdown {
        au BufRead,BufNewFile *.{md,mdown,mkd,mkdn,markdown,mdwn} set filetype=mkd
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
" }
    " vimwiki {
        let g:vimwiki_use_mouse = 1
        let g:vimwiki_list = [{'path': '~/MyCode/vimwiki/',  
                    \ 'path_html': '~/MyCode/vimwiki_html/',
                    \ 'html_header': '~/MyCode/vimwiki_template/header.htm',
                    \ 'html_footer': '~/MyCode/vimwiki_template/footer.htm',}]
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

" general settting {
    " ========================================
    "  General Settings 基础设置
    " ========================================
    " 修改leader键
    let mapleader = ','
    let g:mapleader = ','

    filetype on                    "启用文件类型侦测
    filetype plugin on             "针对不同的文件类型加载对应的插件
    filetype plugin indent on      "启用缩进
    "syntax on                      "代码高亮
    syntax enable
    set t_Co=256



    if has("gui_running")
        set background=dark
        "let g:solarized_termcolors=256
        "colorscheme solarized
        let g:molokai_original = 1
        colorscheme molokai
    elseif $TERM=~'cons25'
        colorscheme default
    else
        set background=dark
        let g:molokai_original = 1
        colorscheme molokai
    endif
    "colorscheme solarized           
    set mouse=a                    "任何模式下启用鼠标
    set mousehide                  "Hide the mouse cursor while typing
    scriptencoding utf-8

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
    set vb t_vb=                   "关闭提示音

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
    nnoremap <Leader>R :call Replace(0, 0, input('Replace '.expand('<cword>').' with: '))<CR>
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
    autocmd BufNewFile *.cpp,*.[ch],*.sh,*.rb,*.py,*.asm,*md,*.html,*.xml,*.xhtml exec ":call SetTitle()" 
    func! SetTitle() 
        "如果文件类型为.sh文件 
        if &filetype == 'sh' 
            call setline(1,"\#!/bin/bash") 
            call append(line("."), "") 
        elseif &filetype == 'mkd'
            call setline(1,          "Title: ")
            call append(line("."),   "Author: honmaple ") 
            call append(line(".")+1, "date: ".strftime("%F")) 
            call append(line(".")+2, "Category: ") 
            call append(line(".")+3, "Tags: ") 
            call append(line(".")+4, "Slug: ".expand("%")) 
            call append(line(".")+5, "Summary: ") 
            call append(line(".")+6, "")
        elseif &filetype == 'python' || &filetype == 'ruby'
            call setline(1,          "#*************************************************************************") 
            call append(line("."),   "#   Copyright © 2015 JiangLin. All rights reserved.") 
            call append(line(".")+1, "#   File Name: ".expand("%")) 
            call append(line(".")+2, "#   Author:JiangLin ") 
            call append(line(".")+3, "#   Mail:xiyang0807@gmail.com ") 
            call append(line(".")+4, "#   Created Time: ".strftime("%F %T")) 
            call append(line(".")+5, "#*************************************************************************")
            if &filetype == 'python'
                call append(line(".")+6,"#!/usr/bin/env python")
                call append(line(".")+7,"# -*- coding=UTF-8 -*-")
                call append(line(".")+8,"") 
            elseif &filetype == 'ruby'
                call append(line(".")+6,"#!/usr/bin/env ruby")
                call append(line(".")+7,"# encoding: utf-8")
                call append(line(".")+8,"")
            endif
        endif

        if expand("%:e") == 'cpp' ||expand("%:e") == 'c' ||expand("%:e") == "h" ||expand("%:e") == 'asm'
            call setline(1,          "/**************************************************************************") 
            call append(line("."),   "   Copyright © 2015 JiangLin. All rights reserved.") 
            call append(line(".")+1, "   File Name: ".expand("%")) 
            call append(line(".")+2, "   Author:JiangLin ") 
            call append(line(".")+3, "   Mail:xiyang0807@gmail.com ") 
            call append(line(".")+4, "   Created Time: ".strftime("%F %T")) 
            call append(line(".")+5, "**************************************************************************/")
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
        elseif expand("%:e") == 'html' || expand("%:e") == 'xml' ||  expand("%:e") == 'xhtml'
            call setline(1, "<!--")
            call append(line("."),   "  Copyright © 2015 JiangLin. All rights reserved.") 
            call append(line(".")+1, "  File Name: ".expand("%")) 
            call append(line(".")+2, "  Author:JiangLin ") 
            call append(line(".")+3, "  Mail:xiyang0807@gmail.com ") 
            call append(line(".")+4, "  Created Time: ".strftime("%F %T")) 
            call append(line(".")+5, "-->")
            call append(line(".")+6, "")
        endif
                "新建文件后，自动定位到文件末尾
    endfunc 
    autocmd BufNewFile * normal G
" }

" 编译配置 {
    " -----------------------------------------------------------------------------
    "  < 编译运行配置
    " -----------------------------------------------------------------------------

    let s:LastShellReturn_C = 0
    let s:LastShellReturn_L = 0
    let s:ShowWarning = 1
    let s:Obj_Extension = '.o'
    let s:Exe_Extension = '.exe'
    let s:Sou_Error = 0
    let s:Python_Extension = '.py'

    let s:windows_CFlags = 'gcc\ -fexec-charset=gbk\ -Wall\ -g\ -O0\ -c\ %\ -o\ %<.o'
    let s:linux_CFlags = 'gcc\ -Wall\ -g\ -O0\ -c\ %\ -o\ %<.o'

    let s:windows_CPPFlags = 'g++\ -fexec-charset=gbk\ -Wall\ -g\ -O0\ -c\ %\ -o\ %<.o'
    let s:linux_CPPFlags = 'g++\ -Wall\ -g\ -O0\ -c\ %\ -o\ %<.o'

    let s:PythonFlags = 'python\ -u\ %'

    func! Compile()
        exe ":ccl"
        exe ":update"
        let s:Sou_Error = 0
        let s:LastShellReturn_C = 0
        let Sou = expand("%:p")
        let v:statusmsg = ''
        if expand("%:e") == "c" || expand("%:e") == "cpp" || expand("%:e") == "cxx"
            let Obj = expand("%:p:r").s:Obj_Extension
            let Obj_Name = expand("%:p:t:r").s:Obj_Extension
            if !filereadable(Obj) || (filereadable(Obj) && (getftime(Obj) < getftime(Sou)))
                redraw!
                if expand("%:e") == "c"
                    if g:iswindows
                        exe ":setlocal makeprg=".s:windows_CFlags
                    else
                        exe ":setlocal makeprg=".s:linux_CFlags
                    endif
                    echohl WarningMsg | echo " compiling..."
                    silent make
                elseif expand("%:e") == "cpp" || expand("%:e") == "cxx"
                    if g:iswindows
                        exe ":setlocal makeprg=".s:windows_CPPFlags
                    else
                        exe ":setlocal makeprg=".s:linux_CPPFlags
                    endif
                    echohl WarningMsg | echo " compiling..."
                    silent make
                endif
                redraw!
                if v:shell_error != 0
                    let s:LastShellReturn_C = v:shell_error
                endif
                if g:iswindows
                    if s:LastShellReturn_C != 0
                        exe ":bo cope"
                        echohl WarningMsg | echo " compilation failed"
                    else
                        if s:ShowWarning
                            exe ":bo cw"
                        endif
                        echohl WarningMsg | echo " compilation successful"
                    endif
                else
                    if empty(v:statusmsg)
                        echohl WarningMsg | echo " compilation successful"
                    else
                        exe ":bo cope"
                    endif
                endif
            else
                echohl WarningMsg | echo ""Obj_Name"is up to date"
            endif
        elseif expand("%:e") == "py"
            let Python = expand("%:p:r").s:Python_Extension
            let Python_Name = expand("%:p:t:r").s:Python_Extension
            if  filereadable(Python) || (!filereadable(Python) && (getftime(Python) < getftime(Sou)))
                redraw!
                exe ":setlocal makeprg=".s:PythonFlags
                echohl WarningMsg | echo " compiling..."
                silent make 
                redraw!
                if v:shell_error != 0
                    let s:LastShellReturn_C = v:shell_error
                endif
                if g:iswindows
                    if s:LastShellReturn_C != 0
                        exe ":bo cope"
                        echohl WarningMsg | echo " compilation failed"
                    else
                        if s:ShowWarning
                            exe ":bo cw"
                        endif
                        echohl WarningMsg | echo " compilation successful"
                    endif
                else
                    if empty(v:statusmsg)
                        echohl WarningMsg | echo " compilation successful"
                    else
                        exe ":bo cope"
                    endif
                endif
            else
                echohl WarningMsg | echo ""Python_Name"is up to date"
            endif
        else
            let s:Sou_Error = 1
            echohl WarningMsg | echo " please choose the correct source file"
        endif
        exe ":setlocal makeprg=make"
    endfunc

    func! Link()
        call Compile()
        if s:Sou_Error || s:LastShellReturn_C != 0
            return
        endif
        if expand("%:e") == "c" || expand("%:e") == "cpp" || expand("%:e") == "cxx"
            let s:LastShellReturn_L = 0
            let Sou = expand("%:p")
            let Obj = expand("%:p:r").s:Obj_Extension
            if g:iswindows
                let Exe = expand("%:p:r").s:Exe_Extension
                let Exe_Name = expand("%:p:t:r").s:Exe_Extension
            else
                let Exe = expand("%:p:r")
                let Exe_Name = expand("%:p:t:r")
            endif
            let v:statusmsg = ''
            if filereadable(Obj) && (getftime(Obj) >= getftime(Sou))
                redraw!
                if !executable(Exe) || (executable(Exe) && getftime(Exe) < getftime(Obj))
                    if expand("%:e") == "c"
                        setlocal makeprg=gcc\ -o\ %<\ %<.o
                        echohl WarningMsg | echo " linking..."
                        silent make
                    elseif expand("%:e") == "cpp" || expand("%:e") == "cxx"
                        setlocal makeprg=g++\ -o\ %<\ %<.o
                        echohl WarningMsg | echo " linking..."
                        silent make
                    endif
                    redraw!
                    if v:shell_error != 0
                        let s:LastShellReturn_L = v:shell_error
                    endif
                    if g:iswindows
                        if s:LastShellReturn_L != 0
                            exe ":bo cope"
                            echohl WarningMsg | echo " linking failed"
                        else
                            if s:ShowWarning
                                exe ":bo cw"
                            endif
                            echohl WarningMsg | echo " linking successful"
                        endif
                    else
                        if empty(v:statusmsg)
                            echohl WarningMsg | echo " linking successful"
                        else
                            exe ":bo cope"
                        endif
                    endif
                else
                    echohl WarningMsg | echo ""Exe_Name"is up to date"
                endif
            endif
            setlocal makeprg=make
        elseif expand("%:e") == "py"
            return
        endif
    endfunc

    func! Run()
        let s:ShowWarning = 0
        call Link()
        let s:ShowWarning = 1
        if s:Sou_Error || s:LastShellReturn_C != 0 || s:LastShellReturn_L != 0
            return
        endif
        let Sou = expand("%:p")
        if expand("%:e") == "c" || expand("%:e") == "cpp" || expand("%:e") == "cxx"
            let Obj = expand("%:p:r").s:Obj_Extension
            if g:iswindows
                let Exe = expand("%:p:r").s:Exe_Extension
            else
                let Exe = expand("%:p:r")
            endif
            if executable(Exe) && getftime(Exe) >= getftime(Obj) && getftime(Obj) >= getftime(Sou)
                redraw!
                echohl WarningMsg | echo " running..."
                if g:iswindows
                    exe ":!%<.exe"
                else
                    if g:isGUI
                        exe ":!xfce4-terminal -x bash -c './%<; echo; echo 请按 Enter 键继续; read'"
                    else
                        exe ":!clear; ./%<"
                    endif
                endif
                redraw!
                echohl WarningMsg | echo " running finish"
            endif
        elseif expand("%:e") == "py"
            let python = expand("%:p:r").s:Python_Extension
            if getftime(python) >= getftime(Sou)
                redraw!
                echohl WarningMsg | echo " running..."
                if g:iswindows
                    exe ":!python %"
                else
                    if g:isGUI
                        exe ":!xfce4-terminal -x bash -c 'python %; echo; echo 请按 Enter 键继续; read'"
                    else
                        exe ":!clear; python %"
                    endif
                endif
                redraw!
                echohl WarningMsg | echo " running finish"
            endif
        endif
    endfunc

    func! RunPython()
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
        exec "w"
        "if &filetype == 'c'
            "exec "!astyle --style=ansi -a --suffix=none %"
        if &filetype == 'cpp' || &filetype == 'hpp'
            exec "r !astyle --style=ansi --one-line=keep-statements -a --suffix=none %> /dev/null 2>&1"
        elseif &filetype == 'perl'
            exec "!astyle --style=gnu --suffix=none %"
"        elseif &filetype == 'py'||&filetype == 'python'
            "exec "r !autopep8 -i --aggressive %"
        elseif &filetype == 'java'
            exec "!astyle --style=java --suffix=none %"
        elseif &filetype == 'jsp'
            exec "!astyle --style=gnu --suffix=none %"
        elseif &filetype == 'xml'
            exec "!astyle --style=gnu --suffix=none %"
        else
            exec "normal gg=G"
            return
        endif
        exec "e! %"
    endfunc
    "结束定义FormartSrc
" }

" 快捷键 {
    "=======================================================================
    "常用快捷键设置
    "=======================================================================

    " 常规模式下用空格键来开关光标行所在折叠（注：zR 展开所有折叠，zM 关闭所有折叠）
    nnoremap <space> @=((foldclosed(line('.')) < 0) ? 'zc' : 'zo')<CR>

    " 常规模式下输入 cS 清除行尾空格
    nmap cS :%s/\s\+$//g<CR>:noh<CR>

    " 常规模式下输入 cM 清除行尾 ^M 符号
    nmap cM :%s/\r$//g<CR>:noh<CR>

    " Ctrl + K 插入模式下光标向上移动
    imap <c-k> <Up>

    " Ctrl + J 插入模式下光标向下移动
    imap <c-j> <Down>

    " Ctrl + H 插入模式下光标向左移动
    imap <c-h> <Left>

    " Ctrl + L 插入模式下光标向右移动
    imap <c-l> <Right>
    "设置esc键
    imap jj <Esc>
    nmap ;; <Esc>
    vmap ;; <Esc>

    nmap <C-Z> <Esc>u
    "map! <C-O> <C-Y>,
    map <C-A> ggVG$"+y
    "map <F12> gg=G
    vmap <leader>y "+y
    nmap <leader>p "+p

    " 选中状态下 Ctrl+c 复制
    "map <C-v> "*pa
    "imap <C-v> <Esc>"*pa
    imap <C-a> <Esc>^
    imap <C-e> <Esc>$
    "vmap <C-v> "+p
    "map <c-v> "+gp
    "map <c-c> "+y

    " 开启/关闭对齐线
    nmap <leader>il :IndentLinesToggle<CR>

    " 在不使用 MiniBufExplorer 插件时也可用<C-k,j,h,l>切换到上下左右的窗口中去
    noremap <c-k> <c-w>k
    noremap <c-j> <c-w>j
    noremap <c-h> <c-w>h
    noremap <c-l> <c-w>l

    "行首行尾
    noremap H ^
    noremap L $

    " 设置NerdTree
    map <F2> :NERDTreeMirror<CR>
    map <F2> :NERDTreeToggle<CR>

    " 增强源代码浏览，其功能就像Windows中的"Source Insight"
    nmap <F3> :SrcExplToggle<CR>                "打开/闭浏览窗口

    " 常规模式下输入 tb 调用插件，如果有打开 TagList 窗口则先将其关闭
    "nmap tb :TlistClose<CR>:TagbarToggle<CR>
    nmap tb :TagbarToggle<CR>
    "nmap tb :TagbarToggle<CR>

    " 常规模式下输入 tl 调用插件，如果有打开 Tagbar 窗口则先将其关闭
    nmap tl :TagbarClose<CR>:Tlist<CR>
    "切换buffer
    nmap <Tab> :bn<CR>
    nmap <S-Tab> :tabn<CR>
    "C，C++ 按F5编译运行
    map <F5> :call Run()<CR>
    map <F8> :call RunPython()<CR>

    "代码格式优化化
    map <F6> :call FormartSrc()<CR>
    noremap <F1> <Esc> "废弃F1键以防调出系统帮助
    map <leader>zz :call ToggleFold()<cr>     "代码折叠快捷键
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
    "<C-y>,  emmet快捷键
    "<leader><leader>fa 快速移动
    "<leader>cc，注释当前选中文本，如果选中的是整行则在每行首添加 //，如果选中一行的部分内容则在选中部分前后添加分别 / 、 /；
    "<leader>cu，取消选中文本块的注释
    "
    "<Leader>be　　全屏方式打来 buffer 列表
    "<Leader>bs　　水平窗口打来 buffer 列表
    "<Leader>bv　　垂直窗口打开 buffer 列表
" }
