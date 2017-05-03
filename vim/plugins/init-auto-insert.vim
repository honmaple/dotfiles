function! SetTitle()
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
endfunction
autocmd BufNewFile *.cpp,*.[ch],*.sh,*.rb,*.py,*.asm,*md exec ":call SetTitle()"
"新建文件后，自动定位到文件末尾
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
autocmd BufWrite *.py,*.java call SetLastModifiedTime()
