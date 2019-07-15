function! InsertHeader()
    let number=0
    let char=repeat('*', 79)
    let message=[char,
                \'Copyright © '.strftime('%Y'),
                \'File Name: '.expand('%'),
                \'Author: '.'honmaple',
                \'Email: '.'mail@honmaple.com',
                \'Created: '..strftime('%F %T (%Z)'),
                \'Last Update: ',
                \'         By: ',
                \'Description: ',
                \char]
    let header=join(message, '\n')
    let start = []
    let end = []
    let comment = '# '
    let ext = expand('%:e')
    let isblock = 0

    if ext == 'py'
        let start = ['#!/usr/bin/env python', '# -*- coding=UTF-8 -*-']
    elseif ext == 'rb'
        let start = ['#!/usr/bin/env ruby', '# encoding: utf-8']
    elseif ext == 'sh'
        let start = ['#!/bin/bash']
    elseif ext == 'go'
        let isblock = 1
    elseif ext == 'c'
        let isblock = 1
        let end = ['#include<stdio.h>', '#include<string.h>']
    elseif ext == 'cpp'
        let isblock = 1
        let end = ['#include<iostream>', 'using namespace std;']
    endif

    if isblock
        let message[0] = '/' . message[0]
        let message[-1] = message[-1] . '/'
        let comment = ''
    endif

    for m in start
        call append(number, m)
        let number += 1
    endfor

    for m in message
        call append(number,comment . m)
        let number += 1
    endfor

    for m in end
        call append(number,m)
        let number += 1
    endfor
endfunction

autocmd BufNewFile *.cpp,*.[ch],*.go,*.sh,*.rb,*.py,*.asm,*md exec ":call InsertHeader()"
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
            execute '/# *Last Update:/s@:.*$@\=strftime("%A %Y-%02m-%02d %02H:%02M:%02S (%Z)")@'
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
