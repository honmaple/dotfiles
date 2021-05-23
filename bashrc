#
# ~/.bashrc
#
fortune -a | fmt -80 -s | cowsay -$(shuf -n 1 -e b d g p s t w y) -f $(shuf -n 1 -e $(cowsay -l | tail -n +2)) -n

# If not running interactively, don't do anything

[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias more='less'
alias df='df -h'
alias du='du -c -h'
alias mkdir='mkdir -p -v'
alias nano='nano -w'
alias ping='ping -c 5'
alias ..='cd ..'

alias ve='. venv/bin/activate'
alias vi='vim'
alias emacs='$HOME/script/emacs.sh --chdir $HOME'
alias emacsd='emacs --daemon'
alias ec='emacsclient -n'
alias et='TERM=xterm-256color emacsclient -t'
# alias netease-cloud-music="netease-cloud-music --disable-seccomp-filter-sandbox"
# alias vim='gvim'
# alias gvim='gvim --remote-tab-silent'
alias da='date "+%A, %B %d, %Y [%T]"'
alias du1='du --max-depth=1'
alias hist='history | grep $1'
alias openports='ss --all --numeric --processes --ipv4 --ipv6'
alias pg='ps -Af | grep $1'
alias pyserver='python -m http.server --bind 127.0.0.1'


# safety features
# alias cp='cp -i'
alias mv='mv -i'
# alias rm='rm -i'                    # 'rm -i' prompts for every file
alias ln='ln -i'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'
alias rm='trash-put'

# PS1="\[\e[0;37m\]\342\224\214\342\224\200\[[\e[0;33m\]\u\[\e[0;37m\]]\342\224\200[\[\e[1;33m\]\w\[\e[0;37m\]]\n\[\e[0;37m\]\342\224\224\342\224\200\342\224\200\342\225\274\[\e[0m\] "

export GOPATH=$HOME/repo/golang
export NODE_PATH="$HOME/repo/npm/lib/node_modules:$NODE_PATH"

alias luarocks='luarocks --tree=$HOME/repo/lua'
export LUA_PATH="$HOME/repo/lua/share/lua/5.3/?.lua;$HOME/repo/lua/share/lua/5.3/?/init.lua;$LUA_PATH;"
export LUA_CPATH="$HOME/repo/lua/lib/lua/5.3/?.so;$LUA_CPATH;"

export PYTHONUSERBASE=$HOME/repo/python
# export PYTHONPATH=$PYTHONPATH:/usr/lib/python3.6/site-packages:$HOME/repo/python/lib/python3.6/site-packages
export PATH=$PATH:$HOME/repo/python/bin:$HOME/repo/golang/bin:$HOME/repo/lua/bin:$HOME/repo/npm/bin

export HISTTIMEFORMAT="\"TIME\":\"%F %T\",\"CMD\":"
export WORKON_HOME=$HOME/repo/python/virtualenv
source $HOME/repo/python/bin/virtualenvwrapper.sh

alias upansible='docker exec -it ansible /bin/bash'
# PS1="\$(if [[ \$? == 0 ]]; then echo \"\e[01;32m\342\234\223\"; else echo \"\e[01;31m\342\234\227\"; fi)"

git_status() {
    cmd=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/[(（]//' -e 's/[）)]//'  -e 's/* \(.*\)/(\1)/'`
    echo -en "\033[32m$cmd\033[0m"
}
PS1="\[\e[0;37m\]\342\224\214\342\224\200\[[\e[0;33m\]\u\[\e[0;37m\]]\342\224\200[\[\e[1;33m\]\w\[\e[0;37m\]]\$(git_status)\n\[\e[0;37m\]\342\224\224\342\224\200\342\224\200\342\225\274\[\e[0m\] "
