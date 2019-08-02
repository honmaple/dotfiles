#!/bin/bash
# ********************************************************************************
# Copyright © 2019 jianglin
# File Name: archlinux.sh
# Author: jianglin
# Email: mail@honmaple.com
# Created: 2019-08-01 11:44:49 (CST)
# Last Update: Friday 2019-08-02 21:05:55 (CST)
#          By:
# Description:
# ********************************************************************************
PACMAN="pacman"

yes_or_no(){
    local m="$1"
    read -p "$m [Y/n] "  answer
    answer="${answer:-Y}"

    if [ "$answer" == "Y" ] || [ "$answer" == "y" ];then
        return 0
    elif [ "$answer" == "n" ];then
        return 1
    else
        return $(yes_or_no "$m")
    fi
}

conf_network(){
    if $(ping -c 1 www.baidu.com > /dev/null 2>&1);then
        echo "please configure network?"
    else
        echo "network connection success."
    fi
}

conf_time(){
    timedatectl set-ntp true
}

conf_partition(){
    echo "Configure partition"
    fdisk -l
    read -p "Input the dest partition [/dev/sda]: " SDX
    SDX="${SDX:-/dev/sda}"

    cfdisk $SDX
    lsblk
    if $(yes_or_no "Whether mkfs partition");then
        echo "Enter bash to format partition, use exit to exit bash"
        echo -e "Make format Choice
        1)    mkfs.ext4 /dev/sdXY    # sdXY is dest partition
        2)    mkswap /dev/sdXY
        3)    swapon /dev/sdXY"
        bash
        echo "Now is normal bash"
    fi
}

conf_mirror(){
    echo "Configure mirrorlist"
    read -p "Input Your Country [China]: " COUNTRY
    COUNTRY="${COUNTRY:-China}"
    sed -i "/$COUNTRY/!{n;/Server/s/^/#/};t;n" /etc/pacman.d/mirrorlist

    cat /etc/pacman.d/mirrorlist | grep -A1 China
    if $(yes_or_no "Whether edit /etc/pacman.d/mirrorlist"); then
        vi /etc/pacman.d/mirrorlist
    fi
}

conf_mount(){
    echo "Configure mount"
    read -p "Input the mount partition [/dev/sda1]: " SDX
    SDX="${SDX:-/dev/sda1}"
    mount $SDX /mnt
}

conf_unmount(){
    umount -R /mnt
}

conf_base(){
    pacstrap /mnt base
    genfstab -U /mnt >> /mnt/etc/fstab

    export -f yes_or_no
    export -f conf_chroot
    export -f conf_desktop
    export -f conf_vbox
    export -f conf_hostname
    export -f conf_user
    export -f conf_grub

    cat <<EOF >/mnt/root/setup
conf_chroot
conf_hostname
conf_user
conf_desktop
conf_vbox
conf_grub
conf_other
exit
EOF
    chmod 0755 /mnt/root/setup
    arch-chroot /mnt /root/setup
}

conf_chroot() {
    echo "Configure timezone"
    read -p "Input zoneinfo (Asia/Shanghai): " ZONEINFO
    ZONEINFO="${ZONEINFO:-Asia/Shanghai}"
    ln -sf /usr/share/zoneinfo/$ZONEINFO /etc/localtime

    if $(yes_or_no "Configure hwclock to UTC");then
        hwclock --systohc
    else
        hwclock --systohc --localtime
    fi

    echo "Configure locale"
    sed -i '/en_US.UTF-8/{s/#//}' /etc/locale.gen

    if $(yes_or_no "Configure zh_CN to locale.gen");then
        sed -i '/zh_CN.UTF-8/{s/#//}' /etc/locale.gen
        echo 'LANG=zh_CN.UTF-8'  > /etc/locale.conf
    else
        echo 'LANG=en_US.UTF-8'  > /etc/locale.conf
    fi
    locale-gen
}

conf_hostname(){
    echo "Configure hostname"
    read -p "Input hostname: " HOST
    echo "$HOST" > /etc/hostname
    echo "127.0.0.1 localhost" >> /etc/hosts
    echo "::1       localhost" >> /etc/hosts
    echo "127.0.0.1 $HOST.localdomain $HOST" >> /etc/hosts
    cat /etc/hosts
    if $(yes_or_no "Whether edit /etc/hosts"); then
        vi /etc/hosts
    fi
}

conf_user(){
    echo "Configure user"
    passwd
    if $(yes_or_no "Whether add new user"); then
        read -p "Add new user for: " USERNAME
        useradd -m -g users -s /bin/bash $USERNAME
        passwd $USERNAME
    fi
}

conf_desktop(){
    echo "Configure desktop"
    if $(yes_or_no "Whether install i3wm");then
        pacman -S xorg-server xf86-video-vesa xorg-xinit
        pacman -S i3-wm py3status i3status feh dmenu dunst
    fi
    pacman -S emacs sudo git the_silver_searcher bash-completion
    pacman -S lxappearance lxterminal
    pacman -S xcompmgr sl screenfetch htop
    pacman -S netease-cloud-music google-chrome filezilla gimp
    pacman -S fortune-mod-zh wqy-microhei fcitx fcitx-sunpinyin fcitx-cloudpinyin fcitx-configtool
    pacman -S ttf-inconsolata ttf-font-awesome deepin-icon-theme deepin-gtk-theme
}

conf_vbox(){
    if $(yes_or_no "Whether install vbox support");then
        if $(yes_or_no "Whether install X support");then
            pacman -S virtualbox-guest-utils
        else
            pacman -S virtualbox-guest-utils-nox
        fi
        systemctl enable dhcpcd
        systemctl enable vboxservice
    fi
}

conf_other(){
    if $(yes_or_no "Whether Configure others within chroot"); then
        echo "Use exit to exit bash"
        bash
        echo "Now is normal chroot bash"
    fi
}

conf_grub(){
    echo "Configure grub"
    pacman -S grub
    fdisk -l
    read -p "Input dest disk [/dev/sda]: " SDX
    SDX="${SDX:-/dev/sda}"

    grub-install --target=i386-pc $SDX
    grub-mkconfig -o /boot/grub/grub.cfg
}

conf_dotfiles(){
    if $(yes_or_no "Whether configure from github.com/honmaple/dotfiles"); then
        if [ ! -f "/bin/git" ];then
            $PACMAN -S git
        fi

        local dir="$PWD/dotfiles"
        read -p "Input dest directory [$dir]: " DIR
        DIR="${DIR:-$dir}"

        if [ ! -d "$DIR" ];then
            git clone https://github.com/honmaple/dotfiles $DIR
        fi

        local dirs=("script  $HOME/.script"
                    "emacs.d $HOME/.emacs.d"
                    "vim     $HOME/.vim"
                    "moc     $HOME/.moc"
                    "i3      $HOME/.config/i3"
                    "yapf    $HOME/.config/yapf"
                    "fonts   $HOME/.local/share/fonts"
                    "bashrc  $HOME/.bashrc"
                    "xinitrc $HOME/.xinitrc"
                    "inputrc $HOME/.inputrc"
                    "Xmodmap $HOME/.Xmodmap")
        for dir in "${dirs[@]}";do
            local dir=($dir)
            local from="$DIR/${dir[0]}"
            local to="${dir[1]}"
            if $(yes_or_no "Linking $from to $to");then
                ln -s "$from" "$to"
            fi
        done
    fi
}

main(){
    if $(yes_or_no "Whether install archlinux"); then
        conf_network
        conf_time
        conf_mirror
        conf_partition
        conf_mount
        conf_base
        conf_unmount
    fi

    if $(yes_or_no "Whether reboot");then
        reboot
    fi
}

conf(){
    if $(yes_or_no "Whether use sudo"); then
        PACMAN="sudo pacman"
    fi

    if $(yes_or_no "Whether configure archlinux"); then
        conf_desktop
        conf_dotfiles
    fi
}

usage(){
    echo -e "install:\n\tsh archlinux.sh install"
    echo -e "configure:\n\tsh archlinux.sh config"
}

case "$1" in
    install)
        main
        ;;
    config)
        conf
        ;;
    *)
        usage
        ;;
esac
