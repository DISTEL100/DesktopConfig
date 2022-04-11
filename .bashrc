#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias r="ranger"
gitfe() {
    SEP='echo "" && echo"" && echo "#######################################################" && echo "" && echo""' 
    eval $SEP
    COMMAND="git submodule foreach 'git $1 && $SEP'"
    eval $COMMAND
}

PS1='[\u@\h \W]\$ '

export PATH="$HOME/.local/bin:$PATH"

export GEM_HOME="$(ruby -e 'puts Gem.user_dir')"
export PATH="$PATH:$GEM_HOME/bin"

export FZF_DEFAULT_COMMAND='rg --files'
export JDTLS_HOME="/home/jonathan/.local/opt/jdtls-launcher/jdtls"
export EDITOR='nvim'
export VISUAL='nvim'

export NNN_OPENER='mimeopen'
export NNN_FIFO=$HOME/.nnn_fifo
export NNN_PLUG='<:preview-tui;p:preview-tabbed'
export NNN_COLOR='#54'
BLK="04" CHR="04" DIR="04" EXE="9e" REG="E7" HARDLINK="E2" SYMLINK="06" MISSING="00" ORPHAN="01" FIFO="0F" SOCK="0F" OTHER="02"
export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"
n ()
{
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    nnn "$@"
    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

[ -n "$NNNLVL" ] && PS1="N$NNNLVL $PS1"

if [ -f /usr/share/nnn/quitcd/quitcd.bash_zsh ]; then
    source /usr/share/nnn/quitcd/quitcd.bash_zsh
fi

# BEGIN_KITTY_SHELL_INTEGRATION
if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; fi
# END_KITTY_SHELL_INTEGRATION
