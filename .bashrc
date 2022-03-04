#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion

alias ls='ls --color=auto'
alias r="ranger"
PS1='[\u@\h \W]\$ '

export PATH="$HOME/.local/bin:$PATH"

export GEM_HOME="$(ruby -e 'puts Gem.user_dir')"
export PATH="$PATH:$GEM_HOME/bin"

export JDTLS_HOME=$HOME/.local/opt/jdtls-launcher/jdtls
export WORKSPACE=$HOME/workspace

export FZF_DEFAULT_COMMAND='rg --hidden --no-ignore --files'

export EDITOR='nvim'
export VISUAL='nvim'

export NNN_OPENER='mimeopen'
export NNN_FIFO=$HOME/.nnn_fifo
export NNN_PLUG='<:preview-tui;p:preview-tabbed'

[ -n "$NNNLVL" ] && PS1="N$NNNLVL $PS1"

if [ -f /usr/share/nnn/quitcd/quitcd.bash_zsh ]; then
    source /usr/share/nnn/quitcd/quitcd.bash_zsh
fi

eval "$(stack --bash-completion-script stack)"


# BEGIN_KITTY_SHELL_INTEGRATION
if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; fi
# END_KITTY_SHELL_INTEGRATION
