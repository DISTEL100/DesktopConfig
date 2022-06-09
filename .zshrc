# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/jonathan/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
source ./.profile	
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh
source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme
PS1=$'%{\e]0;%d\a%}\n%F{green}%n@%m %F{yellow}%d%f\n%# '
