#
# ~/.bashrc
#
source .profile
source    /usr/share/fzf/key-bindings.bash
source    /usr/share/fzf/completion.bash


# enable bash completion in interactive shells
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
