### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit's installer chunk

zinit ice wait lucid atload="after_load_git"; zinit snippet OMZ::plugins/git/git.plugin.zsh
function after_load_git () {
  alias ggpush='git push origin "$(git rev-parse --abbrev-ref HEAD)"'
}

zinit ice wait lucid; zinit light zsh-users/zsh-syntax-highlighting

zinit ice wait lucid; zinit light zsh-users/zsh-completions

function _bind_autosuggest_accept {
  bindkey '^ ' autosuggest-accept
}
zinit ice wait lucid atload'_zsh_autosuggest_start; _bind_autosuggest_accept'
zinit light zsh-users/zsh-autosuggestions

# zinit light mafredri/zsh-async
# zinit ice compile'(pure|async).zsh' pick'async.zsh' src'pure.zsh' wait'!0' atload'prompt_pure_precmd'
# zinit light sindresorhus/pure

zinit ice atpull"make build" atclone"make build" wait'!0' lucid atload"_prompt_purs_precmd $(pwd)"
zinit light entropitor/purs

# zinit light denysdovhan/spaceship-prompt

autoload -U colors && colors

# zinit ice depth=1; zinit light romkatv/powerlevel10k

# Lazy load packages
zinit wait lucid for \
  OMZ::lib/compfix.zsh \
  OMZ::lib/completion.zsh \
  OMZ::lib/git.zsh \
  OMZ::lib/directories.zsh \
  OMZ::lib/theme-and-appearance.zsh \
  OMZ::plugins/kubectl/kubectl.plugin.zsh \
  OMZ::plugins/minikube/minikube.plugin.zsh \
  OMZ::plugins/helm/helm.plugin.zsh \
  OMZ::plugins/rbenv/rbenv.plugin.zsh \
  OMZ::plugins/pyenv/pyenv.plugin.zsh \
  OMZ::plugins/tmux/tmux.plugin.zsh \
  OMZ::plugins/terraform/terraform.plugin.zsh
alias k="kubectl"
alias kg="kubectl get"
alias kd="kubectl describe"
alias kdel="kubectl delete"

HYPHEN_INSENSITIVE="true"
HIST_STAMPS="dd.mm.yyyy"
zinit snippet OMZ::lib/history.zsh

zinit ice wait lucid; zinit light willghatch/zsh-cdr
alias c='cdr $(cdr -l | fzf | awk -F " " "{print \$1}")'

zinit ice wait lucid; zinit light zsh-users/zaw

# Popular: onedark, datacamp, eighties
export THEME=datacamp
alias b="~/.dotfiles/.base16/install.sh"
# User configuration
if [ -f ~/.zshrc.local ]; then
    . ~/.zshrc.local
fi
# source $ZSH/oh-my-zsh.sh

eval $(fnm env)
fnm use 2> /dev/null
function chpwd {
  fnm use 2> /dev/null
}

DEFAULT_USER="jens"
if [ -f ~/.zshrc.aliases ]; then
    . ~/.zshrc.aliases
fi

if type nvim > /dev/null 2>&1; then
  export EDITOR="nvim"
  alias vim='nvim'
fi

set -o vi

if [ -f ~/.fzf.zsh ]; then
  source ~/.fzf.zsh
  if [ ! -z "$THEME" ]; then
    source ~/.dotfiles/.base16/templates/fzf/bash/base16-${THEME}.config
  fi
  export FZF_DEFAULT_COMMAND='fd --hidden --no-ignore -E .git -E node_modules/ -E build/ -E Build/ -t file'
  alias preview="fzf --preview 'bat --color \"always\" {}'"
  export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(nvim {})+abort'"
fi

PATH="./node_modules/.bin:$PATH"
export PATH="../node_modules/.bin:$PATH"

export REVIEW_BASE="master"

_zsh_cli_fg() { fg; }
zle -N _zsh_cli_fg
bindkey '^F' _zsh_cli_fg

unalias zi
eval "$(zoxide init zsh)"
