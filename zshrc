# zmodload zsh/zprof

ZGEN_RESET_ON_CHANGE=(${HOME}/.zshrc ${HOME}/.zshrc.local)
source "${HOME}/.zgen/zgen.zsh"
if ! zgen saved; then

  zgen oh-my-zsh
  zgen oh-my-zsh plugins/git
  # zgen oh-my-zsh plugins/vi-mode
  zgen oh-my-zsh plugins/httpie
  zgen oh-my-zsh plugins/docker
  zgen oh-my-zsh plugins/tmux

  # zgen load denysdovhan/spaceship-prompt
  # zgen oh-my-zsh themes/robbyrussell

  zgen load mafredri/zsh-async
  zgen load sindresorhus/pure

  # generate the init script from plugins above
  zgen save
fi

custom_get_prompt () {
  kccc_alias=$(command -v kccc)

  if [ -n "$kccc_alias" ]; then
    echo "[%{$fg[magenta]%}$(kubectl config current-context)%{$reset_color%}]"
  fi
}
PROMPT="\$(custom_get_prompt) $PROMPT"

# Lazy load packages
alias k="kubectl > /dev/null"
function kubectl() {
  unalias k

  unfunction $0
  zgen oh-my-zsh plugins/kubectl
  $0 $@
}
function minikube() {
  unfunction $0
  zgen oh-my-zsh plugins/minikube
  $0 $@
}
function nvm() {
  unfunction $0
  zgen load lukechilds/zsh-nvm
  $0 $@
}
function rbenv() {
  unfunction $0
  zgen load kadaan/zsh-rbenv-lazy
  $0 $@
}
function helm() {
  unfunction $0
  zgen oh-my-zsh plugins/helm
  $0 $@
}

HYPHEN_INSENSITIVE="true"
HIST_STAMPS="dd.mm.yyyy"
# plugins=(git command-not-found docker httpie)

# User configuration
if [ -f ~/.zshrc.local ]; then
    . ~/.zshrc.local
fi
# source $ZSH/oh-my-zsh.sh

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
  export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git --ignore node_modules/ --ignore build/ --ignore Build/ -g ""'
  alias preview="fzf --preview 'bat --color \"always\" {}'"
  export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(nvim {})+abort'"
fi
