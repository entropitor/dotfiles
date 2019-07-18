# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"
# ZSH_THEME="agnoster"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="dd.mm.yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git command-not-found docker httpie)

# User configuration

if [ -f ~/.zshrc.local ]; then
    . ~/.zshrc.local
fi
source $ZSH/oh-my-zsh.sh

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

DEFAULT_USER="jens"
if [ -f ~/.zshrc.aliases ]; then
    . ~/.zshrc.aliases
fi

export EDITOR="nvim"

if type nvim > /dev/null 2>&1; then
  alias vim='nvim'
fi

set -o vi

if [ -f ~/.fzf.zsh ]; then
  source ~/.fzf.zsh
  export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git --ignore node_modules/ --ignore build/ --ignore Build/ -g ""'
  alias preview="fzf --preview 'bat --color \"always\" {}'"
  export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(nvim {})+abort'"
fi
