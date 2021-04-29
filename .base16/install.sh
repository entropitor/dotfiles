#!/bin/bash

DOTFILES_DIR="$HOME/.dotfiles"
BASE16_DIR="${DOTFILES_DIR}/.base16"

function update {
  if ! command -v base16-builder; then
    echo "base16-builder not installed. Run cargo install base16-builder"
    exit 1
  fi

  cd ${BASE16_DIR} && base16-builder update
}

function update-alacritty {
  local ALACRITTY_PATH="config/alacritty/alacritty.yml"
  local TARGET="${HOME}/.${ALACRITTY_PATH}"
  rm ${TARGET} 2>/dev/null || true
  cp ${DOTFILES_DIR}/${ALACRITTY_PATH} ${TARGET}
  yq -iy .colors=$(yq .colors ${BASE16_DIR}/templates/alacritty/colors/base16-${THEME}.yml -c) ${TARGET}
}

function update-tmux {
  tmux source-file ${BASE16_DIR}/templates/tmux/colors/base16-${THEME}.conf 2>/dev/null || true
}

function update-vim {
  cp -r ${BASE16_DIR}/templates/vim/colors/ ~/.vim/autoload/colors
  cp -r ${BASE16_DIR}/templates/vim/colors/ ~/.vim/bundle/base16-vim/colors || true
}

function update-vim-airline {
  cd ${BASE16_DIR}/templates/vim-airline-themes/ && ./after.sh

  mkdir -p ~/.vim/autoload/airline/themes
  cp -r ${BASE16_DIR}/templates/vim-airline-themes/autoload/airline/themes/ ~/.vim/autoload/airline/themes
}

function update-fzf {
  # Doesn't work because not the same shell
  source ${BASE16_DIR}/templates/fzf/bash/base16-${THEME}.config
  # echo "Run the following command to update the fzf prompt"
  # echo source ${BASE16_DIR}/templates/fzf/bash/base16-${THEME}.config
}

function copy-kitty {
  cp -r ${BASE16_DIR}/templates/kitty/colors/ ~/.config/kitty
}
function update-kitty {
  if [[ -z "$TMUX" ]]; then
    kitty @ set-colors -c -a ~/.config/kitty/base16-${THEME}.conf
  else
    echo "Cannot update this shell from a TMUX session"
  fi
}

if [[ "$1" == "update" ]]; then
  update
  copy-kitty
  update-vim
  update-vim-airline
fi
update-alacritty
update-kitty
update-tmux
update-fzf
