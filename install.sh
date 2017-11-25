# The basics
sudo apt-get install zsh vim git python-dev python3-dev python-pip python3-pip curl
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
sudo apt-get install nodejs build-essential
chsh -s $(which zsh) # logout after this step to change shell

# stack client
echo 'deb http://mirror.transip.net/stack/software/deb/Ubuntu_16.04/ ./' | sudo tee /etc/apt/sources.list.d/stack-client.list
wget -O - https://mirror.transip.net/stack/release.key | sudo apt-key add - 
sudo apt-get update
sudo apt-get install stack-client

# Powerline fonts
git clone https://github.com/powerline/fonts.git --depth=1
cd fonts
./install.sh
cd ..
rm -rf fonts

# rcm
sudo add-apt-repository ppa:martin-frost/thoughtbot-rcm
sudo apt-get update
sudo apt-get install rcm
rcup -v

# neovim
sudo add-apt-repository ppa:neovim-ppa/stable
sudo apt-get update
sudo apt-get install neovim

# kupfer
sudo add-apt-repository ppa:kupfer-team/ppa
sudo apt-get update
sudo apt-get install kupfer

# Xmonad
sudo apt-get install haskell-plaform
# cabal install xmonad xmonad-contrib xmobar
sudo apt-get install xmonad libghc-xmonad-dev libghc-xmonad-contrib-dev xmobar xcompmgr nitrogen stalonetray ssh-askpass-gnome udiskie xfce4-power-manager dzen2 gnome-settings-daemon libappindicator3-dev
# moreutils consolekit synapse ssh-askpass-gnome thunar terminator remmina

sudo add-apt-repository ppa:skeuchel/haskell
sudo apt-get update
sudo apt-get install xmonad-extras

# # volnoti
# sudo apt-get install autoconf automake dbus-1-dbg
# git clone git://github.com/davidbrazdil/volnoti.git
# cd volnoti
# ./prepare.sh
# ./configure --prefix=/usr
# make
# sudo make install
# cd ..
# rm -rf volnoti
