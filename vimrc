nnoremap <SPACE> <Nop>
let mapleader = " "

if filereadable(expand("~/.vimrc.bundles"))
  source ~/.vimrc.bundles
endif

syntax on
filetype plugin indent on

set noswapfile
set nobackup
set nowb

" use spaces instead of tabs
set expandtab
set shiftwidth=2
set softtabstop=2

set nopaste
set autoread
set number
set relativenumber

set cursorline

imap jj <Esc>
set backspace=indent,eol,start

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
nnoremap <leader><leader><space> :noh<cr>
if exists('&inccommand')
  highlight Substitute cterm=NONE ctermfg=black ctermbg=9
  set inccommand=split
endif

" Configure tab completion
set wildmode=longest,list,full
set wildmenu

set list
set listchars=tab:>-,trail:~,nbsp:.,precedes:<,extends:>
nmap <leader>l :set list!<CR>

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

if has('mouse') | set mouse=a | endif

autocmd BufEnter term://* startinsert

tnoremap <Esc> <C-\><C-n>

set foldmethod=syntax
set foldlevelstart=10000

" Allow buffer to be hidden (with unedited changes)
set hidden

let &titlestring = expand('%:t')
set title

if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has("termguicolors"))
  set termguicolors
endif

" colorscheme onedark
colorscheme base16-tomorrow-night-eighties
set background=dark

nnoremap <leader>wv <C-w>v<C-w>l
nnoremap <leader>ws <C-w>s<C-w>l

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <leader>bq :%bd\|:e#<CR>

nnoremap <leader>tn :tabnew<CR>
nnoremap <leader>td :tabclose<CR>
nnoremap [w :tabprevious<CR>
nnoremap ]w :tabnext<CR>
nnoremap [W :tabfirst<CR>
nnoremap ]W :tablast<CR>

nnoremap <leader>nq :qa<CR>
nnoremap <leader>nw :w<CR>
