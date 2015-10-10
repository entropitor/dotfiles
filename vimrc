set t_Co=256

if filereadable(expand("~/.vimrc.bundles"))
  source ~/.vimrc.bundles
endif

set backspace=indent,eol,start

syntax on
filetype plugin indent on

set omnifunc=syntaxcomplete#Complete

let mapleader = ","

set noswapfile
set nobackup
set nowb

set autoread

set number

colorscheme base16-tomorrow
set background=dark

imap jj <Esc>

set list
set listchars=tab:→\ ,trail:·
nmap <leader>l :set list!<CR>

set hlsearch

nmap <F8> :TagbarToggle<CR>
nmap <F7> :NERDTreeToggle<CR>

let g:indent_guides_enable_on_vim_startup = 1

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>
nnoremap <tab> %
vnoremap <tab> %

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
"inoremap <up> <nop>
"inoremap <down> <nop>
"inoremap <left> <nop>
"inoremap <right> <nop>
"nnoremap j gj
"nnoremap k gk

nnoremap <leader>w <C-w>v<C-w>l

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

"let g:ctags_statusline=1
"let g:ctags_title=1
"let generate_tags=1

"imap . .<C-x><C-o><C-p>
"imap <leader>f <C-x><C-o><C-p>

"let g:SuperTabMappingForward = '<c-space>'
"let g:SuperTabMappingBackward = '<s-c-space>'
"let g:SuperTabMappingForward = '<nul>'
"let g:SuperTabMappingBackward = '<s-nul>'

"let g:UltiSnips = {}
"let g:UltiSnips.ExpandTrigger = "<c-e>"
""let g:UltiSnips.JumpForwardTrigger = "<tab>"
""let g:UltiSnips.JumpBackwardTrigger = "<s-tab>"
"let g:UltiSnips.JumpForwardTrigger = "<c-N>"
"let g:UltiSnips.JumpBackwardTrigger = "<c-P>"
""let g:UltiSnips.always_use_first_snippet =1

"nnoremap <leader>ff :FufFile ./**[^node_modules]/<CR>
"nnoremap <leader>fg :FufFile **/<CR>
"nnoremap <leader>fr :FufFile<CR>
"nnoremap <leader>fb :FufBuffer<CR>
"nnoremap <leader>fb :FufBuffer<CR>

nnoremap <C-P> <ESC>:CtrlP
let g:ctrlp_working_path_mode = 'ra'

if has('mouse') | set mouse=a | endif


if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif


set guifont=Ubuntu\ Mono\ derivative\ Powerline\ 12
let g:airline_theme = 'powerlineish'
let g:airline_powerline_fonts=1
set laststatus=2 "Start drawing on start of session
let g:airline_enable_branch = 1
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#whitespace#checks = [ 'indent', 'trailing' ]
let g:airline#extensions#whitespace#show_message = 1
let g:airline#extensions#whitespace#trailing_format = 'trailing[%s]'
let g:airline#extensions#whitespace#mixed_indent_format = 'mixed-indent[%s]'
let g:airline#extensions#tabline#enabled = 0

nnoremap <leader>b :b<Space>

"let g:syntastic_javascript_checkers = ['standard', 'jsxhint']
let g:syntastic_javascript_checkers = ['standard']
"let g:indentLine_char = '︙'
let g:indentLine_char = '|'
let g:loaded_matchparen=1
