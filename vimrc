if filereadable(expand("~/.vimrc.bundles"))
  source ~/.vimrc.bundles
endif

syntax on
filetype plugin indent on
au BufNewFile,BufRead *.pro set filetype=prolog
au BufNewFile,BufRead *.pl set filetype=prolog
autocmd BufNewFile,BufRead *.scss set ft=scss.css

set noswapfile
set nobackup
set nowb

set nopaste
set autoread
set number

let mapleader = ","
imap jj <Esc>
set backspace=indent,eol,start

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>

colorscheme base16-tomorrow
set background=dark
set t_Co=256

set list
set listchars=tab:→\ ,trail:·
nmap <leader>l :set list!<CR>

set hlsearch

"nnoremap <tab> %
"vnoremap <tab> %

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
"nnoremap j gj
"nnoremap k gk

nnoremap <leader>w <C-w>v<C-w>l

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

if has('mouse') | set mouse=a | endif

nnoremap <leader>b :b<Space>

nmap <F7> :NERDTreeToggle<CR>
let g:NERDTreeShowHidden=1

nmap <F8> :TagbarToggle<CR>
let g:tagbar_type_prolog = {
    \ 'ctagstype' : 'Prolog',
    \ 'kinds' : [
        \ 'p:Predicates',
        \ ]
    \ }

"let g:ctags_statusline=1
"let g:ctags_title=1
"let generate_tags=1

let g:UltiSnips={}
let g:UltiSnipsExpandTrigger = "<c-e>"
let g:UltiSnipsJumpForwardTrigger = "<c-l>"
let g:UltiSnipsJumpBackwardTrigger = "<c-h>"
""let g:UltiSnips.always_use_first_snippet =1

"nnoremap <leader>ff :FufFile ./**[^node_modules]/<CR>
"nnoremap <leader>fg :FufFile **/<CR>
"nnoremap <leader>fr :FufFile<CR>
"nnoremap <leader>fb :FufBuffer<CR>
"nnoremap <leader>fb :FufBuffer<CR>

nnoremap <C-P> <ESC>:CtrlP<CR>
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]((\.(git|hg|svn))|node_modules)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
  \ }

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

""let g:syntastic_javascript_checkers = ['standard', 'jsxhint']
""let g:syntastic_javascript_checkers = ['flow', 'standard']
"let g:syntastic_javascript_checkers = ['standard', 'flow']
"let g:syntastic_check_on_wq = 0
"nmap <leader>rn :let g:syntastic_javascript_checkers = ['eslint','flow']<CR>
"nmap <leader>sc :SyntasticToggleMode<CR>

let g:neomake_javascript_enabled_makers = ['standard', 'flow']
autocmd! BufWritePost * Neomake
nnoremap <leader>ss :Neomake!<CR>
nnoremap <leader>sl :lopen<CR>
"let g:neomake_open_list=2

"let g:indentLine_char = '︙'
let g:indentLine_char = '|'
let g:loaded_matchparen=1
let g:indent_guides_enable_on_vim_startup = 1

