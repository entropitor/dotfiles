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
set relativenumber

let mapleader = ","
imap jj <Esc>
set backspace=indent,eol,start

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>

"colorscheme base16-tomorrow
colorscheme onedark
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

let g:NERDSpaceDelims = 1

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
let g:UltiSnipsEditSplit = "vertical"
let g:UltiSnipsSnippetsDir = "~/.vim/bundle/snippets/UltiSnips"
""let g:UltiSnips.always_use_first_snippet =1

"nnoremap <leader>ff :FufFile ./**[^node_modules]/<CR>
"nnoremap <leader>fg :FufFile **/<CR>
"nnoremap <leader>fr :FufFile<CR>
"nnoremap <leader>fb :FufBuffer<CR>
"nnoremap <leader>fb :FufBuffer<CR>

nnoremap <C-p> <ESC>:CtrlP<CR>
nnoremap <C-b> <ESC>:CtrlPBuffer<CR>
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]((\.(git|hg|svn))|node_modules|build|Build)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
  \ }
let g:ctrlp_show_hidden = 1

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

set guifont=Ubuntu\ Mono\ derivative\ Powerline\ 12
"let g:airline_theme = 'powerlineish'
let g:airline_powerline_fonts=1
set laststatus=2 "Start drawing on start of session
let g:airline_enable_branch = 1
let g:airline#extensions#whitespace#enabled = 0
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

"let g:neomake_javascript_enabled_makers = ['standard', 'flow']
"autocmd! BufWritePost * Neomake
"nnoremap <leader>ss :Neomake!<CR>
"let g:neomake_open_list=2

nnoremap <leader>sl :lopen<CR>
let g:ale_sign_column_always = 1
let g:ale_sign_error = 'XX'
let g:ale_sign_warning = '??'

let g:flow#autoclose = 1
let g:flow#enable = 0

"let g:indentLine_char = '︙'
let g:indentLine_char = '|'
let g:loaded_matchparen=1
let g:indent_guides_enable_on_vim_startup = 1

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

if exists('&inccommand')
  "highlight Search term=NONE cterm=NONE ctermfg=black ctermbg=9
  "highlight IncSearch cterm=NONE ctermfg=black ctermbg=10
  highlight Substitute cterm=NONE ctermfg=black ctermbg=9
  set inccommand=split
endif

tnoremap <Esc> <C-\><C-n>

set foldmethod=syntax
set foldlevelstart=1

let &titlestring = expand('%:t')
set title

" let g:deoplete#enable_at_startup = 1
" inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : deoplete#mappings#manual_complete()
" function! s:check_back_space() abort "{{{
"   let col = col('.') - 1
"   return !col || getline('.')[col - 1]  =~ '\s'
" endfunction"}}}

" let g:flow_path = 'npx --no-install flow'
" if g:flow_path != 'not found: flow'
"   let g:deoplete#sources#flow#flow_bin = g:flow_path
" endif

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
imap <c-space> <Plug>(asyncomplete_force_refresh)

let g:asyncomplete_auto_popup = 1
set completeopt+=preview
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#flow#get_source_options({
  \ 'name': 'flow',
  \ 'priority': 100,
  \ 'whitelist': ['javascript'],
  \ 'completor': function('asyncomplete#sources#flow#completor'),
  \ 'config': {
  \    'prefer_local': 1
  \  },
  \ }))
call asyncomplete#register_source(asyncomplete#sources#ultisnips#get_source_options({
  \ 'name': 'ultisnips',
  \ 'priority': 10,
  \ 'whitelist': ['*'],
  \ 'completor': function('asyncomplete#sources#ultisnips#completor'),
  \ }))
" call asyncomplete#register_source(asyncomplete#sources#omni#get_source_options({
"   \ 'name': 'omni',
"   \ 'whitelist': ['*'],
"   \ 'completor': function('asyncomplete#sources#omni#completor')
"   \  }))
call asyncomplete#register_source(asyncomplete#sources#buffer#get_source_options({
  \ 'name': 'buffer',
  \ 'whitelist': ['*'],
  \ 'blacklist': ['go'],
  \ 'completor': function('asyncomplete#sources#buffer#completor'),
  \ }))
au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#emoji#get_source_options({
  \ 'name': 'emoji',
  \ 'whitelist': ['*'],
  \ 'completor': function('asyncomplete#sources#emoji#completor'),
  \ }))
