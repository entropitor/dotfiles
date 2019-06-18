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

let mapleader = ","
imap jj <Esc>
set backspace=indent,eol,start

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>
if exists('&inccommand')
  highlight Substitute cterm=NONE ctermfg=black ctermbg=9
  set inccommand=split
endif

" Configure tab completion
set wildmode=longest,list,full
set wildmenu

set scrolloff=10

set list
set listchars=tab:>-,trail:~,nbsp:.,precedes:<,extends:>
nmap <leader>l :set list!<CR>

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

nnoremap <leader>w <C-w>v<C-w>l

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <leader>qj :cnext<CR>
nnoremap <leader>qk :cprevious<CR>
nnoremap <leader>qo :copen<CR>
nnoremap <leader>qc :cclose<CR>
nnoremap <leader>lj :lnext<CR>
nnoremap <leader>lk :lprevious<CR>
nnoremap <leader>lo :lopen<CR>
nnoremap <leader>lc :lclose<CR>

if has('mouse') | set mouse=a | endif

nnoremap <leader>b :b<Space>

autocmd BufEnter term://* startinsert

tnoremap <Esc> <C-\><C-n>

set foldmethod=syntax
set foldlevelstart=0

let &titlestring = expand('%:t')
set title

" =======================================================================
" ===   CONFIGURATION FOR PLUGINS IN SAME SECTIONS AS VIMRC.BUNDLES   ===
" =======================================================================

" ===========================
" ===   Git Integration   ===
" ===========================
" [Config for tpope/vim-fugitive]: Add git commands

" [Config for airblade/vim-gitgutter]: Add git diff symbols in the gutter

" ========================
" ===   Improved VIM   ===
" ========================
" [Config for Raimondi/delimitMate]: Close brackets automatically

" [Config for tpope/vim-endwise]: Closes def automatically

" [Config for Yggdroot/indentLine]: Show indentation line
" let g:indentLine_char = '︙'
let g:indentLine_char = '|'
let g:indent_guides_enable_on_vim_startup = 1
" let g:indentLine_setConceal = 0
let g:indentLine_concealcursor = 'nc'

" [scrooloose/nerdcommenter]: (Un)comment line
let g:NERDSpaceDelims = 1

" [Config for tpope/vim-surround]: Easily handle quotes, ...

" [Config for tpope/vim-abolish]: Case-sensitive replace + conversion between cases (snake-case, ...)

" [Config for tpope/vim-unimpaired]: Add extra [ commands

" [Config for easymotion/vim-easymotion]: Easily move to a certain spot in the file

" [Config for embear/vim-localvimrc]: Allow adding a local vimrc file to a project

" ================
" ===   Repl   ===
" ================
" " [Config for ujihisa/repl.vim]:
" " [Config for Shougo/vimproc.vim]:
" " [Config for Shougo/vimshell.vim]:

" =====================
" ===   Utilities   ===
" =====================
" [Config for kburdett/vim-nuuid]: Generate uuid

" [Config for chrisbra/Colorizer]: See colors inline
let g:colorizer_auto_filetype='css,html'

" [Config for ruanyl/vim-gh-line]: Copy line on github
let g:gh_open_command = 'fn() { echo "$@" | pbcopy; }; fn '
let g:gh_use_canonical = 1

" [Config for wakatime/vim-wakatime]: Time tracking (WakaTime)

" [Config for editorconfig/editorconfig-vim]: EditorConfig

" [Config for vim-scripts/scratch.vim]: Add a :Scratch buffer

" ==========================
" ===   Autocompletion   ===
" ==========================
inoremap <expr> <Tab> pumvisible() ? "\<Down>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<Up>" : "\<S-Tab>"
imap <c-space> <Plug>(asyncomplete_force_refresh)

set completeopt=noselect,menu,menuone,preview,noinsert
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

" [Config for prabirshrestha/async.vim]:
" [Config for prabirshrestha/asyncomplete.vim]:
let g:asyncomplete_auto_popup = 1
let g:asyncomplete_smart_completion = 0
let g:asyncomplete_remove_duplicates = 1

" [Config for yami-beta/asyncomplete-omni.vim]:
au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#omni#get_source_options({
  \ 'name': 'omni',
  \ 'whitelist': ['*'],
  \ 'completor': function('asyncomplete#sources#omni#completor')
  \  }))
" [Config for prabirshrestha/asyncomplete-ultisnips.vim]:
au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#ultisnips#get_source_options({
  \ 'name': 'ultisnips',
  \ 'priority': 10,
  \ 'whitelist': ['*'],
  \ 'completor': function('asyncomplete#sources#ultisnips#completor'),
  \ }))

" [Config for prabirshrestha/vim-lsp]:
let g:lsp_async_completion = 1
nnoremap <silent> K :LspHover<CR>
nnoremap <silent> gd :LspDefinition<CR>
nnoremap <silent> <leader>lr :LspRename<CR>
nnoremap <silent> <leader>ld :LspDocumentSymbol<CR>
nnoremap <silent> <leader>lf :LspDocumentFormat<CR>
nnoremap <silent> <leader>le :LspDocumentDiagnostics<CR>

" [Config for prabirshrestha/asyncomplete-lsp.vim]:
" See Languages section for the config!

" =============================
" ===   Checkers/Builders   ===
" =============================
" [Config for w0rp/ale]:
nnoremap <leader>sl :lopen<CR>
let g:ale_sign_column_always = 1
let g:ale_sign_error = 'XX'
let g:ale_sign_warning = '??'
let g:ale_fix_on_save = 1
let g:ale_lint_delay = 750
let g:ale_fixers = {}
" See Languages section for the rest!

" ==================
" ===   Search   ===
" ==================
" [Config for mileszs/ack.vim]:
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" ===========================
" ===   File Management   ===
" ===========================
" [Config for junegunn/fzf.vim]: FZF
nnoremap <C-p> <ESC>:Files<CR>
let g:fzf_buffers_jump = 1

" [Config for ctrlpvim/ctrlp.vim]: CtrlP
" nnoremap <C-p> <ESC>:CtrlP<CR>
" nnoremap <C-S-p> <ESC>:CtrlPBuffer<CR>
" nnoremap <leader>fp <ESC>:CtrlP<CR>
" nnoremap <leader>fb <ESC>:CtrlPBuffer<CR>
" let g:ctrlp_working_path_mode = 'ra'
" let g:ctrlp_custom_ignore = {
"   \ 'dir':  '\v[\/]((\.(git|hg|svn))|node_modules|build|Build|target)$',
"   \ 'file': '\v\.(exe|so|dll)$',
"   \ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
"   \ }
" let g:ctrlp_show_hidden = 1

" ==========================
" ===   Test Managment   ===
" ==========================
" [Config for janko-m/vim-test]:
nmap <silent> <leader>tn :TestNearest<CR>
nmap <silent> <leader>tf :TestFile<CR>
nmap <silent> <leader>ts :TestSuite<CR>
nmap <silent> <leader>tl :TestLast<CR>
nmap <silent> <leader>tv :TestVisit<CR>

" ==============================
" ===   Project Management   ===
" ==============================
" [Config for scrooloose/nerdtree]: File Sidebar
nmap <F7> :NERDTreeToggle<CR>
let g:NERDTreeShowHidden=1
" [Config for Xuyuanp/nerdtree-git-plugin]

" [Config for majutsushi/tagbar]: Tags sidebar
nmap <F8> :TagbarToggle<CR>

" [Config for vim-airline/vim-airline]: bottombar
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

set guifont=Ubuntu\ Mono\ derivative\ Powerline\ 12
"let g:airline_theme = 'powerlineish'
let g:airline_powerline_fonts=1
set laststatus=2 "Start drawing on start of session
let g:airline_enable_branch = 0
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#whitespace#checks = [ 'indent', 'trailing' ]
let g:airline#extensions#whitespace#show_message = 0
let g:airline#extensions#whitespace#trailing_format = 'trailing[%s]'
let g:airline#extensions#whitespace#mixed_indent_format = 'mixed-indent[%s]'
let g:airline#extensions#tabline#enabled = 0
let g:airline_section_b = ''
let g:airline_section_y = ''
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#formatter = 'unique_tail'

" ==================
" ===   Themes   ===
" ==================
set t_Co=256
colorscheme onedark
set background=dark
if (has("termguicolors"))
  set termguicolors
endif

" [Config for chriskempson/base16-vim]:
" [Config for joshdick/onedark.vim]:
" [Config for godlygeek/csapprox]:

" ====================
" ===   Snippets   ===
" ====================
let g:UltiSnips={}
let g:UltiSnipsExpandTrigger = "<c-e>"
let g:UltiSnipsJumpForwardTrigger = "<c-l>"
let g:UltiSnipsJumpBackwardTrigger = "<c-h>"
let g:UltiSnipsEditSplit = "vertical"
let g:UltiSnipsSnippetsDir = "~/.vim/bundle/snippets/UltiSnips"
""let g:UltiSnips.always_use_first_snippet =1

" =====================
" ===   Languages   ===
" =====================

" Antlr

" C / C++
if executable('clangd')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'clangd',
        \ 'cmd': {server_info->['clangd']},
        \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp'],
        \ })
endif

" CSS
autocmd BufNewFile,BufRead *.scss set ft=scss.css
let g:ale_fixers['css'] = ['prettier']

" Go
if executable('go-langserver')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'go-langserver',
        \ 'cmd': {server_info->['go-langserver', '-mode', 'stdio']},
        \ 'whitelist': ['go'],
        \ })
    autocmd FileType go setlocal omnifunc=lsp#complete
endif
" [Config for fatih/vim-go]:
let g:go_fmt_command = "goimports"

" Graphql

" HTML
let g:ale_fixers['html'] = ['prettier']

" JSON
let g:ale_fixers['json'] = ['prettier']

" JavaScript / Typescript
let g:ale_fixers['javascript'] = ['prettier']
let g:ale_javascript_prettier_use_local_config = 1
if executable('flow-language-server')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'flow-language-server',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'flow-language-server --stdio --try-flow-bin']},
        \ 'root_uri':{server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), '.flowconfig'))},
        \ 'whitelist': ['javascript'],
        \ })
    autocmd FileType javascript setlocal omnifunc=lsp#complete
endif

" Ledger

" Platuml

" Prolog
au BufNewFile,BufRead *.pro set filetype=prolog
au BufNewFile,BufRead *.pl set filetype=prolog
let g:tagbar_type_prolog = {
    \ 'ctagstype' : 'Prolog',
    \ 'kinds' : [
      \ 'p:Predicates',
    \ ]
  \ }

" Python
let g:ale_fixers['python'] = ['autopep8']

" Reason
if executable('ocaml-language-server')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'ocaml-language-server',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'ocaml-language-server --stdio']},
        \ 'whitelist': ['reason', 'ocaml'],
        \ })
endif

" Ruby
let g:ale_fixers['ruby'] = ['rubocop']
let g:ale_ruby_rubocop_executable = 'bin/rubocop'
if executable('solargraph')
    " gem install solargraph
    au User lsp_setup call lsp#register_server({
        \ 'name': 'solargraph',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'solargraph stdio']},
        \ 'initialization_options': {"diagnostics": "true"},
        \ 'whitelist': ['ruby'],
        \ })
endif

" Rust
if executable('rls')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'rls',
        \ 'cmd': {server_info->['rustup', 'run', 'stable', 'rls']},
        \ 'whitelist': ['rust'],
        \ })
endif
" [Config for rust-lang/rust.vim]:
let g:rustfmt_autosave = 1
let g:rustfmt_command = 'rustup run stable rustfmt'

" Typescript
au BufNewFile,BufRead *.ts set filetype=typescript
let g:ale_fixers['typescript'] = ['prettier']
let g:ale_typescript_prettier_use_local_config = 1
autocmd FileType typescript let g:lsp_async_completion=0
if executable('typescript-language-server')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'typescript-language-server',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'typescript-language-server --stdio']},
        \ 'root_uri':{server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'tsconfig.json'))},
        \ 'whitelist': ['typescript'],
        \ })
    autocmd FileType typescript setlocal omnifunc=lsp#complete
endif

" SQL

" Yaml
autocmd FileType yaml setlocal foldmethod=indent
