call plug#begin('~/.local/share/nvim/plugged')

" General plugins
Plug 'Shougo/deoplete.nvim'
Plug 'airblade/vim-rooter'
Plug 'benekastah/neomake'
Plug 'scrooloose/nerdtree'
" Plug 'w0rp/ale'
Plug 'itchyny/lightline.vim'
Plug 'janko-m/vim-test'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'mhinz/vim-startify'
Plug 'rking/ag.vim'
Plug 'tpope/vim-commentary'
" Plug 'tpope/vim-sleuth'
Plug 'embear/vim-localvimrc'
Plug 'tpope/vim-surround'

" Git plugins
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" Ruby plugins
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rbenv'
Plug 'vim-ruby/vim-ruby'

" Go plugins
Plug 'fatih/vim-go'

" Rust plugins
Plug 'rust-lang/rust.vim'
Plug 'sebastianmarkow/deoplete-rust'

" Toml plugins
Plug 'cespare/vim-toml'

" Swift plugins
Plug 'keith/swift.vim'
Plug 'mitsuse/autocomplete-swift'

call plug#end()

" filetype settings
filetype indent on
filetype plugin on

" ui
set termguicolors
colorscheme fruity
set number
set showcmd
set lazyredraw
set showmatch
set incsearch
set hlsearch
set ruler
set fillchars+=vert:│
set cc=80

" enable syntax processesing
syntax on

" indentation
set expandtab
set shiftwidth=2
set tabstop=2

" whitespace
set list listchars=tab:»\ ,trail:·

" allow unsaved buffers
set hidden

" don't move curser to start of line
set nostartofline

" confirm saving
set confirm

" chdir to the current file's dir
set autochdir

" treat characters with special meaning by default
set magic

" use system clipboard
set clipboard+=unnamedplus

" leader
let mapleader=","

" bad habits
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" search highligting
nnoremap <esc> :noh<return><esc>

" terminal mode
tnoremap <Esc> <C-\><C-n>

" neomake
let g:neomake_ruby_enabled_makers = ['rubocop', 'mri']
let g:neomake_open_list = 2

let g:neomake_error_sign = {
        \ 'text': 'X',
        \ 'texthl': 'NeomakeErrorSign',
        \ }

let g:neomake_warning_sign = {
        \ 'text': '!',
        \ 'texthl': 'NeomakeWarningSign',
        \ }

augroup neomake_makers
  autocmd!
  let project_maker_exts = ['rs']

  autocmd BufReadPost,BufWritePost * if index(project_maker_exts, &ft) > 0 | Neomake | endif

  " Have neomake run cargo when Rust files are saved.
  autocmd BufReadPost,BufWritePost *.rs Neomake! cargo
augroup END

" deoplete
let g:deoplete#enable_at_startup=1
let g:deoplete#enable_smart_case = 1
let g:deoplete#enable_refresh_always=1
let g:deoplete#file#enable_buffer_path=1
let g:deoplete#auto_completion_start_length=2

let g:deoplete#sources={}
let g:deoplete#sources._    = ['buffer', 'member', 'file']
let g:deoplete#sources.ruby = ['buffer', 'member', 'file']

let g:deoplete#sources#rust#racer_binary='/Users/nan/.cargo/bin/racer'
let g:deoplete#sources#rust#rust_source_path='/Users/nan/src/rust/src'

" fzf
function! s:find_git_root()
	return system('git rev-parse --show-toplevel 2> /dev/null')[:-2]
endfunction

command! ProjectFiles execute 'Files' s:find_git_root()

noremap <leader>f :Files<CR>
noremap <leader>p :ProjectFiles<CR>
noremap <leader>b :Buffers<CR>

" vim-test
let test#strategy = "neovim"
let test#ruby#rspec#executable = 'bundle exec rspec'
nmap <silent> <leader>t :TestNearest<CR>
nmap <silent> <leader>T :TestFile<CR>
nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>
nmap <silent> <leader>g :TestVisit<CR>

" vim-rooter
let g:rooter_patterns = ['OWNERS', 'Rakefile', 'Makefile', 'README.md', '.git/']
let g:rooter_silent_chdir = 0

" vim-go
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>rt <Plug>(go-run-tab)
au FileType go nmap <Leader>rs <Plug>(go-run-split)
au FileType go nmap <Leader>rv <Plug>(go-run-vertical)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
au FileType go nmap <Leader>ds <Plug>(go-def-split)
au FileType go nmap <Leader>dv <Plug>(go-def-vertical)
au FileType go nmap <Leader>dt <Plug>(go-def-tab)
au FileType go nmap <Leader>gd <Plug>(go-doc)
au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
au FileType go nmap <Leader>gb <Plug>(go-doc-browser)
au FileType go nmap <Leader>s <Plug>(go-implements)
au FileType go nmap <Leader>i <Plug>(go-info)
au FileType go nmap <Leader>e <Plug>(go-rename)

let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

let g:go_fmt_command = "goimports"

" rust.vim
"let g:rustfmt_autosave = 1

" NERDTree
map <A-t> :NERDTreeToggle<CR>
