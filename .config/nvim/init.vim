call plug#begin('~/.config/nvim/plugged')

" General plugins
Plug 'Shougo/deoplete.nvim'
Plug 'airblade/vim-rooter'
Plug 'benekastah/neomake'
Plug 'itchyny/lightline.vim'
Plug 'janko-m/vim-test'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'mhinz/vim-startify'
Plug 'rking/ag.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'

" Git plugins
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" Ruby plugins
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rbenv'
Plug 'vim-ruby/vim-ruby'

" Markdown plugins
Plug 'shime/vim-livedown'

" Colorschemes
Plug 'justinmk/molokai'

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
set fillchars+=vert:\ 
set cc=80

" enable syntax processesing
syntax on

" whitespace
set list listchars=tab:»·,trail:·

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
autocmd BufReadPost,BufWritePost * Neomake
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

" deoplete
let g:deoplete#enable_at_startup=1
let g:deoplete#enable_smart_case = 1
let g:deoplete#enable_refresh_always=1
let g:deoplete#file#enable_buffer_path=1
let g:deoplete#auto_completion_start_length=2

let g:deoplete#sources={}
let g:deoplete#sources._    = ['buffer', 'member', 'file']
let g:deoplete#sources.ruby = ['buffer', 'member', 'file']

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
let g:rooter_silent_chdir = 0
