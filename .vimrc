set nocompatible

filetype off

let mapleader = "\<Space>"

" \\\ EASY VIMRC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" opens vimrc
nnoremap <silent> <leader>ev :e $MYVIMRC<cr>

filetype plugin indent on

syntax enable

" \\\ ETC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" i love fast redraws
set ttyfast

" smaller timeout to remove lag from airline when leaving insert mode
set ttimeoutlen=50

" syntax highlight by default
syntax on

" allows unsaved buffers
set hidden

" shows line numbers
set number

" required for powerline fonts symbols
set encoding=utf-8

" search immediately
set incsearch

" highlight search
set hlsearch

" toggling
nnoremap <leader>h :set hlsearch!<cr>

" i want my backspace back!
set backspace=indent,eol,start

" use tabs instead of spaces
set tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab

set listchars=tab:¦\ ,trail:•,extends:»,precedes:«

" i want to see the whitespace, sometimes
nnoremap <leader>w :set list!<cr>

" centralize backups, swapfiles and undo history
set backupdir=~/.vim/backups
set directory=~/.vim/swaps
if exists("&undodir")
	set undodir=~/.vim/undo
endif

" remember more commands
set history=1000

" use more levels of undo
set undolevels=1000

" start scrolling the horizontal window border
set scrolloff=8

" opens new horizontal split to the bottom
set splitbelow

" opens new vertical split windows to the right
set splitright

" do not inc/dec octal numbers as it can lead to errors
set nrformats-=octal

" do not want to miss the changes
set autoread

" turns off physical line wrapping (ie: automatic insertion of newlines)
set nowrap
set textwidth=0 wrapmargin=0

set clipboard+=unnamed,unnamedplus

" visual autocomplete for commands
set wildmenu

" \\\ KEYMAPS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

nnoremap <left> :bprev<cr>
nnoremap <right> :bnext<cr>
nnoremap <up> :tabnext<cr>
nnoremap <down> :tabprev<cr>

" use ctrl-[hjkl] to select the active split
nnoremap <silent> <C-k> :wincmd k<cr>
nnoremap <silent> <C-j> :wincmd j<cr>
nnoremap <silent> <C-h> :wincmd h<cr>
nnoremap <silent> <C-l> :wincmd l<cr>

" for pasting in a lot of text
nnoremap <leader>p :set paste!<cr>

" too lazy to enter-leave insert mode, but i love inner and outer space :o
nnoremap <leader><space> a<space><esc>

" reselects a just-pasted text
nnoremap <leader>v V`]

" allows to save quickly
nnoremap <leader>s :w<cr>

" i want this things to center
nnoremap G Gzz
nnoremap n nzz
nnoremap N Nzz
nnoremap } }zz
nnoremap { {zz
nnoremap * *zz

nnoremap <leader>z zA

" \\\ GUI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

if has("gui_running")
	" hides menus, toolbars, widgets and use console dialog promts
	set guioptions=c
	if has("gui_win32") || has("gui_win64")
		set guifont=Fira_Mono_for_Powerline:h10:cRUSSIAN
	else
		set guifont=Fira\ Mono\ Medium\ for\ Powerline\ Medium\ 10
	endif
endif

" \\\ AUTOCMD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

if has("autocmd")
	augroup vimrc
		autocmd!

		" sources vimrc for short interactions ;)
		autocmd BufWritePost _vimrc,.vimrc,.gvimrc so $MYVIMRC
	augroup end
endif

" \\\ THEME \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

set background=dark
