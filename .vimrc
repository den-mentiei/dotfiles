set nocompatible

filetype off

" \\\ EASY VIMRC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" opens vimrc
nmap <silent> <leader>ev :e $MYVIMRC<cr>

" sources vimrc for short interactions ;)
nmap <silent> <leader>sv :so $MYVIMRC<cr>

" \\\ NEOBUNDLE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

set runtimepath+=~/.vim/bundle/neobundle.vim/

" Required:
call neobundle#begin(expand('~/.vim/bundle/'))

" let NeoBundle manage itself
NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make -f make_mac.mak',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'bling/vim-airline'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'Shougo/unite.vim'

call neobundle#end()

filetype plugin indent on

NeoBundleCheck

" \\\ ETC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

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

" TODO: toggle hlsearch on hotkey

" http://www.viemu.com/blog/2009/06/16/a-vim-and-viemu-mapping-you-really-cant-miss-never-type-noh-again/
nnoremap <esc> :noh<return><esc>

" i want my backspace back!
set backspace=indent,eol,start

" highlight current line
set cursorline

" i want to see the whitespace, sometimes
nmap <leader>l :set list!<cr>
set listchars=tab:›—,trail:•,extends:»,precedes:«

" centralize backups, swapfiles and undo history
set backupdir=~/.vim/backups
set directory=~/.vim/swaps
if exists("&undodir")
	set undodir=~/.vim/undo
endif

" start scrolling the horizontal window border
set scrolloff=5

" \\\ GUI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

if has("gui_running")
  " hides menus, toolbars, widgets
  set guioptions=
  if has("gui_win32")
    set guifont=Ubuntu_mono_derivative_Powerlin:h10:cRUSSIAN
    " start maximized on windows
    au GUIEnter * simalt ~x
  endif
endif

" \\\ THEME \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

set background=light
let g:solarized_termcolors = 256
colorscheme solarized

" switch between light/dark with <F5>. TODO: remap to other button, maybe.
call togglebg#map("")

" \\\ GITGUTTER \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:gitgutter_max_signs = 500
let g:gitgutter_map_keys = 0

" \\\ ARILINE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:airline_enabled = 1
let g:airline_enable_fugitive = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme = 'solarized'

" to be always shown
set laststatus=2

" \\\ UNITE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

nnoremap <C-p> :Unite file_rec/async<cr>

