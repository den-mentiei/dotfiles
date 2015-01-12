set nocompatible

filetype off

let mapleader = ","

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
\	'build' : {
\		'cygwin' : 'make -f make_cygwin.mak',
\		'mac' : 'make -f make_mac.mak',
\		'linux' : 'make',
\		'unix' : 'gmake',
\	},
\}
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'bling/vim-airline'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'Lokaltog/vim-easymotion'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'vim-scripts/vim-sjson'

call neobundle#end()

" auto-detect file types
filetype plugin indent on

NeoBundleCheck

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

" highlight current line
set cursorline

" use tabs instead of spaces
set tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab

" i want to see the whitespace
set list
set listchars=tab:›\ ,trail:•,extends:»,precedes:«

" i dont want to see the whitespace, sometimes
nmap <leader>l :set list!<cr>

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

" \\\ KEYMAPS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

nnoremap <left> :bprev<cr>
nnoremap <right> :bnext<cr>
nnoremap <up> :tabnext<cr>
nnoremap <down> :tabprev<cr>

" use ctrl-[hjkl] to select the active split
nmap <silent> <C-k> :wincmd k<cr>
nmap <silent> <C-j> :wincmd j<cr>
nmap <silent> <C-h> :wincmd h<cr>
nmap <silent> <C-l> :wincmd l<cr>

" TODO: make it a function that can handle different states correctly or
" replace with some already-done plugin.
" closes a buffer without breaking a split, if more than one left
nnoremap <leader>d :bp<bar>bd #<cr><cr>

" for pasting in a lot of text
nnoremap <leader>p :set paste!<cr>

" \\\ GUI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

if has("gui_running")
	" hides menus, toolbars, widgets and use console dialog promts
	set guioptions=c
	if has("gui_win32") || has("gui_win64")
		set guifont=Ubuntu_mono_derivative_Powerlin:h10:cRUSSIAN
		" start maximized on windows
		au GUIEnter * simalt ~x
	endif
else
	if $COLORTERM == 'gnome-terminal'
		" gnome-terminal doesn't tell the correct number of colors
		set t_Co=256
	endif
endif

" \\\ THEME \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

if &t_Co >= 256 || has("gui_running")
	let g:solarized_termcolors = 256
endif

" temporary - as dark theme is broken in gnome terminal
" (https://github.com/altercation/vim-colors-solarized/issues/72#issuecomment-66922017)
let g:solarized_termcolors = 16
set background=dark
colorscheme solarized

" solarized bg of special chars is not ok
highlight clear SpecialKey

" \\\ GITGUTTER \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:gitgutter_max_signs = 500
let g:gitgutter_map_keys = 0

" \\\ AIRLINE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:airline_enabled = 1
let g:airline_enable_fugitive = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme = 'solarized'

" to be always shown
set laststatus=2

" dont duplicate airline info
if (g:airline_enabled == 1)
	set noshowmode
endif

" \\\ UNITE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" setups a line source alias
let g:unite_source_alias_aliases = {}
let g:unite_source_alias_aliases.line_fuzzy = 'line'
" with a custom matcher for it
call unite#custom#source('line_fuzzy', 'matchers', 'matcher_fuzzy')

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#custom#source('line,line_fuzzy,buffer,file,file_rec', 'sorters', 'sorter_rank')

nnoremap <C-p> :Unite -start-insert -no-split -auto-preview buffer file_rec/async<cr>
nnoremap <leader>l :Unite -start-insert -no-split line_fuzzy<cr>

" custom mapping for unite buffers
function! s:unite_settings()
	" close unite buffer
	imap <buffer> jj <Plug>(unite_exit)
	" enable navigation with C-j and C-k in insert mode
	imap <buffer> <C-j> <Plug>(unite_select_next_line)
	imap <buffer> <C-k> <Plug>(unite_select_previous_line)
endfunction

" \\\ EASYMOTION \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" disable default mappings
"let g:EasyMotion_do_mapping

map <Leader>e <Plug>(easymotion-prefix)

map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

" \\\ AUTOCMD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

if has("autocmd")
	autocmd FileType unite call s:unite_settings()

	autocmd BufRead,BufNewFile *.render_config setfiletype sjson
	autocmd BufRead,BufNewFile *.shader_node setfiletype sjson
endif
