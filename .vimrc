set nocompatible

filetype off

let mapleader = "\<Space>"

" \\\ EASY VIMRC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" opens vimrc
nnoremap <silent> <leader>ev :e $MYVIMRC<cr>

" \\\ DEIN \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

set runtimepath+=~/.vim/bundle/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.vim/bundle')
	call dein#begin('~/.vim/bundle')

	" let dein manage itself
	call dein#add('~/.vim/bundle/repos/github.com/Shougo/dein.vim')"

	call dein#add('Shougo/vimproc.vim', {'build' : 'make'})

	"" colorschemes
	call dein#add('morhetz/gruvbox')

	" git
	call dein#add('tpope/vim-fugitive')
	call dein#add('airblade/vim-gitgutter')

	"" interface & ux
	call dein#add('bling/vim-airline')
	"call dein#add('tpope/vim-surround'
	"call dein#add('tpope/vim-repeat'
	"call dein#add('tpope/vim-unimpaired'
	"call dein#add('Lokaltog/vim-easymotion'
	call dein#add('terryma/vim-multiple-cursors')
	call dein#add('ddollar/nerdcommenter')
	call dein#add('SirVer/ultisnips')
	"call dein#add('rking/ag.vim'
	call dein#add('qpkorr/vim-bufkill')
	""call dein#add('a.vim'
	"call dein#add('tmhedberg/matchit'
	"call dein#add('sjl/gundo.vim'
	"call dein#add('Shougo/neomru.vim'
	call dein#add('Yggdroot/indentLine')
	""call dein#add('scrooloose/syntastic'
	"" languages
	call dein#add('davidhalter/jedi-vim')
	"call dein#add('Valloric/YouCompleteMe')
	""" shaders
	call dein#add('beyondmarc/hlsl.vim')
	call dein#add('tikhomirov/vim-glsl')
	""" lispy
	"call dein#add('wlangstroth/vim-racket'
	"call dein#add('vim-scripts/vim-niji'
	"call dein#add('vim-scripts/paredit.vim'
	""" ruby
	"call dein#add('vim-ruby/vim-ruby'
	""" web
	"NeoBundleLazy 'othree/html5.vim', {'autoload': {'filetypes': ['html']}}
	"NeoBundleLazy 'ap/vim-css-color', {'autoload': {'filetypes': ['css', 'scss', 'sass', 'less', 'styl']}}
	"NeoBundleLazy 'jelera/vim-javascript-syntax', {'autoload': {'filetypes': ['javascript']}}
	"NeoBundleLazy 'pangloss/vim-javascript', {'autoload': {'filetypes': ['javascript']}}
	"call dein#add('mxw/vim-jsx'
	"NeoBundleLazy 'othree/javascript-libraries-syntax.vim', {'autoload': {'filetypes': ['javascript']}}
	"NeoBundleLazy 'digitaltoad/vim-pug', {'autoload': {'filetypes': ['pug']}}

	" TODO: Make it lazy and only loaded for a specific file type.
	call dein#add('digitaltoad/vim-pug')

	call dein#add('lambdatoast/elm.vim')

	call dein#end()
	call dein#save_state()
endif

filetype plugin indent on

syntax enable

" installs not installed plugins on startup
if dein#check_install()
	call dein#install()
endif

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

" highlight current line - currently disabled as it makes terminal vim deadly slow
"set cursorline

" use tabs instead of spaces
set tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab

set listchars=tab:¦\ ,trail:•,extends:»,precedes:«

" i want to see the whitespace, sometimes
" TODO: Use :IndentLinesToggle to toggle plugin space indentation too.
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

nnoremap <F5> :silent make <bar> copen<cr>
nnoremap <F6> :make<cr>

nnoremap <leader>m :marks<cr>

nnoremap <leader>z zA

" Fugitive bindings
" TODO: Revise and uncomment this.
"nnoremap <leader>gs :Gstatus<CR>
"nnoremap <leader>gd :Gdiff<CR>
"nnoremap <leader>gc :Gcommit -v -q<CR>
"nnoremap <leader>gC :Gcommit -v -q --all<CR>
"nnoremap <leader>gr :Gread<CR>
"nnoremap <leader>gw :Gwrite<CR>
"nnoremap <leader>gb :Gblame<CR>

" \\\ GUI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

if has("gui_running")
	" hides menus, toolbars, widgets and use console dialog promts
	set guioptions=c
	if has("gui_win32") || has("gui_win64")
		set guifont=Fira_Mono_for_Powerline:h10:cRUSSIAN
	else
		set guifont=Fira\ Mono\ Medium\ for\ Powerline\ Medium\ 10
	endif
else
	if $COLORTERM == 'gnome-terminal'
		" gnome-terminal doesn't tell the correct number of colors
		set t_Co=256
	endif
endif

" \\\ GITGUTTER \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:gitgutter_max_signs = 500
let g:gitgutter_map_keys = 0

" \\\ AIRLINE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:airline_enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = 'gruvbox'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#branch#enabled = 1

" to be always shown
set laststatus=2

" dont duplicate airline info
if (g:airline_enabled == 1)
	set noshowmode
endif

" \\\ EASYMOTION \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" disable default mappings
"let g:EasyMotion_do_mapping

map <leader>e <Plug>(easymotion-prefix)

map <leader>j <Plug>(easymotion-j)
map <leader>k <Plug>(easymotion-k)

" \\\ ULTISNIPS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:UltiSnipsExpandTrigger ="<tab>"
let g:UltiSnipsListSnippets = "<C-tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<S-tab>"
let g:UltiSnipsSnippetDirectories = ["my_snippets"]

" \\\ GUNDO \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

nnoremap <leader>u :GundoToggle<cr>

" \\\ NIJI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:niji_matching_filetypes = ['lisp', 'scheme', 'racket']

" \\\ PAREDIT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

map <leader>) :call PareditToggle()<cr>

" \\\ JAVASCRIPT LIBRARIES \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:used_javascript_libs = 'underscore,react,requirejs,jquery'

" \\\ JSX \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:jsx_ext_required = 0

" \\\ MERLIN \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" custom key mappings for the ocaml files
function! s:merlin_mappings()
	nnoremap <buffer> <leader>t :MerlinTypeOf<cr>
	vnoremap <buffer> <leader>t :MerlinTypeOfSel<cr>

	nnoremap <buffer> <leader>gd :MerlinLocate<cr>
	nnoremap <buffer> <cr> :MerlinClearEnclosing<cr>

	nnoremap <buffer> <leader>c :MerlinErrorCheck<cr>
endfunction

" \\\ OMNISHARP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:OmniSharp_timeout = 1

" custom key mappings for the csharp files
function! s:csharp_mappings()
	nnoremap gd :OmniSharpGotoDefinition<cr>

	nnoremap <leader>fi :OmniSharpFindImplementations<cr>
endfunction

" \\\ YCM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

function! s:ycm_mappings()
	nnoremap <leader>g :YcmCompleter GoTo<cr>
endfunction

" \\\ INDENTLINES \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" TODO: Toggle if list is set.
let g:indentLine_enabled = 1

" \\\ BUFKILL \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

nnoremap <silent> <leader>d :BD<cr>

" \\\ SYNTASTIC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:syntastic_yaml_checkers=['yamllint']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" \\\ AUTOCMD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

if has("autocmd")
	augroup shaders
		autocmd!

		autocmd BufRead,BufNewFile *.fx,*.fxc,*.fxh,*.hlsl setfiletype hlsl
		autocmd BufRead,BufNewFile *.frag,*.vert,*.fp,*.vp,*.glsl,*.sc setfiletype glsl
	augroup end

	augroup scheme
		autocmd!

		autocmd BufRead,BufNewFile *.emb setfiletype scheme
		" i love tabs -_-
		autocmd FileType scheme setlocal tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab
		autocmd FileType racket setlocal tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab
	augroup end

	augroup filetypes
		autocmd!

		autocmd BufRead,BufNewFile *.md setfiletype markdown
	augroup end

	augroup plugins
		autocmd!

		autocmd BufReadPre,BufNewFile *.ml4,*.ml,*.mli let b:did_ftplugin = 1
	augroup end

	augroup vimrc
		autocmd!

		" sources vimrc for short interactions ;)
		autocmd BufWritePost _vimrc,.vimrc,.gvimrc so $MYVIMRC
	augroup end

	augroup ocaml
		autocmd!

		autocmd FileType ocaml call s:merlin_mappings()
	augroup end

	augroup csharp
		autocmd!

		autocmd FileType cs call s:csharp_mappings()

		autocmd FileType cs setlocal omnifunc=OmniSharp#Complete

		" automatically add new files to the neares project on save
		autocmd BufWritePost *.cs call OmniSharp#AddToProject()
		autocmd BufWritePost *.xaml call OmniSharp#AddToProject()
	augroup end

	augroup yaml
		autocmd!

		autocmd FileType yaml setlocal tabstop=2 softtabstop=2 shiftwidth=2 expandtab
	augroup end

	augroup python
		autocmd!

		"autocmd FileType python call s:ycm_mappings()
		" use tabs instead of spaces
		autocmd FileType python setlocal tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab
	augroup end
endif

" \\\ THEME \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

set background=dark

let g:gruvbox_termcolors = 256
let g:gruvbox_sign_column = "dark0"
colorscheme gruvbox

" for some secret non-committable stuff
if filereadable("~/.vimrc_local")
	source ~/.vimrc_local
endif
