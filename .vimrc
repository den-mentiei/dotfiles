set nocompatible

filetype off

let mapleader = "\<Space>"

" \\\ EASY VIMRC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" opens vimrc
nnoremap <silent> <leader>ev :e $MYVIMRC<cr>

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
" git
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'airblade/vim-gitgutter'
" interface & ux
NeoBundle 'bling/vim-airline'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'Lokaltog/vim-easymotion'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'ddollar/nerdcommenter'
NeoBundle 'SirVer/ultisnips'
NeoBundle 'rking/ag.vim'
NeoBundle 'qpkorr/vim-bufkill'
"NeoBundle 'a.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'tmhedberg/matchit'
NeoBundle 'sjl/gundo.vim'
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'vim-scripts/vim-sjson'
NeoBundle 'Yggdroot/indentLine'
" colorschemes
NeoBundle 'morhetz/gruvbox'
" languages
"" shaders
NeoBundle 'beyondmarc/hlsl.vim'
NeoBundle 'tikhomirov/vim-glsl'
"" lispy
NeoBundle 'wlangstroth/vim-racket'
NeoBundle 'vim-scripts/vim-niji'
NeoBundle 'vim-scripts/paredit.vim'
"" ruby
NeoBundle 'vim-ruby/vim-ruby'
"" web
NeoBundleLazy 'othree/html5.vim', {'autoload': {'filetypes': ['html']}}
NeoBundleLazy 'ap/vim-css-color', {'autoload': {'filetypes': ['css', 'scss', 'sass', 'less', 'styl']}}
NeoBundleLazy 'jelera/vim-javascript-syntax', {'autoload': {'filetypes': ['javascript']}}
NeoBundleLazy 'pangloss/vim-javascript', {'autoload': {'filetypes': ['javascript']}}
NeoBundle 'mxw/vim-jsx'
NeoBundleLazy 'othree/javascript-libraries-syntax.vim', {'autoload': {'filetypes': ['javascript']}}
NeoBundleLazy 'digitaltoad/vim-jade', {'autoload': {'filetypes': ['jade']}}
"" c#
NeoBundleLazy 'OmniSharp/omnisharp-vim', {'autoload': {'filetypes': ['cs']}}

if executable('opam')
	" remove the default ftplugin
	let g:opamshare = substitute(system('opam config var share'),'\n$','','''')

	if !empty(glob(g:opamshare."/merlin/vim"))
		NeoBundleLazy g:opamshare . "/merlin/vim",
			\ {
				\ 'autoload': {'filetypes': ['ocaml']},
				\ 'disabled' : (!executable('ocamlmerlin'))
			\ }
	endif
endif

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

nnoremap <leader>m :marks<cr>

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

" \\\ UNITE \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:unite_source_grep_max_candidates = 1000
let g:unite_winheight = 30

" setups a line source alias
let g:unite_source_alias_aliases = {}
let g:unite_source_alias_aliases.line_fuzzy = 'line'
" with a custom matcher for it
call unite#custom#source('line_fuzzy', 'matchers', 'matcher_fuzzy')

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#custom#source('line,line_fuzzy,buffer,file,file_rec', 'sorters', 'sorter_rank')

" use ag for search
if executable('ag')
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '--nogroup --nocolor --column'
  let g:unite_source_grep_recursive_opt = ''
  let g:unite_source_rec_async_command = ['ag', '--follow', '--nocolor', '--nogroup', '--hidden', '-g', '']
endif

nnoremap <C-p> :Unite -start-insert -no-split buffer file_rec/async<cr>
nnoremap <leader>r :Unite -start-insert -no-split buffer file_mru<cr>
nnoremap <leader>l :Unite -start-insert -no-split line_fuzzy<cr>
"nnoremap <silent> <leader>g :Unite -buffer-name=search -auto-preview -no-quit -no-empty grep:.::<cr>

nnoremap <leader>n :Unite -vertical -winwidth=40 outline<cr>

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

map <leader>e <Plug>(easymotion-prefix)

map <leader>j <Plug>(easymotion-j)
map <leader>k <Plug>(easymotion-k)

" \\\ ULTISNIPS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

let g:UltiSnipsExpandTrigger ="<tab>"
let g:UltiSnipsListSnippets = "<C-tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<S-tab>"
let g:UltiSnipsSnippetDirectories = ["my_snippets"]

" \\\ A \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" switch to the alternate file
nnoremap <leader>o :A<cr>
" opens alternate file in a split
nnoremap <leader>vo :AV<cr>

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

" \\\ INDENTLINES \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" TODO: Toggle if list is set.
let g:indentLine_enabled = 1

" \\\ BUFKILL \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

nnoremap <silent> <leader>d :BD<cr>

" \\\ AUTOCMD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

if has("autocmd")
	augroup unite
		autocmd!

		autocmd FileType unite call s:unite_settings()
	augroup end

	augroup sjson
		autocmd!

		autocmd BufRead,BufNewFile *.render_config setfiletype sjson
		autocmd BufRead,BufNewFile *.shader_node setfiletype sjson
		autocmd BufRead,BufNewFile *.shader_source setfiletype sjson
		autocmd BufRead,BufNewFile *.shading_environment_template setfiletype sjson
		autocmd BufRead,BufNewFile *.shading_environment setfiletype sjson
		autocmd BufRead,BufNewFile *.material setfiletype sjson
		autocmd BufRead,BufNewFile *.texture setfiletype sjson
		autocmd BufRead,BufNewFile *.unit setfiletype sjson
		autocmd BufRead,BufNewFile *.strings setfiletype sjson
		autocmd BufRead,BufNewFile *.script_flow_nodes setfiletype sjson
		autocmd BufRead,BufNewFile *.package setfiletype sjson
	augroup end

	augroup shaders
		autocmd!

		autocmd BufRead,BufNewFile *.fx,*.fxc,*.fxh,*.hlsl setfiletype hlsl
		autocmd BufRead,BufNewFile *.frag,*.vert,*.fp,*.vp,*.glsl,*.sc setfiletype glsl
	augroup end

	augroup scheme
		autocmd!

		autocmd BufRead,BufNewFile *.emb setfiletype scheme
		" i love tabs -_-
		autocmd FileType scheme set tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab
		autocmd FileType racket set tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab
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
