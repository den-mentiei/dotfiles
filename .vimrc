set nocompatible

filetype off

" \\\ EASY VIMRC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" opens vimrc
nmap <silent> <leader>ev :e $MYVIMRC<cr>

" sources vimrc for short interactions ;)
nmap <silent> <leader>sv :so $MYVIMRC<cr>

" \\\ ETC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

" syntax highlight by default
syntax on

" allows unsaved buffers
set hidden

" shows line numbers
set number

" \\\ GUI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

if has('gui_running')
  " hides menus, toolbars, widgets
  set guioptions=
  if has("gui_win32")
    set guifont=Consolas:h11:cANSI
  endif
endif
