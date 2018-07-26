" matchit package, add this before the vundle mustache
set nocompatible
packadd! matchit

" start vundle
filetype off
if has("gui_running")
    set rtp+=$HOME/vimfiles/bundle/Vundle.vim/
else
    set rtp+=~/.vim/bundle/Vundle.vim
endif
call vundle#begin('$HOME/vimfiles/bundle/')
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'pangloss/vim-javascript'
Plugin 'octol/vim-cpp-enhanced-highlight'
Plugin 'DoxygenToolkit.vim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'rust-lang/rust.vim'
"Plugin 'valloric/youcompleteme'  <- can't get this to work
call vundle#end()
filetype plugin indent on
syntax on
" end vundle

"because of youcompleteme  <-- uncomment to use youcompleteme
"set encoding=utf-8

set nobackup
set noswapfile
set path=.,,**
set ffs=unix,dos
set hidden

set wrap
set ruler
set number
set numberwidth=5
set colorcolumn=81
set textwidth=80
set hls
set matchpairs+=<:>
set backspace=indent,eol,start
set autoindent
set cino=(1s,+0,g0
set tabstop=4
set shiftwidth=4
set expandtab
set incsearch
set ignorecase

set tags=./tags,tags;

if has("gui_running")
    set guifont=Consolas:h10:cANSI
    set guicursor+=a:blinkon0
    set guioptions-=m
    set guioptions-=T
    set belloff=all
    set lines=40
    set columns=85
else
    " fix mintty escape key delay
    let &t_ti.="\e[?7727h"
    let &t_te.="\e[?7727l"
    noremap <Esc>O[ <Esc>
    noremap! <Esc>O[ <C-c>
endif

nnoremap <F6> :buffers<CR>:buffer<Space>
nnoremap <Leader>] :bn<CR>
nnoremap <Leader>[ :bp<CR>

" copy visually selected text
vnoremap // y/\V<C-r>=escape(@",'/\')<CR><CR>

" disable netrw because it is buggy
let loaded_netrwPlugin=1
" let g:netrw_liststyle=3


" cmake copy_resources custom target for trout project (pwd = src)
nnoremap <F8> :!cmake --build ../build --target copy_resources<CR>
