set nocompatible
syntax enable
set encoding=utf-8
set showcmd
set number

"" Whitespace
set tabstop=2 shiftwidth=2
set expandtab
set backspace=indent,eol,start
filetype plugin indent on

"" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

"" CommandT
let mapleader = ","
nnoremap <silent> <leader>t :CommandT<CR>

"" Python
autocmd FileType python setlocal tabstop=4 shiftwidth=4
