set nocompatible
syntax enable
set encoding=utf-8
set showcmd ruler number numberwidth=6
set history=1000
set scrolloff=3
set wildignore+=*~,*.tar.*,*.tgz
set listchars=tab:▸\ ,eol:$

let mapleader = ","
nnoremap <C-s> :w<CR>
inoremap <C-s> <Esc>:w<CR>a
nnoremap <leader>l :set list!<CR>
nnoremap <silent> <F5> :call <SID>StripTrailingSpaces()<CR>
nnoremap <C-u> viwUw
inoremap <C-u> <Esc>viwUwa

call pathogen#infect()

"" Whitespace
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab
set backspace=indent,eol,start
set autoindent shiftround

"" Searching
set hlsearch incsearch ignorecase smartcase

"" Wildmenu
set wildmode=longest,list
set wildmenu

"" Buffers
set hidden

"" Windows
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

"" Mouse
set mouse=a

"" 256 Color
set t_Co=256
colorscheme xoria256

"" Invisible character colors
highlight NonText guifg=#4a4a59
highlight SpecialKey guifg=#4a4a59

if has("autocmd")
  filetype plugin indent on
  autocmd FileType python setlocal tabstop=4 softtabstop=4 shiftwidth=4
  autocmd BufNewFile,BufRead *.rss,*.atom setfiletype xml
  autocmd BufWritePre *.py,*.js,*.rb,*.lisp :call <SID>StripTrailingSpaces()
  autocmd InsertEnter * set cursorline
  autocmd InsertLeave * set nocursorline
endif

"" Javascript
set wildignore+=node_modules/*

"" Python
set wildignore+=dist/*,build/*,*.egg-info,*.egg

"" Functions
function! <SID>StripTrailingSpaces()
  " save last search, and cursor position
  let _s=@/
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  let @/=_s
  call cursor(l, c)
endfunction

command! -nargs=* Stab call Stab()
function! Stab()
  let l:tabstop = 1 * input('set ts = sts = sw = ')
  if l:tabstop > 0
    let &l:sts = l:tabstop
    let &l:ts = l:tabstop
    let &l:sw = l:tabstop
  endif
  set et
endfunction
