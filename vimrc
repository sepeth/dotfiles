set nocompatible
syntax enable
set encoding=utf-8
set showcmd number ruler
set wildignore+=*~,*.tar.*,*.tgz
set listchars=tab:▸\ ,eol:$

let mapleader = ","
nmap <c-s> :w<CR>
imap <c-s> <Esc>:w<CR>a
nmap <leader>l :set list!<CR>
nnoremap <silent> <F5> :call <SID>StripTrailingSpaces()<CR>

call pathogen#infect()

"" Whitespace
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab
set backspace=indent,eol,start
set autoindent

"" Searching
set hlsearch incsearch ignorecase smartcase

"" Wildmenu
set wildmode=longest:full
set wildmenu

"" Buffers
set hidden

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
