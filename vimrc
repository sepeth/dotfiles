" Prelude {{{
set nocompatible
if filereadable('/usr/local/bin/bash')
  set shell=/usr/local/bin/bash
else
  set shell=/bin/bash
end
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
" }}}

" Plugins {{{
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-fugitive'
Plugin 'godlygeek/tabular'
Plugin 'sjl/gundo.vim'
Plugin 'tomasr/molokai'
Plugin 'kien/ctrlp.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'ervandew/supertab'
Plugin 'mattn/emmet-vim'
call vundle#end()
" }}}

" Basic Settings {{{
filetype plugin indent on
runtime macros/matchit.vim
syntax enable
set encoding=utf-8
set showcmd ruler number relativenumber numberwidth=6
set history=1000
set scrolloff=3
set listchars=tab:▸\ ,eol:$
set display=lastline
set cursorline
" }}}

" Status Line {{{
set laststatus=2
set statusline=%n\       " Buffer number
set statusline+=%f\      " Path to the file
set statusline+=%m\      " Modified flag 
set statusline+=%r\      " Read-Only flag
set statusline+=%=       " Switch to the right side
set statusline+=%l:%c\   " Current col and line
set statusline+=[%L]\    " Total lines
set statusline+=%B\      " Hex-value of a current char
set statusline+=%y       " File type
" }}}

" Keymappings {{{
let mapleader = ","
let maplocalleader = '\'
cnoremap %% <C-R>=expand('%:p:h').'/'<CR>
 noremap <leader>ae :Tabularize /=<CR>
 noremap <leader>ac :Tabularize /:<CR>
nnoremap <leader>b  :CtrlPBuffer<CR>
nnoremap <leader>c  :set cursorcolumn!<CR>
nnoremap <leader>dd :CtrlPBookmarkDir<CR>
nnoremap <leader>da :CtrlPBookmarkDirAdd<CR>
nnoremap <leader>dm :CtrlPMixed<CR>
    nmap <leader>ew :e %%
    nmap <leader>es :sp %%
    nmap <leader>ev :vsp %%
"   nmap <leader>et :tabe %%
nnoremap <leader>f :set fullscreen!<CR>
nnoremap <leader>h :nohlsearch<CR>
nnoremap <leader>l :set list!<CR>
nnoremap <leader>o :only<CR>
nnoremap <leader>r :CtrlPBufTagAll<CR>
nnoremap <leader>s :call <SID>StripTrailingSpaces()<CR>
nnoremap <leader>v :split $MYVIMRC<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>y :Ag<CR>
nnoremap <leader>z :match Error /\s\+$/<CR>
nnoremap <leader>Z :match Error //<CR>
nnoremap <leader>/ /\v
nnoremap <leader><leader> <C-^>
let g:ctrlp_map = '<leader>t'
nnoremap <F5> :GundoToggle<CR>

" Bubble single lines
nmap <C-up> [e
nmap <C-down> ]e
" Bubble multiple lines
vmap <C-up> [egv
vmap <C-down> ]egv

" Visually select last edited/pasted text
nnoremap gV `[v`]

" make it harder to do bad habits
inoremap jk <esc>
inoremap <esc> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
"inoremap <C-c> <nop>
"inoremap <BS> <nop>

" Window navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" start new undo seq before some kill keys
inoremap <C-u> <C-g>u<C-u>
inoremap <C-w> <C-g>u<C-w>

" move inside visible line
noremap <D-j> gj
noremap <D-k> gk
noremap <D-$> g$
noremap <D-7> g0
" }}}

" Abbrevs {{{
iabbrev <// </<C-X><C-O>
" }}}

" Whitespace & Searching {{{
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab
set backspace=indent,eol,start
set autoindent shiftround
set hlsearch incsearch ignorecase smartcase
" }}}

" User Interface {{{
set wildmode=longest,list
set wildmenu
set hidden                                        " Permit hidden buffers
set t_Co=256                                       " 16 colors
set background=dark
colorscheme molokai

" GUI specific options
set visualbell guioptions-=T guioptions-=L guioptions-=r
set guifont=Monaco:h12
set linespace=1

if has('mouse')
  set mouse=a
endif
" }}}

" Wildignore {{{
set wildignore+=*~,*.sw?
set wildignore+=*.tar.*,*.tgz
set wildignore+=.DS_Store
set wildignore+=node_modules/*,*.min.js               " Javascript
set wildignore+=*.pyc,dist/*,build/*,*.egg-info,*.egg " Python
" }}}

" Functions {{{
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
  setlocal expandtab
endfunction
" }}}

" General programming autocmds {{{
augroup programming_au
  autocmd!
  autocmd FileType ruby,vim,jade,stylus,javascript,html setl ts=2 sts=2 sw=2
  autocmd FileType html setl nowrap
  autocmd FileType snippet,snippets setlocal noexpandtab
  autocmd BufEnter *.rss,*.atom,*.odrl setf xml
  autocmd BufEnter *.md setf markdown
  autocmd BufEnter *.arc setf arc
  autocmd BufEnter *.go setl noet ts=4 sts=4 sw=4
  autocmd BufEnter *.sml setl et ts=4 sts=4 sw=4 commentstring=\(*\ %s\ *\)
  autocmd BufEnter volofile setf javascript
  autocmd BufWritePre *.py,*.js,*.rb,*.lisp,*.css :call <SID>StripTrailingSpaces()
  " Jump to last cursor position
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
augroup END
" }}}

" Python autocmds {{{
augroup python_au
  autocmd!
  autocmd FileType python nnoremap <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>
augroup END
" }}}

" Vimscript file settings {{{
augroup filetype_vim
  autocmd!
  autocmd FileType vim setl foldmethod=marker
  autocmd BufWritePost .vimrc source $MYVIMRC
augroup END
" }}}

augroup paste
  autocmd!
  autocmd InsertLeave * set nopaste
augroup END

" Netrw {{{
let g:netrw_banner = 0
let g:netrw_keepdir = 0
let g:netrw_liststyle = 3
let g:netrw_sort_options = 'i'
" }}}

" CtrlP {{{
let g:ctrlp_follow_symlinks = 1
let g:ctrlp_prompt_mappings = {
  \ 'PrtSelectMove("j")':   ['<c-n>', '<down>'],
  \ 'PrtSelectMove("k")':   ['<c-p>', '<up>'],
  \ 'PrtHistory(-1)':       ['<c-k>'],
  \ 'PrtHistory(1)':        ['<c-j>'],
  \ }

let g:ctrlp_user_command = {
  \ 'types': {
    \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
    \ 2: ['.hg', 'hg --cwd %s locate -I .'],
    \ },
  \ 'fallback': 'find %s -type f'
  \ }

" }}}

" jedi {{{
let g:jedi#use_tabs_not_buffers = 0
" }}}

" emmet {{{
"let g:user_emmet_leader_key='<C-Z>'
" }}}

" syntastic {{{
let g:syntastic_python_checkers=['pylama', 'pylint', 'flake8']
let g:syntastic_javascript_checkers=['jsxhint', 'jshint']
let g:syntastic_cpp_checkers=['gcc']
let g:syntastic_cpp_config_file='.syntastic_config'
" }}}

" YouCompleteMe, UltiSnips, Supertab {{{
let g:ycm_key_list_select_completion = ['<C-TAB>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-S-TAB>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-Tab>'
" }}}
