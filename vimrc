" Fix broken things
set nocompatible " Make vim more useful 
set backspace=indent,eol,start " Make backspace work properly
" Why is Y the same as yy and not like C and D by default?
noremap Y y$
set encoding=utf-8 nobomb " BOM often causes trouble
set fileformats=unix,dos,mac
set nojoinspaces " Only insert single space after a '.', '?' and '!' with a join command
set magic " Enable extended regexes
set lazyredraw " Don't redraw when we don't have to
set noerrorbells " Disable error bells
set visualbell " Use visual bell instead of audible bell (annnnnoying)
set ttyfast " Send more characters at a given time
set report=0 " Show all changes
set noexrc " don't read local .vimrc-like files, just in case

" LOAD THE PLUGINS
execute pathogen#infect()
filetype plugin indent on

" 'UI' and cursor setting
set shortmess=atI " Don't show the intro message when starting vim
set title " Show the filename in the window titlebar
set showtabline=2 " Always show tab bar
set laststatus=2 " Always show status line
set showcmd " show what you're doing
set ruler " Show the cursor position
set matchtime=5 " Show matching paren for 0.5 seconds

" Colors are purrdy, use 256 of them
set t_Co=256
set background=dark
syntax on " Well, duh
colorscheme solarized

" Line number and scrolling stuff
set number " Enable line numbers
set relativenumber " Relative line numbers 
set numberwidth=5 " For realllllly long files
set sidescrolloff=3 " Start scrolling three columns before vertical border of window
set scrolloff=3 " Start scrolling three lines before horizontal border of window
set nostartofline " Don't reset cursor to start of line when moving around
set linespace=0 " Adjust if font is wacky

" Split stuff
set splitbelow " New window goes below
set splitright " New windows goes right
set winminheight=0 " Allow splits to be reduced to a single line

" Make vim keep its own house in order, make history better and allow persistant undo
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp
set undodir=~/.vim/undo
set hidden " When a buffer is brought to foreground, remember undo history and marks
set history=1000 " Increase history from 20 default to 1000
set undofile " Persistent Undo
set viminfo=%,'9999,s512,n~/.vim/viminfo " Restore buffer list, marks are remembered for 9999 files, registers up to 512Kb are remembered

" Tabs should be small and actually 2 spaces by default, imho.
set shiftwidth=2 " The # of spaces for indenting
set softtabstop=2 " Tab key results in 2 spaces
set expandtab " Expand tabs to spaces
set autoindent " Copy indent from last line when starting new line
set smarttab " At start of line, <Tab> inserts shiftwidth spaces, <Bs> deletes shiftwidth spaces
set tabstop=2

" Search is helpful, make it moreso 
set incsearch " Highlight dynamically as pattern is typed
set ignorecase " Ignore case of searches
set smartcase " Ignore 'ignorecase' if search patter contains uppercase characters
set wrapscan " Searches wrap around end of file
" Ignore these filetypes when searching
set suffixes=.bak,~,.swp,.swo,.o,.d,.info,.aux,.log,.dvi,.pdf,.bin,.bbl,.blg,.brf,.cb,.dmg,.exe,.ind,.idx,.ilg,.inx,.out,.toc,.pyc,.pyd,.dll
" set gdefault " By default add g flag to search/replace. Add g to toggle

" 'Wild' tab-completion when opening files and doing other things
set wildchar=<TAB> " Character for CLI expansion (TAB-completion)
set wildignore+=.DS_Store " Ignore files we don't care about
set wildignore+=*.jpg,*.jpeg,*.png,*.gif,*.psd,*.o,*.obj,*.min.js,*.pyc,*.beam
set wildignore+=*/bower_components/*,*/node_modules/*
set wildignore+=*/smarty/*,*/vendor/*,*/.git/*,*/.hg/*,*/.svn/*,*/.sass-cache/*,*/log/*,*/tmp/*,*/build/*,*/ckeditor/*,*/doc/*,*/source_maps/*,*/dist/*
set wildmenu " Hitting TAB in command mode will show possible completions above command line
set wildmode=list:longest " Complete only until point of ambiguity

" Fold settings, for when I remember how to use folds effectively
set foldmethod=marker
set foldmarker=/*,*/
set foldcolumn=0 " Column to show folds
set foldenable " Enable folding
set foldlevel=100
set foldmethod=syntax " Syntax are used to specify folds
set foldminlines=0 " Allow folding single lines
set foldnestmax=5 " Set max fold nesting level

" Vimdiff looks better this way
set diffopt=filler " Add vertical spaces to keep right and left aligned
set diffopt+=iwhite " Ignore whitespace changes (focus on code changes)

" Format options stuff, not sure what it's all doing.
set formatoptions=
set formatoptions+=c " Format comments
set formatoptions+=r " Continue comments by default
set formatoptions+=o " Make comment when using o or O from comment line
set formatoptions+=q " Format comments with gq
set formatoptions+=n " Recognize numbered lists
set formatoptions+=2 " Use indent from 2nd line of a paragraph
set formatoptions+=l " Don't break lines that are already long
set formatoptions+=1 " Break before 1-letter words

" set mouse=a " Enable mouse in all in all modes

" Add some helpful mappings
" Mapleader
let mapleader="\<Space>"
" remap j/k to travers virtual lines while ignoring virtual lines when used in multiline operations (ie: 10j)
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
" jk is faster than escape
imap jk <Esc>
imap jj <Esc>
" Make w!! force save with sudo for when you forgot to sudo vim
cmap w!! %!sudo tee > /dev/null %
" not sure about these ones, maybe if leader wasn't , they've be easier to type
map <Leader>f :bn<cr>
map <Leader>b :bp<cr>
" map <Leader>d :bd<cr>
" preface y d and p with leader to use system clipboard
" y isn't working at the moment (it just goes forward a char)
" vmap <Leader>y "*y
" vmap <Leader>Y "*Y
vmap <Leader>d "*d
vmap <Leader>D "*D
nmap <Leader>p "*p
nmap <Leader>P "*P
vmap <Leader>p "*p
vmap <Leader>P "*P


" Filetype specific stuff, mostly tabs vs spaces holy war related
autocmd FileType html setlocal shiftwidth=2 tabstop=2
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2
autocmd FileType jsx setlocal shiftwidth=2 tabstop=2
autocmd FileType css setlocal shiftwidth=2 tabstop=2
autocmd FileType scss setlocal shiftwidth=2 tabstop=2
autocmd FileType python setlocal shiftwidth=4 tabstop=4
autocmd FileType cs setlocal shiftwidth=2 tabstop=2 noexpandtab
let javascript_enable_domhtmlcss=1 " for react, i think (might be for a plugin)

" Make MacVim use a pretty font
:set guifont=Monaco:h14

" PLUGIN STUFF

" NERDTree is useful. Start it automatically and map C^n and F2 to open/close
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
map <F2> :NERDTreeToggle<CR>
map <C-n> :NERDTreeToggle<CR>

" Rainbow parens. They're cool, except in .html files (TODO find fix)
let g:rainbow_active = 1

" syntastic stuff. I give up, the syntax checkers win.
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_checkers = ['flake8']
let g:syntastic_javascript_checkers = ['eslint']
" let g:syntastic_auto_loc_list = 1

" Ack stuff
cnoreabbrev Ack Ack!
nnoremap <Leader>a :Ack!<Space>

" ROVER STUFFS
let g:syntastic_python_flake8_args = '--ignore=E123,E126,E127,E128,W503 --max-line-length 120'
let g:pymode_rope = 1
let g:pymode_lint = 0 " let syntastic deal with it instead
let g:pymode_virtualenv_path = '/Users/cassidydowning-bryant/.virtualenvs/roverweb'
let g:pymode_options_max_line_length = 120
let g:pymode_lint_options_pep8 = {'max_line_length': g:pymode_options_max_line_length}
" Skip some PEP8 errors that we don't adhere to at Rover
let g:pymode_lint_ignore = 'E123,E126,E127,E128'

" Rebind go to definition command
let g:pymode_rope_goto_definition_bind = '<C-]>'
