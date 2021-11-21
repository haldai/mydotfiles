# If accessed by tramp, use the basic prompt
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
 
# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh/

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# ZSH_THEME="jonathan" "strug" "takashiyoshida" "sorin"
# ZSH_THEME="ys"

# Set list of themes to load
# Setting this variable when ZSH_THEME=random
# cause zsh load theme from this variable instead of
# looking in ~/.oh-my-zsh/themes/
# An empty array have no effect
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
  extract
  colorize
  archlinux
  emacs
  sudo
  thefuck
  dircycle
  colored-man-pages
)


# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias cp='cp -i'                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano -w PKGBUILD'
alias more=less
alias la='ls -a'
alias ll='ls -l'
alias ek="emacsclient -e '(client-save-kill-emacs)'"
alias b='PAGER="less -FR" bat'
alias as='amixer sset Speaker toggle'
alias ah='amixer sset Headphone toggle'
alias am='amixer sset Master toggle'
alias ac='amixer sset Capture toggle'
alias pip-upgrade="pip freeze --user | cut -d'=' -f1 | xargs -n1 pip install -U"
alias pip-upgrade-venv="pip freeze | cut -d'=' -f1 | xargs -n1 pip install -U"

# some aliases avoid make mistake
alias mv='mv -i'
alias rm='rm -i'


ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

source $ZSH/oh-my-zsh.sh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# better yaourt colors
export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"

# user environment
SCRIPTS_PATH="/home/daiwz/.scripts"
CUDAPATH="/opt/cuda"
SWIPLPATH="/home/daiwz/.local/lib/swipl"
CARGO_HOME="/home/daiwz/.cargo"
GEM_HOME="/home/daiwz/.gem/ruby/2.7.0"

# most
export PAGER="most"
export TERM=xterm-256color

# visual
export VISUAL="emacs -Q -nw"
export PATH="$SCRIPTS_PATH:$CARGO_HOME/bin:$CUDAPATH/bin:$HOME/.local/bin/:$GEM_HOME/bin:$PATH"
export LD_LIBRARY_PATH="$SWIPLPATH/lib/x86_64-linux:$CUDAPATH/include/:${HOME}/.local/include/:${HOME}/.local/lib:$HOME/.local/lib64/:$LD_LIBRARY_PATH"
export LD_RUN_PATH="$CUDAPATH/lib64/:$LD_RUN_PATH"
export PKG_CONFIG_PATH="$HOME/.local/share/pkgconfig:$HOME/.local/lib/pkgconfig/:$HOME/.local/lib64/pkgconfig/:$PKG_CONFIG_PATH"
export PYTHON_EGG_CACHE="/tmp/python-eggs/"
export OPENCV_LOG_LEVEL="ERROR"

# My alias
# alias emacs='emacs -nw'
alias pl='LD_PRELOAD=/usr/lib/libc.so.6 swipl'
alias jl='julia'
alias sc='scheme'
alias jlo='optirun julia'
alias matlab='LANG=en_US.utf-8 matlab'
alias f='fuck'

# optimus
PRIMUS_PREFIX="primusrun"
OPTIMUS_PREFIX="optirun"

# julia threading
export JULIA_NUM_THREADS=32

# spaceship promp
autoload -U promptinit; promptinit
prompt spaceship
SPACESHIP_PROMPT_ORDER=(
    # time        # Time stampts section (Disabled)
    user          # Username section
    host          # Hostname section
    dir           # Current directory section
    git           # Git section (git_branch + git_status)
    # hg            # Mercurial section (hg_branch  + hg_status)
    # package     # Package version (Disabled)
    # node          # Node.js section
    # ruby          # Ruby section
    # elixir        # Elixir sections
    # xcode       # Xcode section (Disabled)
    # swift         # Swift section
    # golang        # Go section
    # php           # PHP section
    # rust          # Rust section
    # haskell       # Haskell Stack section
    julia       # Julia section (Disabled)
    # docker      # Docker section (Disabled)
    # aws           # Amazon Web Services section
    venv          # virtualenv section
    # conda         # conda virtualenv section
    # pyenv         # Pyenv section
    # dotnet        # .NET section
    # ember       # Ember.js section (Disabled)
    # kubecontext   # Kubectl context section
    exec_time     # Execution time
    line_sep      # Line break
    battery       # Battery level and status
    vi_mode     # Vi-mode indicator (Disabled)
    jobs          # Background jobs indicator
    exit_code     # Exit code section
    char          # Prompt character
)
SPACESHIP_RPROMPT_ORDER=(
    time        # Time stampts section
)
SPACESHIP_TIME_SHOW=true
SPACESHIP_USER_SHOW=always
SPACESHIP_HOST_SHOW=always
SPACESHIP_DIR_TRUNC=0
SPACESHIP_USER_SUFFIX=
SPACESHIP_HOST_PREFIX=@
SPACESHIP_HOST_SUFFIX=
SPACESHIP_DIR_PREFIX=:
SPACESHIP_CHAR_SYMBOL=⊨\ 
SPACESHIP_JULIA_SYMBOL=ஃ\ 
