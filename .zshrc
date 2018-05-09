# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh/

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# ZSH_THEME="jonathan" "strug" "takashiyoshida" "sorin"
ZSH_THEME="ys"

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

ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

source $ZSH/oh-my-zsh.sh

# bullet-train order
export BULLETTRAIN_PROMPT_ORDER=(
    time
    status
    context
    dir
    screen
    virtualenv
    git
    hg
    cmd_exec_time
)

# alias
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano -w PKGBUILD'
alias more=less
alias la='ls -a'
alias ll='ls -l'

# some aliases avoid make mistake
alias mv='mv -i'
alias rm='rm -i'

# better yaourt colors
export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"

# user environment
JRE_HOME="/usr/local/jdk1.8.0/jre"
JDK_HOME="/usr/local/jdk1.8.0"
JAVA_HOME=${JRE_HOME}
#MATLAB_JAVA="/usr/lib/jvm/java-8-openjdk-amd64/jre/"
MATLAB_JAVA=${JRE_HOME}
MATLAB_PATH=${HOME}/MATLAB/R2017a/bin/glnxa64
MATLAB_INCLUDE_PATH=${HOME}/MATLAB/R2017a/extern/include/
JAVA_PROXY="-Dhttp.proxyHost=127.0.0.1 -Dhttp.proxyPort=8088"
QT_HOME="${HOME}/APPs/Qt/5.7/"
GUROBI_HOME="/home/daiwz/APPs/gurobi563/linux64"
GRB_LICENSE_FILE="$HOME/APPs/gurobi563/gurobi.lic"
export JRE_HOME
export JDK_HOME
export JAVA_HOME
export MATLAB_JAVA
export JAVA_PROXY
export QT_HOME
export GUROBI_HOME
export GRB_LICENSE_FILE

I3_PATH="/home/daiwz/.i3/bin"

# swi-prolog
SWI_HOME_DIR=/home/daiwz/.local/lib/swipl
export SWI_HOME_DIR

# most
export PAGER=most

PATH=$I3_PATH:$MATLAB_PATH:$QT_HOME/gcc_64/bin:$JRE_HOME/bin:$HOME/.local/bin/:/usr/share/ActiveTcl-8.5/bin/:/usr/local/WordNet-3.0/bin:${SWI_HOME_DIR}/library:$PATH:${GUROBI_HOME}/bin

LD_LIBRARY_PATH=$MATLAB_INCLUDE_PATH:$MATLAB_PATH:${HOME}/.local/include/:${JDK_HOME}/include:${GUROBI_HOME}/lib:$QT_HOME/gcc_64/include:${QT_HOME}/gcc_64/lib:${JRE_HOME}/lib/amd64/server/:${JRE_HOME}/lib/amd64/:${HOME}/.local/lib/scala/lib/:${HOME}/.local/lib:${SWI_HOME_DIR}/lib:${SWI_HOME_DIR}/lib/x86_64-linux:/usr/lib/openblas-base/:/usr/lib/atlas-base/:/usr/share/ActiveTcl-8.5/lib:/usr/local/lib:${HOME}/APPs/MATLAB/R2016b/extern/include/:$LD_LIBRARY_PATH

LD_RUN_PATH=$MATLAB_PATH:${JRE_HOME}/lib/amd64/:${GUROBI_HOME}/lib:${QT_HOME}/gcc_64/include:${QT_HOME}/gcc_64/lib:${HOME}/.local/lib/scala/lib/:${SWI_HOME_DIR}:${SWI_HOME_DIR}/lib/x86_64-linux:$HOME/.local:/usr/lib/openblas-base/:/usr/lib/atlas-base/:/usr/share/ActiveTcl-8.5/lib/:/usr/local/lib:$LD_RUN_PATH

export PATH
export LD_LIBRARY_PATH
export LD_RUN_PATH
export CLASSPATH=.:$JRE_HOME/lib/dt.jar:$JRE_HOME/lib/tools.jar
export PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig/:${QT_HOME}/gcc_64/lib/pkgconfig/:$PKG_CONFIG_PATH
export PYTHON_EGG_CACHE=/tmp/python-eggs/

# emacs alias
alias emacs='emacs -nw'

# prolog alias
alias pl='swipl'

# optimus
PRIMUS_PREFIX="primusrun"
OPTIMUS_PREFIX="optirun"

PATH="/home/daiwz/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/daiwz/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/daiwz/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/daiwz/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/daiwz/perl5"; export PERL_MM_OPT;
