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
    # vi_mode     # Vi-mode indicator (Disabled)
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
