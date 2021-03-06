# -*- shell-script -*-

umask 022

export HISTFILE=~/.histfile
export HISTSIZE=10000
export SAVEHIST=$HISTSIZE
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# Colorify the output of some commonly used programs
export GREP_COLOR='1;32'
alias grep="grep --color=auto"
alias ls="ls -F --human-readable --color=auto"
alias ll="ls -l --group-directories-first"

# Shortcuts
alias rezshrc="source ~/.zshrc"
alias del="gvfs-trash -f"
alias ocml="rlwrap ocaml"
alias pigs="tree --du -sh --sort=size"

# Turn on control keys in mpg123
alias mpg123="mpg123 --control"

# Shut up idiotic Lynx prompts
alias lynx="lynx -cookies"

# Emacsclient aliases
alias ec="emacsclient -t -a zile"
alias ecw="emacsclient -a gedit"
alias ecc="emacsclient -c -a gedit"

# get the name of the Git branch we are on
# http://www.jukie.net/~bart/blog/zsh-git-branch2

typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions

export __CURRENT_GIT_BRANCH=
export __CURRENT_GIT_VARS_INVALID=1

zsh_git_invalidate_vars() {
    export __CURRENT_GIT_VARS_INVALID=1
}

zsh_git_compute_vars() {
    export __CURRENT_GIT_BRANCH="$(zsh_git_parse_git_branch)"
    export __CURRENT_GIT_VARS_INVALID=
}

zsh_git_parse_git_branch() {
    git branch --no-color 2> /dev/null \
        | sed -e '/^[^*]/d' -e 's/* \(.*\)/[\1]/'
}

chpwd_functions+='zsh_git_chpwd_update_vars'

zsh_git_chpwd_update_vars() {
    zsh_git_invalidate_vars
}

preexec_functions+='zsh_git_preexec_update_vars'
zsh_git_preexec_update_vars() {
    case "$(history $HISTCMD)" in
        *git*) zsh_git_invalidate_vars ;;
    esac
}

get_git_prompt_info() {
    test -n "$__CURRENT_GIT_VARS_INVALID" && zsh_git_compute_vars
    echo $__CURRENT_GIT_BRANCH
}

autoload -U colors
colors
setopt prompt_subst

# My funky PS1
PS1='%{$fg[yellow]%}%n%{$reset_color%}@%{$fg_bold[blue]%}%M%{$reset_color%}:%{$fg[green]%}%~%{$fg[red]%}$(get_git_prompt_info)%{$reset_color%}$ '

# Some options
bindkey -e  # Emacs-style keymap
setopt autopushd share_history appendhistory autocd extendedglob nomatch
setopt hist_ignore_all_dups hist_ignore_space hist_reduce_blanks
setopt multios # multiple redirection for I/O
unsetopt beep # don't EVER use the pc speaker

# TEH COMPLET1ON!!!!1
autoload -Uz compinit
compinit -u

# Prediction
# Turn on/off with C-z/C-x-z
autoload predict-on
zle -N predict-on
zle -N predict-off
bindkey '^Z'   predict-on
bindkey '^X^Z' predict-off
zstyle ':predict' toggle true
zstyle ':predict' verbose true

# C-x e - Edit command line with $EDITOR
autoload edit-command-line
zle -N edit-command-line
bindkey '^Xe' edit-command-line

# Ctrl-[Right,Left] for word movement
bindkey ';5D' emacs-backward-word
bindkey ';5C' emacs-forward-word

# Ctrl-[Up, Down] for history movement (a la Emacs)
bindkey ';5A' up-history
bindkey ';5B' down-history

# Make PgUp/PgDn work like C-a/C-e
bindkey '^[[5~' beginning-of-line
bindkey '^[[6~' end-of-line

# Emacs-like kill-region
bindkey "\C-w" kill-region

# Ctrl-backspace for backward-kill-word
# Does not work in gnome-terminal/cygwin, only in xterm:-(.
if [ ! "$CYGWIN" ]
then
    bindkey '^H' backward-kill-word
fi

# In case we're run from Emacs...
if [[ $TERM == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
fi
