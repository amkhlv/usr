
_td() {
    local cur prev opts car cdr x
    cur="${COMP_WORDS[COMP_CWORD]}"
    opts=$(td)
    if [[ ${cur} == -* ]] ; then
        COMPREPLY=()
    else
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
    fi
    return 0
}
complete -F _td td

