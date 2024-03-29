OPC=${PROMPT_COMMAND} PROMPT_COMMAND='' && OPS1=${PS1} PS1='' && OPS2=${PS2} PS2=''
# init_bash.sh - emacs shell mode initializations
# Note the intentional first line to suppress extraneous executions and new lines
unset TERMCAP COLUMNS
# Good explanation: https://stackoverflow.com/a/54951844/1124740
#export TERM=emacs               # though see https://stackoverflow.com/a/54951844/1124740
export PAGER=cat
ESHELL_DIR=~/.emacs.d/eshell

update_eshell () {
    if [[ ~/.bashrc -nt "$ESHELL_DIR"/alias ]]; then
        # dump aliases for emacs eshell (if eshell is installed and this has been updated)
        # Yakshave: does ~/.bashrc symlink reflect update
        printf "eshell aliases out of date wrt ~/.bashrc, dumping to %s" "${ESHELL_DIR}/alias"
        mkdir -p "$ESHELL_DIR"
        alias | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/'\\\''/'/g;" >|"${ESHELL_DIR}/alias"
        cd "$HOME" || exit  # Avoid complaint below
    fi
}

check_directory () {
    # shell mode sometimes starts in ~/.emacs.d
    if [[ "$PWD" != "$HOME" ]]; then
        cd || exit "$HOME"
        printf "\nChanged directory from %s to %s\n" "$OLDPWD" "$PWD"
        printf "\nBuffer default-directory handled by jwd/resync-directories\n"
    else
        printf "\n"
    fi
}

update_eshell
check_directory
# Local Variables:
# mode: sh
# sh-shell: bash
# End:
# restore suppressions
PS1=${OPS1} && PS2=${OPS2} && PROMPT_COMMAND=${OPC} && unset OPS1 OPS2 OPC ESHELL_DIR update_shell check_directory
