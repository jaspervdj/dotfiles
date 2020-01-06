. "$HOME/.aliases"

function cdt
    cd $argv
    if [ (pwd) = "$HOME" ]
        set title '~'
    else
        set title (basename (pwd))
    end
    echo "$title"
    printf '\ek%s\e\\' "$title"
end
