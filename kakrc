# Indentation
set global tabstop 4
set global indentwidth 4

# Split window
define-command -docstring "vsplit [<commands>]: split tmux vertically" \
vsplit -params .. -command-completion %{
    tmux-terminal-horizontal kak -c %val{session} -e "%arg{@}"
}
define-command -docstring "hsplit [<commands>]: split tmux horizontally" \
hsplit -params .. -command-completion %{
    tmux-terminal-vertical kak -c %val{session} -e "%arg{@}"
}

# Line numbers
add-highlighter global/ number-lines

# Wrapping and reformatting
set global autowrap_column 80
add-highlighter global/ column %opt{autowrap_column} default,blue
map global normal = '|par ${kak_opt_autowrap_column}q g1<ret>'

# Highlight trailing whitespace
add-highlighter global/ regex '\h+$' 0:default,red

# File lookups
def f -params 1 -shell-script-candidates %{ find -type f } %{ edit %arg{1} }
def gf -params 1 -shell-script-candidates %{ git ls-files } %{ edit %arg{1} }

# Rego
hook global WinCreate .*\.rego %{ set buffer filetype ruby }

# PureScript
hook global WinCreate .*\.purs %{ set buffer filetype haskell }

# Email
hook global WinCreate /tmp/mutt-.* %{ set buffer filetype mail }
hook global WinSetOption filetype=mail %{
  set window autowrap_column 70
}

# Grepping
set global grepcmd 'rgrep'

# kak-lsp
eval %sh{kak-lsp --kakoune -s $kak_session}
# set global lsp_cmd "kak-lsp -s %val{session} -vvv --log /tmp/kak-lsp.log"
lsp-enable
lsp-auto-hover-enable
