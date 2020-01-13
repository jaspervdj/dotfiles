# Indentation
set global tabstop 4
set global indentwidth 4

# Line numbers
add-highlighter global/ number-lines

# Wrapping and reformatting
set global autowrap_column 80
add-highlighter global/ column %opt{autowrap_column} default,blue
map global normal = '|fmt -w $kak_opt_autowrap_column<ret>'

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
hook global WinSetOption filetype=mail %{
  set window autowrap_column 72
}
