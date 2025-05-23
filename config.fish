. "$HOME/.aliases"

functions -c fish_prompt _old_fish_prompt

function fish_prompt
  if test -n "$IN_NIX_SHELL"
    echo -n "nix|"
  end
  _old_fish_prompt
end

function envsource
  while read line
    if string match -q 'export *' -- $line
      set line (string sub --start (math (string length 'export ') + 1) -- $line)
    end
    if not string match -qr '^#|^$' "$line"
      set item (string split -m 1 '=' $line)
      set -gx $item[1] $item[2]
      echo "set -gx $item[1]=$item[2]"
    end
  end
end
