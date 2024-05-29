. "$HOME/.aliases"

functions -c fish_prompt _old_fish_prompt

function fish_prompt
  if test -n "$IN_NIX_SHELL"
    echo -n "nix|"
  end
  _old_fish_prompt
end

function envsource
  for line in (grep -v '^#')
    set item (string split -m 1 '=' $line)
    set -gx $item[1] $item[2]
    echo "$item[1]=$item[2]" >&2
  end
end
