. "$HOME/.aliases"

functions -c fish_prompt _old_fish_prompt

function fish_prompt
  if test -n "$IN_NIX_SHELL"
    echo -n "nix|"
  end
  _old_fish_prompt
end
