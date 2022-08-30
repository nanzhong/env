function fish_user_key_bindings
  fish_default_key_bindings -M insert
  fish_vi_key_bindings --no-erase insert

  fzf_key_bindings
  bind -e \cx
  bind -e \cv
  bind -e \ct
  bind \cl fzf-file-widget
  bind \ct transpose-chars

  # Configure fzf-fish bindings
  fzf_configure_bindings
end
