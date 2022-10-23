function fish_user_key_bindings
  fzf_key_bindings
  bind -e \cx
  bind -e \cv
  bind -e \ct
  bind \cl fzf-file-widget
  bind \ct transpose-chars

  # Configure fzf-fish bindings
  fzf_configure_bindings
end
