self: super: {
  alacritty = super.alacritty.overrideAttrs (old: rec {
    version = "nanzhong-master";
    src = super.fetchFromGitHub {
      owner = "nanzhong";
      repo = old.pname;
      rev = "refs/heads/patched-macos-icon";
      hash = "sha256-zwXEdNY6mHhzukGlveZjdVWMbVSsNVZrs23uy3uSjvM=";
    };

    checkFlags = [
       "--skip=term::test::mock_term"
       "--skip=tty::unix::test_get_pw_entry"
    ];
  });
}
