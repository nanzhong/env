self: super: {
  alacritty = super.alacritty.overrideAttrs (old: rec {
    version = "nanzhong-patched-macos-icon";
    src = super.fetchFromGitHub {
      owner = "nanzhong";
      repo = old.pname;
      rev = "refs/heads/patched-macos-icon";
      hash = "sha256-IfQFmDvl1zkvSTqK4hYOURfpB5gKbqt58A5MfmksNZ0=";
    };

    checkFlags = [
       "--skip=term::test::mock_term"
       "--skip=tty::unix::test_get_pw_entry"
    ];
  });
}
