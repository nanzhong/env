self: super: {
  alacritty = super.alacritty.overrideAttrs (old: rec {
    version = "nanzhong-patched-macos-icon";
    src = super.fetchFromGitHub {
      owner = "nanzhong";
      repo = old.pname;
      rev = "refs/heads/patched-macos-icon";
      hash = "sha256-CO7cZ3ljdM/Bd5fcr23SfEm/GeGa5ftk+1kNO2MAj/I=";
    };

    checkFlags = [
       "--skip=term::test::mock_term"
       "--skip=tty::unix::test_get_pw_entry"
    ];
  });
}
