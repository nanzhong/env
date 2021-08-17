self: super: {
  mosh = super.mosh.overrideAttrs ( old: rec {
    name = "mosh-${version}";
    version = "master";
    src = super.fetchFromGitHub {
      owner = "mobile-shell";
      repo = "mosh";
      rev = "03087e7a761df300c2d8cd6e072890f8e1059dfa";
      sha256 = "170m3q9sxw6nh8fvrf1l0hbx0rjjz5f5lzhd41143kd1rps3liw8";
    };
    patches = [
      "${super.path}/pkgs/tools/networking/mosh/ssh_path.patch"
      "${super.path}/pkgs/tools/networking/mosh/mosh-client_path.patch"
      "${super.path}/pkgs/tools/networking/mosh/utempter_path.patch"
      "${super.path}/pkgs/tools/networking/mosh/bash_completion_datadir.patch"
    ];
  } );
}
