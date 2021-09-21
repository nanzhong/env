self: super: {
  mosh = super.mosh.overrideAttrs ( old: rec {
    name = "mosh-${version}";
    version = "master";
    src = super.fetchFromGitHub {
      owner = "mobile-shell";
      repo = "mosh";
      rev = "e023e81c08897c95271b4f4f0726ec165bb6e1bd";
      sha256 = "sha256-X2xJCiC5/vSijzZgQsWDzD+R8D8ppdZD6WeG4uoxyYw=";
    };
    patches = [
      "${super.path}/pkgs/tools/networking/mosh/ssh_path.patch"
      "${super.path}/pkgs/tools/networking/mosh/mosh-client_path.patch"
      "${super.path}/pkgs/tools/networking/mosh/utempter_path.patch"
      # Removed backported w/c++17, ::bind vs std::bind patch since it's
      # included in master branch.
      "${super.path}/pkgs/tools/networking/mosh/bash_completion_datadir.patch"
    ];

    # Removed the aarch64-darwin patch in post patch since it's unused and
    # causing conflicts with master branch.
    postPatch = ''
      substituteInPlace scripts/mosh.pl \
          --subst-var-by ssh "${super.openssh}/bin/ssh"
      substituteInPlace scripts/mosh.pl \
          --subst-var-by mosh-client "$out/bin/mosh-client"
    '';
  } );
}
