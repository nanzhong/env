{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.nanzhong.dev;
  emacs-darwin = (pkgs.emacs-git.override {
    withNS = true;
    withX = false;
    withGTK3 = false;
  }).overrideAttrs (old: {
    postPatch = concatStringsSep "\n" [
      old.postPatch
      ''
        echo "Patching emacs icon..."
        cp "${../../patches/emacs/Emacs.icns}" "nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns"
        cp "${../../patches/emacs/emacs.tiff}" "nextstep/GNUstep/Emacs.base/Resources/emacs.tiff"
        cp "${../../patches/emacs/emacs.svg}" "etc/images/icons/hicolor/scalable/apps/emacs.svg"
        cp "${../../patches/emacs/emacs.ico}" "etc/images/icons/hicolor/scalable/apps/emacs.ico"
        cp "${../../patches/emacs/emacs-128.png}" "etc/images/icons/hicolor/128x128/apps/emacs.png"
        cp "${../../patches/emacs/emacs-48.png}" "etc/images/icons/hicolor/48x48/apps/emacs.png"
        cp "${../../patches/emacs/emacs-32.png}" "etc/images/icons/hicolor/32x32/apps/emacs.png"
        cp "${../../patches/emacs/emacs-24.png}" "etc/images/icons/hicolor/24x24/apps/emacs.png"
        cp "${../../patches/emacs/emacs-16.png}" "etc/images/icons/hicolor/16x16/apps/emacs.png"
      ''
    ];

    # Spell checking (e.g. hunspell, libenchant) expect XDG_DATA_DIRS to be set in order to find the necessary dictionaries
    postInstall = old.postInstall + ''
      wrapProgram "$out/Applications/Emacs.app/Contents/MacOS/Emacs" \
        --set XDG_DATA_DIRS ${concatStringsSep ":" (map (path: path + "/share") config.environment.profiles)}
    '';
  });
  emacs-custom = ((pkgs.emacsPackagesFor emacs-darwin).emacsWithPackages (
    epkgs: [ epkgs.jinx ]
  ));
in
{
  imports = [ ./default.nix ];
  config = mkIf cfg.enable {
    environment = {
      pathsToLink = [ "/share/hunspell" ];

      systemPackages = [
        emacs-custom
      ];
    };
  };
}
