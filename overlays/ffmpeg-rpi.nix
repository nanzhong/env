self: super: {
  ffmpeg = (super.ffmpeg.overrideAttrs (old: rec {
    configureFlags = old.configureFlags ++ [ "--enable-omx-rpi" ];
  })).override {
    withUnfree = true;
  };
}
