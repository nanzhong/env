self: super: {
  ffmpeg = (super.ffmpeg.overrideAttrs (old: rec {
    configureFlags = old.configureFlags ++ [ "--enable-omx-rpi --enable-nonfree" ];
  })).override {
    nonfreeLicensing = true;
  };
}
