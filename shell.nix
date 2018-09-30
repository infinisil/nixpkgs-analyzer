with import ./. {};

shellFor {
  packages = p: with p; [ nixpkgs-analyze ];
  withHoogle = true;
  nativeBuildInputs = [ cabal-install ];
}
