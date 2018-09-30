let
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/infinisil/nixpkgs/archive/7d7215dc80c8654581342e81236aad91508b0edd.tar.gz";
    sha256 = "1l0zaqhx72s6gihf612jh634qg53j6gi5kqngax31fsgjzp3kjdy";
  };

  nixpkgs = (import nixpkgsSrc {}).srcOnly {
    name = "nixpkgs-patched";
    src = nixpkgsSrc;
    patches = [
      # https://github.com/NixOS/nixpkgs/pull/46453
      (builtins.fetchurl {
        url = "https://github.com/NixOS/nixpkgs/commit/e6dd03d554e65badd9cdc8b9c137a5998a642e42.patch";
        sha256 = "0aisra3arv6x6z59dfw4bfxyj40pm6liixgkwpj1rjrr0ql4yc9s";
      })
    ];
  };
in
{ pkgs ? import nixpkgsSrc {}
}:
with pkgs.haskell.lib;

(pkgs.haskellPackages.extend (packageSourceOverrides {
  nixpkgs-analyze = ./.;
})).extend (self: super: {
  stm-containers = dontCheck (doJailbreak super.stm-containers);
}) // { inherit pkgs; }
