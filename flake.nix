{
  description = "TAPL in OCaml";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
    in
    {
      devShells.${system}.default =
        let
          pkgs = import nixpkgs { inherit system; };
        in
        pkgs.mkShell {
          packages = with pkgs; [
            opam rlwrap

            # Required packages to build Bonsai, in case not already installed
            gmp pkg-config openssl_legacy zlib
            libffi pcre zstd
          ];

          shellHook = ''
            # Shorter bash prompt
            PS1="\[\e[1;32m\]\W\[\e[0m\]\$ "
            eval $(opam env --switch=bonsai)
          '';
        };
    };
}

