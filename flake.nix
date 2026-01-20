{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs =
    { self, flake-utils, opam-nix, nixpkgs, }@inputs:
    let
      package = "TAPL";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};

        scope = on.buildOpamProject { } package ./. {
          ocaml-base-compiler = "*";
          ocaml-lsp-server = "*";
          ocamlformat = "*";
          utop = "*";
        };
      in
      {
        legacyPackages = scope;
        packages.default = scope.${package};

        devShells.default = pkgs.mkShell {
          inputsFrom = [ scope.${package} ];

          packages = with pkgs; [
            scope.utop
            scope.ocaml-lsp-server
            scope.ocamlformat
          ];
        };
      }
    );
}
