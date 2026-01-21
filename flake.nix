{
  description = "TAPL Opam nix flake";

  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
    jail-nix.url = "sourcehut:~alexdavid/jail.nix";
    llm-agents.url = "github:numtide/llm-agents.nix";
  };

  outputs = { self, flake-utils, opam-nix, nixpkgs, jail-nix, llm-agents }@inputs:
    let
      # Uses <package>.opam to solve dependencies from
      package = "TAPL";

      # ocaml-version = "*" will let opam solver find a compatible version
      ocaml-version = "*";
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        on = opam-nix.lib.${system};

        # OCaml Project Scope
        scope = on.buildDuneProject { } package ./. {
          ocaml-base-compiler = ocaml-version;
          ocaml-lsp-server = "*";
          ocamlformat = "*";
          utop = "*";
        };

        # Expose OCaml packages from opam derivation to opencode
        ocamlPkgs = builtins.filter pkgs.lib.isDerivation (builtins.attrValues scope);
        ocamlPath = pkgs.lib.makeSearchPath "lib/ocaml/${scope.ocaml-base-compiler.version}/site-lib" ocamlPkgs;

        opencode-pkg = llm-agents.packages.${system}.opencode;
        jail = jail-nix.lib.init pkgs;

        # Sandboxing Logic
        makeJailedOpencode = { extraPkgs ? [] } :
          jail "jailed-opencode" opencode-pkg (with jail.combinators; [
            network
            time-zone
            no-new-session

            # Automatically grants access to your project source
            mount-cwd

            # Persistent config/cache for the agent
            (readwrite (noescape "~/.config/opencode"))
            (readwrite (noescape "~/.local/share/opencode"))
            (readonly "/nix/store")

            (set-env "OCAMLPATH" ocamlPath)

            # Inject OCaml tools and common utilities that opencode can use
            (add-pkg-deps (with pkgs; extraPkgs ++ ocamlPkgs ++ [
                bashInteractive curl wget jq git which
                ripgrep gnugrep gawkInteractive ps findutils
                gzip unzip gnutar diffutils binutils gcc
            ]))
          ]);

      in
      {
        legacyPackages = scope;
        packages.default = scope.${package};

        devShells = {
          # Shell with opam packages
          default = pkgs.mkShell {
            inputsFrom = [ scope.${package} ];
            packages = [
              ocamlPkgs
            ];
          };

          # Shell with jailed opencode agent, nix develop ".#opencode"
          opencode = pkgs.mkShell {
            inputsFrom = [ scope.${package} ];
            packages = [
              ocamlPkgs
              (makeJailedOpencode {})
            ];
          };
        };
      }
    );
}
