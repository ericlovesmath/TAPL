{
  description = "TAPL nix flake";

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

      # Opam packages used in developer mode
      devOpamPackagesQuery = {
        utop = "*";
        ocaml-lsp-server = "*";
        merlin = "*";
      };
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};

        # ocaml-base-compiler = "*" for any version
        opamPackagesQuery = devOpamPackagesQuery // {
          ocaml-base-compiler = "*";
        };

        # OCaml Project Scope
        scope = on.buildDuneProject {
          repos = [ opam-nix.inputs.opam-repository ];
        } package ./. opamPackagesQuery;

        # Prevent the ocaml dependencies from leaking into dependent environments
        overlay = final: prev: {
          ${package} = prev.${package}.overrideAttrs (_: {
            doNixSupport = false;
          });
        };
        scope' = scope.overrideScope overlay;

        # Expose OCaml packages defined in [devOpamPackagesQuery] to devshell
        devOpamPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devOpamPackagesQuery) scope');
        main = scope'.${package};

        # Expose OCaml packages from opam derivation to opencode, except our package itself
        opamPackages = builtins.filter pkgs.lib.isDerivation (builtins.attrValues (removeAttrs scope' [ package ]));
        ocamlPath = pkgs.lib.makeSearchPath "lib/ocaml/${scope'.ocaml-base-compiler.version}/site-lib" opamPackages;

        opencode-pkg = llm-agents.packages.${system}.opencode;
        jail = jail-nix.lib.init pkgs;

        # Sandboxing for Opencode using bubblewrap
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
            (add-pkg-deps (with pkgs; extraPkgs ++ opamPackages ++ [
                bashInteractive curl wget jq which
                ripgrep gnugrep gawkInteractive ps findutils
                diffutils binutils gcc
            ]))
          ]);

      in
      {
        legacyPackages = scope';
        packages.default = main;

        devShells = {
          # Shell with opam packages
          default = pkgs.mkShell {
            inputsFrom = [ main ];
            packages = [
              devOpamPackages
            ];
          };

          # Shell with jailed opencode agent, nix develop ".#opencode"
          opencode = pkgs.mkShell {
            inputsFrom = [ main ];
            packages = [
              devOpamPackages
              (makeJailedOpencode {})
            ];
          };
        };
      }
    );
}
