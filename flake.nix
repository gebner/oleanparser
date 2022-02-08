{
  description = "Olean binary format parser in Lean";

  inputs = {
    lean = {
      url = "github:leanprover/lean4/e626b3d4aa4f9efdc1bf1c5a9b45a6a2507d75fa";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, lean, nixpkgs, flake-utils }:
    let
      supportedSystems = [
        "aarch64-linux"
        "aarch64-darwin"
        "i686-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        leanPkgs = lean.packages.${system};
        pkgs = nixpkgs.legacyPackages.${system};
        name = "Oleanparser";  # must match the name of the top-level .lean file
        project = leanPkgs.buildLeanPackage {
          inherit name;
          # deps = [ lean-ipld.project.${system} ];
          # Where the lean files are located
          src = ./.;
        };
        Main = leanPkgs.buildLeanPackage {
          name = "Main";
          deps = [ project ];
          # Where the lean files are located
          src = ./.;
        };
        Demo = leanPkgs.buildLeanPackage {
          name = "Demo";
          deps = [ project ];
          # Where the lean files are located
          src = ./.;
        };
        joinDepsDerivationns = getSubDrv:
          pkgs.lib.concatStringsSep ":" (map (d: "${getSubDrv d}") (project.allExternalDeps));
      in
      {
        packages = project // {
          ${name} = project;
          inherit Demo Main;
        };

        defaultPackage = self.packages.${system}.Main.executable;
        devShell = pkgs.mkShell {
          inputsFrom = [ project.executable ];
          buildInputs = with pkgs; [
            leanPkgs.lean-dev leanPkgs.lean-all
          ];
          LEAN_PATH = ".:" + joinDepsDerivationns (d: d.modRoot);
          LEAN_SRC_PATH = ".:" + joinDepsDerivationns (d: d.src);
        };
      });
}
