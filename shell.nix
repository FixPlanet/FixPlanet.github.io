{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

let org = builtins.fetchGit {
            ref = "add-committee-members";
            url = "https://github.com/FixPlanet/org";
          };
    path = "project-selection-committee";
 in

mkShell {
  buildInputs = [ ];
  shellHook = ''
    rm -rf src/${path}
    mkdir src/${path}
    cp ${org.outPath}/${path}/* src/${path}
  '';
}
