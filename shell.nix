{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

let org = pkgs.fetchgit {
            url      = "https://github.com/FixPlanet/org";
            rev      = "d62c8d9c321f5a2dffa5cac13b384300cf009fcc";
            sha256   = "sha256-WDUrGrR4DwRSajjSgYPWtdZA9joZUd8hHkWh8/gr0Lc=";
            fetchLFS = true;
          };
    path = "project-selection-committee";
 in

mkShell {
  buildInputs = [ ];
  shellHook = ''
    # Markdown files
    rm -rf src/${path}
    mkdir src/${path}
    cp ${org.outPath}/${path}/*.md src/${path}

    # Images
    rm -rf src/images/${path}
    mkdir src/images/${path}
    cp ${org.outPath}/${path}/*.{png,jpg} src/images/${path}
  '';
}
