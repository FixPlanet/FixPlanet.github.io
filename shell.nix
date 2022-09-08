{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

let org = pkgs.fetchgit {
            url      = "https://github.com/FixPlanet/org";
            rev      = "75d4ba04eea558544c672227b772c8c7e30db1c9";
            sha256   = "sha256-W5GHfMSo+WV+Rr79G2Fa4k5A0kZ4DFA5G7whY0BKWlg=";
            fetchLFS = true;
          };
    path = "project-selection-committee";
 in

mkShell {
  buildInputs = [ pandoc ];
  shellHook = ''
    # Committee-member markdown files
    rm -rf src/${path}
    mkdir src/${path}
    cp ${org.outPath}/${path}/*.md src/${path}

    # Committee-member mmages
    rm -rf src/images/${path}
    mkdir src/images/${path}
    cp ${org.outPath}/${path}/*.{png,jpg} src/images/${path}

    # "Misc" org docs
    #
    # Note: Because of the way Hakyll works, we use pandoc (the tool) to
    # convert these to HTML so that we can include them in the website using
    # the $partial(...)$ directive.
    rm -rf src/org-docs
    mkdir src/org-docs

    # The "essential criteria" part of the document.
    pandoc ${org.outPath}/constitution/essential-criteria.md -o src/org-docs/essential-criteria.html

  '';
}
