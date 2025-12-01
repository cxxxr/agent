{
  description = "Common Lisp development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Common Lisp implementations
            sbcl                    # Steel Bank Common Lisp
            # ecl                   # Embeddable Common Lisp
            # ccl                   # Clozure Common Lisp
            # clisp                 # GNU CLISP

            # Development tools
            rlwrap                  # readline wrapper for REPL
            curl                    # for downloading quicklisp

            # Native libraries
            openssl

            # Editor support (optional)
            # emacs                 # with SLIME
            # lispPackages.slime
          ];

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.openssl ];

          shellHook = ''
            export CL_SOURCE_REGISTRY="$PWD//"
            export QUICKLISP_HOME="$HOME/.quicklisp"

            # Auto-install Quicklisp if not present
            if [ ! -f "$QUICKLISP_HOME/setup.lisp" ]; then
              echo "Installing Quicklisp..."
              QUICKLISP_INSTALLER=$(mktemp)
              curl -s -o "$QUICKLISP_INSTALLER" https://beta.quicklisp.org/quicklisp.lisp
              sbcl --non-interactive \
                   --load "$QUICKLISP_INSTALLER" \
                   --eval "(quicklisp-quickstart:install :path \"$QUICKLISP_HOME\")" \
                   --eval "(ql-util:without-prompting (ql:add-to-init-file))"
              rm "$QUICKLISP_INSTALLER"
              echo "Quicklisp installed!"
            fi

            echo "Common Lisp development environment"
            echo "  SBCL: $(sbcl --version)"
            echo "  Quicklisp: $QUICKLISP_HOME"
            echo ""
            echo "Usage:"
            echo "  sbcl              - Start SBCL REPL (Quicklisp auto-loaded)"
            echo "  rlwrap sbcl       - Start SBCL with readline support"
          '';
        };
      }
    );
}
