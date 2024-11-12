{
  description = "A Nix-flake-based R development environment";

  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1.*.tar.gz";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };
      });
    in
    {
      overlays.default = final: prev: rec {
        # rEnv = final.rWrapper.override {
        #   packages = with final.rPackages; [ knitr ];
        # };
        RStudio = final.rstudioWrapper.override {
          packages = with final.rPackages; [
            dplyr
            gam
            knitr
            mgcv
            pROC
            randomForest
            readr
            readxl
            rpart
            ggplot2
            xts
          ];
        };
      };

      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs;
            [
              RStudio
              # rEnv
              pandoc
              texlive.combined.scheme-full
            ];
        };
      });
    };
}
