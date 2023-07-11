{
  nixConfig.bash-prompt = "[nix(simple-dear-imgui)] ";
  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/3176a8460ff51d1fa223e2962b11bd85543a36ba";
  };
  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };

      haskellExtend = hpFinal: hpPrev: {
        simple-dear-imgui = hpPrev.callCabal2nix "simple-dear-imgui" self { };
        # relax bound: megaparsec >=9.0 && <9.3
        dear-imgui = pkgs.haskell.lib.doJailbreak hpPrev.dear-imgui;
      };
      hsPkgs = pkgs.haskellPackages.extend haskellExtend;

      ciTools = with pkgs; [
        cabal-install
        hlint
        pkgs.haskellPackages.fourmolu
      ];
      devTools = with pkgs; [ ghcid haskell-language-server ];

    in {
      haskellExtend = haskellExtend;
      packages."x86_64-linux".default =
        pkgs.haskell.lib.justStaticExecutables hsPkgs.simple-dear-imgui;
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.simple-dear-imgui ];
        buildInputs = ciTools ++ devTools;
      };
    };
}
