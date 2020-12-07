{ pkgs ? (import nix/pkgs.nix { })
, withProfiling ? false
}:
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "unliftio-protocols";
    src = ./.;
  };
  projectFileName = "cabal.project";
  compiler-nix-name = "ghc8102";
  # compiler-nix-name = "ghc865";
  pkg-def-extras = [ ];
  modules =
    [
      {
        packages.unliftio-protocols.components.library.doCoverage = false;
      }
    ] ++
    (if withProfiling then
      [{
        packages.unliftio-protocols.components.library.enableLibraryProfiling = true;
        packages.unliftio-protocols.components.exes.unliftio-protocols-example-1.enableExecutableProfiling = true;
        packages.unliftio-protocols.components.tests.unliftio-protocols-test.enableExecutableProfiling = true;
      }] else [ ]);

}

