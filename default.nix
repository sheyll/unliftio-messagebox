{ pkgs ? (import nix/pkgs.nix)
, withProfiling ? false
, withCoverage ? false
}:
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "unliftio-protocols";
    src = ./.;
  };
  projectFileName = "cabal.project";
  compiler-nix-name = "ghc8103";
  modules =
    [
      {
        packages.unliftio-protocols.components.library.doCoverage = withCoverage;
        packages.unliftio-protocols.components.tests.unliftio-protocols-test.doCoverage = withCoverage;
      }
    ] ++
    (if withProfiling then
      [{
        packages.unliftio-protocols.components.library.enableLibraryProfiling = true;
        packages.unliftio-protocols.components.exes.unliftio-protocols-example-1.enableExecutableProfiling = true;
        packages.unliftio-protocols.components.tests.unliftio-protocols-test.enableExecutableProfiling = true;
        packages.unliftio-protocols.components.benchmarks.unliftio-protocols-bench.enableExecutableProfiling = true;
      }] else [ ]);

}

