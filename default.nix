{ pkgs ? (import nix/pkgs.nix)
, withProfiling ? false
, withCoverage ? false
}:
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "unliftio-messagebox";
    src = ./.;
  };
  projectFileName = "cabal.project";
  compiler-nix-name = "ghc8103";
  modules =
    [
      {
        packages.unliftio-messagebox.components.library.doCoverage = withCoverage;
        packages.unliftio-messagebox.components.tests.unliftio-messagebox-test.doCoverage = withCoverage;
      }
    ] ++
    (if withProfiling then
      [{
        packages.unliftio-messagebox.components.library.enableLibraryProfiling = true;
        packages.unliftio-messagebox.components.exes.unliftio-messagebox-example-1.enableExecutableProfiling = true;
        packages.unliftio-messagebox.components.tests.unliftio-messagebox-test.enableExecutableProfiling = true;
        packages.unliftio-messagebox.components.benchmarks.unliftio-messagebox-bench.enableExecutableProfiling = true;
      }] else [ ]);

}

