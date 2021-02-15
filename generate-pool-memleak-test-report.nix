let pkgs = import nix/pkgs.nix;
in
pkgs.runCommand "pool-memleak-test-report"
{
  memleakTest = import ./pool-memleak-test.nix { withProfiling = true; };
  hp2pretty = pkgs.haskellPackages.hp2pretty;
} ''
  mkdir -p $out
  cd $out

  echo "Pool Memory Leak Test Running."
  echo "       Log-file: $out/test.log"
  echo ""
  echo "This may take more than 45 minutes..."
  echo ""
  $memleakTest/bin/unliftio-pool-memleak-test 5000 50 +RTS -M1000m -RTS > test.log  
  # produce unliftio-pool-memleak-test.svg
  $hp2pretty/bin/hp2pretty --bands=8 unliftio-pool-memleak-test.hp
  rm unliftio-pool-memleak-test.hp
''

