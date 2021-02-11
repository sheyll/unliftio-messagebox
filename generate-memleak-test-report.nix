let pkgs = import nix/pkgs.nix;
in
pkgs.runCommand "memleak-test-report"
{
  memleakTest = import ./memleak-test.nix { withProfiling = true; };
  hp2pretty = pkgs.haskellPackages.hp2pretty;
} ''
  mkdir -p $out
  cd $out

  echo "Pool Memory Leak Test Running."
  echo "       Log-file: $out/pool-memleak-test.log"
  echo ""
  echo "This may take more than 45 minutes..."
  echo ""
  $memleakTest/bin/unliftio-pool-memleak-test 100 30 +RTS -M400m -RTS > pool-memleak-test.log  
  # produce unliftio-pool-memleak-test.svg
  $hp2pretty/bin/hp2pretty --bands=8 unliftio-pool-memleak-test.hp
  rm unliftio-pool-memleak-test.hp

  echo "Pool Memory Leak Test Running."
  echo "       Log-file: $out/messagebox-memleak-test.log"
  echo ""
  echo "This may take more than 45 minutes..."
  echo ""
  $memleakTest/bin/unliftio-messagebox-memleak-test 100 30 +RTS -M400m -RTS > messagebox-memleak-test.log
  # produce unliftio-pool-memleak-test.svg
  $hp2pretty/bin/hp2pretty --bands=8 unliftio-pool-memleak-test.hp
  rm unliftio-messagebox-memleak-test.hp
''

