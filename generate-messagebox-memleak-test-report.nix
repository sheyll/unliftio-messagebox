let pkgs = import nix/pkgs.nix;
in
pkgs.runCommand "messagebox-memleak-test-report"
{
  memleakTest = import ./messagebox-memleak-test.nix { withProfiling = true; };
  hp2pretty = pkgs.haskellPackages.hp2pretty;
} ''
  mkdir -p $out
  cd $out

  echo "Message-Box Memory Leak Test Running."
  echo "       Log-file: $out/test.log"
  echo ""
  echo "This may take more than 45 minutes..."
  echo ""
  $memleakTest/bin/unliftio-messagebox-memleak-test 10 5 +RTS -M460m -RTS > test.log
  # produce unliftio-messagebox-memleak-test.svg
  $hp2pretty/bin/hp2pretty --bands=8 unliftio-messagebox-memleak-test.hp
  rm unliftio-messagebox-memleak-test.hp
''

