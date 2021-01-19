let pkgs = import nix/pkgs.nix;
in
pkgs.runCommand "memleak-test-report" {
   memleakTest = import ./memleak-test.nix { withProfiling = true; }; 
   hp2pretty = pkgs.haskellPackages.hp2pretty;
  } ''
  mkdir -p $out
  cd $out

  echo "Writing into: $out/test.log"
  echo ""
  echo "This may take more than 13 minutes..."
  echo ""
  $memleakTest/bin/unliftio-messagebox-memleak-test 100 10 +RTS -M400m -RTS > test.log
  
  # produce unliftio-messagebox-memleak-test.svg
  $hp2pretty/bin/hp2pretty unliftio-messagebox-memleak-test.hp
''

