let pkgs = import nix/pkgs.nix;
in
pkgs.runCommand "test-reports" {
   unitTest = import ./test.nix { withProfiling = true; }; 
   hp2html = pkgs.haskellPackages.hp2html;
   hp2pretty = pkgs.haskellPackages.hp2pretty;
  } ''
  mkdir -p $out
  cd $out
  $unitTest/bin/unliftio-protocols-test --html test-result.html
  # produce unliftio-protocols-test.html
  $hp2html/bin/hp2html unliftio-protocols-test.hp
  # produce unliftio-protocols-test.svg
  $hp2pretty/bin/hp2pretty unliftio-protocols-test.hp
''

