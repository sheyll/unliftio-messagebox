{ withProfiling ? false,
  withCoverage ? true
}:
let 
  prj = (import ./default.nix { inherit withProfiling withCoverage; }).unliftio-protocols;
in  
if withCoverage then 
    prj.coverageReport
else    
    prj.components.tests.unliftio-protocols-test

