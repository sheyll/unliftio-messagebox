{ withProfiling ? false
}:
(import ./default.nix { inherit withProfiling; }).unliftio-messagebox.components.benchmarks.unliftio-messagebox-bench

