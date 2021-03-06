let pkgs = import ./nix/pkgs.nix; in 
pkgs.runCommand "module-graph"
{
    moduleSources = ./.;
    graphviz = pkgs.graphviz;
    graphmod = pkgs.haskellPackages.graphmod;
}
''
mkdir -p $out
cd $moduleSources 
$graphmod/bin/graphmod \
    -p \
    -a \
    --remove-module=Control.Applicative \
    --remove-module=Control.Monad \
    --remove-module=Control.Monad \
    --remove-module=Data.Coerce \
    --remove-module=Data.Set \
    --remove-module=Data.Map \
    --remove-module=Data.Kind \
    --remove-module=Data.Functor \
    --remove-module=Data.Maybe \
    --remove-module=UnliftIO \
    --remove-module=UnliftIO.Concurrent \
    --remove-module=Control.Monad.Reader \
    --remove-module=Main \
    --remove-module=MediaBenchmark \
    --remove-module=System.Environment \
    --remove-module=GHC.Stack \
    --remove-module=GHC.Stats \
    --no-module-in-cluster \
    -i src \
    > $out/module-graph.dot
cd $out
$graphviz/bin/dot -Tpng -Gsize=4,5 -Gdpi=200 -o $out/module-graph.png $out/module-graph.dot
''