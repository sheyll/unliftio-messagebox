module Main (main) where

import MediaBenchmark

main = do
  args <- getArgs
  let rounds =
        case args of
          [n'] -> read n'
          _ -> 1
  putStrLn ""
  putStrLn "================= BEGIN ================"
  putStrLn ""
  let p =
        Param
          { nDsps = 100,
            nGroups = 1000,
            nMembers = 4,
            nRounds = rounds
          }
  print p 
  putStrLn ""
  mediaAppBenchmark p  
  putStrLn ""
  putStrLn "================= DONE ================"
