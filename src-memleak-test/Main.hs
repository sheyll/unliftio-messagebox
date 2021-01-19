module Main (main) where

import MediaBenchmark
import System.Environment (getArgs)
import UnliftIO.MessageBox
  ( MessageLimit (MessageLimit_256),
    WaitingBoxLimit (WaitingBoxLimit),
  )
import Control.Monad (when, forM_)

main :: IO ()
main = do
  args <- getArgs
  let (rounds, repetitions) =
        case args of
          [n',r'] -> (read n', read r')
          [n'] -> (read n', 1)
          _ -> (1,1)
  forM_ [1 .. repetitions] $ \rep -> do
    putStrLn ""
    putStrLn ("================= BEGIN (rep: "++ show rep ++") ================")
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
    mediaAppBenchmark (WaitingBoxLimit (Just (24 * 3600 * 1000000)) 10000000 MessageLimit_256) p
    putStrLn ""
    putStrLn ("================= DONE (rep: "++ show rep ++") ================")
    when (rep < repetitions) $ do 
      putStrLn ""
      putStrLn "(Sleeping...)"
      