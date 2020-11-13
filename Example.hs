import Control.Concurrent.ParallelIO
import qualified Control.Concurrent.ParallelIO.Local as Local

main = do
  -- Example with the global pool. You should call stopGlobalPool
  -- after the last use of a global parallelism combinator - the
  -- end of the main function is a fine place.
  parallel_ [putStrLn "Hello", putStrLn "World"]
  stopGlobalPool
  
  -- Example of an explicitly sized and passed pool. Use 2 threads.
  Local.withPool 2 $ \pool -> do
    Local.parallel_ pool [putStrLn "Goodbype", putStrLn "World"]
