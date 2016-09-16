module Random where
  
import System.Random
import System.IO.Unsafe


randomNums :: (Random a, Num a) => [a]
randomNums = unsafePerformIO $ randoms <$> newStdGen
