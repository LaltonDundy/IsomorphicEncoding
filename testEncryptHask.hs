import Crypt (encryption, decryption)
import System.Random
import Data.Char (chr)



randomStr = do  gen <- newStdGen
                let ns =  randoms gen :: [Int]
                return $ (fmap (\x -> chr (mod x 127))) (take 10 ns)

runExp = do x <- randomStr
            let bool = ((decryption.encryption $ x) == x)
            if (not bool) then print x else print bool


main = sequence $ fmap (\x -> runExp) [1..1000]
