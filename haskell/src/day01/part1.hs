import Data.Char
import Distribution.Compat.Prelude (exitSuccess)
import System.IO (isEOF)

solve :: IO Int
solve = do
  done <- isEOF
  if done
    then return 0
    else do
      str <- getLine
      rest <- solve
      let digits = filter isDigit str
          firstDigit = head digits
          lastDigit = last digits
       in return (read [firstDigit, lastDigit] + rest)

main = do
  done <- isEOF
  if done
    then exitSuccess
    else do
      ans <- solve
      print (show ans)
