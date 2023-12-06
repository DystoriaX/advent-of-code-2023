import Data.Char
import Distribution.Compat.Prelude (exitSuccess)
import System.IO (isEOF)

getDigits :: String -> [Char]
getDigits [] = []
getDigits s@('o' : 'n' : 'e' : xs) = '1' : getDigits (tail s)
getDigits s@('t' : 'w' : 'o' : xs) = '2' : getDigits (tail s)
getDigits s@('t' : 'h' : 'r' : 'e' : 'e' : xs) = '3' : getDigits (tail s)
getDigits s@('f' : 'o' : 'u' : 'r' : xs) = '4' : getDigits (tail s)
getDigits s@('f' : 'i' : 'v' : 'e' : xs) = '5' : getDigits (tail s)
getDigits s@('s' : 'i' : 'x' : xs) = '6' : getDigits (tail s)
getDigits s@('s' : 'e' : 'v' : 'e' : 'n' : xs) = '7' : getDigits (tail s)
getDigits s@('e' : 'i' : 'g' : 'h' : 't' : xs) = '8' : getDigits (tail s)
getDigits s@('n' : 'i' : 'n' : 'e' : xs) = '9' : getDigits (tail s)
getDigits (x : xs) =
  let rest = getDigits xs
   in if isDigit x
        then x : rest
        else rest

solve :: IO Int
solve = do
  done <- isEOF
  if done
    then return 0
    else do
      str <- getLine
      rest <- solve
      let digits = getDigits str
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
