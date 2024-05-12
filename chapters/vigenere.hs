import Data.List (elemIndex)
import Debug.Trace

message = "MEET AT DAWN"

encryptedMessage = "MPPR AE OYWY"

letters :: [Char]
letters = ['A' .. 'Z']

type Keyword = String

key :: Keyword
key = "ALLY"

replaceLetter :: Int -> Char -> Char
replaceLetter 0 b = b
replaceLetter a b =
  let idx = elemIndex b letters
   in case idx of
        Nothing -> ' '
        Just i ->
          let newPos = mod (i + a) $ length letters
           in letters !! newPos

encrypt :: Keyword -> String -> String
encrypt _ [] = []
encrypt [] b = encrypt key b
encrypt k (' ' : xs) = ' ' : encrypt k xs
encrypt (k : xk) (m : xm) =
  let idx = elemIndex k letters
   in case idx of
        Nothing -> encrypt (k : xk) xm
        Just i -> replaceLetter i m : encrypt xk xm

decrypt :: Keyword -> String -> String
decrypt _ [] = []
decrypt [] b = decrypt key b
decrypt k (' ' : xs) = ' ' : decrypt k xs
decrypt (k : xk) (m : xm) =
  let idx = elemIndex k letters
   in case idx of
        Nothing -> decrypt (k : xk) xm
        Just i -> replaceLetter (-i) m : decrypt xk xm

main :: IO ()
main = do
  print ("Decrypted Value: " ++ decrypt key encryptedMessage)
  print ("Encrypted Value: " ++ encrypt key message)