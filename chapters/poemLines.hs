firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences =
  firstSen
    ++ secondSen
    ++ thirdSen
    ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines s = extractedStr : myLines rest
  where
    extractedStr = takeWhile (/= '\n') s
    len = length extractedStr
    rest = drop (len + 1) s

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
  == shouldEqual)