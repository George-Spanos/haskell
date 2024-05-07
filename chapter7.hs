tensDigit :: Integer -> Integer
tensDigit x =
  let d = fst $ divMod x 10; k = divMod d 10
   in snd k

hundredsDigit :: Integer -> Integer
hundredsDigit x =
  let d = fst $ divMod x 100; k = divMod d 100
   in snd k
