import Addition (add, sayHello)

main :: IO ()
main = do
  sayHello
  print (add 1 2)
