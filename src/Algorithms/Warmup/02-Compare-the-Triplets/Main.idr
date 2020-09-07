import Data.Vect

parse : String -> List Integer
parse = map (\x => the Integer (cast x)) . words

calc : Vect n Integer -> Vect m Integer -> (Integer, Integer)
calc []         []        = (0, 0)
calc (x :: xs)  (y :: ys) = let (a, b) = calc xs ys in
                              if x > y then (a + 1, b)
                              else if x < y then (a, b + 1)
                              else (a, b)
calc _          _         = (0, 0)

main : IO ()
main = do
  a_in  <- getLine
  b_in  <- getLine
  let a_lst   = parse a_in
  let b_lst   = parse b_in
  let as      = fromList a_lst
  let bs      = fromList b_lst
  let (a, b)  = calc as bs
  putStrLn $ show a ++ " " ++ show b
