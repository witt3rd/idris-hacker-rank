import Data.Vect

parse : String -> List Integer
parse = map (\x => the Integer (cast x)) . words

readVect : IO (len ** Vect len Integer)
readVect = do 
  str <- getLine
  let xs = parse str
  pure (_ ** (fromList xs))

sameLen : {n : Nat} -> {m : Nat} -> Vect n e -> Vect m e -> Maybe (Vect n e, Vect n e)
sameLen xs ys {n} {m} with (decEq m n)
  -- sameLen xs ys | (Yes prf) = Just (xs, rewrite prf in ys)
  sameLen xs ys {m = n} | (Yes Refl) = Just (xs, ys)
  sameLen xs ys | (No _) = Nothing

calc : Vect n Integer -> Vect n Integer -> (Integer, Integer)
calc [] [] = (0, 0)
calc (x :: xs)  (y :: ys) = 
  let (a, b) = calc xs ys in
    if x > y then (a + 1, b)
    else if x < y then (a, b + 1)
    else (a, b)

main : IO ()
main = do
  (a_len ** a_vec) <- readVect
  (b_len ** b_vec) <- readVect
  case sameLen a_vec b_vec of
    Nothing => putStrLn "Invalid input"
    Just (as, bs) => 
      let (a, b) = calc as bs in
        putStrLn $ show a ++ " " ++ show b
