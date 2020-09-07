import Data.Vect

%default total

fromListOfLength : (n : Nat) -> (xs : List a) -> Maybe (Vect n a)
fromListOfLength n xs with (decEq n (length xs))
  fromListOfLength n xs | (Yes prf) = Just (rewrite prf in fromList xs)
  fromListOfLength n xs | (No _) = Nothing

arraySum : (cnt : String) -> (arr : String) -> String
arraySum cnt arr =
  let
    n   = the Nat (cast cnt)
    xs  = (map (\x => the Integer (cast x)) . words) arr
    v   = fromListOfLength n xs
  in
    case v of
      Nothing => "Invalid input"
      Just xs => show $ sum xs

main : IO ()
main = do
  cnt     <- getLine  
  arr     <- getLine
  putStrLn $ arraySum cnt arr
