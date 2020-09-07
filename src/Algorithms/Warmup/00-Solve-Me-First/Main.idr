main : IO ()
main = repl "\n> " (show . sum . map cast . words)