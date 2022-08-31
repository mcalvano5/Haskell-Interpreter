module Main where
import ParserMIRIANA
import InterpreterMIRIANA

main :: IO ()
main = do
  putStrLn "Welcome!"
  putStrLn "Insert the path to the file you want to use!"
  input <- getLine;
  p <- readFile input
  let c = parse p
  if parseFailed c
    then do
      putStrLn "\nParsing failed\n"
      putStrLn "\nRemaining input:\n"
      print (getRemainingInput c)
    else do
      putStrLn "\nParsing success!\n"
      let s = execProgr emptyState (getParsedCommands c)
      putStrLn "\nInput Program\n"
      putStrLn p
      putStrLn "\nRepresentation of the program:\n"
      print (getParsedCommands c)
      putStrLn "\nState of the memory:\n"
      print s
