import System.IO

main = loop

loop = 
  do iseof <- hIsEOF stdin
     if iseof 
        then return ()
        else 
          do str <- hGetLine stdin
             hPutStrLn stdout str
             loop


