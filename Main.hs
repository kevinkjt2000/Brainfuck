-- This program is an interpreter of the Brainfuck programming language

import Data.Array
import Data.Char
import System.Environment
import System.Exit
import System.IO

--Below table is taken from the Brainfuck wikipedia page
-- brainfuck command	C equivalent
-- (Program Start)	char array[infinitely large size] = {0};
--                  char *ptr=array;
-- >	++ptr;
-- <	--ptr;
-- +	++*ptr;
-- -	--*ptr;
-- .	putchar(*ptr);
-- ,	*ptr=getchar();
-- [	while (*ptr) {
-- ]	}

-- brainfuck' :: String -> Int -> Array Int Int -> IO ()
brainfuck' [] _ _ = return ()
brainfuck' (ch:input) ptr memory = do
    hFlush stdout
    case ch of '>' -> brainfuck' input (ptr+1) memory
               '<' -> brainfuck' input (ptr-1) memory
               '+' -> brainfuck' input ptr (memory // [(ptr, 1+(memory ! ptr))])
               '-' -> brainfuck' input ptr (memory // [(ptr, 1-(memory ! ptr))])
               '.' -> do putChar $ chr $ head memory
                         brainfuck' input ptr memory
               ',' -> do c <- getChar
                         brainfuck' input ptr (memory // [ptr, (ord c)])
               -- '[' -> 0
               -- ']' -> 0
               _ -> brainfuck' input ptr memory

brainfuck :: String -> IO ()
brainfuck input = brainfuck' input 0 (repeat 0)

usage = do name <- getProgName
           putStrLn $ "Usage: " ++ name ++ " input_file"
exit = exitWith ExitSuccess
die  = exitWith $ ExitFailure 1

main = do
    args <- getArgs
    if 1 /= length args
    then
        usage >> exit
    else do
        file <- readFile $ head args
        brainfuck file