import Data.Char

--need more comments

nlowers = length ['a'..'z']
------------------- need to add code
--nUppers = 

-- converts a lower-case letter between 'a' and 'z' into the corresponding
-- integer between 0 and 25
let2int :: Char -> Int
let2int c = ord c - ord 'a'

------------------ need to add code
--let2int' :: Char -> Int
--let2int' c = 



-- perform the opposite conversion of the above
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

------------------ need to add code
--int2let' :: Int -> Char
--int2let' n = 

-- shift
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` nlowers)
--          | isUpper c = 
          | otherwise      = c

-- encode
encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]