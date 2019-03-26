{--
This is the basic interface of the game:
need to add more options about the different rooms
need to add more options inside the function description

Further development could be utilized the type declaration of the new data structure to store
the default rooms' attributes, and enable the user to have the interactions with the rooms' objects,
for example, pick up the swords, etc.
--}
import System.IO 

game :: IO ()
game = do putStrLn "Input which direction "
          direction <- getLine
          putStrLn "input which room are you going to enter:"
          play direction

play :: String -> IO ()
play direction =
   do guess <- getLine
      if guess == "quit" then
         putStrLn "You won/game over!"
      else
         do putStrLn (match direction guess)
            play direction

match :: String -> String -> String
match xs ys = description xs ys

--need to add more options of the rooms, for example
-- description "s" "room4"= "you are inside room4"
description :: String->String -> String
description "w" "room1" =
    "You are in a room1.  To the north is the dark mouth\n" ++
    "of a room2; to the south is a small room5.  Your\n" ++
    "assignment, should you decide to accept it, is to\n" ++
    "recover the famed Bar-Abzad ruby and return it to\n" ++
    "this room3."
description "w" "room2" = "Congratulations!!  You have recovered the ruby and won the game."
description "s" "room1" =
    "You are in the mouth of a dank room2.  The exit is to\n" ++
    "the south; there is a large, dark, round passage to\n" ++
    "the east."
--description "s" "room4"= "you are inside room4"
description _ _ = "you can't see anything."




