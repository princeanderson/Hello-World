{--
This is the basic interface of the game:
need to add more options about the different rooms
need to add more options inside the function description

Further development could be utilized the type declaration of the new data structure to store
the default rooms' attributes, and enable the user to have the interactions with the rooms' objects,
for example, pick up the swords, etc.
--}
import System.IO 

instructions =
    "Enter commands\n" ++
    "Available commands are:\n" ++
    "n  s  e  w     -- enter to go in that direction.\n" ++
    "quit               -- to end the game and quit.\n\n"++
	"You are in Text Adventure Palace. Enter GO!!.\n"


game :: IO ()
game = do putStrLn "Input which direction "
          direction <- getLine
          putStrLn "input which room are you going to enter:"
          play direction

play :: String -> IO ()
play direction =
   do guess <- getLine
      if guess == "quit" then
         putStrLn "Game over!"
      else
         do putStrLn (match direction guess)
            play direction

match :: String -> String -> String
match xs ys = description xs ys

--need to add more options of the rooms, for example
-- description "s" "room4"= "you are inside room4"
description :: String->String -> String
description _ "n room5" =
    "You are in a room5, be ready for a battle, wear a gladiator pit, then go west."
description _ "w room1" = "Your enemy could be here, pick up a sword. \n" ++
	"There are  noises, are you scared? Are  you ready to kill your enemy? Yes, Then go east."
description _ "e room2" =
    "You are in room2, look ahead, your enemy is at the corner, find enemy and  start fighting, try to kill him .\n" ++
    "Well done, you have killed your enemy, finally go south.\n" ++
    "GAME OVER."

--description "s" "room4"= "you are inside room4"
description _ "s room10" = "Congratulations!!  Strip naked to have a cold bath.\n" ++
	"you are a lucky WINNER!!!!!"
description _ _ ="invalid, input again"




