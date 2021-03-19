module Main where

import Control.Monad
import Data.List
import System.Environment (getArgs)
import System.Random

-- =====
-- Points
-- =====

-- Player's starting points
startingPoints :: Int
startingPoints = 0

-- =====
-- Dice Roll
-- =====

-- Maximum attempts to roll your dice
maxAttempts :: Int
maxAttempts = 3

emptyRoll :: IO [Int]
emptyRoll = rollDice 0

doRoll :: IO [Int]
doRoll = rollDice 5

rollDice :: Int -> IO [Int]
rollDice n = replicateM n rollDie

rollDie :: IO Int
rollDie = randomRIO (1, 6)

doReroll :: [Int] -> [Int] -> IO [Int]
doReroll dice rerolledDice = rerollDice (dropDice dice rerolledDice)

rerollDice :: [Int] -> IO [Int]
rerollDice dice     | 5 <= length dice = return dice
                    | otherwise = do 
                            newDie <- rollDie
                            rerollDice (newDie:dice)
                        
dropDice :: Eq a => [a] -> [a] -> [a]
dropDice (x:xs) [] = x:xs
dropDice (x:xs) (y:ys) = if x == y then dropDice xs ys else x:xs

-- =====
-- Combinations
-- =====

-- Player's starting combination fields
startingScoreFields :: [Int]
startingScoreFields = []

addCombo :: Int -> [Int] -> Int
-- Sum of value from 1 to 6
addCombo combo dice | 0 < combo && combo <= 6 = sum $ filter (==combo) dice
-- Three of a kind
addCombo 7 dice = if isOfAKind 3 dice then sum dice else 0
-- Four of a kind
addCombo 8 dice = if isOfAKind 4 dice then sum dice else 0
-- Full House
addCombo 9 dice = if isFullHouse dice then 25 else 0
-- Small Straight
addCombo 10 dice = if isStraight (init $ rankedDice dice) || isStraight (tail $ rankedDice dice) then 30 else 0
-- Large Straight
addCombo 11 dice = if isStraight $ rankedDice dice then 40 else 0
-- Yahtzee
addCombo 12 dice = if isOfAKind 5 dice then 50 else 0
-- Chance
addCombo 13 dice = sum dice
-- Invalid Combo
addCombo _ _ = 0

--Sort and group the dice by value
sortedDice :: Ord a => [a] -> [[a]]
sortedDice dice = group $ sort dice

--Rank the dice by value. There are no duplicates
rankedDice :: Ord b => [b] -> [b]
rankedDice dice = head `map` sortedDice dice

--Get the quantity for each value
valueCount :: [Int] -> [Int]
valueCount dice = length `map` sortedDice dice

--Check if a value appears n times in the dice roll
isOfAKind :: Int -> [Int] -> Bool
isOfAKind n dice = (>=n) `any` valueCount dice

--Check if a full house is present
isFullHouse :: [Int] -> Bool
isFullHouse dice = (==2) `any` valueCount dice && (==3) `any` valueCount dice

--Check if a straight is present
isStraight :: (Ord a, Num a, Enum a) => [a] -> Bool
isStraight [] = True
isStraight [x] = True
isStraight (x:y:zs) = x < 6 && succ x == y && isStraight (y:zs)

--Cast a list of String to a list of Int
castListStringToInt :: [String] -> [Int]
castListStringToInt = map read

main :: IO ()
main = do 
    printInstructions
    loopGame startingScoreFields startingPoints emptyRoll 0 where
    loopGame :: [Int] -> Int -> IO [Int] -> Int -> IO ()
    loopGame scoreFields points roll currentAttempt = do
        input <- getLine  
        if 13 == length scoreFields
            then do
                print ("Game over. Your total score is " ++ show points ++ "!")
        else do
            currentRoll <- roll
            case words input of
                "roll":_    ->  do
                    if null currentRoll
                        then loopGame scoreFields points doRoll (succ currentAttempt)
                    else do
                        printRollError currentRoll
                        loopGame scoreFields points roll currentAttempt
                "dice":_    ->  do 
                    printDice currentRoll currentAttempt
                    loopGame scoreFields points roll currentAttempt
                "reroll":x  ->  do 
                    if currentAttempt < 3 && not (null currentRoll)
                        then case length $ castListStringToInt x of
                            l   | l == 5    -> loopGame scoreFields points doRoll (succ currentAttempt)
                                | l < 5     -> loopGame scoreFields points (doReroll currentRoll (castListStringToInt x)) (succ currentAttempt)                      
                            _ -> do
                                print "Invalid command"
                                loopGame scoreFields points roll currentAttempt
                    else loopGame scoreFields points roll currentAttempt          
                "combo":x   ->  do 
                    let combo = (read $ head x)::Int in
                        if combo `elem` scoreFields
                             then do
                                 printComboError
                                 printCombo scoreFields
                                 loopGame scoreFields points roll currentAttempt      
                        else do 
                            case combo of
                                c   | 0 < combo && combo <= 13  ->  do
                                                                        print ("Combo " ++ show combo ++ "filled in!")
                                                                        print ("You get " ++ show (addCombo combo currentRoll) ++ "points!")
                                                                        loopGame (combo:scoreFields) (addCombo combo currentRoll) emptyRoll currentAttempt
                                    | otherwise                 ->  do 
                                                                        print "Invalid Command"
                                                                        loopGame scoreFields points roll currentAttempt
                "score":_   ->  do
                    printCombo scoreFields
                    printScore points
                    loopGame scoreFields points roll currentAttempt
                "help":_ -> do
                    printInstructions
                    loopGame scoreFields points roll currentAttempt
                "stop":_ -> return ()
                _ -> do 
                    print "Invalid command"
                    loopGame scoreFields points roll currentAttempt

printInstructions :: IO ()
printInstructions = do
    print "Currently playing Yahtzee!"
    print "Commands: "
    print "Type 'help' to show these instructions again"
    print "Type 'roll' to do your first roll!"
    print "Type 'dice' to show current roll!"
    print "Type 'reroll x y z' to reroll your current roll! Example: 'reroll 2 3' will reroll the dice with 2 and 3."
    print "Type 'combo x' to fill in your score! See below for the possible combinations. Example: 'combo 7' will fill in score for Three of a Kind."
    print "Type 'score' to show total score!"
    print "Instructions: "
    print "You have 3 attempts to roll your dice. You roll five dice for your first attempt. For your second and third attempt you can keep some dice or reroll them all."
    print "Do your first attempt with 'roll' and your second and third attempt with 'reroll x y z'."
    print "After every attempt you can fill in a combinations to get points. You have to fill in a combinations each roll. You get 0 points if you fill in a combinations with the incorrect conditions"
    print "The game ends when you have filled in all 13 combinations"
    print "Possible combinations:"
    print "The combinations are as followed:"
    print "1  - Sum of all 1 of your roll" 
    print "2  - Sum of all 2 of your roll" 
    print "3  - Sum of all 3 of your roll" 
    print "4  - Sum of all 4 of your roll" 
    print "5  - Sum of all 5 of your roll" 
    print "6  - Sum of all 6 of your roll" 
    print "7  - Sum of all dice when you have a Three of a Kind" 
    print "8  - Sum of all dice when you have a Four of a Kind" 
    print "9  - When you have a Full House (one pair and Three of a Kind), you get 25 points" 
    print "10 - When you have a Small Straight (4 consecutive dice), you get 30 points" 
    print "11 - When you have a Large Straight (5 consecutive dice), you get 40 points" 
    print "12 - When you have Yahtzee (Five of a Kind), you get 50 points" 
    print "13 - Chance: Sum of all of your roll" 

printDice :: [Int] -> Int -> IO ()
printDice dice attempt = print ("Your current roll is " ++ show dice ++ "! This is attempt " ++ show attempt ++ "out of 3 attempts")

printCombo :: [Int] -> IO ()
printCombo scoreFields = print ("You have filled in the following combinations " ++ show scoreFields ++ "! See the instructions what each combination is")

printScore :: Int -> IO ()
printScore points = print ("You have a total of " ++ show points ++ " points!") 

printRollError :: [Int] -> IO ()
printRollError dice = print ("You can't roll now! Your current roll is " ++ show dice ++ "! To finish your current attempt, fill in an empty combo.")

printComboError :: IO ()
printComboError = print "This combinations is already filled in. Pick an empty combinations to fill in!"
