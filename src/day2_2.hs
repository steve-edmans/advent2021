{-# LANGUAGE NamedFieldPuns #-}

import Data.Char

data Action = Up Int
            | Down Int
            | Forward Int deriving (Show, Eq)

instance Read Action where
    readsPrec _ input =
        let (direction, (_:rest)) = span isLetter input
            (amountS, rest2) = span isDigit rest
            amount = read amountS :: Int
            in
          case direction of
            "up"      -> [(Up amount, rest2)]
            "down"    -> [(Down amount, rest2)]
            "forward" -> [(Forward amount, rest2)]
            _         -> []

data Position = Position { horizontal :: Int, depth :: Int, aim :: Int } deriving (Show, Eq) 

moveUp :: Int -> Position -> Position
moveUp change (pos@Position{ aim }) = pos{ aim = (aim - change) }

moveDown :: Int -> Position -> Position
moveDown change (pos@Position{ aim }) = pos{ aim = (aim + change) }

moveForward :: Int -> Position -> Position
moveForward change (pos@Position{ horizontal, depth, aim }) = pos{ horizontal = (horizontal + change), depth = (depth + (aim * change)) }

processAction :: Position -> Action -> Position
processAction pos (Up amount)      = moveUp      amount pos
processAction pos (Down amount)    = moveDown    amount pos
processAction pos (Forward amount) = moveForward amount pos

process filename = do
    content <- readFile filename
    let stringData = lines content
    let actions = map read stringData :: [Action]
    let finalPosition = foldl processAction (Position { horizontal = 0, depth = 0, aim = 0 }) actions
    let answer = (horizontal finalPosition) * (depth finalPosition)
    print answer


