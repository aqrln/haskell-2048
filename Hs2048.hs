import Data.List
import System.Random
import Control.Applicative

type Tile = Maybe Integer
newtype GameField = GameField [[Tile]]
                    deriving (Eq)

instance Show GameField where
    show (GameField gameField) =
         let showTile Nothing = ""
             showTile (Just x)  = show x
             maxTileLength =
                 maximum $ map (maximum . map (length . showTile)) gameField
             showRow row =
                 let s = '|' : (intercalate "|" $ map (decorate . showTile) row) ++ "|"
                  in s ++ '\n' : showLine (length s)
             showLine n = replicate n '-'
             showFullLine = showLine ((maxTileLength + 3) * (length $ head gameField) + 1)
             decorate s = ' ' : s ++ replicate (maxTileLength - length s + 1) ' '
          in intercalate "\n" $ showFullLine : map showRow gameField

gfmap :: ([[Tile]] -> [[Tile]]) -> GameField -> GameField
gfmap f (GameField gameFieldData) = GameField (f gameFieldData)

moveLeft :: GameField -> GameField
moveLeft = gfmap $ map moveRow where
    moveRow row = let (tiles, nothings) = partition (/= Nothing) row
                   in join tiles ++ nothings
                     where join [] = []
                           join (x:y:ys)
                             | x == y = ((+) <$> x <*> y) : join ys ++ [Nothing]
                           join (x:xs) = x : join xs

moveRight :: GameField -> GameField
moveRight = gfmap (map reverse) . moveLeft . gfmap (map reverse)

moveUp :: GameField -> GameField
moveUp = gfmap transpose . moveLeft . gfmap transpose

moveDown :: GameField -> GameField
moveDown = gfmap transpose . moveRight . gfmap transpose

getFreeTiles :: GameField -> [(Int, Int)]
getFreeTiles (GameField gf) =
    concat $ enumerateRows $ zip [0..] $ map freeInRow gf
    where enumerateRows = map (\(m, ns) -> [(m, n) | n <- ns])
          freeInRow = foldr (\(i, x) a -> if x == Nothing then i:a else a) []
                      . zip [0..]

addTileToRandomPos :: GameField -> Tile -> IO GameField
addTileToRandomPos gf@(GameField gfData) tile = do
    gen <- newStdGen
    let tiles = getFreeTiles gf
        (index, _) = randomR (0, length tiles - 1) gen :: (Int, StdGen)
        (i, j) = tiles !! index  
        (upper, row:lower) = splitAt i gfData
        (left, _:right) = splitAt j row
    return $ GameField $ upper ++ (left ++ tile:right):lower

hasAvailableMoves :: GameField -> Bool
hasAvailableMoves gameField = or $
                              map ((/= gameField) . ($ gameField))
                              [moveLeft, moveRight, moveUp, moveDown]

gameLoop :: GameField -> IO ()
gameLoop gameField = do
    gameField' <- addTileToRandomPos gameField $ Just 2
    performInput gameField'
      where
        performInput gameField = do
            print gameField
            if hasAvailableMoves gameField
               then putStrLn "Your command ([l]eft, [r]ight, [u]p, [d]own, [s]top)?"
               else putStrLn "Game over. Press [s] to exit."
            input <- getLine
            case input of "l" -> newMove moveLeft  gameField
                          "r" -> newMove moveRight gameField
                          "u" -> newMove moveUp    gameField
                          "d" -> newMove moveDown  gameField
                          "s" -> return ()
                          _   -> do putStrLn "Incorrect input!"
                                    performInput gameField
        newMove moveFn gameField = do
            let gameField' = moveFn gameField
            if gameField /= gameField' then gameLoop gameField'
                                       else performInput gameField

main = do
    putStrLn "2048 in Haskell!"
    putStrLn "Enter game field width:"
    width <- read <$> getLine :: IO Int
    putStrLn "Enter game field height:"
    height <- read <$> getLine :: IO Int
    let gameField = GameField $ replicate height $ replicate width Nothing
    gameLoop gameField
