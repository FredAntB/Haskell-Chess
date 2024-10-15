module Main where

import GHC.Float
import GHC.IO
import Data.ByteString (readFile)
import PngToPic
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game

data Side = White | Black | None deriving Eq
data Piece = Pawn | Tower | Knight | Bishop | King | Queen | Empty deriving Eq
data Tile = Open Picture | Piece Piece Picture Side

type Location = (Int, Int)
type Coordinates = (Float, Float)
type State = Int -- -1 if moving a piece, 1 if choosing a piece.
type GameState = Int -- 0 if displaying menu, 1 if currently playing

type Row = [Tile]
type Board = [Row]

data Game = Game {
   current_player :: Side,
   board :: Board,
   game_over :: Bool,
   winner :: String,
   mousePos :: Coordinates,
   choose_move :: State,
   current_location :: Location,
   playing :: GameState
}

rf = Data.ByteString.readFile

-- GENERAL IMAGE PATH
img_path :: String
img_path = "src/sprites"

-- LOCATIONS OF EACH IMAGE
loc_img_white_pawn   = img_path ++ "/W_pawn.png"
loc_img_white_tower  = img_path ++ "/W_tower.png"
loc_img_white_knight = img_path ++ "/W_knight.png"
loc_img_white_bishop = img_path ++ "/W_bishop.png"
loc_img_white_king   = img_path ++ "/W_king.png"
loc_img_white_queen  = img_path ++ "/W_queen.png"

loc_img_black_pawn   = img_path ++ "/B_pawn.png"
loc_img_black_tower  = img_path ++ "/B_tower.png"
loc_img_black_knight = img_path ++ "/B_knight.png"
loc_img_black_bishop = img_path ++ "/B_bishop.png"
loc_img_black_king   = img_path ++ "/B_king.png"
loc_img_black_queen  = img_path ++ "/B_queen.png"

-- return a picture given a location of it
getPic :: String -> IO Picture
getPic filename = do {
   png <- rf filename;
   return $ pngToPic png;
}

img_scale :: Float
img_scale = squareSide / 16

-- PICTURES OF EACH IMAGE
pic_white_pawn   = color (makeColor 1 1 1 5) (scale img_scale img_scale (rotate 180 (unsafePerformIO $ getPic loc_img_white_pawn)))
pic_white_tower  = color (makeColor 1 1 1 5) (scale img_scale img_scale (rotate 180 (unsafePerformIO $ getPic loc_img_white_tower)))
pic_white_knight = color (makeColor 1 1 1 5) (scale img_scale img_scale (rotate 180 (unsafePerformIO $ getPic loc_img_white_knight)))
pic_white_bishop = color (makeColor 1 1 1 5) (scale img_scale img_scale (rotate 180 (unsafePerformIO $ getPic loc_img_white_bishop)))
pic_white_king   = color (makeColor 1 1 1 5) (scale img_scale img_scale (rotate 180 (unsafePerformIO $ getPic loc_img_white_king)))
pic_white_queen  = color (makeColor 1 1 1 5) (scale img_scale img_scale (rotate 180 (unsafePerformIO $ getPic loc_img_white_queen)))

pic_black_pawn   = color (makeColor 0 0 0 5) (scale img_scale img_scale (unsafePerformIO $ getPic loc_img_black_pawn))
pic_black_tower  = color (makeColor 0 0 0 5) (scale img_scale img_scale (unsafePerformIO $ getPic loc_img_black_tower))
pic_black_knight = color (makeColor 0 0 0 5) (scale img_scale img_scale (unsafePerformIO $ getPic loc_img_black_knight))
pic_black_bishop = color (makeColor 0 0 0 5) (scale img_scale img_scale (unsafePerformIO $ getPic loc_img_black_bishop))
pic_black_king   = color (makeColor 0 0 0 5) (scale img_scale img_scale (unsafePerformIO $ getPic loc_img_black_king))
pic_black_queen  = color (makeColor 0 0 0 5) (scale img_scale img_scale (unsafePerformIO $ getPic loc_img_black_queen))

getPicFromTile :: Tile -> Picture
getPicFromTile (Piece _ pic _) = pic
getPicFromTile (Open pic)      = pic

----- LOGICA DEL JUEGO
swapSides :: Side -> Side
swapSides White = Black
swapSides Black = White

checkValidRow :: Int -> Bool
checkValidRow n = n `elem` [1..8]

checkValidColumn :: Char -> Bool
checkValidColumn c = c `elem` ['A'..'H']

letterToNumber :: Char -> Int
letterToNumber 'A' = 0
letterToNumber 'B' = 1
letterToNumber 'C' = 2
letterToNumber 'D' = 3
letterToNumber 'E' = 4
letterToNumber 'F' = 5
letterToNumber 'G' = 6
letterToNumber 'H' = 7

genPawns :: Side -> Row
genPawns s = replicate 8 t where
   pic = if s==White then pic_white_pawn else pic_black_pawn
   t = Piece Pawn pic s

genStarterPieces :: Side -> Board
genStarterPieces White = [genPawns White, [tower, knight, bishop, queen, king, bishop, knight, tower]] where
   tower  = Piece Tower pic_white_tower White
   knight = Piece Knight pic_white_knight White
   bishop = Piece Bishop pic_white_bishop White
   queen  = Piece Queen pic_white_queen White
   king   = Piece King pic_white_king White

genStarterPieces Black = [[tower, knight, bishop, queen, king, bishop, knight, tower], genPawns Black] where
   tower  = Piece Tower pic_black_tower Black
   knight = Piece Knight pic_black_knight Black
   bishop = Piece Bishop pic_black_bishop Black
   queen  = Piece Queen pic_black_queen Black
   king   = Piece King pic_black_king Black

genStarterPieces None = replicate 4 (replicate 8 t) where
   pic = Blank
   t = Open pic

genPieceStarterBoard :: Board
genPieceStarterBoard = top ++ middle ++ bottom where
   top    = genStarterPieces Black
   middle = genStarterPieces None
   bottom = genStarterPieces White

isEmpty :: Tile -> Bool
isEmpty (Open _) = True
isEmpty _        = False

isTower :: Tile -> Bool
isTower (Piece Tower _ _) = True
isTower _                 = False

castling_moves = [(0, 2), (0, 6), (7, 2), (7, 6)]

isCastlingMove :: Tile -> Location -> Bool
isCastlingMove (Piece King _ _) l = l `elem` castling_moves
isCastlingMove _ _                = False

getTileInfo :: Tile -> (Piece, Side)
getTileInfo (Piece p _ sd) = (p,sd)
getTileInfo (Open _) = (Empty, None)
{-
getColumn :: IO Int
getColumn = do {
   putStrLn "Ingrese la columna seleccionada A-H:";
   col <- getLine;
   if checkValidColumn (head col) then return $ letterToNumber (head col) else return (-1);
}

getColumnMask :: IO Int
getColumnMask = do {
   res <- getColumn;
   if res /= -1 then return res else getColumnMask;
}

getRow :: IO Int
getRow = do {
   putStrLn "Ingrese la fila seleccionada 1-8:";
   row <- getLine;
   if checkValidRow (read row) then return $ (read row) - 1 else return (-1);
}

getRowMask :: IO Int
getRowMask = do {
   res <- getRow;
   if res /= -1 then return res else getRowMask;
}

updateTile :: Tile -> Piece -> Side -> Tile
updateTile t p s = t { piece=p, side=s }

getPieceAndSideFromTile :: Tile -> (Piece, Side)
getPieceAndSideFromTile t = (piece t, side t)

getLocation :: IO Location
getLocation = do
   row <- getRowMask
   col <- getColumnMask
   return (col, row)

getPieceLocationMask :: Board -> Side -> IO Location
getPieceLocationMask b sd = do
   loc <- getLocation
   let t = getTileAt b loc
   if checkValidPiece t sd then return loc else getPieceLocationMask b sd

getDestinyLocationMask :: Board -> Side -> [Location] -> IO Location
getDestinyLocationMask b sd destinies = do
   loc <- getLocation
   let t = getTileAt b loc
   let valid_space = checkValidPiece t (swapSides sd) || isEmpty t
   let valid_dest  = loc `elem` destinies
   if valid_space && valid_dest then return loc else getDestinyLocationMask b sd destinies
-}

getColumnInRowAt :: Row -> Int -> Tile
getColumnInRowAt row i = row !! i

getRowAt :: Board -> Int -> Row
getRowAt b i = b !! i

checkValidPiece :: Tile -> Side -> Bool
checkValidPiece (Piece _ _ sd) s = sd==s
checkValidPiece _ _              = False

getTileAt :: Board -> Location -> Tile
getTileAt b (x,y) = getColumnInRowAt row x where
   row = getRowAt b y

removeTileFromRowAt :: Row -> Int -> ([Tile], [Tile])
removeTileFromRowAt r i = (izq, der) where
   (izq, der') = splitAt i r
   der = tail der'

removeTileAt :: Board -> Location -> ([[Tile]], ([Tile], [Tile]), [[Tile]])
removeTileAt b (x,y) = (izq, res, der) where
   (izq,der') = splitAt y b
   der = tail der'
   middle = head der'
   res = removeTileFromRowAt middle x

replaceTileAt :: Board -> Tile -> Location -> Board
replaceTileAt b t l = izq ++ [row] ++ der where
   (izq,(i_row,d_row),der) = removeTileAt b l
   row = i_row ++ [t] ++ d_row

checkValidDestiny :: Location -> Bool
checkValidDestiny (x,y) = (x >= 0 && x <= 7) && (y >= 0 && y <= 7)

checkPieceCollision :: Side -> Tile -> Bool
checkPieceCollision current_side (Piece _ _ sd) = sd /= current_side
checkPieceCollision _ _                         = False

genNextStepMovement :: Location -> Int -> Int -> Location
genNextStepMovement (x,y) i_x i_y = (x+i_x, y+i_y)

genCruxMovement :: Location -> [Location]
genCruxMovement l = [up, down, left, right] where
   up = genNextStepMovement l 0 1
   down = genNextStepMovement l 0 (-1)
   left = genNextStepMovement l (-1) 0
   right = genNextStepMovement l 1 0

genXMovement :: Location -> [Location]
genXMovement l = [up_left, up_right, down_left, down_right] where
   up_left = genNextStepMovement l (-1) 1
   up_right = genNextStepMovement l 1 1
   down_left = genNextStepMovement l (-1) (-1)
   down_right = genNextStepMovement l 1 (-1)

moveWhileEmpty :: Board -> Location -> Int -> Int -> Location
moveWhileEmpty _ (-1, _) _ _ = (-1, -1)
moveWhileEmpty _ (_, -1) _ _ = (-1, -1)
moveWhileEmpty _ (8, _) _ _ = (-1, -1)
moveWhileEmpty _ (_, 8) _ _ = (-1, -1) 
moveWhileEmpty b l@(x, y) i_x i_y
   |  isEmpty t = moveWhileEmpty b (x+i_x, i_y+y) i_x i_y
   | otherwise  = l
   where t = getTileAt b l

genCastling :: Location -> Side -> Board -> [Location]

genCastling l@(0, y) sd b = left_castling ++ right_castling where
   left  = moveWhileEmpty b l 0 (-1)
   right = moveWhileEmpty b l 0 1
   valid_l = left == (0,0)
   valid_r = right == (0,7)
   l_tower = if valid_l then getTileAt b left else Open Blank
   r_tower = if valid_r then getTileAt b right else Open Blank
   left_castling = if isTower l_tower then [(0, y-2)] else []
   right_castling = if isTower r_tower then [(0, y+2)] else []

genCastling l@(7, y) sd b = left_castling ++ right_castling where
   left  = moveWhileEmpty b l 0 (-1)
   right = moveWhileEmpty b l 0 1
   valid_l = left == (7,0)
   valid_r = right == (7,7)
   l_tower = if valid_l then getTileAt b left else Open Blank
   r_tower = if valid_r then getTileAt b right else Open Blank
   left_castling = if isTower l_tower then [(7, y-2)] else []
   right_castling = if isTower r_tower then [(7, y+2)] else []

genCastling _ _ _ = []

genLinearMove :: Location -> Int -> Int -> Side -> Board -> [Location]
genLinearMove l i_x i_y sd b
   | (not.checkValidDestiny) next_pos = []
   | sd == next_side                  = []
   | checkPieceCollision sd t         = [next_pos]
   | otherwise                        = next_pos : (genLinearMove next_pos i_x i_y sd b)
   where next_pos = genNextStepMovement l i_x i_y
         t = getTileAt b next_pos
         (_, next_side) = getTileInfo t

genTowerMovement :: Location -> Side -> Board -> [Location]
genTowerMovement l sd b = full_movement where
   up = genLinearMove l 0 1 sd b
   left = genLinearMove l (-1) 0 sd b
   right = genLinearMove l 1 0 sd b
   down = genLinearMove l 0 (-1) sd b
   full_movement = up ++ left ++ right ++ down

genBishopMovement :: Location -> Side -> Board -> [Location]
genBishopMovement l sd b = full_movement where
   top_left = genLinearMove l (-1) (-1) sd b
   top_right = genLinearMove l (-1) 1 sd b
   bottom_left = genLinearMove l 1 (-1) sd b
   bottom_right = genLinearMove l 1 1 sd b
   full_movement = top_left ++ top_right ++ bottom_left ++ bottom_right

genKnightMovement :: Location -> Side -> Board -> [Location]
genKnightMovement l sd b = fully_valid_moves where
   up = [genNextStepMovement l (-1) (-2), genNextStepMovement l 1 (-2)]
   left = [genNextStepMovement l (-2) (-1), genNextStepMovement l (-2) 1]
   right = [genNextStepMovement l 2 (-1), genNextStepMovement l 2 1]
   bottom = [genNextStepMovement l (-1) 2, genNextStepMovement l 1 2]
   full_moves = up ++ left ++ right ++ bottom
   valid_moves = filter checkValidDestiny full_moves
   fully_valid_moves = filter (\nex_pos -> checkPieceCollision sd (getTileAt b nex_pos) || isEmpty (getTileAt b nex_pos)) valid_moves

genPieceMovement :: Location -> Piece -> Side -> Board -> [Location]

genPieceMovement l@(_,6) Pawn White b = full_moves where
   moves = [genNextStepMovement l 0 (-2), genNextStepMovement l 0 (-1)]
   forward_moves = filter checkValidDestiny moves
   valid_forward_moves = filter (\l -> isEmpty (getTileAt b l)) forward_moves
   diagMoves = [genNextStepMovement l (-1) (-1), genNextStepMovement l 1 (-1)]
   diagonal_moves = filter checkValidDestiny diagMoves
   valid_diag_moves = filter (\l -> checkPieceCollision White (getTileAt b l) ) diagonal_moves
   full_moves = valid_forward_moves ++ valid_diag_moves

genPieceMovement l Pawn White b = full_moves where
   moves = [genNextStepMovement l 0 (-1)]
   forward_moves = filter checkValidDestiny moves
   valid_forward_moves = filter (\l -> isEmpty (getTileAt b l)) forward_moves
   diagMoves = [genNextStepMovement l (-1) (-1), genNextStepMovement l 1 (-1)]
   diagonal_moves = filter checkValidDestiny diagMoves
   valid_diag_moves = filter (\l -> checkPieceCollision White (getTileAt b l) ) diagonal_moves
   full_moves = valid_forward_moves ++ valid_diag_moves

genPieceMovement l@(_,1) Pawn Black b = full_moves where
   moves = [genNextStepMovement l 0 2, genNextStepMovement l 0 1]
   forward_moves = filter checkValidDestiny moves
   valid_forward_moves = filter (\l -> isEmpty (getTileAt b l)) forward_moves
   diagMoves = [genNextStepMovement l (-1) 1, genNextStepMovement l 1 1]
   diagonal_moves = filter checkValidDestiny diagMoves
   valid_diag_moves = filter (\l -> checkPieceCollision Black (getTileAt b l) ) diagonal_moves
   full_moves = valid_forward_moves ++ valid_diag_moves

genPieceMovement l Pawn Black b = full_moves where
   moves = [genNextStepMovement l 0 1]
   forward_moves = filter checkValidDestiny moves
   valid_forward_moves = filter (\l -> isEmpty (getTileAt b l)) forward_moves
   diagMoves = [genNextStepMovement l (-1) 1, genNextStepMovement l 1 1]
   diagonal_moves = filter checkValidDestiny diagMoves
   valid_diag_moves = filter (\l -> checkPieceCollision Black (getTileAt b l) ) diagonal_moves
   full_moves = valid_forward_moves ++ valid_diag_moves

genPieceMovement l Tower sd b = genTowerMovement l sd b

genPieceMovement l Knight sd b = genKnightMovement l sd b

genPieceMovement l Bishop sd b = genBishopMovement l sd b

genPieceMovement l Queen sd b = crux_mov ++ x_mov where
   crux_mov = genTowerMovement l sd b
   x_mov = genBishopMovement l sd b

genPieceMovement l King sd b = valid_movements where
   x_mov = genXMovement l
   crux_move = genCruxMovement l --castling_move = genCastling l sd b
   full_movement = x_mov ++ crux_move -- ++ castling_move
   in_boundaries_movement = filter checkValidDestiny full_movement
   valid_movements = filter (\l -> checkPieceCollision sd (getTileAt b l)) in_boundaries_movement

genPieceMovement _ Empty _ _ = []

genTileMovement :: Board -> Location -> [Location]
genTileMovement b l = genPieceMovement l p sd b where
   t = getTileAt b l 
   (p, sd) = getTileInfo t

countKings :: Board -> Int -> Int -> Int
countKings b _ 8 = 0
countKings b 8 i_y = 0 + countKings b 0 (i_y + 1)
countKings b i_x i_y = val + (countKings b (i_x + 1) i_y) where
   t = getTileAt b (i_x, i_y)
   (p,_) = getTileInfo t
   val = if p == King then 1 else 0

gameOver :: Board -> Bool
gameOver b = (countKings b 0 0) == 1

findRemainingKing :: Board -> Int -> Int -> Side
findRemainingKing b _ 8 = None
findRemainingKing b 8 i_y = findRemainingKing b 0 (i_y+1)
findRemainingKing b i_x i_y = if p == King then sd else findRemainingKing b (i_x+1) i_y where
   t = getTileAt b (i_x, i_y)
   (p, sd) = getTileInfo t

getWinner :: Board -> String
getWinner b = if sd == White then "Felicidades Jugador 1" else "Felicidades Jugador 2" where
   sd = findRemainingKing b 0 0
----- FIN DE LOGICA DEL JUEGO

----- LOGICA GLOSS
width :: Int
width = 600

height :: Int
height = 600

x0 :: Float
x0 = - (int2Float width) / 2

y0 :: Float
y0 = (int2Float width) / 2

squareSide :: Float
squareSide = (int2Float height) / 8

playButtonRadius :: Float
playButtonRadius = (int2Float height) / 6

window :: Display
window = InWindow "Chess game" (width, height) (350, 0)

background :: Color
background = makeColor 0.4 0 0 1

square :: Picture
square = color (makeColor 0.8 0 0.2 1) (polygon [(0,0), (0,squareSide),(squareSide,squareSide),(squareSide, 0)])

beigeColor :: Color
beigeColor = (makeColor 1 0.8 0.7 1)

playSign :: Picture
playSign = translate x y sign where
   sign = color beigeColor (polygon [(0,0), (0, playButtonRadius), (playButtonRadius, playButtonRadius/2)])
   x = playButtonRadius / 3 * (-1)
   y = playButtonRadius / 2 * (-1)

circlePlayButton :: Picture
circlePlayButton = color beigeColor (circle playButtonRadius)

playButton :: Picture
playButton = Pictures [circlePlayButton, playSign]

gameTitle :: Picture
gameTitle = scale 0.7 0.7 (translate x y title) where
   title = color beigeColor (text "CHESS")
   x = squareSide * (-2.5)
   y = int2Float height/(2.5)

menu :: Picture
menu = Pictures [playButton, gameTitle]

checkValidCoordPlayButton :: Coordinates -> Bool
checkValidCoordPlayButton (x,y) = (x >= (playButtonRadius * (-1)) && x <= playButtonRadius) && (y >= (playButtonRadius * (-1)) && y <= playButtonRadius)

genLine :: Int -> Int -> Picture -> [Picture]

genLine 8 _ _ = []
genLine fila 9 pic = genLine (fila+1) 0 pic
genLine fila 8 pic = genLine (fila+1) 1 pic
genLine fila columna pic = (translate (x0 + ((int2Float columna)*squareSide)) (y0 - ((int2Float fila)*squareSide) - squareSide) pic) : (genLine fila (columna+2) pic)

genSquares :: Int -> Int -> Int -> Picture -> [Picture]
genSquares 8 _ _ _ = []
genSquares fila columna incremento pic = (genLine fila columna pic)

drawBoard :: Picture
drawBoard = Pictures (genSquares 0 0 1 square)

drawTile :: Int -> Int -> Board -> [Picture]
drawTile _ 8 b     = []
drawTile 8 i_y b   = drawTile 0 (i_y+1) b
drawTile i_x i_y b = pic : drawTile (i_x+1) i_y b where
   t = getTileAt b (i_x, i_y)
   p = getPicFromTile t
   pic = translate (x0 + ((int2Float i_x)*squareSide) + squareSide/2) (y0 - ((int2Float i_y)*squareSide) - squareSide/2) p

drawPieces :: Board -> Picture
drawPieces b = Pictures (drawTile 0 0 b)

starterBoard :: Board
starterBoard = genPieceStarterBoard

drawFullBoard :: Board -> Picture
drawFullBoard b = Pictures [drawBoard, drawPieces b]

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (MouseButton LeftButton) Up _ (x,y)) game = if play == 0 then (if checkValidCoordPlayButton (x,y) then game { playing=1 } else game) else game { mousePos = (x+300, 600-(y+300)) } where
   play = playing game
handleKeys _ game = game

coordinateToLocation :: Coordinates -> Location
coordinateToLocation (x,y) = (x', y') where
   x' = float2Int (x / squareSide)
   --x' = if x'' < 0 then x'' * (-1) else x''
   y' = float2Int (y / squareSide)
   --y' = if y'' < 0 then y'' * (-1) else y''

checkValidCoordinateAndSelection :: Coordinates -> Board -> Side -> State -> Location -> Bool
checkValidCoordinateAndSelection (-1,-1) _ _ _ _ = False
checkValidCoordinateAndSelection cord b sd st prev_loc = if st == (-1) then checkValidPiece t sd else res where
   loc = coordinateToLocation cord
   t = getTileAt b loc -- if st == (-1) then getTileAt b loc else getTileAt b prev_loc
   l = if st == 1 then genTileMovement b prev_loc else []
   res = loc `elem` l

movePiece :: Board -> Location -> Location -> Board
movePiece b prev_loc loc = replaceTileAt (replaceTileAt  b (Open Blank) prev_loc) t loc where
   t = getTileAt b prev_loc

castlePiece :: Board -> Location -> Location -> Board

castlePiece b prev_loc@(0, y) loc@(0, 2) = final_board where
   moved_king = movePiece b prev_loc loc
   final_board = movePiece moved_king (0,0) (0, y-1)

castlePiece b prev_loc@(0, y) loc@(0, 6) = final_board where
   moved_king = movePiece b prev_loc loc
   final_board = movePiece moved_king (0,7) (0, y+1)

castlePiece b prev_loc@(7, y) loc@(7, 2) = final_board where
   moved_king = movePiece b prev_loc loc
   final_board = movePiece moved_king (7,0) (7, y-1)

castlePiece b prev_loc@(7, y) loc@(7, 6) = final_board where
   moved_king = movePiece b prev_loc loc
   final_board = movePiece moved_king (7,7) (7, y+1)

updateGame :: Game -> Game
updateGame game = if new_go then initialGameState else game { current_player=new_sd, board=new_board, game_over=new_go, winner=wn, mousePos=(-1,-1), choose_move=new_state, current_location=new_loc } where
   sd = current_player game
   b = board game
   go = game_over game
   wnnr = winner game
   cord = mousePos game
   state = choose_move game
   prev_loc = current_location game
   new_sd = if state == (-1) then sd else swapSides sd
   loc = coordinateToLocation cord
   t = if state == (-1) then Open Blank else getTileAt b prev_loc
   new_state = state * (-1)
   new_board= if prev_loc == (-1, -1) then b else movePiece b prev_loc loc --(if isCastlingMove t loc then castlePiece b prev_loc loc else movePiece b prev_loc loc)
   new_loc = if prev_loc == (-1, -1) then loc else (-1, -1)
   new_go = gameOver new_board
   wn= if new_go then getWinner new_board else ""

update :: Float -> Game -> Game
update _ game = new_game where-- game { current_player=new_sd, board=new_board, game_over=new_go, winner=new_wn, mousePos=(-1,-1), st=new_state } where
   mp = mousePos game
   sd = current_player game
   b = board game
   curr_state = choose_move game
   prev_loc = current_location game
   new_game = if checkValidCoordinateAndSelection mp b sd curr_state prev_loc then updateGame game else game

initialGameState :: Game
initialGameState = Game {
   current_player = White,
   board = genPieceStarterBoard,
   game_over = False,
   winner = "",
   mousePos = (-1, -1),
   choose_move = -1,
   current_location = (-1, -1),
   playing = 0
}

drawGame :: Game -> Picture
drawGame game = if play == 1 then drawFullBoard b else menu where
   b = board game
   play = playing game

fps :: Int
fps = 60
----- FIN LOGICA GLOSS

----- MAIN FUNCTION
main :: IO ()
--main = putStrLn $ concat $ map (\row -> concat $ map show row) starterBoard
--main = display window background drawFullBoard
main = play window background fps initialGameState drawGame handleKeys update
----- END MAIN FUNCTION
