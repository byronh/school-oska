-- CPSC 312 Project 1 - Oska
--
-- Byron Henze #66809088 l6f7
-- Ian Lavery #36506095 t2i7

--oska_t2i7 :: [String]->Char->Int->[String]
--oska_t2i7 initBoard p ahead = stateSearch_t2i7 initBoard p ahead []

--stateSearch_t2i7 :: [String]->Char->Int->[String]->[String]
--stateSearch_t2i7 initBoard p ahead nextMove
--        | initGameoverCheck_t2i7 initBoard p decideOpponent (numPieces_t2i7 0 initBoard 'b') (numPieces_t2i7 0 initBoard 'w') == True      = initBoard
--        | otherwise                     = -- ???    
--                where decideOpponent
--                        | p =='w'          = 'b'
--                        | p =='b'          = 'w'

-- CHECKING FOR GAME OVER ON INITIAL INPUT       
initGameoverCheck_t2i7 initBoard p o whiteCount blackNum
        | whiteCount == 0
          || blackNum == 0
          || parseLineForWin_t2i7 (head initBoard) 'b' blackNum 0 == True
          || parseForWhites_t2i7 initBoard whiteCount == True           = True 
        | otherwise                                                     = False 


-- Custom data type to represent an n-ary tree of board states

data StateTree = StateTree { state :: [String], stateTree :: [StateTree] } deriving (Show)


-- Generates a move tree of a given depth

generateMoveTree :: Int -> Char -> [String]-> StateTree
generateMoveTree depth whosTurn boardState
    | depth == 0    = StateTree boardState []
    | otherwise     = StateTree boardState (map (generateMoveTree (depth - 1) nextTurn) (generateNewStates_t2i7 boardState whosTurn))
        where nextTurn = if whosTurn == 'w' then 'b' else 'w'


-- Perform minimax evaluation algorithm

miniMax :: Char -> StateTree -> Float
miniMax 'w' (StateTree boardState []) = boardEval_t2i7 boardState 'w' 'b' 'w'
miniMax 'b' (StateTree boardState []) = boardEval_t2i7 boardState 'b' 'w' 'b'
miniMax 'w' (StateTree boardState children) = maximum (map (miniMax 'b') children)
miniMax 'b' (StateTree boardState children) = maximum (map (miniMax 'w') children)


{- ************************************** 
        STATIC BOARD EVALUATOR 
   ************************************** -}
boardEval_t2i7 board p o whosTurn 
        | boardEvalGameoverCheck_t2i7 board p o pCount oCount == p   =  100
        | boardEvalGameoverCheck_t2i7 board p o pCount oCount == o   = -100
        | otherwise             = (piecesEval_t2i7 board p o pCount oCount whosTurn rowNum 0) 
                                   + (positionEval_t2i7 board p o pCount oCount whosTurn rowNum)
               where pCount = numPieces_t2i7 0 board p
                     oCount = numPieces_t2i7 0 board o
                     rowNum = fromIntegral (length board)   


-- CHECKS FOR GAMEOVER SITUATION                
boardEvalGameoverCheck_t2i7 board p o pCount oCount
        | pCount == 0     = o
        | oCount == 0     = p
        | parseLineForWin_t2i7 (head board) 'b' whicheverBlackCount 0 == True   = whicheverBlack
        | parseForWhites_t2i7 board whicheverWhiteCount == True                 = whicheverWhite 
        | otherwise                                                             = '-'
                where whicheverBlack 
                        | p == 'b'       = p
                        | o == 'b'       = o
                      whicheverBlackCount 
                        | p == 'b'       = pCount
                        | o == 'b'       = oCount
                      whicheverWhite
                        | p == 'w'       = p
                        | o == 'w'       = o        
                      whicheverWhiteCount
                        | p == 'w'       = pCount
                        | o == 'w'       = oCount
                        
-- CHECKING FIRST LINE FOR BLACKS AND LAST LINE FOR WHITES       
parseForWhites_t2i7 initBoard whiteCount
        | tail initBoard==[]     = parseLineForWin_t2i7 (head initBoard) 'w' whiteCount 0 
        | otherwise              = parseForWhites_t2i7 (tail initBoard) whiteCount       

parseLineForWin_t2i7 initLine p pCount count
        | count==pCount                 = True
        | initLine == []                = False
        | head initLine == p            = parseLineForWin_t2i7 (tail initLine) p pCount (count+1)
        | otherwise                     = parseLineForWin_t2i7 (tail initLine) p pCount count
        
-- Counts number of pieces for specified colour 'p'
numPieces_t2i7 :: Float->[String]->Char->Float
numPieces_t2i7 pCount initBoard p
        | initBoard == []       = pCount
        | otherwise             = numPieces_t2i7 
                           (parseLineForPieces_t2i7 pCount (head initBoard) p) (tail initBoard) p
                           
parseLineForPieces_t2i7 :: Float-> String-> Char->Float                                              
parseLineForPieces_t2i7 pCount line p
        | line == []            = pCount
        | head line == p        = parseLineForPieces_t2i7 (pCount+1) (tail line) p
        | otherwise             = parseLineForPieces_t2i7 pCount (tail line) p                         
                        
-- PIECE EVALUATOR
-- Heuristic looking at number of pieces for each player and their placement in relation to the opponents side of the board
piecesEval_t2i7 :: [String]->Char->Char->Float->Float->Char->Float->Float->Float
piecesEval_t2i7 board p o pCount oCount whosTurn rowNum rowCount
        | board == []           = (pCount-oCount)
        | otherwise             = ((piecesEval_line_t2i7 (head board) p o pCount oCount whosTurn rowNum rowCount 0)
                                   + (piecesEval_t2i7 (tail board) p o pCount oCount whosTurn rowNum (rowCount+1)))
-- CALCULATES VALUE OF PIECES ON THE BOARD AND PLACEMENT
piecesEval_line_t2i7 :: String->Char->Char->Float->Float->Char->Float->Float->Float->Float
piecesEval_line_t2i7 line p o pCount oCount whosTurn rowNum rowCount count
        | line == []                    = count
        | head line == p && p=='b'      = piecesEval_line_t2i7 (tail line) p o pCount oCount whosTurn rowNum rowCount 
                                                (count+((((rowNum-1)-rowCount)/(pCount*0.5))*pBonus)) 
        | head line == p && p=='w'      = piecesEval_line_t2i7 (tail line) p o pCount oCount whosTurn rowNum rowCount
                                                (count+((rowCount/(pCount*0.5))*pBonus))      
        | head line == o && o=='b'      = piecesEval_line_t2i7 (tail line) p o pCount oCount whosTurn rowNum rowCount
                                                (count+(((rowCount-(rowNum-1))/(oCount*0.5))*oBonus))
        | head line == o && o=='w'      = piecesEval_line_t2i7 (tail line) p o pCount oCount whosTurn rowNum rowCount
                                                (count+(((rowCount*(-1))/(oCount*0.5))*oBonus))
        | otherwise                     = piecesEval_line_t2i7 (tail line) p o pCount oCount whosTurn rowNum rowCount count
                where pBonus
                        | whosTurn == p         = 2
                        | otherwise             = 1
                      oBonus
                        | whosTurn == o         = 2
                        | otherwise             = 1                                                                                   

-- POSITION EVALUATOR
-- Heuristic looking at position of each piece in relation to the opponents
positionEval_t2i7 board p o pCount oCount whosTurn rowNum = takeSlicesWhite_t2i7 board p o pCount oCount whosTurn rowNum 0
                                                          + takeSlicesWhite_t2i7 (map reverse board) p o pCount oCount whosTurn rowNum 0    
                                                          + takeSlicesBlack_t2i7 (reverse board) p o pCount oCount whosTurn rowNum 0
                                                          + takeSlicesBlack_t2i7 (map reverse (reverse board)) p o pCount oCount whosTurn rowNum 0

-- SLICER
-- SLICES BOARD INTO DIAGONALS AND CALCULATES VALUE OF SLICES BY THE MOVES THAT CAN BE MADE ON THEM
takeSlicesWhite_t2i7 board p o pCount oCount whosTurn rowNum columnCount
        | columnCount > (length (head board))           = 0
        | otherwise                                     = (evalSliceWhite_t2i7 (getSlice_t2i7 board rowNum columnCount 0 []) p o pCount oCount whosTurn)
                                                        + (takeSlicesWhite_t2i7 board p o pCount oCount whosTurn rowNum (columnCount+1))

takeSlicesBlack_t2i7 board p o pCount oCount whosTurn rowNum columnCount        
        | columnCount > (length (head board))           = 0
        | otherwise                                     = (evalSliceBlack_t2i7 (getSlice_t2i7 board rowNum columnCount 0 []) p o pCount oCount whosTurn)
                                                        + (takeSlicesBlack_t2i7 board p o pCount oCount whosTurn rowNum (columnCount+1))


evalSliceWhite_t2i7 slice p o pCount oCount whosTurn
        | slice == []                           = 0
        | (head slice) == 'w'                   = evalSegmentWhite + (evalSliceWhite_t2i7 (tail slice) p o pCount oCount whosTurn)
        | otherwise                             = evalSliceWhite_t2i7 (tail slice) p o pCount oCount whosTurn
                where evalSegmentWhite
                        | (take 3 slice) == "wb-" && p == 'w' && whosTurn == p          = 5
                        | (take 3 slice) == "wb-" && p == 'b' && whosTurn == o          = (-5)
                        | otherwise                                                     = 0                                                 

evalSliceBlack_t2i7 slice p o pCount oCount whosTurn
        | slice == []                           = 0
        | (head slice) == 'b'                   = evalSegmentBlack + (evalSliceBlack_t2i7 (tail slice) p o pCount oCount whosTurn)
        | otherwise                             = evalSliceBlack_t2i7 (tail slice) p o pCount oCount whosTurn
                where evalSegmentBlack
                        | (take 3 slice) == "bw-" && p == 'b' && whosTurn == p          = 5
                        | (take 3 slice) == "bw-" && p == 'w' && whosTurn == o          = (-5)
                        | otherwise                                                     = 0                                                 
                        
getSlice_t2i7 board rowNum columnCount rowCount slice        
        | board == []                           = slice
        | columnCount >= length(head board)     = slice
        | otherwise                             = elementTaken: getSlice_t2i7 (tail board) rowNum columnCount (rowCount+1) slice        
        where elementTaken    
                | rowCount <= midpoint                  = (head board)!!columnCount 
                | rowCount >  midpoint                  = oneOrTwo                      
                        where midpoint = length(last board)-2
                              oneOrTwo
                                | columnCount == 0      = (head board)!!(length(head board)-2)
                                | columnCount == 1      = last (head board)      


-- MOVE GENERATION

-- Generates all possible legal moves (slides and jumps) for the given player

generateNewStates_t2i7 :: [String] -> Char -> [[String]]
generateNewStates_t2i7 boardState player
    | player == 'w'  = generateNewStates_t2i7' boardState player (getPiecePositions_t2i7 boardState player)
    | otherwise      = map reverse (generateNewStates_t2i7' (reverse boardState) player (getPiecePositions_t2i7 (reverse boardState) player))

generateNewStates_t2i7' :: [String] -> Char -> [(Int, Int)] -> [[String]]
generateNewStates_t2i7' boardState player positions
    | null positions    = []
    | otherwise         = filter (\state -> not (null state)) newStates
        where newStates = concat [generateSlidesForPiece_t2i7 boardState (head positions),
                                  generateJumpsForPiece_t2i7 boardState player (head positions),
                                  generateNewStates_t2i7' boardState player (tail positions)]


-- Generate sliding moves for a given piece - requires the target space to be empty
-- If the piece is above midpoint, a piece at `i` can move to `i - 1` or `i` in the row below, if unoccupied and valid
-- If the piece is at/below midpoint, a piece at `i` can move to `i` or `i + 1` in the row below, if unoccupied and valid

generateSlidesForPiece_t2i7 :: [String] -> (Int, Int) -> [[String]]
generateSlidesForPiece_t2i7 boardState pos 
    | row < midpoint    = [movePiece_t2i7 boardState pos (row+1, col), movePiece_t2i7 boardState pos (row+1, col-1) ]
    | otherwise         = [movePiece_t2i7 boardState pos (row+1, col), movePiece_t2i7 boardState pos (row+1, col+1) ]
        where midpoint = length (head boardState) - 2 ;
              row = fst pos ;
              col = snd pos


-- Generate jumping moves for a given piece. There are six cases in total taking into account the opponent's piece
-- Requires opponent's piece to be in between the piece and its destination, the opponent is captured in the process
-- If the piece is at least 2 above midpoint, a piece at `i` can jump to `i` or `i - 2` two rows below, if unoccupied and valid
-- If the piece is 1 above midpoint, a piece at `i` can jump to `i - 1` or `i + 1` two rows below, if unoccupied and valid
-- If the piece is at/below midpoint, a piece at `i` can jump to `i` or `i + 2` two rows below, if unoccupied and valid

generateJumpsForPiece_t2i7 :: [String] -> Char -> (Int, Int) -> [[String]]
generateJumpsForPiece_t2i7 boardState player pos
    | row <  midpoint - 1 = [if enemyPieceAt (row+1, col)
                             then fst (replacePiece_t2i7 (movePiece_t2i7 boardState pos (row+2, col)) (row+1, col) '-') else [],
                             if enemyPieceAt (row+1, col-1)
                             then fst (replacePiece_t2i7 (movePiece_t2i7 boardState pos (row+2, col-2)) (row+1, col-1) '-') else []]
    | row == midpoint - 1 = [if enemyPieceAt (row+1, col-1)
                             then fst (replacePiece_t2i7 (movePiece_t2i7 boardState pos (row+2, col-1)) (row+1, col-1) '-') else [],
                             if enemyPieceAt (row+1, col)
                             then fst (replacePiece_t2i7 (movePiece_t2i7 boardState pos (row+2, col+1)) (row+1, col) '-') else []]
    | otherwise           = [if enemyPieceAt (row+1, col)
                             then fst (replacePiece_t2i7 (movePiece_t2i7 boardState pos (row+2, col)) (row+1, col) '-') else [],
                             if enemyPieceAt (row+1, col+1)
                             then fst (replacePiece_t2i7 (movePiece_t2i7 boardState pos (row+2, col+2)) (row+1, col+1) '-') else []] 
        where midpoint = length (head boardState) - 2 ;
              row = fst pos ;
              col = snd pos ;
              enemyPieceAt position = (not (outOfBounds boardState position)) && (value /= '-' && value /= player)
                where value = (boardState !! (fst position)) !! (snd position)


-- Determines whether a not a position is within the boundaries of the board

outOfBounds :: [String] -> (Int, Int) -> Bool
outOfBounds boardState pos =  fst pos < 0
                           || snd pos < 0
                           || fst pos >= length boardState
                           || snd pos >= length (boardState !! (fst pos))


-- Moves a piece from one position (row,column) to another position
-- Returns an empty list if that move is illegal

movePiece_t2i7 :: [String] -> (Int, Int) -> (Int, Int)-> [String]
movePiece_t2i7 boardState oldPos newPos
    | not (outOfBounds boardState newPos) && snd target == '-'  = fst target
    | otherwise                                                 = []
    where source = replacePiece_t2i7 boardState oldPos '-' ;
          target = replacePiece_t2i7 (fst source) newPos (snd source)


-- Replaces a piece (or a blank) at the specified (row,column) position (row,column) with another piece (or a blank)
-- Returns a tuple containing the new board state and the previous value at that position

replacePiece_t2i7 :: [String] -> (Int, Int) -> Char -> ([String], Char)
replacePiece_t2i7 boardState pos value
  | null boardState = ([], '-')
  | otherwise       = (replaceSegment_t2i7 boardState row [rowState], (boardState !! row) !! col)
      where rowState = replaceSegment_t2i7 (boardState !! row) col [value] ;
            row = fst pos ;
            col = snd pos


-- Returns a list of position tuples (row,column) for all pieces for the given player

getPiecePositions_t2i7 :: [String] -> Char -> [(Int, Int)]
getPiecePositions_t2i7 boardState player = getPiecePositions_t2i7' boardState player 0

getPiecePositions_t2i7' :: [String] -> Char -> Int -> [(Int, Int)]
getPiecePositions_t2i7' boardState player rowNum
    | null boardState = []
    | otherwise = concat [positions, positionsTail]
        where positions = getPiecesInRow_t2i7 (head boardState) player rowNum 0 ;
              positionsTail = getPiecePositions_t2i7' (tail boardState) player (rowNum + 1)

getPiecesInRow_t2i7 :: String -> Char -> Int -> Int -> [(Int, Int)]
getPiecesInRow_t2i7 rowState player rowNum colNum
    | null rowState             = []
    | head rowState == player   = (rowNum, colNum) : result
    | otherwise                  = result
        where result = getPiecesInRow_t2i7 (tail rowState) player rowNum (colNum + 1)


-- Adapted `replaceSegment` from PegPuzzle.hs

replaceSegment_t2i7 :: (Eq a, Num a) => [b] -> a -> [b] -> [b]
replaceSegment_t2i7 oldList pos segment
    | pos == 0  = segment ++ drop (length segment) oldList
    | otherwise = (head oldList) : (replaceSegment_t2i7 (tail oldList) (pos - 1) segment)
