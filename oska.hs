-- CPSC 312 Project 1 - Oska
--
-- Byron Henze #66809088
-- Ian Lavery #36506095

oska_t2i7 :: [String]->Char->Int->[String]
oska_t2i7 initBoard p ahead = stateSearch_t2i7 initBoard p ahead []

stateSearch_t2i7 :: [String]->Char->Int->[String]->[String]
stateSearch_t2i7 initBoard p ahead nextMove
        | initGameoverCheck_t2i7 initBoard p decideOpponent (numPieces_t2i7 0 initBoard 'b') (numPieces_t2i7 0 initBoard 'w') == True      = initBoard
        | otherwise                     = -- ???    
                where decideOpponent
                        | p =='w'          = 'b'
                        | p =='b'          = 'w'

-- CHECKING FOR GAME OVER ON INITIAL INPUT       
initGameoverCheck_t2i7 initBoard p o whiteCount blackNum
        | whiteCount == 0
          || blackNum == 0
          || parseForWin_t2i7 (head initBoard) 'b' blackNum 0 == True
          || parseForWhites_t2i7 initBoard whiteCount == True           = True 
        | otherwise                                                     = False 


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