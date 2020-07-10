-- File    : Proj1.hs
-- Author  : Yazad Jamshed Davur, <ydavur@student.unimelb.edu.au>
-- Purpose : Program to implement hider and searcher parts of the guessing
--            game as part of Project 1 COMP90048 Declarative Programming 2020

-- | The aim of the program for the searcher is to guess the exact locations 
--   of the 3 target ships on a fixed (4x8) grid with fewest possible guesses.  
--   We start off with a fixed educated guess and receive a feedback from the 
--   hider, using which the inconsistent target candidates are eliminated and  
--   a better guess is generated. The program will keep calling the next guess 
--   function until all 3 ships are exactly located.
--   The program has dependencies on the Data.Char, Data.List and Data.Maybe
--   modules.


-----------------------------Start of the Program-----------------------------

module Proj1 (Location, toLocation, feedback, GameState,
                initialGuess, nextGuess) where
    import Data.Char
    import Data.List
    import Data.Maybe
    
-----------------------------Data Definitions---------------------------------  

-- | The Location data type is defined as a column and a row of the (4 x 8)
--   grid where the column is a Char type and the row is an Int type. Any   
--   guess made in the program is a combination of 3 unique Location types.

    data Location = Location Char Int
        deriving (Eq)
  
 -- | The following instance declaration will show the Location data type as a
 --   list of two characters - column and row. Example "A1".
 
    instance Show Location where 
        show (Location col row) =  [col] ++ [(intToDigit row)]
        
 -- | The GameState type is simply a list of all unique possible target
 --   combinations. Each combination is a list of Strings that correspond to a
 --   location on the grid.
 
    type GameState =  [[String]]

-----------------------------Function Definitions------------------------------

-- | The toLocation function takes a String as input and returns a Location of
--   type Maybe. If the String is a valid location name, it returns Just the
--   Location else it returns Nothing.

    toLocation :: String -> Maybe Location
    toLocation string
        |and [elem col ['A'..'H'], elem row [1..4], length string == 2]
            =  Just (Location col row)
        |otherwise = Nothing
        where row = digitToInt (last string)
              col = head string

      ---------------------Hider Part of Game-------------------------

-- | The feedback function takes the target ship as a list of Locations and a
--   given guess as a list of Locations and returns a tuple of 3 integers
--   consisting of the number of ships exactly located, the number of guesses
--   that were exactly one space away from the ship and the number of guesses
--   that were exactly two spaces away from the ship in that order keeping in
--   mind that each guess is counted as its closest distance to any ship.

    feedback :: [Location] -> [Location] -> (Int, Int, Int)
    feedback target [] = (0, 0, 0)
    feedback target (g:guess)
        |elem g target = (a + 1, b, c)
        |nStepCheck target g 1 = (a, b + 1, c)
        |nStepCheck target g 2 = (a, b, c + 1)
        |otherwise = (a, b, c)
        where  (a, b, c) = feedback target guess

-- | The nStepCheck function is created as a helper function to the feedback
--   function. It takes as input the target ships as a list of Locations, a
--   single guess of type Location and an Int "n" denoting the number of
--   spaces from the guess and returns a Boolean indicating whether that guess
--   is within n spaces away from any ship using. This is achieved using
--   simple mathematical operations on the row numbers and "ord" values of the
--   columns.

    nStepCheck :: [Location] -> Location -> Int -> Bool
    nStepCheck [] guess n = False
    nStepCheck ((Location col_t row_t) : ts) (Location col_g row_g) (n)
        |((abs(ord(col_t) - ord(col_g))) <= n) && ((abs(row_t - row_g)) <= n)
            = True
        |otherwise = nStepCheck ts (Location col_g row_g) n

      --------------------Searcher Part of Game-----------------------


-- | The initialGuess function takes no inputs as arguments and returns a
--   an initial guess and the initial GameState in a tuple. The Initial Guess
--   combination is hard coded after computing the one that leads to least
--   number of avergage guesses over 4960 combinations (Avg 6.05 guesses). The
--   initial game state is defined as a list of all the 4960 combinations.

    initialGuess :: ([Location], GameState)
    initialGuess = ([Location 'A' 2, Location 'E' 4, Location 'H' 2],
                      allCombos grid 3)
          where grid = [[x] ++ [intToDigit y] | x <- ['A'..'H'], y <- [1..4]]

-- | allCombos is a helper function to the initialGuess function that is
--   designed to take as input all the grid cells as a list of Strings and an
--   Int indicating the length of each combination and returns a list of all
--   combinations (not permutations) of type GameState.

    allCombos ::  [String] -> Int -> GameState
    allCombos x 0 = [[]]
    allCombos [] n = []
    allCombos (g:gs) n = (map (g:) (allCombos gs (n-1))) ++ (allCombos gs n)

-- | The nextGuess function takes as input a tuple of the previous guess and
--   game state, and the feedback of this previous guess as returned by the
--   feedback function and returns a tuple of the nextGuess and the reduced
--   game state. The whole point of this function is to reduce the game state
--   by filtering based on the feedback. The next guess is chosen as the first
--   combination from the reduced gamestate. This function is called
--   repeatedly until the target is found.

    nextGuess :: ([Location], GameState) -> (Int, Int, Int) ->
                        ([Location], GameState)
                    
    nextGuess (prevGuess, prevState) (a, b, c) = (newGuess, reduced)
              
         where
              newGuess = map fromJust (map toLocation (head reduced))
              reduced  = keepCombos  (prevGuess, filtered)  (a, b, c)
              filtered = filterGS  (prevGuess,  prevState)  (a, b, c)
              
      
-- | The filterGS function takes the same input as the nextGuess function and
--   returns a filtered game state based on the feedback of the previous
--   guess. The filterGS function uses helper functions depending on the
--   different cases of feedback.

    filterGS :: ([Location], GameState) -> (Int, Int, Int) -> GameState
    filterGS (prevGuess, prevState) (a, b, c)
        |and [a == 0, b == 0, c == 0] = filtCombos (nStepList prevGuess 2)
                                           (prevState)
                                           
        |and [a == 0, b == 0, c >= 1] = filtCombos (nStepList prevGuess 1)
                                            (prevState)
                                            
        |and [a == 0, b >= 1, c == 0] = filtCombos prevGuess prevState
        |and [a == 0, b >= 1, c >= 1] = filtCombos prevGuess prevState
        |otherwise =  prevState \\ [(map show prevGuess)]
    
 -- | The nstepList function takes the previous guess combination and an
 --   Int "n" as input and generates a list of all unique cells Locations that
 --   are within "n" steps of each guess.
 
    nStepList :: [Location] -> Int -> [Location]
    nStepList [] n = []
    nStepList ((Location col row) : gs) n = nub cells
        where cells = [Location x y | x <- ['A'..'H'], y <- [1..4],
                  (abs(ord(col) - ord(x)) <= n) 
                  && (abs(row - y) <= n)] ++ nStepList gs n
                  
 -- | The filtCombos function is a helper function that takes as input the
 --   list of Locations and the current gamestate and returns the filtered
 --   gamestate. The main operation of this function is to remove each and
 --   every combination from the gamestate that contains any of the grid cells
 --   present in the first argument of this function using filter operation.
 
    filtCombos :: [Location] -> GameState -> GameState
    filtCombos [] prevState = prevState
    filtCombos (y:ys) prevState = 
                     intersect (filter (\x -> (notElem (show y) x)) prevState)
                         (filtCombos ys prevState)

-- | The keepCombos function takes as input the previous guess combination,
--   filtered game state and the feedback of the previous guess and returns a
--   further reduced game state. The given feedback of a guess with the actual
--   target is compared to the feedback of that guess with each possible
--   target present in the filtered game state and only those combinations are
--   kept that have the same feedback and is used by the nextGuess function.
                            
    keepCombos :: ([Location], GameState) -> (Int, Int, Int) -> GameState
    keepCombos (prevGuess, []) (a, b, c) = []
    keepCombos (prevGuess, (g:gs)) (a, b, c)
        |(x, y, z) == (a, b, c) = [g] ++ keepCombos (prevGuess, gs) (a, b, c)
        |otherwise            = keepCombos (prevGuess, gs) (a, b, c)
        where (x, y, z) = feedback candidate prevGuess
              candidate = (map fromJust (map toLocation g))
 
 ------------------------------End of Program----------------------------------