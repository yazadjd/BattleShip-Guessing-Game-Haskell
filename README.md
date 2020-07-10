# BattleShip-Guessing-Game-Haskell

The Game is somewhat akin to the game of Battleship™, but somewhat simplified. The game is played on a 4×8 grid, and involves one player, the searcher trying to find the locations of three battleships hidden by the other player, the hider. The searcher continues to guess until they find all the hidden ships. Unlike Battleship™, a guess consists of three different locations, and the game continues until the exact locations of the three hidden ships are guessed in a single guess.

After each guess, the hider responds with three numbers:
- The number of ships exactly located;
- The number of guesses that were exactly one space away from a ship; and
- The number of guesses that were exactly two spaces away from a ship.

Each guess is only counted as its closest distance to any ship. For example if a guessed location is exactly the location of one ship and is one square away from another, it counts as exactly locating a ship, and not as one away from a ship. The eight squares adjacent to a square, including diagonally adjacent, are counted as distance 1 away. The sixteen squares adjacent to those squares are considered to be distance 2 away.

Note that this feedback does not tell you which of the guessed locations is close to a ship. Your program will have to work that out; that is the challenge of this project.

We use a chess-like notation for describing locations: a letter A–H denoting the column of the guess and a digit 1–4 denoting the row, in that order. The upper left location is A1 and the lower right is H4.

A few caveats:
- The three ships will be at three different locations.
- The guess must consist of exactly three different locations.
- The list of locations may be written in any order, but the order is not significant; the guess A3, D1, H1 is exactly the same as H1, A3, D1 or any other permutation.
