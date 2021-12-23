# pm
A simple minimax Chess Engine implemented with a combination of parallelization strategies and alpha-beta pruning.

### Helpful Commands
- Compile with the Haskell tool stack: `stack build`
- Running basic spec tests: `stack test`
- Running the program: `stack run -- +RTS -N<core_num> -ls -RTS -b <board_file_path> -s <PMStrat,args...> -m <Test/Interactive> ...`
    - For example: `stack run -- +RTS -N8 -ls -RTS -b ./data/stamma.txt -s MinimaxPar,2,4 -m Test`
    - Run `stack run -- -h` for more information

### User options
- `-m <mode>`
    - `Interactive`(default): Program continues running until one side wins. Pretty prints the chess board at every turn.
    - `Test`: Program runs for 2 consecutive player turns. No pretty printing.
- `-s <strategy>`
    - `MinimaxSeq,depth`(default): sequential minimax algorithm.
    - `MinimaxPar,parDepth,depth`: minimax tree traversal parallelized until `parDepth`.
    - `MinimaxSeqAB,depth`: sequential minimax algorithm + alpha-beta pruning.
    - `MinimaxParAB,parDepth,depth`: minimax tree parallelized until `parDepth`, and then alpha-beta pruned.
- `-p <player>`: `Black`(default) or `White` as the starting player.
- `-b <boardSrc>`: path of .txt file that specifies starting board. Default is clean chess opening board.

### Sources for chess combinations
- https://www.thesprucecrafts.com/most-common-chess-openings-611517
- https://thechessworld.com/articles/problems/7-most-famous-chess-combinations/
- https://en.chessbase.com/post/wonderful-chess-compositions
