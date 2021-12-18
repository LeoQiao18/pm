# pm

### Typical run script
`stack run -- +RTS -N8 -ls -RTS -b <board_file_path> -s <MinimaxSeq,int/MinimaxPar,int,int> -m <Test/Interactive> ...`
For example:
`stack run -- +RTS -N8 -ls -RTS -b ./data/stamma.txt -s MinimaxPar,2,4 -m Test`

### Sources for chess combinations
- https://www.thesprucecrafts.com/most-common-chess-openings-611517
- https://thechessworld.com/articles/problems/7-most-famous-chess-combinations/
- https://en.chessbase.com/post/wonderful-chess-compositions
