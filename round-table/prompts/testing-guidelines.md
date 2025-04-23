# General Testing Guidlines
- Focus on writing behavior tests. Never write tests that test mundane things like getters and contructors.
- A good testing platform for a web app is testing on the browser, after the app is server.

# Minesweeper test cases

- A game initializes with at least one mine.
- Each time you lose or win the game, you cannot interact with the board.
- Eeach time you reveal a non-mine cell, all adjacent no mine cells are revealed.
- There's a certain number of flags the player can put. The number of flags has to be equal to the mines in the board.
- Flags are placed with right clicking.
- If all flags are placed correctly, the game ends and you win.

