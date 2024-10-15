# CHESS
This is a chess made in haskell, still in development.

It has a main window with the title in it as well as a play button.

Once the button is clicked the board is displayed.

## Keep in consideration that:
- Currently it's a "you choose you move" chess as there are no ways to pick anothes piece once chosen.
- There's no castling implementation.

## Requirements:
The default language for this project is:
- Haskell2010

You need to have the following instaled for this code to work:
- Gloss library version 1.13
- Gloss-Juicy library version 0.2
- bytestring library version 0.11.5
- JuicyPixels library version 3.3
- JuicyPixels-repa version 0.7.1
- repa library version 3.4.1

# Running Instructions for Windows:
- Pull/Fetch source code
- Open terminal (cmd)
- Use cd command to move into project directory
- Once inside project directory write "cabal v1-run"
- Press Enter
- Enjoy :D

## Current Issues:
- When a king (of either side) is chosen, there's no way to move it.
