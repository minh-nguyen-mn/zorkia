# Zorkia
An Interactive Adventure Game

This is Zorkia, a framework for a classic text-rpg adventure game.
This project was developed by Minh Nguyen, Henry Gold, Howie Youngdahl, and Ethan Wills from November 23th, 2021 to December 6th, 2021.
The purpose of this project was to tentatively explore game development, with Scheme being the langugage.
The game is navigated via cardinal directions, along with prompts from puzzles.
To start the game, please type (play).
Enjoy.

FIXES/IMPROVEMENTS:

Bugs fixed:
- Moving now only works with correct input.
- `campaign-1` is now an actual loop until player guesses right.
- Correctly restoring all HP instead of only healing 50 HP.

Getting inputs:
- Added `string-normalize-spaces` to `get-input` so that it can also read inputs with unnecessary spaces.
- `name` cannot be blank.

Position & Movement:
- Player only needs to enter `n`, `s`, `e`, or `w` as short forms of cardinal directions when moving.
- Used uniform procedures: `pos-generate` to generate positions, `pos-change` to change the value of position instead of `list-set`.
- Added a move counter, `moves`. Move counter increases by 1 whenever position changes.

Stats:
- All stats cannot go below zero and only take rounded-down results. The maximum HP is 100.
- Player gain TOKENS after successful fights.
- Displaying MOVES and MAGARTIFACTS alongside HP, ATK, and TOKENS. Boxing them.
- Explicitly telling player about the losses and gains in stats.

Shop:
- More options, visualized by a table.
- Added a `pass` option for shop. Also, check if trades can happen with enough TOKENS.

Campaigns:
- `campaign-3`: Increased the cost from 30 to 50 TOKENS. Gave player an option of trading or not.

Code optimization & Player's experience enhancement:
- Categorized and annotated different parts of the code. Correctly documented procedures.
- Removed unneccessary uses of parameters by simply deleting them or modifying procedures.
- If's -> Cond.
- Used `section`, `map`, `for-each` to avoid repetitveness. 
- Changed the name of `start` to `play`, `serpent-fight` to `fight-serpent`, `chosen-HP` to `HP-chosen`, `monster-HP` to `HP-monster`, `monster-ATK` to `ATK-monster`.
- Created a uniform procedure for fights and applied it with `fight-normal-monster` and `fight-serpent`.
- Combined procedures when possible to eliminate redundancy, such as merging a procedure and its helpers into one, using `letrec`, etc. 
- Removed `string-downcase` and `equal?`. Used `string-ci=?` instead so that player can enter correct inputs in any form.
- Removed redundant displays of current stats. Stats now only show up right before player have a look around four cardinal directions.
- Fixes in narrative for better readability: no space at the beginning of any line except with intentions, capitalizing,
- removing redundant whitespaces, breaking lines, changing all appearances of `hp` to the uniform form `HP`, etc.

Other Features:
- Added a high score system: Storing high scores in a text file `high-scores.txt` that is created when the program runs. 

FUTURE FIXES/IMPROVEMENTS SUGGESTION:
- Using `racket/gui`.
- Having a text file to store all narratives. Extracting it when putting into the code.
- Making tile clearing go into effect.
- Balancing stats-changing effects of events. Also, considering if TOKENS should be limited.
- `campaign-1` should not make player keep guessing. Considering changing it.
- Having more variety/randomness in cellar/kitchen. Otherwise, player can keep choosing the good one.
- Taking `password?` game into account. Creating more scenarios and games.
- (begin prompt proc)
- Limiting the frequency or having more riddles.
- Fixing moving back and forth bug.
- Deduct ATK over time when TOKENS max.
- Crit rate.
- Each Magartifact has a certain effect. 
