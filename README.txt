Welcome to Mazes of Monad, JP Moresmau's RPG Game!

Mazes of Monad is a console based game similar to Nethack. I haven't looked for originality there, that's true.

To install, it's as usual: runhaskell Setup.lhs configure, runhaskell Setup.lhs build,runhaskell Setup.lhs install.

Everything is console based, so you have to type in actions (usually one word, or two). There is auto completion so you can type 'h' instead of 'help'. Some actions start little wizards that will ask you to type more information, or to choose from a list of items. To choose an item in the list, type in the number of the item. Just pressing the enter key without any number usually means "Cancel".

You first need to create a character. You can reuse a character for several mazes, but only one at a time. Your character will progress over time, and the monsters should become tougher as you progress. You can save the current state of a game (character + game) so you can go back to a previous situation if you die or go mad.

To create a character, type 'c' on the initial screen (get into the characters mode), then 'n' for new. You'll have to type in the name and gender, and then choose a profile. A profile is only an indication on how to generate your initial characteristic, but brings no limitations. As you progress your profile will be updated if you use other characteristics more. For example, a thief is not as strong as a warrior, but better in dexterity and intelligence. If you start as a warrior but do a lot of stuff requiring dexterity or intelligence, you may evolve into a thief.
You can refuse the character given to you any number of times. The number of gold coins and known spells you have is dependant on your characteristics, a merchant with a great charisma for example will probably have more gold to start with than a ranger.
For each characteristic you get three numbers: the first one is the current value, the second the normal value, the third the experience. When a characteristic is temporarily affected, its current value change but its normal value stays unchanged. Everytime you use a characteristic you may gain experience points, and experience points allow you to increase the normal value of the characteristic. So the more you use a skill, the better you get at it. For example, the more you try to trade with people, the better your intelligence and charisma will become.

Once you have a character, you exit the character screens ('b' for back), go into 'g' (ames) and start a 'n'(ew) game. You will find yourself in a maze. The goal is to find the exit.

You navigate with 'n'(orth), 's'(outh), 'e'(ast) and 'w'(est). 'm'(ap) shows you what you've discovered so far. 'i'(nventory) shows what you're carrying (use 'p'(ick) and 'd'(rop) to get or leave behind) items. 'ch'(aracter) shows you your current characteristics.

When you meet a NPC, the NPC may attack you, or leave you alone. In that case you can decide to attack it, trade with it, convert it or steal something from it. Just choose the appropriate action when prompted, or Ignore to leave it alone.

Fighting can be done through melee combat or magic. You can also try to end the fight by praying your opponent to let you go, or bribe it.

You can cast magic spells to your opponent during fights (damaging spells) or to yourself when you're not in a fight (healing or improvement spells).

Send me comments and suggestions!