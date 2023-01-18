# CardGames

# Poker 

See http://en.wikipedia.org/wiki/List_of_poker_hands for details on poker hand
categories.

In the application pokerHandCompare hand1 hand2, hand1 and hand2 are
5-card poker hands.  If either of the hands are illegal (has duplicate cards),
then pokerHandCompare hand1 hand2 returns Nothing; otherwise, pokerHandCompare hand1 hand2 returns
Just GT if hand1 wins against hand2, Just EQ if hand1 and hand2 tie,
and Just LT if hand1 loses against hand2, using "high rules".  Assume
nothing about the order of cards in the tuples representing the hands.

# Cribbage

See http://en.wikipedia.org/wiki/Cribbage,
http://en.wikipedia.org/wiki/Rules_of_cribbage, and
http://www.cribbage.org/NewSite/rules/default.asp for details on scoring in
cribbage.

In the application cribbageScoreTheShow starter crib hand, starter is the
starter (or cut) card, crib is True if the hand is the crib False if
the hand is not the crib, and hand is the 4-card cribbage hand.  If the
combination of the starter card and the hand is illegal (has duplicate cards),
then cribbageScoreTheShow starter crib hand returns Nothing; otherwise,
cribbageScoreTheShow starter crib hand returns Just scr, where scr is the
score for the hand during the show phase of a cribbage round.  Assume nothing
about the order of cards in the tuple representing the hand.

