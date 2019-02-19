# Feature list

- Add updates, calls
- Add gossip states and a model for these
- Include construction of ME* and assorted transducers
- Implement power set construction
- Visualisations thereof 
- Implement a program to generate gossip graphs for testing
- Add in automated testing (QuickCheck?)

## To Consider

- Consider the best way to differentiate between propositions (i.e. S i j, N i j) and other formulas. Specifically, how to restrict the language of propositions to the bit without K
- Consider the type of Postcondition being (Event, Prop) -> Form. One day extend it to be (Event, Form) -> Form? is this even needed? 
- Consider what should happen when we try and make an impossible call

## Bugs / To fix / To update

- Change table2fn s.t. satisfies (example, 4) (K c (P (S a b))) errors; a world that isn't covered from the accessibility relaton should not be able to be used, or something