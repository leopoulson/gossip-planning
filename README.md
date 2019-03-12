# Remember 

- We have made the equivalence relation reflexive (technically, it's an equivalence relation now) for simplicity of implementation - address this in dissertation.

# Feature list

- Add action models ✅
- Consider how to encode a world? We want it to be (World, [Event]) -> Update code to take the event relation as well. Finish update' for this! ✅
- Add gossip states and a model for these  ✅
- Include construction of ME* and assorted transducers
    - Word automata for A ✅
    - Word automata for P ✅
    - Transducers ✅
- Implement power set construction ✅
- Visualisations thereof 
- Implement a program to generate gossip graphs for testing
- Add in automated testing (QuickCheck?)
	- It may still be good to try and use QuickCheck for some property-based stuff. HUnit is cool, but QuickCheck might let us do some more powerful things. 
- Understand how to use Transducers in Haskell ✅

## To Consider

- Consider the best way to differentiate between propositions (i.e. S i j, N i j) and other formulas. Specifically, how to restrict the language of propositions to the bit without K
- Consider the type of Postcondition being (Event, Prop) -> Form. One day extend it to be (Event, Form) -> Form? is this even needed? 
- Consider what should happen when we try and make an impossible call
- Figure out what to do about undefined transitions; move to the empty set? 

## Bugs / To fix / To update

- Change table2fn s.t. satisfies (example, 4) (K c (P (S a b))) errors; a world that isn't covered from the accessibility relaton should not be able to be used, or something
- Use record syntax (Halfway there) ✅
- Update FSM accepting functions to be functions ✅
- Maybe do a tripleCompose function? ✅

## How do we find all the successful paths?

All of the solutions found require us to enumerate the states we have. This is rather impractical, given that a state does not particularly "exist" before it's travelled to. 

We can probably just get the set of relevant states by traversing the graph from the starting point. This is not so bad? 













