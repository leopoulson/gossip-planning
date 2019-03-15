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

- It would be great to think about whether or not we need to enumerate all of the states in the automata before performing BFS. It seems that we don't, however it is probably useful elsewhere - e.g. in removing the loops. So we do need to use it in the end, but this is fine! 

## Bugs / To fix / To update

- Change table2fn s.t. satisfies (example, 4) (K c (P (S a b))) errors; a world that isn't covered from the accessibility relaton should not be able to be used, or something
- Use record syntax (Halfway there) ✅
- Update FSM accepting functions to be functions ✅
- Maybe do a tripleCompose function? ✅

## How do we find all the successful paths?

All of the solutions found require us to enumerate the states we have. This is rather impractical, given that a state does not particularly "exist" before it's travelled to. 

We can probably just get the set of relevant states by traversing the graph from the starting point. This is not so bad? We then just perform a BFS on it to find the shortest successful path. 















