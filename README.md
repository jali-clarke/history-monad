# history-monad
Like a state monad, but it stores a list of state modifiers.

The usual "get" and "set", as well as other MonadState-supported actions, will act
using only the current state.

Later on I'll upgrade this to use a tree of states, analogous to multiple timelines.
This will almost certainly be handled using a zipper.
