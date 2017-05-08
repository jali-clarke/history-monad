# history-monad
Like a state monad, but it stores a list of saved states.

Use "bookmark" to create a snapshot of the current state and push the snapshot onto
the stack.  Use "rewind" to try to revert to the previous state.

The usual "get" and "set", as well as other MonadState-supported actions, will act
using only the current state.

Later on I'll upgrade this to use a tree of states, analogous to multiple timelines.
