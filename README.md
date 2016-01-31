# plc-experiment
arrow instance for a circuit edsl (deep embedding)

## How its done
First there is a state monad called ```CircuitM``` that maintains a collection of nodes.
Then there are some helper methods for creating nodes and linking them together, namely
```createInputNode```, ```createFunctionNode``` and ```wireNodeToNode```. This creates
the basic monadic interface for constructing the graph of nodes.

So what about the ```Arrow``` interface?

The ```Arrow``` interface is constructed by a ```Kleisli``` built on top of ```CircuitM``` giving ```CircuitA```.
This looks like so ```newtype CircuitA a b = CircuitA (Kleisli CircuitM a b)```.

Now the important thing to note that makes the ```Arrow``` instance usable is the fact that in ```CircuitA a b```,
the ```a``` and the ```b``` take on the type ```Int``` most the time, and sometimes ```()``` for input and output ends.
The ```Int``` represents the ID number of the node in the graph. Type synonyms get used for adding types to outputs of
circuit nodes, like ```type CInt32 = Int```, ```type CBool = Int```, ```type CFloat32 = Int```, etc. Sort of like phantom
typing the node ID.

Implementing things this way allows you to have a proper ```Arrow``` interface, and not fall short by landing on a
Cartesian Closed Category, as Haskell offers no syntactical support for that construct.