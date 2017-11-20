# quipks

This pakage is tool for model checking quantum circuits that convert Quipper code into discrete markov chain.
You can get Quipper [here](http://www.mathstat.dal.ca/~selinger/quipper/).

The software requires some Cabal packages to be run, all the required dependencies are listed in "quipks.cabal".

The project is based on [stack](https://docs.haskellstack.org/en/stable/README/)

and use functionalities of [entangle](https://github.com/miniBill/entangle)

### Installation
To use quipks you need `stack`
then run the following commands

```
stack setup
```
then you can run 
```
stack build
```
and finaly
```
make
```
this will produce and run an executable file that will produce two different files (model.pm and model.dot)

### PRISM

The file model.pm contains a discrete markov chain representing the computation of the circuit.
You can use [PRISM](http://www.prismmodelchecker.org/) to load the file and check some LTL properties like
```
P>=1 [ F ( G (s=9)) ]
```
If 9 is a final state, this formula is true iff all the computation of the model terminates. 

Please note that the states are all model in one variable called s.
 
 Quipks put a comment into model.pm file a labeling of the states to discriminate the various states according whit the model described into model.dot
 
For instance s=9 is not in general a terminating state.

### Graphviz

To see the actual  Kripke structure in a readable way type
```
dot -Tps model.dot -o mygraph.pdf  
```
and then open your pdf file.  

You'll need [Graphviz](http://www.graphviz.org/) to visualize the .dot files:     

In the Kripke structure described the labels of the states correspond to the bits (not qubits).
For each non mesured qubit there is a bit containding zero.
The user is invited to formulate queries looking to both files.

Note: parametrized gates aren't supported yet.
