# quipks

The software requires some Cabal packages to be run, all the required dependencies are listed in "quipks.cabal".

The project is based on stack https://docs.haskellstack.org/en/stable/README/

and use functionalities of entangle https://github.com/miniBill/entangle



To use quipks install stack
then run the following commands

stack setup

then you can run 

stack build

or better just run

make

this will produce

followed by  
dot -Tps mygraph.dot -o mygraph.pdf  
and then open your pdf file.  
You'll need Graphviz to visualize the .dot files: http://www.graphviz.org/    

Note: parametrized gates aren't supported yet.
