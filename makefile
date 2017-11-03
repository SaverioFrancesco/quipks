SHELL=C:/Windows/System32/cmd.exe

edit:  
	stack ghc Main.hs
	./Main.exe model.pm graph.dot
	cat model.pm

clean: 
	rm *o 
	rm *hi 
	rm *exe
	rm *pm
	rm *.dot