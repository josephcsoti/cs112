
#####################################################################################################
COURSE=CMPS 112
ASGN=HW0
COMPILER=jcsoti
#####################################################################################################

elsa:
	stack exec elsa tests/01_bool.lc
	stack exec elsa tests/02_plus.lc
	stack exec elsa tests/03_minus.lc

test:
	stack test

bin:
	stack build

clean:
	stack clean

prepare: clean
	tar -zcvf ../$(ASGN)-$(COMPILER).tgz --exclude .git --exclude .stack-work ../$(ASGN)-$(COMPILER)
	mv ../$(ASGN)-$(COMPILER).tgz .
