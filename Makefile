sudbury_architecture.png: sudbury_architecture.gv
	dot -Tpng sudbury_architecture.gv -o sudbury_architecture.png

all: sudbury_architecture.png
