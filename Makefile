build:
	mkdir -p dist
	swipl-ld -o dist/ariel -pl-options,--quiet main.c load.pl -lreadline
	echo "The executable is in ./dist/ariel"
