build:
	mkdir -p lib
	swipl-ld -shared -o lib/readline c/readline.c -lreadline
