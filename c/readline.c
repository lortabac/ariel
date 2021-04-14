#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/history.h>
#include <readline/readline.h>
#include <SWI-Prolog.h>

static foreign_t swireadline(term_t prompt_arg, term_t line_arg)
{
    char *prompt;

    int res_prompt = PL_get_chars(prompt_arg, &prompt, CVT_STRING);
    char *buffer = readline(prompt);
    if(buffer == NULL) {
        atom_t eof = PL_new_atom("eof");
        PL_put_atom(line_arg, eof);
    } else {
        int res_line = PL_put_string_chars(line_arg, buffer);
    }
}

install_t install_swireadline()
{
    PL_register_foreign_in_module("swireadline", "swireadline", 2, swireadline, 0);
}
