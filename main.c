#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/history.h>
#include <readline/readline.h>
#include <SWI-Prolog.h>
// #include "ariel-state.h"

int
main(int argc, char *argv[])
{
    if(!PL_initialise(argc, argv)) { PL_halt(1); }

    atom_t atom_api = PL_new_atom("api");
    module_t module_api = PL_new_module(atom_api);
    predicate_t pred_init_backend = PL_predicate("init_backend", 1, "api");
    predicate_t pred_stop_backend = PL_predicate("stop_backend", 1, "api");
    predicate_t pred_run_query = PL_predicate("run_query", 3, "api");

    term_t backend = PL_new_term_ref();
    if( !PL_call_predicate(module_api, PL_Q_NORMAL, pred_init_backend, backend)) {
        PL_halt(1);
    }

    while(1) {
        char *buffer = readline("> ");
        char *response;
        if(buffer == NULL) {
            break;
        } else if(strcmp(buffer, ":exit") == 0) {
            break;
        } else if (buffer) {
            term_t query_args = PL_new_term_refs(3);
            query_args = backend;
            term_t ariel_str_arg = query_args + 1;
            term_t query_res_arg = query_args + 2;
            int res1 = PL_put_string_chars(ariel_str_arg, buffer);
            qid_t q = PL_open_query(module_api, PL_Q_NORMAL, pred_run_query, query_args);
            int res2 = PL_next_solution(q);
            int res3 = PL_get_chars(query_res_arg, &response, CVT_STRING | BUF_DISCARDABLE);
            printf("%s\n", response);
            PL_close_query(q);
            add_history(buffer);
            free(buffer);
        }
    }

    int res = PL_call_predicate(module_api, PL_Q_NORMAL, pred_stop_backend, backend);
    PL_halt(0);

    return 0;
}
