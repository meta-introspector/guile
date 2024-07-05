// simple interface for calling
// introspector from c
// we start with function apply
//scm_call_n (SCM proc, SCM *argv, size_t nargs)

void
spct_invoke_apply_hook (scm_thread * thread);

void
spct_invoke_return_hook (scm_thread * thread);

void
spct_invoke_next_hook (scm_thread * thread);

void
spct_invoke_abort_hook (scm_thread * thread);

