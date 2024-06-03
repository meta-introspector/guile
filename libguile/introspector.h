// simple interface for calling
// introspector from c
// we start with function apply
//scm_call_n (SCM proc, SCM *argv, size_t nargs)

void
spct_scm_call_n (SCM proc,
                 SCM *argv,
                 size_t nargs,
                 SCM ret,
                 struct scm_vm *vp,
                 scm_thread * thread,
                 union scm_vm_stack_element *call_fp,
                 union scm_vm_stack_element *return_fp
                 );

