#ifndef SCM_GSUBR_H
#define SCM_GSUBR_H

/* Copyright (C) 1995-1996,1998,2000-2001,2006,2008,2009-2011,2013,2015,2018
 *   Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#include "libguile/__scm.h"
#include "libguile/snarf.h"





/* Subrs 
 */

/* Max number of args to the C procedure backing a gsubr */
#define SCM_GSUBR_MAX 10

#define SCM_PRIMITIVE_P(x) (SCM_PROGRAM_P (x) && SCM_PROGRAM_IS_PRIMITIVE (x))

#define SCM_PRIMITIVE_GENERIC_P(x) (SCM_PROGRAM_P (x) && SCM_PROGRAM_IS_PRIMITIVE_GENERIC (x))

#define SCM_SUBRF(x)							\
  ((SCM (*) (void))                                                     \
   SCM_POINTER_VALUE (SCM_PROGRAM_FREE_VARIABLE_REF (x, 0)))

#define SCM_SUBR_NAME(x) (SCM_PROGRAM_FREE_VARIABLE_REF (x, 1))

#define SCM_SUBR_GENERIC(x)						\
  ((SCM *) SCM_POINTER_VALUE (SCM_PROGRAM_FREE_VARIABLE_REF (x, 2)))

#define SCM_SET_SUBR_GENERIC(x, g) \
  (*SCM_SUBR_GENERIC (x) = (g))



SCM_INTERNAL int scm_i_primitive_code_p (const scm_t_uint32 *code);
SCM_INTERNAL scm_t_uintptr scm_i_primitive_call_ip (SCM subr);

union scm_vm_stack_element;
SCM_INTERNAL SCM scm_apply_subr (union scm_vm_stack_element *sp,
                                 scm_t_ptrdiff nargs);

SCM_API SCM scm_c_make_gsubr (const char *name,
			      int req, int opt, int rst, scm_t_subr fcn);
SCM_API SCM scm_c_make_gsubr_with_generic (const char *name,
					   int req, int opt, int rst,
					   scm_t_subr fcn, SCM *gf);
SCM_API SCM scm_c_define_gsubr (const char *name,
				int req, int opt, int rst, scm_t_subr fcn);
SCM_API SCM scm_c_define_gsubr_with_generic (const char *name,
					     int req, int opt, int rst,
					     scm_t_subr fcn, SCM *gf);



/* Casting to a function that can take any number of arguments.  */
#define SCM_FUNC_CAST_ARBITRARY_ARGS  scm_t_subr

#define SCM_DEFINE_GSUBR(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
SCM_UNUSED static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
scm_c_define_gsubr (s_ ## FNAME, REQ, OPT, VAR, \
                    (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME); \
)\
SCM_SNARF_DOCS(primitive, FNAME, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)

/* Always use the generic subr case.  */
#define SCM_DEFINE SCM_DEFINE_GSUBR


#define SCM_PRIMITIVE_GENERIC(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
SCM_UNUSED static const char s_ ## FNAME [] = PRIMNAME; \
static SCM g_ ## FNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
g_ ## FNAME = SCM_PACK (0); \
scm_c_define_gsubr_with_generic (s_ ## FNAME, REQ, OPT, VAR, \
                    		 (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME, \
				 &g_ ## FNAME); \
)\
SCM_SNARF_DOCS(primitive, FNAME, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)

#define SCM_DEFINE_PUBLIC(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
SCM_UNUSED static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
scm_c_define_gsubr (s_ ## FNAME, REQ, OPT, VAR, \
                    (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME); \
scm_c_export (s_ ## FNAME, NULL); \
)\
SCM_SNARF_DOCS(primitive, FNAME, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)

#define SCM_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
SCM_SNARF_HERE(SCM_UNUSED static const char RANAME[]=STR) \
SCM_SNARF_INIT(scm_c_define_gsubr (RANAME, REQ, OPT, VAR, \
                                   (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN))

#define SCM_REGISTER_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
SCM_SNARF_HERE(SCM_UNUSED static const char RANAME[]=STR) \
SCM_SNARF_INIT(scm_c_define_gsubr (RANAME, REQ, OPT, VAR, \
                                   (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN);) \
SCM_SNARF_DOCS(register, CFN, STR, (), REQ, OPT, VAR, \
               "implemented by the C function \"" #CFN "\"")

#define SCM_GPROC(RANAME, STR, REQ, OPT, VAR, CFN, GF)  \
SCM_SNARF_HERE(\
SCM_UNUSED static const char RANAME[]=STR;\
static SCM GF \
)SCM_SNARF_INIT(\
GF = SCM_PACK (0);  /* Dirk:FIXME:: Can we safely use #f instead of 0? */ \
scm_c_define_gsubr_with_generic (RANAME, REQ, OPT, VAR, \
                                 (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN, &GF) \
)




SCM_INTERNAL void scm_init_gsubr (void);

#endif  /* SCM_GSUBR_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
