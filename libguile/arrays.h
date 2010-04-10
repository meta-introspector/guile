/* classes: h_files */

#ifndef SCM_ARRAY_H
#define SCM_ARRAY_H

/* Copyright (C) 1995,1996,1997,1999,2000,2001, 2004, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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
#include "libguile/print.h"



/* Multidimensional arrays. Woo hoo!
   Also see ....
 */


/** Arrays */

SCM_API SCM scm_make_array (SCM fill, SCM bounds);
SCM_API SCM scm_from_contiguous_array (SCM bounds, const SCM *elts,
                                       size_t len);
SCM_API SCM scm_make_typed_array (SCM type, SCM fill, SCM bounds);
SCM_API SCM scm_from_contiguous_typed_array (SCM type, SCM bounds,
                                             const void *bytes,
                                             size_t byte_len);
SCM_API SCM scm_shared_array_root (SCM ra);
SCM_API SCM scm_shared_array_offset (SCM ra);
SCM_API SCM scm_shared_array_increments (SCM ra);
SCM_API SCM scm_make_shared_array (SCM oldra, SCM mapfunc, SCM dims);
SCM_API SCM scm_transpose_array (SCM ra, SCM args);
SCM_API SCM scm_array_contents (SCM ra, SCM strict);
SCM_API SCM scm_list_to_array (SCM ndim, SCM lst);
SCM_API SCM scm_list_to_typed_array (SCM type, SCM ndim, SCM lst);

/* internal. */

typedef struct scm_i_t_array
{
  SCM v;  /* the contents of the array, e.g., a vector or uniform vector.  */
  unsigned long base;
} scm_i_t_array;

SCM_API scm_t_bits scm_i_tc16_array;

#define SCM_I_ARRAY_FLAG_CONTIGUOUS (1 << 0)

#define SCM_I_ARRAYP(a)	    SCM_TYP16_PREDICATE (scm_i_tc16_array, a)
#define SCM_I_ARRAY_NDIM(x)  ((size_t) (SCM_SMOB_FLAGS (x)>>1))
#define SCM_I_ARRAY_CONTP(x) (SCM_SMOB_FLAGS(x) & SCM_I_ARRAY_FLAG_CONTIGUOUS)

#define SCM_I_ARRAY_MEM(a)  ((scm_i_t_array *) SCM_SMOB_DATA_1 (a))
#define SCM_I_ARRAY_V(a)    (SCM_I_ARRAY_MEM (a)->v)
#define SCM_I_ARRAY_BASE(a) (SCM_I_ARRAY_MEM (a)->base)
#define SCM_I_ARRAY_DIMS(a) \
  ((scm_t_array_dim *)((char *) SCM_I_ARRAY_MEM (a) + sizeof (scm_i_t_array)))

SCM_INTERNAL SCM scm_i_make_array (int ndim);
SCM_INTERNAL SCM scm_i_read_array (SCM port, int c);

SCM_INTERNAL void scm_init_arrays (void);

#endif  /* SCM_ARRAYS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
