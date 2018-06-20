#ifndef SCM_ALIST_H
#define SCM_ALIST_H

/* Copyright (C) 1995-1996,2000,2006,2008,2018
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
#include <libguile/error.h>
#include "libguile/pairs.h"




#define SCM_VALIDATE_ALISTCELL(pos, alist) \
  do { \
    SCM_ASSERT (scm_is_pair (alist) && scm_is_pair (SCM_CAR (alist)), \
                alist, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_ALISTCELL_COPYSCM(pos, alist, cvar) \
  do { \
    SCM_ASSERT (scm_is_pair (alist), alist, pos, FUNC_NAME); \
    cvar = SCM_CAR (alist); \
    SCM_ASSERT (scm_is_pair (cvar), alist, pos, FUNC_NAME); \
  } while (0)




SCM_API SCM scm_acons (SCM w, SCM x, SCM y);
SCM_API SCM scm_sloppy_assq (SCM x, SCM alist);
SCM_API SCM scm_sloppy_assv (SCM x, SCM alist);
SCM_API SCM scm_sloppy_assoc (SCM x, SCM alist);
SCM_API SCM scm_assq (SCM x, SCM alist);
SCM_API SCM scm_assv (SCM x, SCM alist);
SCM_API SCM scm_assoc (SCM x, SCM alist);
SCM_API SCM scm_assq_ref (SCM alist, SCM key);
SCM_API SCM scm_assv_ref (SCM alist, SCM key);
SCM_API SCM scm_assoc_ref (SCM alist, SCM key);
SCM_API SCM scm_assq_set_x (SCM alist, SCM key, SCM val);
SCM_API SCM scm_assv_set_x (SCM alist, SCM key, SCM val);
SCM_API SCM scm_assoc_set_x (SCM alist, SCM key, SCM val);
SCM_API SCM scm_assq_remove_x (SCM alist, SCM key);
SCM_API SCM scm_assv_remove_x (SCM alist, SCM key);
SCM_API SCM scm_assoc_remove_x (SCM alist, SCM key);
SCM_INTERNAL void scm_init_alist (void);

#endif  /* SCM_ALIST_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
