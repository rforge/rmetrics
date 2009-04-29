/* glpapi15.c (reading/writing data in MPS format) */

/***********************************************************************
*  This code is part of GLPK (GNU Linear Programming Kit).
*
*  Copyright (C) 2000,01,02,03,04,05,06,07,08,2009 Andrew Makhorin,
*  Department for Applied Informatics, Moscow Aviation Institute,
*  Moscow, Russia. All rights reserved. E-mail: <mao@mai2.rcnet.ru>.
*
*  GLPK is free software: you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.
*
*  GLPK is distributed in the hope that it will be useful, but WITHOUT
*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
*  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
*  License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with GLPK. If not, see <http://www.gnu.org/licenses/>.
***********************************************************************/

#include "glpapi.h"
#include "glpmps.h"

/***********************************************************************
*  NAME
*
*  glp_read_mps - read problem data in MPS format
*
*  SYNOPSIS
*
*  int glp_read_mps(glp_prob *lp, int fmt, const void *parm,
*     const char *fname);
*
*  DESCRIPTION
*
*  The routine glp_read_mps reads problem data in MPS format from a
*  text file.
*
*  The parameter fmt specifies the version of MPS format:
*
*  GLP_MPS_DECK   fixed (ancient) MPS format;
*  GLP_MPS_FILE   free (modern) MPS format.
*
*  The parameter parm is reserved for use in the future and must be
*  specified as NULL.
*
*  The character string fname specifies a name of the text file to be
*  read.
*
*  Note that before reading data the current content of the problem
*  object is completely erased with the routine glp_erase_prob.
*
*  RETURNS
*
*  If the operation was successful, the routine glp_read_mps returns
*  zero. Otherwise, it prints an error message and returns non-zero. */

int glp_read_mps(glp_prob *lp, int fmt, const void *parm,
      const char *fname)
{     /* read problem data in MPS format */
      int ret;
      if (!(fmt == GLP_MPS_DECK || fmt == GLP_MPS_FILE))
         xerror("glp_read_mps: fmt = %d; invalid parameter\n", fmt);
      if (parm != NULL)
         xerror("glp_read_mps: parm = %p; invalid parameter\n", parm);
      switch (fmt)
      {  case GLP_MPS_DECK:
            ret = read_mps(lp, fname);
            break;
         case GLP_MPS_FILE:
            ret = read_freemps(lp, fname);
            break;
         default:
            xassert(fmt != fmt);
      }
      return ret;
}

/***********************************************************************
*  NAME
*
*  glp_write_mps - write problem data in MPS format
*
*  SYNOPSIS
*
*  int glp_write_mps(glp_prob *lp, int fmt, const void *parm,
*     const char *fname);
*
*  DESCRIPTION
*
*  The routine glp_write_mps writes problem data in MPS format to a
*  text file.
*
*  The parameter fmt specifies the version of MPS format:
*
*  GLP_MPS_DECK   fixed (ancient) MPS format;
*  GLP_MPS_FILE   free (modern) MPS format.
*
*  The parameter parm is reserved for use in the future and must be
*  specified as NULL.
*
*  The character string fname specifies a name of the text file to be
*  written.
*
*  RETURNS
*
*  If the operation was successful, the routine glp_write_mps returns
*  zero. Otherwise, it prints an error message and returns non-zero. */

int glp_write_mps(glp_prob *lp, int fmt, const void *parm,
      const char *fname)
{     /* write problem data in MPS format */
      int ret;
      if (!(fmt == GLP_MPS_DECK || fmt == GLP_MPS_FILE))
         xerror("glp_write_mps: fmt = %d; invalid parameter\n", fmt);
      if (parm != NULL)
         xerror("glp_write_mps: parm = %p; invalid parameter\n", parm);
      switch (fmt)
      {  case GLP_MPS_DECK:
            ret = write_mps(lp, fname);
            break;
         case GLP_MPS_FILE:
            ret = write_freemps(lp, fname);
            break;
         default:
            xassert(fmt != fmt);
      }
      return ret;
}

/* eof */
