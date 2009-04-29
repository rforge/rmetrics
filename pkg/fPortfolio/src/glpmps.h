/* glpmps.h (MPS format) */

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

#ifndef _GLPMPS_H
#define _GLPMPS_H

#include "glpapi.h"

#define read_mps _glp_read_mps
int read_mps(LPX *lp, const char *fname);
/* read problem data in fixed MPS format */

#define write_mps _glp_write_mps
int write_mps(LPX *lp, const char *fname);
/* write problem data in fixed MPS format */

#define read_bas _glp_read_bas
int read_bas(LPX *lp, const char *fname);
/* read LP basis in fixed MPS format */

#define write_bas _glp_write_bas
int write_bas(LPX *lp, const char *fname);
/* write LP basis in fixed MPS format */

#define read_freemps _glp_read_freemps
int read_freemps(LPX *lp, const char *fname);
/* read problem data in free MPS format */

#define write_freemps _glp_write_freemps
int write_freemps(LPX *lp, const char *fname);
/* write problem data in free MPS format */

#endif

/* eof */
