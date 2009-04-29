/* glpmps02.c (reading/writing data files in free MPS format) */

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

#include "glpmps.h"
#define strerror(errno) xerrmsg()

/*----------------------------------------------------------------------
-- read_freemps - read problem data in free MPS format.
--
-- *Synopsis*
--
-- #include "glpmps.h"
-- int read_freemps(LPX *lp, const char *fname);
--
-- *Description*
--
-- The routine read_freemps reads LP/MIP problem data in free MPS
-- format from an input text file whose name is the character string
-- fname.
--
-- *Returns*
--
-- If the operation was successful, the routine returns zero. Otherwise
-- the routine prints an error message and returns non-zero. */

struct dsa
{     /* working area used by MPS routines */
      LPX *lp;
      /* LP/MIP problem object */
      const char *fname;
      /* name of input text file */
      XFILE *fp;
      /* stream assigned to input text file */
      int count;
      /* line count */
      int c;
      /* current character */
      char item[255+1];
      /* current item */
      int obj;
      /* ordinal number of free (unbounded) row used as the objective
         function */
};

static int read_c(struct dsa *dsa)
{     /* read character from input file */
      int c;
      if (dsa->c == '\n') dsa->count++;
      c = xfgetc(dsa->fp);
      if (xferror(dsa->fp))
      {  xprintf("%s:%d: read error - %s\n", dsa->fname, dsa->count,
            strerror(errno));
         goto fail;
      }
      if (xfeof(dsa->fp))
      {  if (dsa->c == '\n')
         {  xprintf("%s:%d: unexpected EOF\n", dsa->fname, dsa->count);
            goto fail;
         }
         c = '\n';
      }
      if (c == '\n')
         ;
      else if (isspace(c))
         c = ' ';
      else if (iscntrl(c))
      {  xprintf("%s:%d: invalid control character 0x%02X\n",
            dsa->fname, dsa->count, c);
         goto fail;
      }
      dsa->c = c;
      return 0;
fail: return 1;
}

static int read_char(struct dsa *dsa)
{     /* read character from input file and skip comment lines */
      int nl;
loop: nl = (dsa->c == '\n');
      if (read_c(dsa)) goto fail;
      if (nl && dsa->c == '*')
      {  while (dsa->c != '\n')
            if (read_c(dsa)) goto fail;
         goto loop;
      }
      return 0;
fail: return 1;
}

static int read_item(struct dsa *dsa)
{     /* read item from input file */
      /* skip uninteresting characters */
      while (dsa->c == ' ')
         if (read_char(dsa)) goto fail;
      /* scan item */
      if (dsa->c == '$')
      {  /* dollar sign begins a comment */
         while (dsa->c != '\n')
            if (read_char(dsa)) goto fail;
      }
      if (dsa->c == '\n')
      {  /* empty item indicates end of line */
         dsa->item[0] = '\0';
         if (read_char(dsa)) goto fail;
      }
      else
      {  int len = 0;
         while (!(dsa->c == ' ' || dsa->c == '\n'))
         {  if (len == sizeof(dsa->item) - 1)
            {  xprintf("%s:%d: item `%.15s...' too long\n", dsa->fname,
                  dsa->count, dsa->item);
               goto fail;
            }
            dsa->item[len++] = (char)dsa->c;
            if (read_char(dsa)) goto fail;
         }
         dsa->item[len] = '\0';
      }
      return 0;
fail: return 1;
}

static int read_rows(struct dsa *dsa)
{     /* process data lines in ROWS section */
      int i, type;
loop: /* check if there is more data lines */
      if (dsa->c != ' ') goto done;
      /* create new row */
      i = lpx_add_rows(dsa->lp, 1);
      /* scan row type */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing row type\n", dsa->fname, dsa->count);
         goto fail;
      }
      else if (strcmp(dsa->item, "N") == 0)
      {  type = LPX_FR;
         /* the first free (unbounded) row is used as the objective
            function */
         if (dsa->obj == 0) dsa->obj = i;
      }
      else if (strcmp(dsa->item, "L") == 0)
         type = LPX_UP;
      else if (strcmp(dsa->item, "G") == 0)
         type = LPX_LO;
      else if (strcmp(dsa->item, "E") == 0)
         type = LPX_FX;
      else
      {  xprintf("%s:%d: invalid row type\n", dsa->fname, dsa->count);
         goto fail;
      }
      lpx_set_row_bnds(dsa->lp, i, type, 0.0, 0.0);
      /* scan row name */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing row name\n", dsa->fname, dsa->count);
         goto fail;
      }
      lpx_set_row_name(dsa->lp, i, dsa->item);
      if (dsa->obj == i) lpx_set_obj_name(dsa->lp, dsa->item);
      /* skip remaining characters */
      while (dsa->c != '\n')
         if (read_char(dsa)) goto fail;
      if (read_item(dsa)) goto fail;
      xassert(dsa->item[0] == '\0');
      goto loop;
done: return 0;
fail: return 1;
}

static void put_column(struct dsa *dsa, int j, int len, int ind[],
      double aij[])
{     /* store j-th column to the constraint matrix */
      int k, newlen = 0;
      for (k = 1; k <= len; k++)
      {  if (aij[k] != 0.0)
         {  newlen++;
            ind[newlen] = ind[k];
            aij[newlen] = aij[k];
            if (dsa->obj == ind[newlen])
               lpx_set_obj_coef(dsa->lp, j, aij[newlen]);
         }
      }
      lpx_set_mat_col(dsa->lp, j, newlen, ind, aij);
      return;
}

static int read_columns(struct dsa *dsa)
{     /* process data lines in COLUMNS section */
      int i, j = 0, k, len = 0, m, marker = 0, *map, *ind;
      double *aij, temp;
      char cname[sizeof(dsa->item)], rname[sizeof(dsa->item)];
      /* allocate working arrays */
      m = lpx_get_num_rows(dsa->lp);
      map = xcalloc(1+m, sizeof(int));
      for (i = 1; i <= m; i++) map[i] = 0;
      ind = xcalloc(1+m, sizeof(int));
      aij = xcalloc(1+m, sizeof(double));
loop: /* check if there is more data lines */
      if (dsa->c != ' ') goto done;
      /* scan column name */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf(
            "%s:%d: missing column name\n", dsa->fname, dsa->count);
         goto fail;
      }
      strcpy(cname, dsa->item);
      /* scan row name */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing row name\n", dsa->fname, dsa->count);
         goto fail;
      }
      strcpy(rname, dsa->item);
      /* scan optional INTORG/INTEND marker */
      if (strcmp(rname, "'MARKER'") == 0)
      {  if (read_item(dsa)) goto fail;
         if (dsa->item[0] == '\0')
         {  xprintf(
               "%s:%d: missing marker type\n", dsa->fname, dsa->count);
            goto fail;
         }
         else if (strcmp(dsa->item, "'INTORG'") == 0)
            marker = 1;
         else if (strcmp(dsa->item, "'INTEND'") == 0)
            marker = 0;
         else
         {  xprintf(
               "%s:%d: invalid marker type\n", dsa->fname, dsa->count);
            goto fail;
         }
         goto skip;
      }
      if (j == 0 || strcmp(lpx_get_col_name(dsa->lp, j), cname) != 0)
      {  /* new column begins, so finish the current column */
         if (j != 0)
         {  for (k = 1; k <= len; k++) map[ind[k]] = 0;
            put_column(dsa, j, len, ind, aij), len = 0;
         }
         /* create new column */
         if (lpx_find_col(dsa->lp, cname) != 0)
         {  xprintf("%s:%d: column %s multiply specified\n",
               dsa->fname, dsa->count, cname);
            goto fail;
         }
         j = lpx_add_cols(dsa->lp, 1);
         lpx_set_col_name(dsa->lp, j, cname);
         if (marker)
         {  /* lpx_set_class(dsa->lp, LPX_MIP); */
            lpx_set_col_kind(dsa->lp, j, LPX_IV);
         }
         lpx_set_col_bnds(dsa->lp, j, LPX_LO, 0.0, 0.0);
      }
      /* find row */
      i = lpx_find_row(dsa->lp, rname);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            rname);
         goto fail;
      }
      /* scan coefficient value */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing coefficient value\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (str2num(dsa->item, &temp))
      {  xprintf("%s:%d: invalid coefficient value\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: coefficient in row %s multiply specified\n",
            dsa->fname, dsa->count, rname);
         goto fail;
      }
      map[i] = 1, len++, ind[len] = i, aij[len] = temp;
      /* there may be another row name and coefficient value */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0') goto loop;
      strcpy(rname, dsa->item);
      /* find row */
      i = lpx_find_row(dsa->lp, rname);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            rname);
         goto fail;
      }
      /* scan coefficient value */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing coefficient value\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (str2num(dsa->item, &temp))
      {  xprintf("%s:%d: invalid coefficient value\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: coefficient in row %s multiply specified\n",
            dsa->fname, dsa->count, rname);
         goto fail;
      }
      map[i] = 1, len++, ind[len] = i, aij[len] = temp;
skip: /* skip remaining characters */
      while (dsa->c != '\n')
         if (read_char(dsa)) goto fail;
      if (read_item(dsa)) goto fail;
      xassert(dsa->item[0] == '\0');
      goto loop;
done: /* finish the last column */
      if (j != 0) put_column(dsa, j, len, ind, aij);
      xfree(map), xfree(ind), xfree(aij);
      return 0;
fail: xfree(map), xfree(ind), xfree(aij);
      return 1;
}

static void set_rhs(struct dsa *dsa, int i, double b)
{     /* set right-hand side for i-th row */
      switch (lpx_get_row_type(dsa->lp, i))
      {  case LPX_FR:
            if (dsa->obj == i)
               lpx_set_obj_coef(dsa->lp, 0, b);
            else if (b != 0.0)
               xprintf("%s:%d: warning: non-zero rhs for free row %s ig"
                  "nored\n", dsa->fname, dsa->count,
                  lpx_get_row_name(dsa->lp, i));
            break;
         case LPX_LO:
            lpx_set_row_bnds(dsa->lp, i, LPX_LO, b, 0.0);
            break;
         case LPX_UP:
            lpx_set_row_bnds(dsa->lp, i, LPX_UP, 0.0, b);
            break;
         case LPX_FX:
            lpx_set_row_bnds(dsa->lp, i, LPX_FX, b, b);
            break;
         default:
            xassert(dsa != dsa);
      }
      return;
}

static int read_rhs(struct dsa *dsa)
{     /* process data lines in RHS section */
      int i, m, *map;
      double temp;
      char name[sizeof(dsa->item)] = "";
      /* allocate working array */
      m = lpx_get_num_rows(dsa->lp);
      map = xcalloc(1+m, sizeof(int));
      for (i = 1; i <= m; i++) map[i] = 0;
loop: /* check if there is more data lines */
      if (dsa->c != ' ') goto done;
      /* scan rhs vector name */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing rhs vector name\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (name[0] == '\0') strcpy(name, dsa->item);
      if (strcmp(dsa->item, name) != 0)
      {  xprintf("%s:%d: at most one rhs vector allowed\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      /* scan row name */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing row name\n", dsa->fname, dsa->count);
         goto fail;
      }
      /* find row */
      i = lpx_find_row(dsa->lp, dsa->item);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            dsa->item);
         goto fail;
      }
      /* scan rhs value */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing rhs value\n", dsa->fname, dsa->count);
         goto fail;
      }
      if (str2num(dsa->item, &temp))
      {  xprintf("%s:%d: invalid rhs value\n", dsa->fname, dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: rhs for row %s multiply specified\n",
            dsa->fname, dsa->count, lpx_get_row_name(dsa->lp, i));
         goto fail;
      }
      set_rhs(dsa, i, temp), map[i] = 1;
      /* there may be another row name and rhs value */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0') goto loop;
      /* find row */
      i = lpx_find_row(dsa->lp, dsa->item);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            dsa->item);
         goto fail;
      }
      /* scan rhs value */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing rhs value\n", dsa->fname, dsa->count);
         goto fail;
      }
      if (str2num(dsa->item, &temp))
      {  xprintf("%s:%d: invalid rhs value\n", dsa->fname, dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: rhs for row %s multiply specified\n",
            dsa->fname, dsa->count, lpx_get_row_name(dsa->lp, i));
         goto fail;
      }
      set_rhs(dsa, i, temp), map[i] = 1;
      /* skip remaining characters */
      while (dsa->c != '\n')
         if (read_char(dsa)) goto fail;
      if (read_item(dsa)) goto fail;
      xassert(dsa->item[0] == '\0');
      goto loop;
done: xfree(map);
      return 0;
fail: xfree(map);
      return 1;
}

static void set_range(struct dsa *dsa, int i, double r)
{     /* set range value for i-th row */
      double b;
      switch (lpx_get_row_type(dsa->lp, i))
      {  case LPX_FR:
            xprintf("%s:%d: warning: range value for free row %s ignore"
               "d\n", dsa->fname, dsa->count,
               lpx_get_row_name(dsa->lp, i));
            break;
         case LPX_LO:
            b = lpx_get_row_lb(dsa->lp, i);
            if (r == 0.0)
               lpx_set_row_bnds(dsa->lp, i, LPX_FX, b, b);
            else
               lpx_set_row_bnds(dsa->lp, i, LPX_DB, b, b + fabs(r));
            break;
         case LPX_UP:
            b = lpx_get_row_ub(dsa->lp, i);
            if (r == 0.0)
               lpx_set_row_bnds(dsa->lp, i, LPX_FX, b, b);
            else
               lpx_set_row_bnds(dsa->lp, i, LPX_DB, b - fabs(r), b);
            break;
         case LPX_FX:
            b = lpx_get_row_lb(dsa->lp, i);
            if (r > 0.0)
               lpx_set_row_bnds(dsa->lp, i, LPX_DB, b, b + fabs(r));
            else if (r < 0.0)
               lpx_set_row_bnds(dsa->lp, i, LPX_DB, b - fabs(r), b);
            break;
         default:
            xassert(dsa != dsa);
      }
      return;
}

static int read_ranges(struct dsa *dsa)
{     /* process data lines in RANGES section */
      int i, m, *map;
      double temp;
      char name[sizeof(dsa->item)] = "";
      /* allocate working array */
      m = lpx_get_num_rows(dsa->lp);
      map = xcalloc(1+m, sizeof(int));
      for (i = 1; i <= m; i++) map[i] = 0;
loop: /* check if there is more data lines */
      if (dsa->c != ' ') goto done;
      /* scan range vector name */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing range vector name\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      if (name[0] == '\0') strcpy(name, dsa->item);
      if (strcmp(dsa->item, name) != 0)
      {  xprintf("%s:%d: at most one range vector allowed\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      /* scan row name */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing row name\n", dsa->fname, dsa->count);
         goto fail;
      }
      /* find row */
      i = lpx_find_row(dsa->lp, dsa->item);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            dsa->item);
         goto fail;
      }
      /* scan range value */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf(
            "%s:%d: missing range value\n", dsa->fname, dsa->count);
         goto fail;
      }
      if (str2num(dsa->item, &temp))
      {  xprintf(
            "%s:%d: invalid range value\n", dsa->fname, dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: range for row %s multiply specified\n",
            dsa->fname, dsa->count, lpx_get_row_name(dsa->lp, i));
         goto fail;
      }
      set_range(dsa, i, temp), map[i] = 1;
      /* there may be another row name and range value */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0') goto loop;
      /* find row */
      i = lpx_find_row(dsa->lp, dsa->item);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            dsa->item);
         goto fail;
      }
      /* scan range value */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf(
            "%s:%d: missing range value\n", dsa->fname, dsa->count);
         goto fail;
      }
      if (str2num(dsa->item, &temp))
      {  xprintf(
            "%s:%d: invalid range value\n", dsa->fname, dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: range for row %s multiply specified\n",
            dsa->fname, dsa->count, lpx_get_row_name(dsa->lp, i));
         goto fail;
      }
      set_range(dsa, i, temp), map[i] = 1;
      /* skip remaining characters */
      while (dsa->c != '\n')
         if (read_char(dsa)) goto fail;
      if (read_item(dsa)) goto fail;
      xassert(dsa->item[0] == '\0');
      goto loop;
done: xfree(map);
      return 0;
fail: xfree(map);
      return 1;
}

static int read_bounds(struct dsa *dsa)
{     /* process data lines in BOUNDS section */
      int j, type;
      double lb, ub, temp;
      char btyp[2+1];
      char name[sizeof(dsa->item)] = "";
loop: /* check if there is more data lines */
      if (dsa->c != ' ') goto done;
      /* scan bound type */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf(
            "%s:%d: missing bound type\n", dsa->fname, dsa->count);
         goto fail;
      }
      if (strlen(dsa->item) != 2)
      {  xprintf(
            "%s:%d: invalid bound type\n", dsa->fname, dsa->count);
         goto fail;
      }
      strcpy(btyp, dsa->item);
      /* scan bound vector name */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf("%s:%d: missing bound vector name\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (name[0] == '\0') strcpy(name, dsa->item);
      if (strcmp(dsa->item, name) != 0)
      {  xprintf("%s:%d: at most one bound vector allowed\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      /* scan column name */
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf(
            "%s:%d: missing column name\n", dsa->fname, dsa->count);
         goto fail;
      }
      /* find column */
      j = lpx_find_col(dsa->lp, dsa->item);
      if (j == 0)
      {  xprintf("%s:%d: column %s not found\n",
            dsa->fname, dsa->count, dsa->item);
         goto fail;
      }
      /* scan optional bound value */
      if (!(strcmp(btyp, "LO") == 0 || strcmp(btyp, "UP") == 0 ||
            strcmp(btyp, "FX") == 0 || strcmp(btyp, "LI") == 0 ||
            strcmp(btyp, "UI") == 0))
         goto skip;
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
      {  xprintf(
            "%s:%d: missing bound value\n", dsa->fname, dsa->count);
         goto fail;
      }
      if (str2num(dsa->item, &temp))
      {  xprintf(
            "%s:%d: invalid bound value\n", dsa->fname, dsa->count);
         goto fail;
      }
skip: /* change column bounds */
      type = lpx_get_col_type(dsa->lp, j);
      if (type == LPX_FR || type == LPX_UP)
         lb = -DBL_MAX;
      else
         lb = lpx_get_col_lb(dsa->lp, j);
      if (type == LPX_FR || type == LPX_LO)
         ub = +DBL_MAX;
      else
         ub = lpx_get_col_ub(dsa->lp, j);
      if (strcmp(btyp, "LO") == 0)
         lb = temp;
      else if (strcmp(btyp, "UP") == 0)
         ub = temp;
      else if (strcmp(btyp, "FX") == 0)
         lb = ub = temp;
      else if (strcmp(btyp, "FR") == 0)
         lb = -DBL_MAX, ub = +DBL_MAX;
      else if (strcmp(btyp, "MI") == 0)
         lb = -DBL_MAX;
      else if (strcmp(btyp, "PL") == 0)
         ub = +DBL_MAX;
#if 1 /* 11/V-2006 */
      else if (strcmp(btyp, "LI") == 0)
      {  /* lpx_set_class(dsa->lp, LPX_MIP); */
         lpx_set_col_kind(dsa->lp, j, LPX_IV);
         lb = temp;
      }
#endif
      else if (strcmp(btyp, "UI") == 0)
      {  /* lpx_set_class(dsa->lp, LPX_MIP); */
         lpx_set_col_kind(dsa->lp, j, LPX_IV);
         ub = temp;
      }
      else if (strcmp(btyp, "BV") == 0)
      {  /* lpx_set_class(dsa->lp, LPX_MIP); */
         lpx_set_col_kind(dsa->lp, j, LPX_IV);
         lb = 0.0, ub = 1.0;
      }
      else
      {  xprintf(
            "%s:%d: invalid bound type\n", dsa->fname, dsa->count);
         goto fail;
      }
      if (lb == -DBL_MAX && ub == +DBL_MAX)
         type = LPX_FR;
      else if (ub == +DBL_MAX)
         type = LPX_LO;
      else if (lb == -DBL_MAX)
         type = LPX_UP;
      else if (lb != ub)
         type = LPX_DB;
      else
         type = LPX_FX;
      lpx_set_col_bnds(dsa->lp, j, type, lb, ub);
      /* skip remaining characters */
      while (dsa->c != '\n')
         if (read_char(dsa)) goto fail;
      if (read_item(dsa)) goto fail;
      xassert(dsa->item[0] == '\0');
      goto loop;
done: return 0;
fail: return 1;
}

int read_freemps(LPX *lp, const char *fname)
{     /* read problem data in free MPS format */
      struct dsa _dsa, *dsa = &_dsa;
      glp_erase_prob(lp);
      dsa->lp = lp;
      dsa->fname = fname;
      dsa->fp = NULL;
      dsa->count = 0;
      dsa->c = '\n';
      dsa->obj = 0;
      xprintf("glp_read_mps: reading problem data from `%s'...\n",
         dsa->fname);
      dsa->fp = xfopen(dsa->fname, "r");
      if (dsa->fp == NULL)
      {  xprintf("glp_read_mps: unable to open `%s' - %s\n",
            dsa->fname, strerror(errno));
         goto fail;
      }
      lpx_create_index(dsa->lp);
      /* read the very first character */
      if (read_char(dsa)) goto fail;
      /* read NAME indicator record */
      if (dsa->c == ' ')
err1: {  xprintf("%s:%d: NAME indicator record missing\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (read_item(dsa)) goto fail;
      if (strcmp(dsa->item, "NAME") != 0) goto err1;
      if (read_item(dsa)) goto fail;
      if (dsa->item[0] == '\0')
         xprintf("glp_read_mps: problem name not specified\n");
      else
      {  xprintf("glp_read_mps: problem %s\n", dsa->item);
         lpx_set_prob_name(dsa->lp, dsa->item);
         while (dsa->c != '\n')
            if (read_char(dsa)) goto fail;
         if (read_item(dsa)) goto fail;
      }
      xassert(dsa->item[0] == '\0');
      /* read ROWS section */
      if (dsa->c == ' ')
err2: {  xprintf("%s:%d: ROWS indicator record missing\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      if (read_item(dsa)) goto fail;
      if (strcmp(dsa->item, "ROWS") != 0) goto err2;
      while (dsa->c != '\n')
         if (read_char(dsa)) goto fail;
      if (read_item(dsa)) goto fail;
      xassert(dsa->item[0] == '\0');
      if (read_rows(dsa)) goto fail;
      /* read COLUMNS section */
      xassert(dsa->c != ' ');
      if (read_item(dsa)) goto fail;
      if (strcmp(dsa->item, "COLUMNS") != 0)
      {  xprintf("%s:%d: COLUMNS indicator record missing\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      while (dsa->c != '\n')
         if (read_char(dsa)) goto fail;
      if (read_item(dsa)) goto fail;
      xassert(dsa->item[0] == '\0');
      if (read_columns(dsa)) goto fail;
      {  int m = lpx_get_num_rows(dsa->lp);
         int n = lpx_get_num_cols(dsa->lp);
         int nnz = lpx_get_num_nz(dsa->lp);
         xprintf("glp_read_mps: %d row%s, %d column%s, %d non-zero%s\n",
            m, m == 1 ? "" : "s", n, n == 1 ? "" : "s", nnz,
            nnz == 1 ? "" : "s");
      }
      /* read RHS section (optional) */
      xassert(dsa->c != ' ');
      if (read_item(dsa)) goto fail;
      if (strcmp(dsa->item, "RHS") == 0)
      {  while (dsa->c != '\n')
            if (read_char(dsa)) goto fail;
         if (read_item(dsa)) goto fail;
         xassert(dsa->item[0] == '\0');
         if (read_rhs(dsa)) goto fail;
         xassert(dsa->c != ' ');
         if (read_item(dsa)) goto fail;
      }
      /* read RANGES section (optional) */
      if (strcmp(dsa->item, "RANGES") == 0)
      {  while (dsa->c != '\n')
            if (read_char(dsa)) goto fail;
         if (read_item(dsa)) goto fail;
         xassert(dsa->item[0] == '\0');
         if (read_ranges(dsa)) goto fail;
         xassert(dsa->c != ' ');
         if (read_item(dsa)) goto fail;
      }
      /* read BOUNDS section (optional) */
      if (strcmp(dsa->item, "BOUNDS") == 0)
      {  while (dsa->c != '\n')
            if (read_char(dsa)) goto fail;
         if (read_item(dsa)) goto fail;
         xassert(dsa->item[0] == '\0');
         if (read_bounds(dsa)) goto fail;
         xassert(dsa->c != ' ');
         if (read_item(dsa)) goto fail;
      }
      /* check ENDATA indicator line */
      if (strcmp(dsa->item, "ENDATA") != 0)
      {  xprintf("%s:%d: ENDATA indicator record missing\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      if (lpx_get_class(dsa->lp) == LPX_MIP)
      {  int ni = lpx_get_num_int(dsa->lp);
         int nb = lpx_get_num_bin(dsa->lp);
         char s[50];
         if (nb == 0)
            strcpy(s, "none of");
         else if (ni == 1 && nb == 1)
            strcpy(s, "");
         else if (nb == 1)
            strcpy(s, "one of");
         else if (nb == ni)
            strcpy(s, "all of");
         else
            sprintf(s, "%d of", nb);
         xprintf("glp_read_mps: %d integer column%s, %s which %s binary"
            "\n", ni, ni == 1 ? "" : "s", s, nb == 1 ? "is" : "are");
      }
      xprintf("glp_read_mps: %d records were read\n", dsa->count);
      xfclose(dsa->fp);
      lpx_delete_index(dsa->lp);
      lpx_order_matrix(dsa->lp);
      return 0;
fail: if (dsa->lp != NULL) glp_erase_prob(dsa->lp);
      if (dsa->fp != NULL) xfclose(dsa->fp);
      return 1;
}

/*----------------------------------------------------------------------
-- write_freemps - write problem data in free MPS format.
--
-- *Synopsis*
--
-- #include "glpmps.h"
-- int write_freemps(LPX *lp, const char *fname);
--
-- *Description*
--
-- The routine write_freemps writes problem data in free MPS format to
-- an output text file whose name is the character string fname.
--
-- *Returns*
--
-- If the operation was successful, the routine returns zero. Otherwise
-- the routine prints an error message and returns non-zero. */

static char *mps_name(char name[255+1])
{     int k;
      for (k = 0; name[k] != '\0'; k++)
         if (name[k] == ' ') name[k] = '_';
      return name;
}

static char *row_name(LPX *lp, int i, char rname[255+1])
{     const char *str;
      str = (i == 0 ? lpx_get_obj_name(lp) : lpx_get_row_name(lp, i));
      if (str == NULL)
         sprintf(rname, "R%07d", i);
      else
         strcpy(rname, mps_name((void *)str));
      return rname;
}

static char *col_name(LPX *lp, int j, char cname[8+1])
{     const char *str;
      str = lpx_get_col_name(lp, j);
      if (str == NULL)
         sprintf(cname, "C%07d", j);
      else
         strcpy(cname, mps_name((void *)str));
      return cname;
}

static char *mps_numb(double val, char numb[12+1])
{     int n;
      char str[255+1], *e;
      for (n = 12; n >= 6; n--)
      {  if (val != 0.0 && fabs(val) < 0.002)
#if 0
            sprintf(str, "%.*E", n, val);
#else
            /* n is number of desired decimal places, but in case of E
               format precision means number of digits that follow the
               decimal point */
            sprintf(str, "%.*E", n-1, val);
#endif
         else
            sprintf(str, "%.*G", n, val);
         xassert(strlen(str) <= 255);
         e = strchr(str, 'E');
         if (e != NULL) sprintf(e+1, "%d", atoi(e+1));
         if (strlen(str) <= 12) return strcpy(numb, str);
      }
      xerror("glp_write_mps: unable to convert floating point number %g"
         " to character string\n", val);
      return NULL; /* make the compiler happy */
}

int write_freemps(LPX *lp, const char *fname)
{     XFILE *fp;
      int wide = lpx_get_int_parm(lp, LPX_K_MPSWIDE);
      int skip = lpx_get_int_parm(lp, LPX_K_MPSSKIP);
      int marker = 0; /* intorg/intend marker count */
      int mip, make_obj, nrows, ncols, i, j, flag, *ndx;
      double *obj, *val;
      char rname[255+1], cname[255+1], numb[12+1];
      xprintf("glp_write_mps: writing problem data to `%s'...\n",
         fname);
      /* open the output text file */
      fp = xfopen(fname, "w");
      if (fp == NULL)
      {  xprintf("glp_write_mps: unable to create `%s' - %s\n", fname,
            strerror(errno));
         goto fail;
      }
      /* determine whether the problem is LP or MIP */
      mip = (lpx_get_class(lp) == LPX_MIP);
      /* determine number of rows and number of columns */
      nrows = lpx_get_num_rows(lp);
      ncols = lpx_get_num_cols(lp);
      /* the problem should contain at least one row and one column */
      if (!(nrows > 0 && ncols > 0))
         xerror("glp_write_mps: problem has no rows/columns\n");
      /* determine if the routine should output the objective row */
      make_obj = lpx_get_int_parm(lp, LPX_K_MPSOBJ);
      if (make_obj == 2)
      {  for (i = 1; i <= nrows; i++)
         {  int typx;
            lpx_get_row_bnds(lp, i, &typx, NULL, NULL);
            if (typx == LPX_FR)
            {  make_obj = 0;
               break;
            }
         }
      }
      /* write comment records (if required) */
      if (lpx_get_int_parm(lp, LPX_K_MPSINFO))
      {  const char *name = lpx_get_prob_name(lp);
         if (name == NULL) name = "UNKNOWN";
         xfprintf(fp, "* Problem:    %s\n", name);
         xfprintf(fp, "* Class:      %s\n", !mip ? "LP" : "MIP");
         xfprintf(fp, "* Rows:       %d\n", nrows);
         if (!mip)
            xfprintf(fp, "* Columns:    %d\n", ncols);
         else
            xfprintf(fp, "* Columns:    %d (%d integer, %d binary)\n",
               ncols, lpx_get_num_int(lp), lpx_get_num_bin(lp));
         xfprintf(fp, "* Non-zeros:  %d\n", lpx_get_num_nz(lp));
         xfprintf(fp, "* Format:     Free MPS\n");
         xfprintf(fp, "*\n");
      }
      /* write NAME indicator record */
      {  const char *name = lpx_get_prob_name(lp);
         if (name == NULL)
            xfprintf(fp, "NAME\n");
         else
            xfprintf(fp, "NAME %s\n", mps_name((void *)name));
      }
      /* write ROWS section */
      xfprintf(fp, "ROWS\n");
      if (make_obj)
         xfprintf(fp, " %c %s\n", 'N', row_name(lp, 0, rname));
      for (i = 1; i <= nrows; i++)
      {  int typx;
         lpx_get_row_bnds(lp, i, &typx, NULL, NULL);
         switch (typx)
         {  case LPX_FR: typx = 'N'; break;
            case LPX_LO: typx = 'G'; break;
            case LPX_UP: typx = 'L'; break;
            case LPX_DB: typx = 'E'; break;
            case LPX_FX: typx = 'E'; break;
            default: xassert(typx != typx);
         }
         xfprintf(fp, " %c %s\n", typx, row_name(lp, i, rname));
      }
      /* prepare coefficients of the objective function */
      obj = xcalloc(1+ncols, sizeof(double));
      obj[0] = lpx_get_obj_coef(lp, 0);
      for (j = 1; j <= ncols; j++)
         obj[j] = lpx_get_obj_coef(lp, j);
      ndx = xcalloc(1+ncols, sizeof(int));
      val = xcalloc(1+ncols, sizeof(double));
      for (i = 1; i <= nrows; i++)
#if 0
      {  double ci = lpx_get_row_coef(lp, i);
#else
      {  double ci = 0.0;
#endif
         if (ci != 0.0)
         {  int len, t;
            len = lpx_get_mat_row(lp, i, ndx, val);
            for (t = 1; t <= len; t++)
               obj[ndx[t]] += ci * val[t];
         }
      }
      xfree(ndx);
      xfree(val);
      for (j = 0; j <= ncols; j++)
         if (fabs(obj[j]) < 1e-12) obj[j] = 0.0;
      /* write COLUMNS section */
      xfprintf(fp, "COLUMNS\n");
      ndx = xcalloc(1+nrows, sizeof(int));
      val = xcalloc(1+nrows, sizeof(double));
      for (j = 1; j <= ncols; j++)
      {  int nl = 1, iv, len, t;
         col_name(lp, j, cname);
         iv = (mip && lpx_get_col_kind(lp, j) == LPX_IV);
         if (iv && marker % 2 == 0)
         {  /* open new intorg/intend group */
            marker++;
            xfprintf(fp, " M%07d 'MARKER' 'INTORG'\n", marker);
         }
         else if (!iv && marker % 2 == 1)
         {  /* close the current intorg/intend group */
            marker++;
            xfprintf(fp, " M%07d 'MARKER' 'INTEND'\n", marker);
         }
         /* obtain j-th column of the constraint matrix */
         len = lpx_get_mat_col(lp, j, ndx, val);
         ndx[0] = 0;
         val[0] = (make_obj ? obj[j] : 0.0);
         if (len == 0 && val[0] == 0.0 && !skip)
            xfprintf(fp, " %s %s %s $ empty column\n", cname,
               row_name(lp, 1, rname), mps_numb(0.0, numb));
         for (t = val[0] != 0.0 ? 0 : 1; t <= len; t++)
         {  if (nl) xfprintf(fp, " %s", cname);
            xfprintf(fp, " %s %s", row_name(lp, ndx[t], rname),
               mps_numb(val[t], numb));
            if (wide) nl = 1 - nl;
            if (nl) xfprintf(fp, "\n");
         }
         if (!nl) xfprintf(fp, "\n");
      }
      if (marker % 2 == 1)
      {  /* close the last intorg/intend group (if not closed) */
         marker++;
         xfprintf(fp, " M%07d 'MARKER' 'INTEND'\n", marker);
      }
      xfree(ndx);
      xfree(val);
      /* write RHS section */
      flag = 0;
      {  int nl = 1;
         for (i = make_obj ? 0 : 1; i <= nrows; i++)
         {  int typx;
            double lb, ub, rhs;
            if (i == 0)
               typx = LPX_FR, lb = ub = 0.0;
            else
               lpx_get_row_bnds(lp, i, &typx, &lb, &ub);
            switch (typx)
            {  case LPX_FR:
                  rhs = (make_obj && i == 0 ? obj[0] : 0.0); break;
               case LPX_LO:
                  rhs = lb; break;
               case LPX_UP:
                  rhs = ub; break;
               case LPX_DB:
                  rhs = (ub > 0.0 ? lb : ub); break;
               case LPX_FX:
                  rhs = lb; break;
               default:
                  xassert(typx != typx);
            }
            if (rhs == 0.0) continue;
            if (!flag) xfprintf(fp, "RHS\n"), flag = 1;
            if (nl) xfprintf(fp, " RHS1");
            xfprintf(fp, " %s %s", row_name(lp, i, rname), mps_numb(rhs,
               numb));
            if (wide) nl = 1 - nl;
            if (nl) xfprintf(fp, "\n");
         }
         if (!nl) xfprintf(fp, "\n");
      }
      xfree(obj);
      /* write RANGES section */
      flag = 0;
      {  int nl = 1;
         for (i = 1; i <= nrows; i++)
         {  int typx;
            double lb, ub, rng;
            lpx_get_row_bnds(lp, i, &typx, &lb, &ub);
            if (typx != LPX_DB) continue;
            if (!flag) xfprintf(fp, "RANGES\n"), flag = 1;
            if (nl) xfprintf(fp, " RNG1");
            rng = (ub > 0.0 ? ub - lb : lb - ub);
            xfprintf(fp, " %s %s", row_name(lp, i, rname), mps_numb(rng,
               numb));
            if (wide) nl = 1 - nl;
            if (nl) xfprintf(fp, "\n");
         }
         if (!nl) xfprintf(fp, "\n");
      }
      /* write BOUNDS section */
      flag = 0;
      {  for (j = 1; j <= ncols; j++)
         {  int typx;
            double lb, ub;
            lpx_get_col_bnds(lp, j, &typx, &lb, &ub);
            if (typx == LPX_LO && lb == 0.0) continue;
            if (!flag) xfprintf(fp, "BOUNDS\n"), flag = 1;
            switch (typx)
            {  case LPX_FR:
                  xfprintf(fp, " FR BND1 %s\n", col_name(lp, j, cname));
                  break;
               case LPX_LO:
                  xfprintf(fp, " LO BND1 %s %s\n", col_name(lp, j,
                     cname), mps_numb(lb, numb));
                  break;
               case LPX_UP:
                  xfprintf(fp, " MI BND1 %s\n", col_name(lp, j, cname));
                  xfprintf(fp, " UP BND1 %s %s\n", col_name(lp, j,
                     cname), mps_numb(ub, numb));
                  break;
               case LPX_DB:
                  if (lb != 0.0)
                  xfprintf(fp, " LO BND1 %s %s\n", col_name(lp, j,
                     cname), mps_numb(lb, numb));
                  xfprintf(fp, " UP BND1 %s %s\n", col_name(lp, j,
                     cname), mps_numb(ub, numb));
                  break;
               case LPX_FX:
                  xfprintf(fp, " FX BND1 %s %s\n", col_name(lp, j,
                     cname), mps_numb(lb, numb));
                  break;
               default:
                  xassert(typx != typx);
            }
         }
      }
      /* write ENDATA indicator record */
      xfprintf(fp, "ENDATA\n");
      /* close the output text file */
      xfflush(fp);
      if (xferror(fp))
      {  xprintf("lpx_write_freemps: write error on `%s' - %s\n",
            fname, strerror(errno));
         goto fail;
      }
      xfclose(fp);
      /* return to the calling program */
      return 0;
fail: /* the operation failed */
      if (fp != NULL) xfclose(fp);
      return 1;
}

/* eof */
