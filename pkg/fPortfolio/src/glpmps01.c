/* glpmps01.c (reading/writing data files in fixed MPS format) */

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
-- read_mps - read problem data in fixed MPS format.
--
-- *Synopsis*
--
-- #include "glpmps.h"
-- int read_mps(LPX *lp, const char *fname);
--
-- *Description*
--
-- The routine read_mps reads LP/MIP problem data in fixed MPS format
-- from an input text file whose name is the character string fname.
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
      /* card count */
      char card[80+1];
      /* card image */
      char f1[2+1], f2[8+1], f3[8+1], f4[12+1], f5[8+1], f6[12+1];
      /* data card fields */
      int obj;
      /* ordinal number of free (unbounded) row used as the objective
         function */
};

static int read_card(struct dsa *dsa)
{     /* read image of 80-column punched card from input file */
      int j, c;
loop: dsa->count++;
      memset(dsa->card, ' ', 80), dsa->card[80] = '\0';
      j = 0;
      for (;;)
      {  c = xfgetc(dsa->fp);
         if (xferror(dsa->fp))
         {  xprintf("%s:%d: read error - %s\n",
               dsa->fname, dsa->count, strerror(errno));
            goto fail;
         }
         if (xfeof(dsa->fp))
         {  if (j == 0)
               xprintf("%s:%d: unexpected EOF\n",
                  dsa->fname, dsa->count);
            else
               xprintf("%s:%d: missing final NL\n",
                  dsa->fname, dsa->count);
            goto fail;
         }
         if (c == '\r') continue;
         if (c == '\n') break;
         if (iscntrl(c))
         {  xprintf("%s:%d: invalid control character 0x%02X\n",
               dsa->fname, dsa->count, c);
            goto fail;
         }
         if (j == 80)
         {  xprintf("%s:%d: card image exceeds 80 chars\n",
               dsa->fname, dsa->count);
            goto fail;
         }
         dsa->card[j++] = (char)c;
      }
      /* asterisk in the first column begins a comment */
      if (dsa->card[0] == '*') goto loop;
      return 0;
fail: return 1;
}

static int blank = '_';
/* character code to replace blanks in symbolic names; it may be any
   printable ASCII character (including blank) or '\0', in which case
   all blanks are simply removed */

static void adjust_name(char name[8+1])
{     /* adjust symbolic name by replacing or removing blanks */
      if (blank == '\0')
      {  /* remove all blanks */
         strspx(name);
      }
      else
      {  int k;
         /* remove trailing blanks */
         strtrim(name);
         /* and replace remaining blanks by given character */
         for (k = 0; name[k] != '\0'; k++)
            if (name[k] == ' ') name[k] = (char)blank;
      }
      return;
}

static int split_card(struct dsa *dsa)
{     /* split data card into six fields */
      /* column 1 must be blank */
      xassert(dsa->card[0] == ' ');
      /* scan field 1 (code) in columns 2-3 */
      memcpy(dsa->f1, dsa->card+1, 2);
      dsa->f1[2] = '\0'; strspx(dsa->f1);
      /* column 4 must be blank */
      if (dsa->card[3] != ' ')
      {  xprintf("%s:%d: invalid data card; column 4 must be blank\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      /* scan field 2 (name) in columns 5-12 */
      memcpy(dsa->f2, dsa->card+4, 8);
      dsa->f2[8] = '\0'; adjust_name(dsa->f2);
      /* columns 13-14 must be blank */
      if (memcmp(dsa->card+12, "  ", 2) != 0)
      {  xprintf("%s:%d: invalid data card; columns 13-14 must be blank"
            "\n", dsa->fname, dsa->count);
         goto fail;
      }
      /* dollar sign in column 15 begins a comment */
      if (dsa->card[14] == '$')
      {  dsa->f3[0] = dsa->f4[0] = dsa->f5[0] = dsa->f6[0] = '\0';
         goto done;
      }
      /* scan field 3 (name) in columns 15-22 */
      memcpy(dsa->f3, dsa->card+14, 8);
      dsa->f3[8] = '\0'; adjust_name(dsa->f3);
      /* columns 23-24 must be blank */
      if (memcmp(dsa->card+22, "  ", 2) != 0)
      {  xprintf("%s:%d: invalid data card; columns 23-24 must be blank"
            "\n", dsa->fname, dsa->count);
         goto fail;
      }
      /* scan field 4 (number) in columns 25-36 */
      memcpy(dsa->f4, dsa->card+24, 12);
      dsa->f4[12] = '\0'; strspx(dsa->f4);
      /* columns 37-39 must be blank */
      if (memcmp(dsa->card+36, "   ", 3) != 0)
      {  xprintf("%s:%d: invalid data card; columns 37-39 must be blank"
            "\n", dsa->fname, dsa->count);
         goto fail;
      }
      /* dollar sign in column 40 begins a comment */
      if (dsa->card[39] == '$')
      {  dsa->f5[0] = dsa->f6[0] = '\0';
         goto done;
      }
      /* scan field 5 (name) in columns 40-47 */
      memcpy(dsa->f5, dsa->card+39, 8);
      dsa->f5[8] = '\0'; adjust_name(dsa->f5);
      /* columns 48-49 must be blank */
      if (memcmp(dsa->card+47, "  ", 2) != 0)
      {  xprintf("%s:%d: invalid data card; columns 48-49 must be blank"
            "\n", dsa->fname, dsa->count);
         goto fail;
      }
      /* scan field 6 (number) in columns 50-61 */
      memcpy(dsa->f6, dsa->card+49, 12);
      dsa->f6[12] = '\0'; strspx(dsa->f6);
      /* columns 62-72 must be blank */
      if (memcmp(dsa->card+61, "           ", 11) != 0)
      {  xprintf("%s:%d: invalid data card; columns 62-72 must be blank"
            "\n", dsa->fname, dsa->count);
         goto fail;
      }
done: return 0;
fail: return 1;
}

static int read_rows(struct dsa *dsa)
{     /* process data cards in ROWS section */
      int i, type;
loop: /* read and split next data card */
      if (read_card(dsa)) goto fail;
      if (dsa->card[0] != ' ') goto done;
      if (split_card(dsa)) goto fail;
      /* create new row */
      i = lpx_add_rows(dsa->lp, 1);
      /* scan row type in field 1 */
      if (dsa->f1[0] == '\0')
      {  xprintf("%s:%d: missing row type in field 1\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      else if (strcmp(dsa->f1, "N") == 0)
      {  type = LPX_FR;
         /* the first free (unbounded) row is used as the objective
            function */
         if (dsa->obj == 0) dsa->obj = i;
      }
      else if (strcmp(dsa->f1, "L") == 0)
         type = LPX_UP;
      else if (strcmp(dsa->f1, "G") == 0)
         type = LPX_LO;
      else if (strcmp(dsa->f1, "E") == 0)
         type = LPX_FX;
      else
      {  xprintf("%s:%d: invalid row type in field 1\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      lpx_set_row_bnds(dsa->lp, i, type, 0.0, 0.0);
      /* scan row name in field 2 */
      if (dsa->f2[0] == '\0')
      {  xprintf("%s:%d: missing row name in field 2\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (lpx_find_row(dsa->lp, dsa->f2) != 0)
      {  xprintf("%s:%d: row %s multiply specified\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      lpx_set_row_name(dsa->lp, i, dsa->f2);
      if (dsa->obj == i) lpx_set_obj_name(dsa->lp, dsa->f2);
      /* fields 3-6 must be blank */
      if (!(dsa->f3[0] == '\0' && dsa->f4[0] == '\0' &&
            dsa->f5[0] == '\0' && dsa->f6[0] == '\0'))
      {  xprintf("%s:%d: invalid data card; fields 3-6 must be blank\n",
            dsa->fname, dsa->count);
         goto fail;
      }
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
{     /* process data cards in COLUMNS section */
      int i, j = 0, k, len = 0, m, marker = 0, *map, *ind;
      double *aij, temp;
      /* allocate working arrays */
      m = lpx_get_num_rows(dsa->lp);
      map = xcalloc(1+m, sizeof(int));
      for (i = 1; i <= m; i++) map[i] = 0;
      ind = xcalloc(1+m, sizeof(int));
      aij = xcalloc(1+m, sizeof(double));
loop: /* read and split next data card */
      if (read_card(dsa)) goto fail;
      if (dsa->card[0] != ' ') goto done;
      if (split_card(dsa)) goto fail;
      /* field 1 must be blank */
      if (dsa->f1[0] != '\0')
      {  xprintf("%s:%d: invalid data card; field 1 must be blank\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      /* scan optional INTORG/INTEND marker */
      if (strcmp(dsa->f3, "'MARKER'") == 0)
      {  /* fields 4 and 6 must be blank */
         if (!(dsa->f4[0] == '\0' && dsa->f6[0] == '\0'))
         {  xprintf("%s:%d: invalid data card; fields 4 and 6 must be b"
               "lank\n", dsa->fname, dsa->count);
            goto fail;
         }
         /* scan marker name in field 2 */
         if (dsa->f2[0] == '\0')
         {  xprintf("%s:%d: missing marker name in field 2\n",
               dsa->fname, dsa->count);
            goto fail;
         }
         /* scan marker type in field 5 */
         if (dsa->f5[0] == '\0')
         {  xprintf("%s:%d: missing marker type in field 5\n",
               dsa->fname, dsa->count);
            goto fail;
         }
         else if (strcmp(dsa->f5, "'INTORG'") == 0)
            marker = 1;
         else if (strcmp(dsa->f5, "'INTEND'") == 0)
            marker = 0;
         else
         {  xprintf("%s:%d: invalid marker type in field 5\n",
               dsa->fname, dsa->count);
            goto fail;
         }
         goto loop;
      }
      /* scan column name in field 2 */
      if (dsa->f2[0] == '\0')
      {  if (j == 0)
         {  xprintf("%s:%d: missing column name in field 2\n",
               dsa->fname, dsa->count);
            goto fail;
         }
         /* still the current column */
         goto skip;
      }
      if (j != 0 && strcmp(lpx_get_col_name(dsa->lp, j), dsa->f2) == 0)
      {  /* still the current column */
         goto skip;
      }
      /* new column begins, so finish the current column */
      if (j != 0)
      {  for (k = 1; k <= len; k++) map[ind[k]] = 0;
         put_column(dsa, j, len, ind, aij), len = 0;
      }
      /* create new column */
      if (lpx_find_col(dsa->lp, dsa->f2) != 0)
      {  xprintf("%s:%d: column %s multiply specified\n", dsa->fname,
            dsa->count, dsa->f2);
         goto fail;
      }
      j = lpx_add_cols(dsa->lp, 1);
      lpx_set_col_name(dsa->lp, j, dsa->f2);
      if (marker)
      {  /* lpx_set_class(dsa->lp, LPX_MIP); */
         lpx_set_col_kind(dsa->lp, j, LPX_IV);
      }
      lpx_set_col_bnds(dsa->lp, j, LPX_LO, 0.0, 0.0);
skip: /* scan row name and coefficient in fields 3 and 4 */
      if (dsa->f3[0] == '\0')
      {  xprintf("%s:%d: missing row name in field 3\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      i = lpx_find_row(dsa->lp, dsa->f3);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            dsa->f3);
         goto fail;
      }
      if (dsa->f4[0] == '\0')
      {  xprintf("%s:%d: missing coefficient value in field 4\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      if (str2num(dsa->f4, &temp))
      {  xprintf("%s:%d: invalid coefficient value in field 4\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: coefficient in row %s multiply specified\n",
            dsa->fname, dsa->count, dsa->f3);
         goto fail;
      }
      map[i] = 1, len++, ind[len] = i, aij[len] = temp;
      /* scan optional row name and coefficient in fields 5 and 6 */
      if (dsa->f5[0] == '\0' && dsa->f6[0] == '\0') goto loop;
      if (dsa->f5[0] == '\0')
      {  xprintf("%s:%d: missing row name in field 5\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      i = lpx_find_row(dsa->lp, dsa->f5);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            dsa->f5);
         goto fail;
      }
      if (dsa->f6[0] == '\0')
      {  xprintf("%s:%d: missing coefficient value in field 6\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      if (str2num(dsa->f6, &temp))
      {  xprintf("%s:%d: invalid coefficient value in field 6\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: coefficient in row %s multiply specified\n",
            dsa->fname, dsa->count, dsa->f5);
         goto fail;
      }
      map[i] = 1, len++, ind[len] = i, aij[len] = temp;
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
{     /* process data cards in RHS section */
      int i, m, *map;
      double temp;
      char name[8+1] = "\n";
      /* allocate working array */
      m = lpx_get_num_rows(dsa->lp);
      map = xcalloc(1+m, sizeof(int));
      for (i = 1; i <= m; i++) map[i] = 0;
loop: /* read and split next data card */
      if (read_card(dsa)) goto fail;
      if (dsa->card[0] != ' ') goto done;
      if (split_card(dsa)) goto fail;
      /* field 1 must be blank */
      if (dsa->f1[0] != '\0')
      {  xprintf("%s:%d: invalid data card; field 1 must be blank\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      /* scan optional rhs vector name in field 2 */
      if (name[0] == '\n') strcpy(name, dsa->f2);
      if (!(dsa->f2[0] == '\0' || strcmp(dsa->f2, name) == 0))
      {  xprintf("%s:%d: at most one rhs vector allowed\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      /* scan row name and rhs value in fields 3 and 4 */
      if (dsa->f3[0] == '\0')
      {  xprintf("%s:%d: missing row name in field 3\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      i = lpx_find_row(dsa->lp, dsa->f3);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            dsa->f3);
         goto fail;
      }
      if (dsa->f4[0] == '\0')
      {  xprintf("%s:%d: missing rhs value in field 4\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (str2num(dsa->f4, &temp))
      {  xprintf("%s:%d: invalid rhs value in field 4\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: rhs for row %s multiply specified\n",
            dsa->fname, dsa->count, dsa->f3);
         goto fail;
      }
      set_rhs(dsa, i, temp), map[i] = 1;
      /* scan optional row name and rhs value in fields 5 and 6 */
      if (dsa->f5[0] == '\0' && dsa->f6[0] == '\0') goto loop;
      if (dsa->f5[0] == '\0')
      {  xprintf("%s:%d: missing row name in field 5\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      i = lpx_find_row(dsa->lp, dsa->f5);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            dsa->f5);
         goto fail;
      }
      if (dsa->f6[0] == '\0')
      {  xprintf("%s:%d: missing rhs value in field 6\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (str2num(dsa->f6, &temp))
      {  xprintf("%s:%d: invalid rhs value in field 6\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: rhs for row %s multiply specified\n",
            dsa->fname, dsa->count, dsa->f5);
         goto fail;
      }
      set_rhs(dsa, i, temp), map[i] = 1;
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
{     /* process data cards in RANGES section */
      int i, m, *map;
      double temp;
      char name[8+1] = "\n";
      /* allocate working array */
      m = lpx_get_num_rows(dsa->lp);
      map = xcalloc(1+m, sizeof(int));
      for (i = 1; i <= m; i++) map[i] = 0;
loop: /* read and split next data card */
      if (read_card(dsa)) goto fail;
      if (dsa->card[0] != ' ') goto done;
      if (split_card(dsa)) goto fail;
      /* field 1 must be blank */
      if (dsa->f1[0] != '\0')
      {  xprintf("%s:%d: invalid data card; field 1 must be blank\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      /* scan optional ranges vector name in field 2 */
      if (name[0] == '\n') strcpy(name, dsa->f2);
      if (!(dsa->f2[0] == '\0' || strcmp(dsa->f2, name) == 0))
      {  xprintf("%s:%d: at most one ranges vector allowed\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      /* scan row name and range value in fields 3 and 4 */
      if (dsa->f3[0] == '\0')
      {  xprintf("%s:%d: missing row name in field 3\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      i = lpx_find_row(dsa->lp, dsa->f3);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            dsa->f3);
         goto fail;
      }
      if (dsa->f4[0] == '\0')
      {  xprintf("%s:%d: missing range value in field 4\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (str2num(dsa->f4, &temp))
      {  xprintf("%s:%d: invalid range value in field 4\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: range for row %s multiply specified\n",
            dsa->fname, dsa->count, dsa->f3);
         goto fail;
      }
      set_range(dsa, i, temp), map[i] = 1;
      /* scan optional row name and range value in fields 5 and 6 */
      if (dsa->f5[0] == '\0' && dsa->f6[0] == '\0') goto loop;
      if (dsa->f5[0] == '\0')
      {  xprintf("%s:%d: missing row name in field 5\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      i = lpx_find_row(dsa->lp, dsa->f5);
      if (i == 0)
      {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
            dsa->f5);
         goto fail;
      }
      if (dsa->f6[0] == '\0')
      {  xprintf("%s:%d: missing range value in field 6\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (str2num(dsa->f6, &temp))
      {  xprintf("%s:%d: invalid range value in field 6\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (map[i])
      {  xprintf("%s:%d: range for row %s multiply specified\n",
            dsa->fname, dsa->count, dsa->f5);
         goto fail;
      }
      set_range(dsa, i, temp), map[i] = 1;
      goto loop;
done: xfree(map);
      return 0;
fail: xfree(map);
      return 1;
}

static int read_bounds(struct dsa *dsa)
{     /* process data cards in BOUNDS section */
      int j, type;
      double lb, ub, temp;
      char name[8+1] = "\n";
loop: /* read and split next data card */
      if (read_card(dsa)) goto fail;
      if (dsa->card[0] != ' ') goto done;
      if (split_card(dsa)) goto fail;
      /* scan bound type in field 1 */
      if (dsa->f1[0] == '\0')
      {  xprintf("%s:%d: missing bound type in field 1\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      /* scan optional bounds vector name in field 2 */
      if (name[0] == '\n') strcpy(name, dsa->f2);
      if (!(dsa->f2[0] == '\0' || strcmp(dsa->f2, name) == 0))
      {  xprintf("%s:%d: at most one bounds vector allowed\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      /* scan column name in field 3 */
      if (dsa->f3[0] == '\0')
      {  xprintf("%s:%d: missing column name in field 3\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      j = lpx_find_col(dsa->lp, dsa->f3);
      if (j == 0)
      {  xprintf("%s:%d: column %s not found\n", dsa->fname, dsa->count,
            dsa->f3);
         goto fail;
      }
      /* scan optional bound value in field 4 */
      if (!(strcmp(dsa->f1, "LO") == 0 || strcmp(dsa->f1, "UP") == 0 ||
            strcmp(dsa->f1, "FX") == 0 || strcmp(dsa->f1, "LI") == 0 ||
            strcmp(dsa->f1, "UI") == 0))
         goto skip;
      if (dsa->f4[0] == '\0')
      {  xprintf("%s:%d: missing bound value in field 4\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (str2num(dsa->f4, &temp))
      {  xprintf("%s:%d: invalid bound value in field 4\n", dsa->fname,
            dsa->count);
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
      if (strcmp(dsa->f1, "LO") == 0)
         lb = temp;
      else if (strcmp(dsa->f1, "UP") == 0)
         ub = temp;
      else if (strcmp(dsa->f1, "FX") == 0)
         lb = ub = temp;
      else if (strcmp(dsa->f1, "FR") == 0)
         lb = -DBL_MAX, ub = +DBL_MAX;
      else if (strcmp(dsa->f1, "MI") == 0)
         lb = -DBL_MAX;
      else if (strcmp(dsa->f1, "PL") == 0)
         ub = +DBL_MAX;
#if 1 /* 11/V-2006 */
      else if (strcmp(dsa->f1, "LI") == 0)
      {  /* lpx_set_class(dsa->lp, LPX_MIP); */
         lpx_set_col_kind(dsa->lp, j, LPX_IV);
         lb = temp;
      }
#endif
      else if (strcmp(dsa->f1, "UI") == 0)
      {  /* lpx_set_class(dsa->lp, LPX_MIP); */
         lpx_set_col_kind(dsa->lp, j, LPX_IV);
         ub = temp;
      }
      else if (strcmp(dsa->f1, "BV") == 0)
      {  /* lpx_set_class(dsa->lp, LPX_MIP); */
         lpx_set_col_kind(dsa->lp, j, LPX_IV);
         lb = 0.0, ub = 1.0;
      }
      else
      {  xprintf("%s:%d: invalid bound type in field 1\n", dsa->fname,
            dsa->count);
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
      /* fields 5-6 must be blank */
      if (!(dsa->f5[0] == '\0' && dsa->f6[0] == '\0'))
      {  xprintf("%s:%d: invalid data card; fields 5-6 must be blank\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      goto loop;
done: return 0;
fail: return 1;
}

int read_mps(LPX *lp, const char *fname)
{     /* read problem data in MPS format */
      struct dsa _dsa, *dsa = &_dsa;
      glp_erase_prob(lp);
      dsa->lp = lp;
      dsa->fname = fname;
      dsa->fp = NULL;
      dsa->count = 0;
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
      /* read NAME indicator card */
      if (read_card(dsa)) goto fail;
      if (memcmp(dsa->card, "NAME ", 5) != 0)
      {  xprintf("%s:%d: NAME indicator card missing\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      memcpy(dsa->f3, dsa->card+14, 8);
      dsa->f3[8] = '\0'; adjust_name(dsa->f3);
      lpx_set_prob_name(dsa->lp, dsa->f3);
      if (dsa->f3[0] == '\0')
         xprintf("glp_read_mps: problem name not specified\n");
      else
         xprintf("glp_read_mps: problem %s\n", dsa->f3);
      /* read ROWS section */
      if (read_card(dsa)) goto fail;
      if (memcmp(dsa->card, "ROWS ", 5) != 0)
      {  xprintf("%s:%d: ROWS indicator card missing\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (read_rows(dsa)) goto fail;
      /* read COLUMNS section */
      if (memcmp(dsa->card, "COLUMNS ", 8) != 0)
      {  xprintf("%s:%d: COLUMNS indicator card missing\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      if (read_columns(dsa)) goto fail;
      {  int m = lpx_get_num_rows(dsa->lp);
         int n = lpx_get_num_cols(dsa->lp);
         int nnz = lpx_get_num_nz(dsa->lp);
         xprintf("glp_read_mps: %d row%s, %d column%s, %d non-zero%s\n",
            m, m == 1 ? "" : "s", n, n == 1 ? "" : "s", nnz, nnz == 1 ?
            "" : "s");
      }
      /* read RHS section (optional) */
      if (memcmp(dsa->card, "RHS ", 4) == 0)
         if (read_rhs(dsa)) goto fail;
      /* read RANGES section (optional) */
      if (memcmp(dsa->card, "RANGES ", 7) == 0)
         if (read_ranges(dsa)) goto fail;
      /* read BOUNDS section (optional) */
      if (memcmp(dsa->card, "BOUNDS ", 7) == 0)
         if (read_bounds(dsa)) goto fail;
      /* check ENDATA indicator card */
      if (memcmp(dsa->card, "ENDATA ", 7) != 0)
      {  xprintf("%s:%d: ENDATA indicator card missing\n", dsa->fname,
            dsa->count);
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
      xprintf("glp_read_mps: %d cards were read\n", dsa->count);
      xfclose(dsa->fp);
      lpx_delete_index(dsa->lp);
      lpx_order_matrix(dsa->lp);
      return 0;
fail: if (dsa->lp != NULL) glp_erase_prob(dsa->lp);
      if (dsa->fp != NULL) xfclose(dsa->fp);
      return 1;
}

/*----------------------------------------------------------------------
-- write_mps - write problem data in fixed MPS format.
--
-- *Synopsis*
--
-- #include "glpmps.h"
-- int write_mps(LPX *lp, const char *fname);
--
-- *Description*
--
-- The routine write_mps writes problem data in fixed MPS format to an
-- output text file whose name is the character string fname.
--
-- *Returns*
--
-- If the operation was successful, the routine returns zero. Otherwise
-- the routine prints an error message and returns non-zero. */

static char *row_name(LPX *lp, int i, char rname[8+1])
{     const char *str = NULL;
      if (lpx_get_int_parm(lp, LPX_K_MPSORIG))
      {  if (i == 0)
            str = lpx_get_obj_name(lp);
         else
            str = lpx_get_row_name(lp, i);
         if (str != NULL && strlen(str) > 8) str = NULL;
      }
      if (str == NULL)
         sprintf(rname, "R%07d", i);
      else
         strcpy(rname, str);
      return rname;
}

static char *col_name(LPX *lp, int j, char cname[8+1])
{     const char *str = NULL;
      if (lpx_get_int_parm(lp, LPX_K_MPSORIG))
      {  str = lpx_get_col_name(lp, j);
         if (str != NULL && strlen(str) > 8) str = NULL;
      }
      if (str == NULL)
         sprintf(cname, "C%07d", j);
      else
         strcpy(cname, str);
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

int write_mps(LPX *lp, const char *fname)
{     XFILE *fp;
      int wide = lpx_get_int_parm(lp, LPX_K_MPSWIDE);
      int frei = lpx_get_int_parm(lp, LPX_K_MPSFREE);
      int skip = lpx_get_int_parm(lp, LPX_K_MPSSKIP);
      int marker = 0; /* intorg/intend marker count */
      int mip, make_obj, nrows, ncols, i, j, flag, *ndx;
      double *obj, *val;
      char rname[8+1], cname[8+1], vname[8+1], numb[12+1];
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
      /* write comment cards (if required) */
      if (lpx_get_int_parm(lp, LPX_K_MPSINFO))
      {  const char *name = lpx_get_prob_name(lp);
         if (name == NULL) name = "UNKNOWN";
         xfprintf(fp, "* Problem:    %.31s\n", name);
         xfprintf(fp, "* Class:      %s\n", !mip ? "LP" : "MIP");
         xfprintf(fp, "* Rows:       %d\n", nrows);
         if (!mip)
            xfprintf(fp, "* Columns:    %d\n", ncols);
         else
            xfprintf(fp, "* Columns:    %d (%d integer, %d binary)\n",
               ncols, lpx_get_num_int(lp), lpx_get_num_bin(lp));
         xfprintf(fp, "* Non-zeros:  %d\n", lpx_get_num_nz(lp));
         xfprintf(fp, "* Format:     Fixed MPS\n");
         xfprintf(fp, "*\n");
      }
      /* write NAME indicator card */
      {  const char *name = lpx_get_prob_name(lp);
         if (name == NULL)
            xfprintf(fp, "NAME\n");
         else
            xfprintf(fp, "NAME          %.8s\n", name);
      }
      /* write ROWS section */
      xfprintf(fp, "ROWS\n");
      if (make_obj)
         xfprintf(fp, " %c  %s\n", 'N', row_name(lp, 0, rname));
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
         xfprintf(fp, " %c  %s\n", typx, row_name(lp, i, rname));
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
           xfprintf(fp, "    M%07d  'MARKER'                 'INTORG'\n"
               , marker);
         }
         else if (!iv && marker % 2 == 1)
         {  /* close the current intorg/intend group */
            marker++;
           xfprintf(fp, "    M%07d  'MARKER'                 'INTEND'\n"
               , marker);
         }
         /* obtain j-th column of the constraint matrix */
         len = lpx_get_mat_col(lp, j, ndx, val);
         ndx[0] = 0;
         val[0] = (make_obj ? obj[j] : 0.0);
         if (len == 0 && val[0] == 0.0 && !skip)
            xfprintf(fp, "    %-8s  %-8s  %12s   $ empty column\n",
               cname, row_name(lp, 1, rname), mps_numb(0.0, numb));
         for (t = val[0] != 0.0 ? 0 : 1; t <= len; t++)
         {  if (nl)
               xfprintf(fp, "    %-8s  ", cname);
            else
               xfprintf(fp, "   ");
            xfprintf(fp, "%-8s  %12s",
               row_name(lp, ndx[t], rname), mps_numb(val[t], numb));
            if (wide) nl = 1 - nl;
            if (nl) xfprintf(fp, "\n");
            if (frei) strcpy(cname, "");
         }
         if (!nl) xfprintf(fp, "\n");
      }
      if (marker % 2 == 1)
      {  /* close the last intorg/intend group (if not closed) */
         marker++;
         xfprintf(fp, "    M%07d  'MARKER'                 'INTEND'\n",
            marker);
      }
      xfree(ndx);
      xfree(val);
      /* write RHS section */
      flag = 0;
      {  int nl = 1;
         strcpy(vname, frei ? "" : "RHS1");
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
            if (nl)
                xfprintf(fp, "    %-8s  ", vname);
            else
                xfprintf(fp, "   ");
            xfprintf(fp, "%-8s  %12s",
               row_name(lp, i, rname), mps_numb(rhs, numb));
            if (wide) nl = 1 - nl;
            if (nl) xfprintf(fp, "\n");
         }
         if (!nl) xfprintf(fp, "\n");
      }
      xfree(obj);
      /* write RANGES section */
      flag = 0;
      {  int nl = 1;
         strcpy(vname, frei ? "" : "RNG1");
         for (i = 1; i <= nrows; i++)
         {  int typx;
            double lb, ub, rng;
            lpx_get_row_bnds(lp, i, &typx, &lb, &ub);
            if (typx != LPX_DB) continue;
            if (!flag) xfprintf(fp, "RANGES\n"), flag = 1;
            if (nl)
                xfprintf(fp, "    %-8s  ", vname);
            else
                xfprintf(fp, "   ");
            rng = (ub > 0.0 ? ub - lb : lb - ub);
            xfprintf(fp, "%-8s  %12s",
               row_name(lp, i, rname), mps_numb(rng, numb));
            if (wide) nl = 1 - nl;
            if (nl) xfprintf(fp, "\n");
         }
         if (!nl) xfprintf(fp, "\n");
      }
      /* write BOUNDS section */
      flag = 0;
      {  strcpy(vname, frei ? "" : "BND1");
         for (j = 1; j <= ncols; j++)
         {  int typx;
            double lb, ub;
            lpx_get_col_bnds(lp, j, &typx, &lb, &ub);
            if (typx == LPX_LO && lb == 0.0) continue;
            if (!flag) xfprintf(fp, "BOUNDS\n"), flag = 1;
            switch (typx)
            {  case LPX_FR:
                  xfprintf(fp, " FR %-8s  %-8s\n", vname,
                     col_name(lp, j, cname));
                  break;
               case LPX_LO:
                  xfprintf(fp, " LO %-8s  %-8s  %12s\n", vname,
                     col_name(lp, j, cname), mps_numb(lb, numb));
                  break;
               case LPX_UP:
                  xfprintf(fp, " MI %-8s  %-8s\n", vname,
                     col_name(lp, j, cname));
                  xfprintf(fp, " UP %-8s  %-8s  %12s\n", vname,
                     col_name(lp, j, cname), mps_numb(ub, numb));
                  break;
               case LPX_DB:
                  if (lb != 0.0)
                  xfprintf(fp, " LO %-8s  %-8s  %12s\n", vname,
                     col_name(lp, j, cname), mps_numb(lb, numb));
                  xfprintf(fp, " UP %-8s  %-8s  %12s\n", vname,
                     col_name(lp, j, cname), mps_numb(ub, numb));
                  break;
               case LPX_FX:
                  xfprintf(fp, " FX %-8s  %-8s  %12s\n", vname,
                     col_name(lp, j, cname), mps_numb(lb, numb));
                  break;
               default:
                  xassert(typx != typx);
            }
         }
      }
      /* write ENDATA indicator card */
      xfprintf(fp, "ENDATA\n");
      /* close the output text file */
      xfflush(fp);
      if (xferror(fp))
      {  xprintf("glp_write_mps: write error on `%s' - %s\n", fname,
            strerror(errno));
         goto fail;
      }
      xfclose(fp);
      /* return to the calling program */
      return 0;
fail: /* the operation failed */
      if (fp != NULL) xfclose(fp);
      return 1;
}

/*----------------------------------------------------------------------
-- read_bas - read LP basis in fixed MPS format.
--
-- *Synopsis*
--
-- #include "glpmps.h"
-- int read_bas(LPX *lp, char *fname);
--
-- *Description*
--
-- The routine read_bas reads LP basis in fixed MPS format from an
-- input text file whose name is the character string fname.
--
-- *Returns*
--
-- If the operation was successful, the routine returns zero. Otherwise
-- the routine prints an error message and returns non-zero. */

int read_bas(LPX *lp, const char *fname)
{     struct dsa _dsa, *dsa = &_dsa;
      int i, j;
      dsa->lp = lp;
      dsa->fname = fname;
      dsa->fp = NULL;
      dsa->count = 0;
      dsa->obj = 0;
      xprintf("lpx_read_bas: reading LP basis from `%s'...\n",
         dsa->fname);
      dsa->fp = xfopen(dsa->fname, "r");
      if (dsa->fp == NULL)
      {  xprintf("lpx_read_bas: unable to open `%s' - %s\n",
            dsa->fname, strerror(errno));
         goto fail;
      }
      lpx_create_index(dsa->lp);
      /* read NAME indicator card */
      if (read_card(dsa)) goto fail;
      if (memcmp(dsa->card, "NAME ", 5) != 0)
      {  xprintf("%s:%d: NAME indicator card missing\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      /* build the "standard" basis of all slacks */
      lpx_std_basis(dsa->lp);
loop: /* read and split next data card */
      if (read_card(dsa)) goto fail;
      if (dsa->card[0] != ' ') goto fini;
      if (split_card(dsa)) goto fail;
      /* check indicator in field 1 */
      if (!(strcmp(dsa->f1, "XL") == 0 || strcmp(dsa->f1, "XU") == 0 ||
            strcmp(dsa->f1, "LL") == 0 || strcmp(dsa->f1, "UL") == 0))
      {  xprintf("%s:%d: invalid indicator in field 1\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      /* scan column name in field 2 */
      if (dsa->f2[0] == '\0')
      {  xprintf("%s:%d: missing column name in field 2\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      j = lpx_find_col(dsa->lp, dsa->f2);
      if (j == 0)
      {  xprintf("%s:%d: column %s not found\n", dsa->fname, dsa->count,
            dsa->f2);
         goto fail;
      }
      /* process field 3 */
      if (dsa->f1[0] == 'X')
      {  /* scan row name in field 3 */
         if (dsa->f3[0] == '\0')
         {  xprintf("%s:%d: missing row name in field 3\n", dsa->fname,
               dsa->count);
            goto fail;
         }
         i = lpx_find_row(dsa->lp, dsa->f3);
         if (i == 0)
         {  xprintf("%s:%d: row %s not found\n", dsa->fname, dsa->count,
               dsa->f3);
            goto fail;
         }
      }
      else
      {  /* field 3 must be blank */
         if (dsa->f3[0] != '\0')
         {  xprintf("%s:%d: invalid data card; field 3 must be blank\n",
               dsa->fname, dsa->count);
            goto fail;
         }
         i = 0;
      }
      /* fields 4-6 must be blank */
      if (!(dsa->f4[0] == '\0' && dsa->f5[0] == '\0' &&
            dsa->f6[0] == '\0'))
      {  xprintf("%s:%d: invalid data card; fields 4-6 must be blank\n",
            dsa->fname, dsa->count);
         goto fail;
      }
      /* apply a "patch" */
      if (dsa->f1[0] == 'X')
      {  lpx_set_row_stat(dsa->lp, i, dsa->f1[1] == 'L' ? LPX_NL :
            LPX_NU);
         lpx_set_col_stat(dsa->lp, j, LPX_BS);
      }
      else
         lpx_set_col_stat(dsa->lp, j, dsa->f1[0] == 'L' ? LPX_NL :
            LPX_NU);
      goto loop;
fini: /* check ENDATA indicator card */
      if (memcmp(dsa->card, "ENDATA ", 7) != 0)
      {  xprintf("%s:%d: ENDATA indicator card missing\n", dsa->fname,
            dsa->count);
         goto fail;
      }
      xprintf("lpx_read_bas: %d cards were read\n", dsa->count);
      xfclose(dsa->fp);
      lpx_delete_index(dsa->lp);
      return 0;
fail: if (dsa->fp != NULL) xfclose(dsa->fp);
      lpx_delete_index(dsa->lp);
      return 1;
}

/*----------------------------------------------------------------------
-- write_bas - write LP basis in fixed MPS format.
--
-- *Synopsis*
--
-- #include "glpmps.h"
-- int write_bas(LPX *lp, char *fname);
--
-- *Description*
--
-- The routine write_bas writes current LP basis in fixed MPS format to
-- an output text file whose name is the character string fname.
--
-- *Returns*
--
-- If the operation was successful, the routine returns zero. Otherwise
-- the routine prints an error message and returns non-zero. */

int write_bas(LPX *lp, const char *fname)
{     XFILE *fp;
      int nrows, ncols, i, j, rtype, ctype, rstat, cstat;
      char rname[8+1], cname[8+1];
      xprintf("lpx_write_bas: writing LP basis to `%s'...\n", fname);
      /* open the output text file */
      fp = xfopen(fname, "w");
      if (fp == NULL)
      {  xprintf("lpx_write_bas: unable to create `%s' - %s\n", fname,
            strerror(errno));
         goto fail;
      }
      /* determine number of rows and number of columns */
      nrows = lpx_get_num_rows(lp);
      ncols = lpx_get_num_cols(lp);
      /* the problem should contain at least one row and one column */
      if (!(nrows > 0 && ncols > 0))
         xerror("lpx_write_bas: problem has no rows/columns\n");
      /* write comment cards (if required) */
      if (lpx_get_int_parm(lp, LPX_K_MPSINFO))
      {  int dir, status;
         double obj;
         const char *name;
         /* problem name and statistics */
         name = lpx_get_prob_name(lp);
         if (name == NULL) name = "UNKNOWN";
         xfprintf(fp, "* Problem:    %.31s\n", name);
         xfprintf(fp, "* Rows:       %d\n", nrows);
         xfprintf(fp, "* Columns:    %d\n", ncols);
         xfprintf(fp, "* Non-zeros:  %d\n", lpx_get_num_nz(lp));
         /* solution status */
         status = lpx_get_status(lp);
         xfprintf(fp, "* Status:     %s\n",
            status == LPX_OPT    ? "OPTIMAL" :
            status == LPX_FEAS   ? "FEASIBLE" :
            status == LPX_INFEAS ? "INFEASIBLE (INTERMEDIATE)" :
            status == LPX_NOFEAS ? "INFEASIBLE (FINAL)" :
            status == LPX_UNBND  ? "UNBOUNDED" :
            status == LPX_UNDEF  ? "UNDEFINED" : "???");
         /* objective function */
         name = lpx_get_obj_name(lp);
         dir = lpx_get_obj_dir(lp);
         obj = lpx_get_obj_val(lp);
         xfprintf(fp, "* Objective:  %s%s%.10g %s\n",
            name == NULL ? "" : name,
            name == NULL ? "" : " = ", obj,
            dir == LPX_MIN ? "(MINimum)" :
            dir == LPX_MAX ? "(MAXimum)" : "(" "???" ")");
         xfprintf(fp, "* Format:     Fixed MPS\n");
         xfprintf(fp, "*\n");
      }
      /* write NAME indicator card */
      {  const char *name = lpx_get_prob_name(lp);
         if (name == NULL)
            xfprintf(fp, "NAME\n");
         else
            xfprintf(fp, "NAME          %.8s\n", name);
      }
      /* write information about which columns should be made basic
         and which rows should be made non-basic */
      i = j = 0;
      for (;;)
      {  /* find a next non-basic row */
         for (i++; i <= nrows; i++)
         {  lpx_get_row_info(lp, i, &rstat, NULL, NULL);
            if (rstat != LPX_BS) break;
         }
         /* find a next basic column */
         for (j++; j <= ncols; j++)
         {  lpx_get_col_info(lp, j, &cstat, NULL, NULL);
            if (cstat == LPX_BS) break;
         }
         /* if all non-basic rows and basic columns have been written,
            break the loop */
         if (i > nrows && j > ncols) break;
         /* since the basis is valid, there must be nor extra non-basic
            rows nor extra basic columns */
         xassert(i <= nrows && j <= ncols);
         /* write the pair (basic column, non-basic row) */
         lpx_get_row_bnds(lp, i, &rtype, NULL, NULL);
         xfprintf(fp, " %s %-8s  %s\n",
            (rtype == LPX_DB && rstat == LPX_NU) ? "XU" : "XL",
            col_name(lp, j, cname), row_name(lp, i, rname));
      }
      /* write information about statuses of double-bounded non-basic
         columns */
      for (j = 1; j <= ncols; j++)
      {  lpx_get_col_bnds(lp, j, &ctype, NULL, NULL);
         lpx_get_col_info(lp, j, &cstat, NULL, NULL);
         if (ctype == LPX_DB && cstat != LPX_BS)
            xfprintf(fp, " %s %s\n",
               cstat == LPX_NU ? "UL" : "LL", col_name(lp, j, cname));
      }
      /* write ENDATA indcator card */
      xfprintf(fp, "ENDATA\n");
      /* close the output text file */
      xfflush(fp);
      if (xferror(fp))
      {  xprintf("lpx_write_bas: write error on `%s' - %s\n", fname,
            strerror(errno));
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
