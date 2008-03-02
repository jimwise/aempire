/*
 *    Copyright (C) 1987, 1988 Chuck Simmons
 * 
 * See the file COPYING, distributed with empire, for restriction
 * and warranty information.
 *
 * $Id: display.c,v 1.3 2008/03/02 23:46:26 jwise Exp $
 */

/*
 * display.c -- This file contains routines for displaying sectors and
 * moving the cursor about in a sector.  We need to remember the following
 * information:
 * 
 * 	the current map portion displayed on the screen;
 * 
 * 	whether the displayed portion is from the user's or the computer's
 * 	point of view;
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "empire.h"
#include "extern.h"

#ifdef USE_NCURSES
#include <ncurses.h>
#else
#include <curses.h>
#endif

int     cur_sector (void);
void    display_loc (int, view_map_t[], long);
void    display_locx (int, view_map_t[], long);
void    display_score (void);
void    help (const char **, int);
void	kill_display (void);
void	map_init (void);
int     move_cursor (long *, int);
void	print_movie_screen(const char *);
void    print_sector (char, view_map_t[], int);
void    print_pzoom (const char *, const path_map_t *, const view_map_t *);
void    print_zoom (const view_map_t *);
void	sector_change (void);
static void	display_screen (view_map_t[]);
static void	disp_square(view_map_t *);
static int	on_screen (long);
static void	print_pzoom_cell (const path_map_t *, const view_map_t *, int, int, int, int);
static void	print_zoom_cell (const view_map_t *, int, int, int, int);
static void	show_loc (view_map_t[], long);

static int whose_map = UNOWNED; /* user's or computer's point of view */
static int ref_row; /* map loc displayed in upper-left corner */
static int ref_col;
static int save_sector; /* the currently displayed sector */
static int save_cursor; /* currently displayed cursor position */
static int change_ok = TRUE; /* true if new sector may be displayed */

#define NUMBOTS		1
#define MAPWIN_HEIGHT	(lines - NUMTOPS - NUMBOTS)
#define	MAPWIN_WIDTH	(cols - NUMSIDES)

static WINDOW *mapwin;

void
map_init (void)
{
	mapwin = newwin(MAPWIN_HEIGHT, MAPWIN_WIDTH, NUMTOPS, 0);
}

/*
 * This routine is called when the current display has been
 * trashed and no sector is shown on the screen.
 */

void
kill_display (void)
{
	whose_map = UNOWNED;
}

/*
 * This routine is called when a new sector may be displayed on the
 * screen even if the location to be displayed is already on the screen.
 */

void
sector_change (void)
{
	change_ok = TRUE;
}

/*
 * Return the currently displayed user sector, if any.  If a user
 * sector is not displayed, return -1.
 */

int
cur_sector (void)
{
  -- NEED TO return 0 in first case, not -1 (FOR NOW)
	if (whose_map != USER)
		return (-1);

	return (save_sector);
}

/*
 * Display a location on the screen. We figure out the sector the
 * location is in and display that sector.  The cursor is left at
 * the requested location.
 * 
 * We redisplay the sector only if we either have been requested to
 * redisplay the sector, or if the location is not on the screen.
 */

void
display_loc (int whose, view_map_t vmap[], long loc)
{
	if (change_ok || (whose != whose_map) || !on_screen (loc))
		print_sector (whose, vmap, loc_sector (loc));
		
	show_loc (vmap, loc);
}

/* Display a location iff the location is on the screen. */

void
display_locx (int whose, view_map_t vmap[], long loc)
{
	if (whose == whose_map && on_screen (loc))
		show_loc (vmap, loc);
}

/* Display a location which exists on the screen. */

static void
show_loc (view_map_t vmap[], long loc)
{
	int r, c;
	
	r = loc_row (loc);
	c = loc_col (loc);
	wmove(mapwin, r-ref_row+1, c-ref_col+1);
	disp_square(&vmap[loc]);
	save_cursor = loc; /* remember cursor location */
	wmove(mapwin, r-ref_row+1, c-ref_col+1);
	wrefresh(mapwin);
}

/*
 * Print a sector of the user's on the screen.  If it is already displayed,
 * we do nothing.  Otherwise we redraw the screen.  Someday, some intelligence
 * in doing this might be interesting.  We heavily depend on curses to update
 * the screen in a reasonable fashion.
 * 
 * If the desired sector
 * is not displayed, we clear the screen.  We then update the screen
 * to reflect the current map.  We heavily depend on curses to correctly
 * optimize the redrawing of the screen.
 * 
 * When redrawing the screen, we figure out where the
 * center of the sector is in relation to the map.  We then compute
 * the screen coordinates where we want to display the center of the
 * sector.  We will remember the sector displayed, the map displayed,
 * and the map location that appears in the upper-left corner of the
 * screen.
 */
 
void
print_sector (char whose, view_map_t vmap[], int sector)
{
	int first_row, first_col, last_row, last_col;
	int display_rows, display_cols;
	int r, c;

	save_sector = sector; /* remember last sector displayed */
	change_ok = FALSE; /* we are displaying a new sector */

	display_rows = MAPWIN_HEIGHT - 2; /* num lines to display */
	display_cols = MAPWIN_WIDTH - 2;

	/* compute row and column edges of sector */
	first_row = sector_row (sector) * ROWS_PER_SECTOR;
	first_col = sector_col (sector) * COLS_PER_SECTOR;
	last_row = first_row + ROWS_PER_SECTOR - 1;
	last_col = first_col + COLS_PER_SECTOR - 1;

	if ((whose != whose_map)
		    || (!on_screen(row_col_loc(first_row, first_col)))
		    || (!on_screen(row_col_loc(last_row, last_col))))
		wclear(mapwin); /* erase current screen */

	/* figure out first row and col to print; subtract half the extra lines from the first line */

	ref_row = first_row - (display_rows - ROWS_PER_SECTOR) / 2;
	ref_col = first_col - (display_cols - COLS_PER_SECTOR) / 2;

	/* try not to go past bottom of usable map (outer rim cannot be used) */
	if (ref_row > MAP_HEIGHT - display_rows - 1)
		ref_row = MAP_HEIGHT - display_rows - 1;

	/* never go past top of usable map */
        if (ref_row < 1)
		ref_row = 1;

	/* same with columns */
	if (ref_col > MAP_WIDTH - display_cols - 1)
		ref_col = MAP_WIDTH - display_cols - 1;

	if (ref_col < 1)
		ref_col = 1;

        whose_map = whose; /* remember whose map is displayed */
	display_screen (vmap);

	/* print x-coordinates along bottom of screen */
	wmove(stdscr, lines-1, 0);
	wclrtoeol(stdscr);
	for (c = ref_col; c < ref_col + display_cols && c < MAP_WIDTH; c++)
		if (c % 10 == 0)
			mvwprintw(stdscr, lines-1, c-ref_col+1, "%d", c);

	/* print y-coordinates along right of screen */
	for (r = ref_row; r < ref_row + display_rows && r < MAP_HEIGHT; r++)
		if (r % 2 == 0)
			mvwprintw(stdscr, r-ref_row+NUMTOPS+1, cols-NUMSIDES, "%2d", r);
		else
		{
			wmove(stdscr, r-ref_row+NUMTOPS+1, cols-NUMSIDES);
			wclrtoeol(stdscr);
		}
	
	/* print round number */
	snprintf (jnkbuf, STRSIZE, "Sector %d Round %ld", sector, date);
	for (r = 0; jnkbuf[r] != '\0'; r++)
	{
		if (r+NUMTOPS + 2 >= MAP_HEIGHT)
			break;

		mvwaddch(stdscr, r+NUMTOPS + 2, cols-NUMSIDES+4, jnkbuf[r]);
	}

	wrefresh(stdscr);
}

/* Display the contents of a single map square. */


static void
disp_square(view_map_t *vp)
{
	waddch(mapwin, (chtype)vp->contents);
}


/* Display the portion of the map that appears on the screen. */

static void
display_screen (view_map_t vmap[])
{
	int display_rows, display_cols;
	int r, c;
	long t;

	display_rows = MAPWIN_HEIGHT - 2; /* num lines to display */
	display_cols = MAPWIN_WIDTH - 2;

	for (r = ref_row; r < ref_row + display_rows && r < MAP_HEIGHT - 1; r++)
		for (c = ref_col; c < ref_col + display_cols && c < MAP_WIDTH - 1; c++)
		{
			t = row_col_loc (r, c);
			wmove (mapwin, r-ref_row+1, c-ref_col+1);
			disp_square(&vmap[t]);
		}
	box(mapwin, 0, 0);
	wrefresh(mapwin);
}

/*
 * Move the cursor in a specified direction.  We return TRUE if the
 * cursor remains in the currently displayed screen, otherwise FALSE.
 * We display the cursor on the screen, if possible.
 */

int
move_cursor (long *cursor, int offset)
/* cursor == current cursor position */
/* offset == offset to add to cursor */
{
	long t;
	int r, c;
 
	t = *cursor + offset; /* proposed location */

	if (!on_screen (t))
		return (FALSE); /* loc is off screen */
	-- !!!! in Ada version, Ui.alert instead of returning false (verified with only call site)
	*cursor = t; /* update cursor position */
	save_cursor = *cursor;
	       
	r = loc_row (save_cursor);
	c = loc_col (save_cursor);

#ifdef USE_NCURSES
	/*
	 * Under ncurses (only) we need to redraw here, or we get weird errors, but
	 * only when moving the cursor to the right...
	 */
	redraw();
#endif
	wmove(mapwin, r-ref_row+1, c-ref_col+1);
	wrefresh(mapwin);

	return (TRUE);
}

/* See if a location is displayed on the screen. */

static int
on_screen (long loc)
{
	int new_r, new_c;
	
	new_r = loc_row (loc);
	new_c = loc_col (loc);

	if (new_r < ref_row					/* past top of screen */
		    || new_r > (ref_row + MAPWIN_HEIGHT - 3)	/* past bot of screen? */
		    || new_c < ref_col				/* past left edge of screen? */
		    || new_c > (ref_col + MAPWIN_WIDTH - 3))	/* past right edge of screen? */
		return (FALSE);

	return (TRUE);
}

/* Print a condensed version of the map. */

char zoom_list[] = "XO*tcbsdpfaTCBSDPFAzZ+. ";

void
print_zoom (const view_map_t *vmap)
{
	int row_inc, col_inc;
	int r, c;

	row_inc = (MAP_HEIGHT + MAPWIN_HEIGHT - 2) / (MAPWIN_HEIGHT - 2);
	col_inc = (MAP_WIDTH + MAPWIN_WIDTH - 2) / (MAPWIN_WIDTH - 2);

	wclear(mapwin);

	for (r = 0; r < MAP_HEIGHT; r += row_inc)
		for (c = 0; c < MAP_WIDTH; c += col_inc)
			print_zoom_cell (vmap, r, c, row_inc, col_inc);

	box(mapwin, 0, 0);
	wrefresh(mapwin);

        /* print x-coordinates along bottom of screen */
	wmove(stdscr, lines-1, 0);
	wclrtoeol(stdscr);
        for (c = 0; c < MAP_WIDTH; c+=col_inc)
                if ((c/col_inc)%10  == 0)
                        mvwprintw(stdscr, lines-1, c/col_inc+1, "%d", c);

        /* print y-coordinates along right of screen */
        for (r = 0; r/row_inc < MAPWIN_HEIGHT; r+=row_inc)
                if (((r/row_inc)%10 == 0) && r < MAP_HEIGHT)
                        mvwprintw(stdscr, r/row_inc+NUMTOPS+1, cols-NUMSIDES, "%2d", r);
                else
                {
                        wmove(stdscr, r/row_inc+NUMTOPS+1, cols-NUMSIDES);
                        wclrtoeol(stdscr);
                }

	/* print round number */
	snprintf (jnkbuf, STRSIZE, "Zoomed Map Round %ld", date);
	for (r = 0; jnkbuf[r] != '\0'; r++)
	{
		if (r+NUMTOPS + 2 >= MAP_HEIGHT)
			break;

		mvwaddch(stdscr, r+NUMTOPS + 2, cols-NUMSIDES+4, jnkbuf[r]);
	}

	wrefresh(stdscr);

	prompt("Press any key to continue");
	get_chx();
	prompt("");

	if (whose_map == USER)
		print_sector_u(save_sector);
}

/* Print a single cell in condensed format. */

static void
print_zoom_cell (const view_map_t *vmap, int row, int col, int row_inc, int col_inc)
{
	int r, c;
	char cell;

	cell = ' ';
	for (r = row; r < row + row_inc; r++)
		for (c = col; c < col + col_inc; c++)
			if (strchr(zoom_list, vmap[row_col_loc(r,c)].contents) < strchr(zoom_list, cell))
				cell = vmap[row_col_loc(r,c)].contents;
	
	wmove (mapwin, row/row_inc + 1, col/col_inc + 1);
	waddch (mapwin, (chtype)cell);
}

/* Print a condensed version of a pathmap. */

void
print_pzoom (const char *s, const path_map_t *pmap, const view_map_t *vmap)
{
	int row_inc, col_inc;
	int r, c;

	wclear(mapwin);

        row_inc = (MAP_HEIGHT + MAPWIN_HEIGHT - 2) / (MAPWIN_HEIGHT - 2);
	col_inc = (MAP_WIDTH + MAPWIN_WIDTH - 2) / (MAPWIN_WIDTH - 2);

	for (r = 0; r < MAP_HEIGHT; r += row_inc)
		for (c = 0; c < MAP_WIDTH; c += col_inc)
			print_pzoom_cell(pmap, vmap, r, c, row_inc, col_inc);

	box(mapwin, 0, 0);
	wrefresh(mapwin);

        /* print x-coordinates along bottom of screen */
        wmove(stdscr, lines-1, 0);
        wclrtoeol(stdscr);
        for (c = 0; c < MAP_WIDTH; c+=col_inc)
                if ((c/col_inc)%10  == 0)
                        mvwprintw(stdscr, lines-1, c/col_inc+1, "%d", c);

        /* print y-coordinates along right of screen */
        for (r = 0; r/row_inc < MAPWIN_HEIGHT; r+=row_inc)
                if (((r/row_inc)%10 == 0) && r < MAP_HEIGHT)
                        mvwprintw(stdscr, r/row_inc+NUMTOPS+1, cols-NUMSIDES, "%2d", r);
                else
                {
                        wmove(stdscr, r/row_inc+NUMTOPS+1, cols-NUMSIDES);
                        wclrtoeol(stdscr);
                }

        /* print round number */
        snprintf (jnkbuf, STRSIZE, "Debugging Map Round %ld", date);
        for (r = 0; jnkbuf[r] != '\0'; r++)
        {
                if (r+NUMTOPS + 2 >= MAP_HEIGHT)
                        break;

                mvwaddch(stdscr, r+NUMTOPS + 2, cols-NUMSIDES+4, jnkbuf[r]);
        }

        wrefresh(stdscr);

	prompt(s);
	get_chx();
	prompt("");

	if (whose_map == USER)
		print_sector_u(save_sector);
}

/*
 * Print a single cell of a pathmap in condensed format.
 * We average all squares in the cell and take the mod 10 value.
 * Squares with a value of -1 are printed with '-', squares with
 * a value of INFINITY/2 are printed with 'P', and squares with
 * a value of INFINITY are printed with 'Z'.  Squares with a value
 * between P and Z are printed as U.
 */

static void
print_pzoom_cell (const path_map_t *pmap, const view_map_t *vmap, int row, int col, int row_inc, int col_inc)
{
	int r, c;
	int sum, d;
	char cell;

	sum = 0;
	d = 0; /* number of squares in cell */
	
	for (r = row; r < row + row_inc; r++)
		for (c = col; c < col + col_inc; c++)
		{
			sum += pmap[row_col_loc(r,c)].cost;
			d += 1;
		}
	sum /= d;
	
	if (pmap[row_col_loc(row,col)].terrain == T_PATH)
		cell = '-';
	else if (sum < 0)
		cell = '!';
	else if (sum == INFINITY/2)
		cell = 'P';
	else if (sum == INFINITY)
		cell = ' ';
	else if (sum > INFINITY/2)
		cell = 'U';
	else
	{
		sum %= 36;
		if (sum < 10)
			cell = sum + '0';
		else
			cell = sum - 10 + 'a';
	}
	
	if (cell == ' ')
		print_zoom_cell (vmap, row, col, row_inc, col_inc);
	else
	{
		wmove (mapwin, row/row_inc + 1, col/col_inc + 1);
		waddch (mapwin, cell);
	}
}

/* Display the score off in the corner of the screen. */

void
display_score (void)
{
	mvwprintw(stdscr, 0, cols-12, " User  Comp");
	mvwprintw(stdscr, 1, cols-12, "%5d %5d", user_score, comp_score);
	wrefresh(stdscr);
}

/*
 * given a frame of a movie (i.e. the map at one particular point), print a zoomed
 * map at that point)
 */

void
print_movie_screen(const char *mapbuf)
{
	int row_inc, col_inc;
	int r, c, i, j;
	char cell;

	row_inc = (MAP_HEIGHT + lines - NUMTOPS - 2) / (lines - NUMTOPS);
	col_inc = (MAP_WIDTH + cols - 1) / (cols - 1);

	for (r = 0; r < MAP_HEIGHT; r += row_inc)
		for (c = 0; c < MAP_WIDTH; c += col_inc)
		{
			cell = ' ';

			for (i = r; i < r + row_inc; i++)
				for (j = c; j < c + col_inc; j++)
					if (strchr(zoom_list, mapbuf[row_col_loc(i,j)]) < strchr(zoom_list, cell))
						cell = mapbuf[row_col_loc(i,j)];

			mvwaddch(stdscr, r/row_inc + NUMTOPS, c/col_inc, cell);
		}

	wrefresh(stdscr);
}

/* Print a screen of help information. */

#define MIN(a,b) ((a)<(b) ? (a) : (b))

void
help (const char **text, int nlines)
{
        int	i, r, c;
        piece_type_t j;
        int	text_lines, obj_lines, start_col, start_row, help_height, help_width;
	WINDOW 	*helpwin;

        text_lines = (nlines + 1) / 2; /* lines of text */
	obj_lines = (NUM_OBJECTS + 1) / 2;

	help_height = MIN((text_lines + obj_lines + 4), MAPWIN_HEIGHT);
	help_width = MIN(78, cols);

	start_row = (MAPWIN_HEIGHT - help_height ) / 2 + NUMTOPS + 1;
	start_col = (MAPWIN_WIDTH - help_width) / 2 + 2;

	helpwin = newwin(help_height, help_width, start_row, start_col);

	if (helpwin == NULL)
		panic("no help window");

	wattron(helpwin, A_REVERSE);
        mvwprintw(helpwin, 1, 1, text[0]); /* mode */
        mvwprintw(helpwin, 1, 40, "See empire(6) for more information.");
	wattroff(helpwin, A_REVERSE);

        for (i = 1; i < nlines; i++)
        {
                if (i > text_lines)
                        mvwprintw(helpwin, i - text_lines + 1, 40, text[i]);
                else
                        mvwprintw(helpwin, i + 1, 1, text[i]);
        }

	wattron(helpwin, A_REVERSE);
        mvwprintw(helpwin, text_lines + 2, 1, "  Piece   Yours Enemy Moves Hits Cost");
        mvwprintw(helpwin, text_lines + 2, 40, "  Piece   Yours Enemy Moves Hits Cost");
	wattroff(helpwin, A_REVERSE);
        
	for (j = FIRST_OBJECT; j < NUM_OBJECTS; j++)
        {
                if (j >= (NUM_OBJECTS+1)/2) {
                        r = j - (NUM_OBJECTS+1)/2;
                        c = 40;
                }
                else
                {
                        r = j;
                        c = 1;
                }
                mvwprintw(helpwin, r + text_lines + 3, c, "%-12s%c     %c%6d%5d%6d",
			  piece_attr[j].nickname,
			  piece_attr[j].sname,
			  tolower ((int)piece_attr[j].sname),
			  piece_attr[j].speed,
			  piece_attr[j].max_hits,
			  piece_attr[j].build_time);
        }

	box(helpwin, 0, 0);
        wrefresh(helpwin);

	prompt("Press any key to continue");
	get_chx();
	prompt("");

	wclear(helpwin);
	wrefresh(helpwin);

	delwin(helpwin);

	if (whose_map == USER)
		print_sector_u(save_sector);
}
