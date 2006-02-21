/*
 *    Copyright (C) 1987, 1988 Chuck Simmons
 * 
 * See the file COPYING, distributed with empire, for restriction
 * and warranty information.
 *
 * $Id: edit.c,v 1.1 2006/02/21 17:33:41 jwise Exp $
 */

/* edit.c -- Routines to handle edit mode commands. */

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "empire.h"
#include "extern.h"

void	edit (long);
void	e_city_attack (city_info_t *, int);
void	e_city_explore (city_info_t *, int);
void	e_city_fill (city_info_t *, int);
void	e_city_random (city_info_t *, int);
void	e_city_repair (city_info_t *, int);
void	e_city_stasis (city_info_t *, int);
void	e_city_wake (city_info_t *, int);
static void	e_attack (long);
static void	e_city_func (long *, long, int *);
static void	e_city_info (long);
static char	e_cursor (long *);
static void	e_end (long *, long, int);
static void	e_explore (long);
static void	e_fill (long);
static void	e_info (long);
static void	e_land (long);
static void	e_leave (void);
static void	e_move (long *, long);
static void	e_piece_info (long, char);
static void	e_print (long *);
static void	e_prod (long);
static void	e_random (long);
static void	e_repair (long);
static void	e_set_city_func (city_info_t *, int, function_t);
static void	e_set_func (long, function_t);
static void	e_sleep (long);
static void	e_stasis (long);
static void	e_transport (long);
static void	e_wake (long loc);

void
edit (long edit_cursor)
{
	long path_start;
	int path_type;
	char e;
	
	path_start = -1; /* not building a path yet */
	
	for (;;) { /* until user gives command to leave */
		display_loc_u (edit_cursor); /* position cursor */
		e = e_cursor (&edit_cursor); /* handle cursor movement */

		switch (e) {
		case 'B': /* change city production */
			e_prod (edit_cursor);
			break;
		case 'F': /* fill */
			e_fill (edit_cursor);
			break;
		case 'G': /* explore */
			e_explore (edit_cursor);
			break;
		case '?': /* help */
			help (help_edit, edit_lines);
			break;
		case 'I': /* directional stasis */
			e_stasis (edit_cursor);
			break;
		case 'K': /* wake up anything and everything */
			e_wake (edit_cursor);
			break;
		case 'L': /* land plane */
			e_land (edit_cursor);
			break;
		case 'M': /* start move to location */
			path_type = NOPIECE;
			e_move (&path_start, edit_cursor);
			break;
		case 'N': /* end move to location */
			e_end (&path_start, edit_cursor, path_type);
			break;
		case 'O': /* leave display mode */
			e_leave ();
			return;
		case 'P': /* print new sector */
			e_print (&edit_cursor);
			break;
		case 'R': /* make piece move randomly */
			e_random (edit_cursor);
			break;
		case 'S': /* sleep */
			e_sleep (edit_cursor);
			break;
		case 'T': /* transport army */
			e_transport (edit_cursor);
			break;
		case 'U': /* repair ship */
			e_repair (edit_cursor);
			break;
		case 'V': /* set city function */
			e_city_func (&path_start, edit_cursor, &path_type);
			break;
		case 'Y': /* set army func to attack */
			e_attack (edit_cursor);
			break;
		case '=': /* request info */
			e_info (edit_cursor);
			break;
		case '\014': /* control-L */
			redraw ();
			break;
		default: /* bad command? */
			huh ();
			break;
		}
	}
}

/* Get the next command.  We handle cursor movement here. */

static char dirchars[] = "WEDCXZAQ";

static char
e_cursor (long *edit_cursor)
{
	int e;
	char *p;
	
	while (1)
	{
		e = get_chx();
		p = strchr(dirchars, e);
		if (!p)
			break;
		if (!move_cursor (edit_cursor, dir_offset[p - dirchars]))
			alert();
	}

	return e;
}

/* Leave edit mode. */

static void
e_leave (void)
{
}

/* Print new sector. */

static void
e_print (long *edit_cursor)
{
        int sector;
	
	sector = get_int ("New Sector? ", 0, NUM_SECTORS-1);

	/* position cursor at center of sector */
	*edit_cursor = sector_loc (sector);
	sector_change (); /* allow change of sector */
}

/* Set the function of a piece. */

static void
e_set_func (long loc, function_t func)
{
	piece_info_t *obj;
	obj = find_obj_at_loc (loc);
	if (obj != NULL && obj->owner == USER) {
		obj->func = func;
		return;
	}
	huh (); /* no object here */
}
	
/* Set the function of a city for some piece. */

static void
e_set_city_func (city_info_t *cityp, int type, function_t func)
{
	cityp->func[type] = func;
}

/* Set a piece to move randomly. */

static void
e_random (long loc)
{
	e_set_func (loc, RANDOM);
}

void
e_city_random (city_info_t *cityp, int type)
{
	e_set_city_func (cityp, type, RANDOM);
}

/* Put a ship in fill mode. */

static void
e_fill (long loc)
{
	if (user_map[loc].contents == 'T' || user_map[loc].contents == 'C')
		e_set_func (loc, FILL);
	else huh ();
}

void
e_city_fill (city_info_t *cityp, int type)
{
	if (type == TRANSPORT || type == CARRIER)
		e_set_city_func (cityp, type, FILL);
	else huh ();
}

/* Set a piece to explore. */

static void
e_explore (long loc)
{
	e_set_func (loc, EXPLORE);
}

void
e_city_explore (city_info_t *cityp, int type)
{
	e_set_city_func (cityp, type, EXPLORE);
}

/* Set a fighter to land. */

static void
e_land (long loc)
{
	if (user_map[loc].contents == 'F')
		e_set_func (loc, LAND);
	else huh ();
}
/* Set an army's function to TRANSPORT. */

static void
e_transport (long loc)
{
	if (user_map[loc].contents == 'A')
		e_set_func (loc, WFTRANSPORT);
	else huh ();
}

/* Set an army's function to ATTACK. */

static void
e_attack (long loc)
{
	if (user_map[loc].contents == 'A')
		e_set_func (loc, ARMYATTACK);
	else huh ();
}

void
e_city_attack (city_info_t *cityp, int type)
{
	if (type == ARMY)
		e_set_city_func (cityp, type, ARMYATTACK);
	else huh ();
}

/* Set a ship's function to REPAIR. */

static void
e_repair (long loc)
{
	if (strchr ("PDSTBC", user_map[loc].contents))
		e_set_func (loc, REPAIR);
	else huh ();
}

void
e_city_repair (city_info_t *cityp, int type)
{
	if (type == ARMY || type == FIGHTER || type == SATELLITE)
		huh ();
	else e_set_city_func (cityp, type, REPAIR);
}

/* Set object to move in a direction. */

static char dirs[] = "WEDCXZAQ";
 
static void
e_stasis (long loc)
{
	int e;
	char *p;
	
	if (!isupper (user_map[loc].contents)) huh (); /* no object here */
	else if (user_map[loc].contents == 'X') huh ();
	else {
		e = get_chx(); /* get a direction */
		p = strchr (dirs, e);

		if (p == NULL) huh ();
		else e_set_func (loc, (long)(MOVE_N - (p - dirs)));
	}
}

void
e_city_stasis (city_info_t *cityp, int type)
{
	char e;
	char *p;
	
	e = get_chx(); /* get a direction */
	p = strchr (dirs, e);

	if (p == NULL) huh ();
	else e_set_city_func (cityp, type, (long)(MOVE_N - (p - dirs)));
}

/* Wake up anything and everything. */

static void
e_wake (long loc)
{
	city_info_t *cityp;
	piece_info_t *obj;
	piece_type_t i;

	cityp = find_city (loc);
        if (cityp != NULL)
		for (i = 0; i < NUM_OBJECTS; i++)
			cityp->func[i] = NOFUNC;
	
	for (obj = map[loc].objp; obj != NULL; obj = obj->loc_link.next)
		obj->func = NOFUNC;
}

void
e_city_wake (city_info_t *cityp, int type)
{
	e_set_city_func (cityp, type, NOFUNC);
}

/*
 * Set a city's function.  We get the piece type to set, then
 * the function itself.
 */

static void
e_city_func (long *path_start, long loc, int *path_type)
{
	int type;
	char e;
	city_info_t *cityp;

	cityp = find_city (loc);
	if (!cityp || cityp->owner != USER) {
		huh ();
		return;
	}

	type = get_piece_name();
	if (type == NOPIECE) {
		huh ();
		return;
	}
	
	e = get_chx ();
	
	switch (e) {
	case 'F': /* fill */
		e_city_fill (cityp, type);
		break;
	case 'G': /* explore */
		e_city_explore (cityp, type);
		break;
	case 'I': /* directional stasis */
		e_city_stasis (cityp, type);
		break;
	case 'K': /* turn off function */
		e_city_wake (cityp, type);
		break;
	case 'M': /* start move to location */
		*path_type = type;
		e_move (path_start, loc);
		break;
	case 'R': /* make piece move randomly */
		e_city_random (cityp, type);
		break;
	case 'U': /* repair ship */
		e_city_repair (cityp, type);
		break;
	case 'Y': /* set army func to attack */
		e_city_attack (cityp, type);
		break;
	default: /* bad command? */
		huh ();
		break;
	}
}

/* Beginning of move to location. */

static void
e_move (long *path_start, long loc)
{
	if (!isupper(user_map[loc].contents)) huh (); /* nothing there? */
	else if (user_map[loc].contents == 'X') huh (); /* enemy city? */
        else *path_start = loc;
}

/* End of move to location. */

static void
e_end (long *path_start, long loc, int path_type)
{
	city_info_t *cityp;
	
	if (*path_start == -1) huh (); /* no path started? */
	else if (path_type == NOPIECE) e_set_func (*path_start, loc);
	else {
		cityp = find_city (*path_start);
		assert (cityp);
		e_set_city_func (cityp, path_type, loc);
	}

	*path_start = -1; /* remember no path in progress */
}

/* Put a piece to sleep. */

static void
e_sleep (long loc)
{
	if (user_map[loc].contents == 'O') huh (); /* can't sleep a city */
	else e_set_func (loc, SENTRY);
}

/* Print out information about a piece. */

static void
e_info (long edit_cursor)
{
	char ab;

	ab = user_map[edit_cursor].contents;

	if (ab == 'O')
		e_city_info(edit_cursor);
	else if (ab == 'X' && debug)
		e_city_info (edit_cursor);
	else if ((ab >= 'A') && (ab <= 'T'))
		e_piece_info(edit_cursor, ab);
	else if ((ab >= 'a') && (ab <= 't') && (debug))
		e_piece_info(edit_cursor, ab);
	else
		huh();
}

/* Print info about a piece. */

static void
e_piece_info (long edit_cursor, char ab)
{
	piece_info_t *obj;
	int type;
	char *p;

	ab = toupper ((int)ab);
	p = strchr (type_chars, ab);
	type = p - type_chars;

	obj = find_obj (type, edit_cursor);
	assert (obj != NULL);
	describe_obj (obj);
}

/* Display info on a city. */

static void
e_city_info (long edit_cursor)
{
	piece_info_t *obj;
	city_info_t *cityp;
	int f, s;
	piece_type_t i;
	char func_buf[STRSIZE * 2];
	char temp_buf[STRSIZE];
	char junk_buf2[STRSIZE];

	f = 0; /* no fighters counted yet */
	for (obj = map[edit_cursor].objp; obj != NULL;
		obj = obj->loc_link.next)
			if (obj->type == FIGHTER) f++;

	s = 0; /* no ships counted yet */
	for (obj = map[edit_cursor].objp; obj != NULL;
		obj = obj->loc_link.next)
			if (obj->type >= DESTROYER) s++;

	if (f == 1 && s == 1) 
		snprintf (jnkbuf, STRSIZE, "1 fighter landed, 1 ship docked");
	else if (f == 1)
		snprintf (jnkbuf, STRSIZE, "1 fighter landed, %d ships docked", s);
	else if (s == 1)
		snprintf (jnkbuf, STRSIZE, "%d fighters landed, 1 ship docked", f);
	else snprintf (jnkbuf, STRSIZE, "%d fighters landed, %d ships docked", f, s);

	cityp = find_city (edit_cursor);
	assert (cityp != NULL);

	*func_buf = 0; /* nothing in buffer */
	for (i = FIRST_OBJECT; i < NUM_OBJECTS; i++)
	{
		if (cityp->func[i] < 0)
			snprintf(temp_buf, STRSIZE, "%c:%s; ", piece_attr[i].sname, func_name[FUNCI(cityp->func[i])]);
		else
			snprintf(temp_buf, STRSIZE, "%c: %d;", piece_attr[i].sname, cityp->func[i]);
		
		strncat(func_buf, temp_buf, STRSIZE);
	}

	snprintf (junk_buf2, STRSIZE, "City at location %ld will complete %s on round %ld",
		cityp->loc,
		piece_attr[cityp->prod].article,
		date + piece_attr[cityp->prod].build_time - cityp->work);

	info(junk_buf2);
	info(jnkbuf);
	info(func_buf);
}

/* Change city production. */

static void
e_prod (long loc)
{
	city_info_t *cityp;
	
	cityp = find_city (loc);

	if (cityp == NULL) huh (); /* no city? */
	else set_prod (cityp);
}
