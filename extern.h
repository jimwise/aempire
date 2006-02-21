/*
 *    Copyright (C) 1987, 1988 Chuck Simmons
 * 
 * See the file COPYING, distributed with empire, for restriction
 * and warranty information.
 *
 * $Id: extern.h,v 1.1 2006/02/21 17:33:41 jwise Exp $
 */

/* extern.h -- define global non-constant storage.  */

/* user-supplied parameters */
extern int SMOOTH;		/* number of times to smooth map		*/
extern int WATER_RATIO;		/* percentage of map that is water		*/
extern int MIN_CITY_DIST;	/* cities must be at least this far apart	*/
extern int save_interval;	/* turns between autosaves			*/
extern int traditional;		/* use `traditional' movement keys instead of new style */
extern int color;		/* use color if available			*/

extern real_map_t map[MAP_SIZE];	/* the way the world really looks	*/
extern view_map_t comp_map[MAP_SIZE];	/* computer's view of the world		*/
extern view_map_t user_map[MAP_SIZE];	/* user's view of the world		*/

extern city_info_t city[NUM_CITY];	/* city information			*/

/*
 * There is one array to hold all allocated objects no matter who
 * owns them.  Objects are allocated from the array and placed on
 * a list corresponding to the type of object and its owner.
 */

extern piece_info_t *free_list;			/* index to free items in object list	*/
extern piece_info_t *user_obj[NUM_OBJECTS];	/* indices to user lists		*/
extern piece_info_t *comp_obj[NUM_OBJECTS];	/* indices to computer lists		*/
extern piece_info_t object[LIST_SIZE];		/* object list				*/

/* Display information. */
extern int lines;	/* lines on screen	*/
extern int cols;	/* columns on screen	*/

/* constant data */
extern const piece_attr_t piece_attr[];
extern const int dir_offset[];
extern const char *func_name[];
extern const int move_order[];
extern const char type_chars[];
extern const char tt_attack[];
extern const char army_attack[];
extern const char fighter_attack[];
extern const char ship_attack[];
extern const char city_char[];

extern const move_info_t tt_load;
extern const move_info_t tt_explore;
extern const move_info_t tt_unload;
extern const move_info_t army_fight;
extern const move_info_t army_load;
extern const move_info_t fighter_fight;
extern const move_info_t ship_fight;
extern const move_info_t ship_repair;
extern const move_info_t user_army;
extern const move_info_t user_army_attack;
extern const move_info_t user_fighter;
extern const move_info_t user_ship;
extern const move_info_t user_ship_repair;

extern const char *help_cmd[];
extern const char *help_edit[];
extern const char *help_user[];
extern const int cmd_lines;
extern const int edit_lines;
extern const int user_lines;

/* miscellaneous */
extern long date;		/* number of game turns played		*/
extern char automove;		/* TRUE iff user is in automove mode	*/
extern char resigned;		/* TRUE iff computer resigned		*/
extern char debug;		/* TRUE iff in debugging mode		*/
extern char print_debug;	/* TRUE iff we print debugging stuff	*/
extern char print_vmap;		/* TRUE iff we print view maps		*/
extern char trace_pmap;		/* TRUE if we are tracing pmaps		*/
extern int win;			/* set when game is over		*/
extern char jnkbuf[STRSIZE];	/* general purpose temporary buffer	*/
extern char save_movie;		/* TRUE iff we are saving movie screens	*/
extern int user_score;		/* "score" for user and computer	*/
extern int comp_score;

/* Screen updating macros */
#define display_loc_u(loc) display_loc(USER,user_map,loc)
#define display_loc_c(loc) display_loc(COMP,comp_map,loc)
#define print_sector_u(sector) print_sector(USER,user_map,sector)
#define print_sector_c(sector) print_sector(COMP,comp_map,sector)
#define loc_row(loc) ((loc)/MAP_WIDTH)
#define loc_col(loc) ((loc)%MAP_WIDTH)
#define row_col_loc(row,col) ((long)((row)*MAP_WIDTH + (col)))
#define sector_row(sector) ((sector)%SECTOR_ROWS)
#define sector_col(sector) ((sector)/SECTOR_ROWS)
#define row_col_sector(row,col) ((int)((col)*SECTOR_ROWS+(row)))

#define loc_sector(loc) \
	row_col_sector(loc_row(loc)/ROWS_PER_SECTOR, \
                       loc_col(loc)/COLS_PER_SECTOR)
		       
#define sector_loc(sector) row_col_loc( \
		sector_row(sector)*ROWS_PER_SECTOR+ROWS_PER_SECTOR/2, \
		sector_col(sector)*COLS_PER_SECTOR+COLS_PER_SECTOR/2)
	
#define panic(why)      emp_panic(__FILE__, __LINE__, (why))

/* global routines */
void	attack (piece_info_t *, long);	/* attack.c	*/
void	comp_move (void);		/* compmove.c	*/
void    edit(long);            		/* edit.c	*/
void    empire (void);          	/* empire.c	*/
void	user_move (void);		/* usermove.c	*/

/* display routines (display.c) */
void	map_init (void);
int	cur_sector (void);
void	display_loc (int, view_map_t[], long);
void	display_locx (int, view_map_t[], long);
void    display_score (void);
void    help (const char **, int);
void	kill_display (void);
int	move_cursor (long *, int);
void    print_movie_screen (const char *);
void	print_pzoom (const char *, const path_map_t *, const view_map_t *);
void	print_sector (char, view_map_t[], int);
void	print_zoom (const view_map_t *);
void    sector_change (void);

/* edit routines (edit.c) */
void    e_city_attack (city_info_t *, int);
void	e_city_explore (city_info_t *, int);
void    e_city_fill (city_info_t *, int);
void    e_city_random (city_info_t *, int);
void    e_city_repair (city_info_t *, int);
void    e_city_stasis (city_info_t *, int);
void    e_city_wake (city_info_t *, int);

/* game routines (game.c) */
void	init_game (void);
void	replay_movie (void);
int	restore_game (void);
void	save_game (void);
void	save_movie_screen (void);

/* map routines (map.c) */
int	rmap_shore (long);
int	vmap_at_sea (const view_map_t *, long);
void	vmap_cont (int *, const view_map_t *, long, char);
scan_counts_t	vmap_cont_scan (int *, const view_map_t *);
long	vmap_find_aobj (path_map_t[], const view_map_t *, long, const move_info_t *);
long	vmap_find_dest (path_map_t[], view_map_t[], long, long, int, int);
long	vmap_find_dir (path_map_t[], const view_map_t *, long, const char *, const char *);
long	vmap_find_lobj (path_map_t[], const view_map_t *, long, const move_info_t *);
long	vmap_find_lwobj (path_map_t[], const view_map_t *, long, const move_info_t *, int);
long	vmap_find_wobj (path_map_t[], const view_map_t *, long, const move_info_t *);
long	vmap_find_wlobj (path_map_t[], const view_map_t *, long, const move_info_t *);
void	vmap_mark_adjacent (path_map_t[], long);
void	vmap_mark_near_path (path_map_t[], long);
void	vmap_mark_path (path_map_t *, const view_map_t *, long);
void	vmap_mark_up_cont (int *, const view_map_t *, long, char);
void	vmap_prune_explore_locs (view_map_t *);

/* math routines (math.c) */
long	dist (long, long);
long	rand_long (long);
int	isqrt (int);
void	rand_init (void);

/* object routines (object.c) */
void	describe_obj (const piece_info_t *);
void	disembark (piece_info_t *);
void	embark (piece_info_t *, piece_info_t *);
city_info_t     *find_city (long);
int	find_nearest_city ( long, int, long *);
piece_info_t	*find_nfull (piece_type_t, long);
piece_info_t	*find_obj (piece_type_t, long);
piece_info_t	*find_obj_at_loc (long);
long	find_transport (int, long);
int	get_piece_name (void);
int	good_loc (const piece_info_t *, long);
void	kill_city (city_info_t *);
void	kill_obj (piece_info_t *, long);
void	move_obj (piece_info_t *, long);
void	move_sat (piece_info_t *);
int	obj_capacity (const piece_info_t *);
int	obj_moves (const piece_info_t *);
void	produce (city_info_t *);
void	scan (view_map_t[], long);
void	set_prod (city_info_t *);

/* terminal routines (term.c) */
void	alert (void);
void	error (const char *, ...);
char    get_chx (void);
int	get_int (const char *, int, int);
void    get_str (char *, int);
int     getyn (const char *);
void	huh (void);
void	info (const char *, ...);
void	prompt (const char *, ...);
void    redraw (void);
void    term_clear (void);
void	term_end (void);
void    term_init (void);

/* utility routines (util.c) */
void    check (void);
void	emp_panic (const char *file, int line, const char *);
