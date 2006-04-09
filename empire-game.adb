-- Routines to initialize, save, and restore a game

-- XXX This produces an utterly unportable save file, and thus clearly
-- XXX needs massive rework C version had been updated by me to use zlib, but I
-- XXX am dodging doing so with the Ada version until I get around to reworking
-- XXX the save file format

with Empire.Ui;

package body Empire.Game is

-- Initialize a new game.  Here we generate a new random map, put cities
-- on the map, select cities for each opponent, and zero out the lists of
-- pieces on the board.

procedure Init_Game is
begin
   Ui.Kill_Display;                     -- nothing on screen
   Automove := FALSE;
   Resigned := FALSE;
   Debug := FALSE;
   Print_Debug := FALSE;
   Print_Vmap := '0';
   Trace_Pmap := FALSE;
   Save_Movie := FALSE;
   Win := UNOWNED;
   Date := 0;                            -- no date yet
   User_Score := 0;
   Comp_Score := 0;

   User_Map := (others => (Contents => ' ', Seen => 0));
   Comp_Map := (others => (Contents => ' ', Seen => 0));
   User_Obj := (others => null);
   Comp_Obj := (others => null);

   Free_List := null;

   Object := (others => (Hits => 0, Owner => UNOWNED);

--         for (i = 0; i < LIST_SIZE; i++)
--         {
--                 /* for each object */
--                 piece_info_t *obj = &(object[i]);
--                 LINK (free_list, obj, piece_link);
--         }

--         make_map(); /* make land and water */

--         do
--         {
--                 for (i = 0; i < MAP_SIZE; i ++)
--                 {
--                         /* remove cities */
--                         if (map[i].contents == '*')
--                                 map[i].contents = '+'; /* land */
--                 }
--                 place_cities(); /* place cities on map */
--         }
--         while (!select_cities()); /* choose a city for each player */
end Init_Game;

-- /*
--  * Create a map.  To do this, we first randomly assign heights to each
--  * map location.  Then we smooth these heights.  The more we smooth,
--  * the better the land and water will clump together.  Then we decide
--  * how high the land will be.  We attempt to choose enough land to meet
--  * some required percentage.
--  *
--  * There are two parameters to this algorithm:  the amount we will smooth,
--  * and the ratio of land to water.  The user can provide these numbers
--  * at program start up.
--  */

-- #define MAX_HEIGHT 999  /* highest height */

-- /* these arrays give some compilers problems when they are automatic */
-- static int height[2][MAP_SIZE];
-- static int height_count[MAX_HEIGHT+1];

-- static void
-- make_map (void)
-- {
--         int from, to, k;
--         long i, j, sum, loc;

--         for (i = 0; i < MAP_SIZE; i++) /* fill map with random sand */
--                 height[0][i] = rand_long (MAX_HEIGHT);

--         from = 0;
--         to = 1;
--         for (i = 0; i < SMOOTH; i++) { /* smooth the map */
--             for (j = 0; j < MAP_SIZE; j++) {
--                 sum = height[from][j];
--                 for (k = 0; k < 8; k++) {
--                         loc = j + dir_offset[k];
--                         /* edges get smoothed in a wierd fashion */
--                         if (loc < 0 || loc >= MAP_SIZE) loc = j;
--                         sum += height[from][loc];
--                 }
--                 height[to][j] = sum / 9;
--             }
--             k = to; /* swap to and from */
--             to = from;
--             from = k;
--         }

--         /* count the number of cells at each height */
--         for (i = 0; i <= MAX_HEIGHT; i++)
--                 height_count[i] = 0;

--         for (i = 0; i <= MAP_SIZE; i++)
--                 height_count[height[from][i]]++;

--         /* find the water line */
--         loc = MAX_HEIGHT; /* default to all water */
--         sum = 0;
--         for (i = 0; i <= MAX_HEIGHT; i++) {
--                 sum += height_count[i];
--                 if (sum * 100 / MAP_SIZE > WATER_RATIO && sum >= NUM_CITY) {
--                         loc = i; /* this is last height that is water */
--                         break;
--                 }
--         }

--         /* mark the land and water */
--         for (i = 0; i < MAP_SIZE; i ++) {
--                 if (height[from][i] > loc)
--                         map[i].contents = '+'; /* land */
--                 else map[i].contents = '.'; /* water */

--                 map[i].objp = NULL; /* nothing in cell yet */
--                 map[i].cityp = NULL;

--                 j = loc_col (i);
--                 k = loc_row (i);

--                 map[i].on_board = !(j == 0 || j == MAP_WIDTH-1
--                                  || k == 0 || k == MAP_HEIGHT-1);
--         }
-- }

-- /*
--  * Randomly place cities on the land.  There is a minimum distance that
--  * should exist between cities.  We maintain a list of acceptable land cells
--  * on which a city may be placed.  We randomly choose elements from this
--  * list until all the cities are placed.  After each choice of a land cell
--  * for a city, we remove land cells which are too close to the city.
--  */

-- /* avoid compiler problems with large automatic arrays */
-- static long land[MAP_SIZE];

-- static void
-- place_cities (void)
-- {
--         long placed, i, loc;
--         piece_type_t j;
--         long num_land;

--         num_land = 0;   /* nothing in land array yet */
--         placed = 0;     /* nothing placed yet */
--         while (placed < NUM_CITY) {
--                 while (num_land == 0) num_land = regen_land (placed);
--                 i = rand_long (num_land-1); /* select random piece of land */
--                 loc = land[i];

--                 city[placed].loc = loc;
--                 city[placed].owner = UNOWNED;
--                 city[placed].work = 0;
--                 city[placed].prod = NOPIECE;

--                 for (j = FIRST_OBJECT; j < NUM_OBJECTS; j++)
--                         city[placed].func[j] = NOFUNC; /* no function */

--                 map[loc].contents = '*';
--                 map[loc].cityp = &(city[placed]);
--                 placed++;

--                 /* Now remove any land too close to selected land. */
--                 num_land = remove_land (loc, num_land);
--         }
-- }

-- /*
--  * When we run out of available land, we recreate our land list.  We
--  * put all land in the list, decrement the min_city_dist, and then
--  * remove any land which is too close to a city.
--  */

-- static long
-- regen_land (long placed)
-- {
--         long num_land;
--         long i;

--         num_land = 0;
--         for (i = 0; i < MAP_SIZE; i++) {
--                 if (map[i].on_board && map[i].contents == '+') {
--                         land[num_land] = i; /* remember piece of land */
--                         num_land++; /* remember number of pieces */
--                 }
--         }
--         if (placed > 0) { /* don't decrement 1st time */
--                 MIN_CITY_DIST -= 1;
--                 assert (MIN_CITY_DIST >= 0);
--         }
--         for (i = 0; i < placed; i++) { /* for each placed city */
--                 num_land = remove_land (city[i].loc, num_land);
--         }
--         return (num_land);
-- }

-- /* Remove land that is too close to a city. */
-- in ada version, second param is in/out (all callers did x = r_l (..., x)

-- static long
-- remove_land (long loc, long num_land)
-- {
--         long new, i;

--         new = 0; /* nothing kept yet */
--         for (i = 0; i < num_land; i++) {
--                 if (dist (loc, land[i]) >= MIN_CITY_DIST) {
--                         land[new] = land[i];
--                         new++;
--                 }
--         }
--         return (new);
-- }

-- /*
--  * Here we select the cities for the user and the computer.  Our choice of
--  * cities will be heavily dependent on the difficulty level the user desires.
--  *
--  * Our algorithm will not guarantee that either player will eventually be
--  * able to move armies to any continent on the map.  There may be continents
--  * which are unreachable by sea.  Consider the case of an island in a lake.
--  * If the lake has no shore cities, then there is no way for a boat to reach
--  * the island.  Our hope is that there will be enough water on the map, or enough
--  * land, and that there will be enough cities, to make this case extremely rare.
--  *
--  * First we make a list of continents which contain at least two cities, one
--  * or more of which is on the coast.  If there are no such continents, we return
--  * FALSE, and our caller should decide again where cities should be placed
--  * on the map.  While making this list, we will rank the continents.  Our ranking
--  * is based on the thought that shore cities are better than inland cities,
--  * that any city is very important, and that the land area of a continent
--  * is mildly important.  Usually, we expect every continent to have a different
--  * ranking.  It will be unusual to have two continents with the same land area,
--  * the same number of shore cities, and the same number of inland cities.  When
--  * this is not the case, the first city encountered will be given the highest
--  * rank.
--  *
--  * We then rank pairs of continents.  We tell the user the number of different
--  * ranks, and ask the user what rank they want to use.  This is how the
--  * user specifies the difficulty level.  Using that pair, we have now decided
--  * on a continent for each player.  We now choose a random city on each continent,
--  * making sure the cities are not the same.
--  */

-- #define MAX_CONT 10 /* most continents we will allow */

-- typedef struct cont { /* a continent */
--         long value;                             /* value of continent   */
--         int ncity;                              /* number of cities     */
--         city_info_t * cityp[NUM_CITY];          /* pointer to city      */
-- } cont_t;

-- typedef struct pair {
--         long value;             /* value of pair for user       */
--         int user_cont;          /* index to user continent      */
--         int comp_cont;          /* index to computer continent  */
-- } pair_t;

-- static int marked[MAP_SIZE];                    /* list of examine cells                */
-- static int ncont;                               /* number of continents                 */
-- static cont_t cont_tab[MAX_CONT];               /* list of good continenets             */
-- static int rank_tab[MAX_CONT];                  /* indices to cont_tab in order of rank */
-- static pair_t pair_tab[MAX_CONT*MAX_CONT];      /* ranked pairs of continents           */

-- static int
-- select_cities (void)
-- {
--         long compi, useri;
--         city_info_t *compp, *userp;
--         int comp_cont, user_cont;
--         int pair;

--         find_cont (); /* find and rank the continents */
--         if (ncont == 0) return (FALSE); /* there are no good continents */

--         make_pair (); /* create list of ranked pairs */

--         snprintf(jnkbuf, STRSIZE, "Choose a difficulty level where 0 is easy and %d is hard: ", ncont*ncont-1);
--         pair = get_int(jnkbuf, 0, ncont*ncont-1);

--         comp_cont = pair_tab[pair].comp_cont;
--         user_cont = pair_tab[pair].user_cont;

--         compi = rand_long ((long)cont_tab[comp_cont].ncity);
--         compp = cont_tab[comp_cont].cityp[compi];

--         do { /* select different user city */
--                 useri = rand_long ((long)cont_tab[user_cont].ncity);
--                 userp = cont_tab[user_cont].cityp[useri];
--         } while (userp == compp);

--         info("Your city is at %d.", userp->loc);

--         /* update city and map */
--         compp->owner = COMP;
--         compp->prod = ARMY;
--         compp->work = 0;
--         scan (comp_map, compp->loc);

--         userp->owner = USER;
--         userp->work = 0;
--         scan (user_map, userp->loc);
--         set_prod (userp);
--         return (TRUE);
-- }

-- /*
--  * Find all continents with 2 cities or more, one of which must be a shore
--  * city.  We rank the continents.
--  */

-- static void
-- find_cont (void)
-- {
--         long i;
--         long mapi;

--         for (i = 0; i < MAP_SIZE; i++) marked[i] = 0; /* nothing marked yet */

--         ncont = 0; /* no continents found yet */
--         mapi = 0;

--         while (ncont < MAX_CONT)
--                 if (!find_next (&mapi)) return; /* all found */
-- }

-- /*
--  * Find the next continent and insert it in the rank table.
--  * If there are no more continents, we return false.
--  */

-- static int
-- find_next (long *mapi)
-- {
--         long i, val;

--         for (;;) {
--                 if (*mapi >= MAP_SIZE) return (FALSE);

--                 if (!map[*mapi].on_board || marked[*mapi]
--                         || map[*mapi].contents == '.') *mapi += 1;
--                 else if (good_cont (*mapi)) {
--                         rank_tab[ncont] = ncont; /* insert cont in rank tab */
--                         val = cont_tab[ncont].value;

--                         for (i = ncont; i > 0; i--) { /* bubble up new rank */
--                                 if (val > cont_tab[rank_tab[i-1]].value) {
--                                         rank_tab[i] = rank_tab[i-1];
--                                         rank_tab[i-1] = ncont;
--                                 }
--                                 else break;
--                         }
--                         ncont++; /* count continents */
--                         return (TRUE);
--                 }
--         }
-- }

-- /*
--  * Map out the current continent.  We mark every piece of land on the continent,
--  * count the cities, shore cities, and land area of the continent.  If the
--  * continent contains 2 cities and a shore city, we set the value of the
--  * continent and return true.  Otherwise we return false.
--  */

-- static long ncity, nland, nshore;

-- static int
-- good_cont (long mapi)
-- {
--         long val;

--         ncity = 0; /* nothing seen yet */
--         nland = 0;
--         nshore = 0;

--         mark_cont (mapi);

--         if (nshore < 1 || ncity < 2) return (FALSE);

--         /*
--          * The first two cities, one of which must be a shore city,
--          * don't contribute to the value.  Otherwise shore cities are
--          * worth 3/2 an inland city.  A city is worth 1000 times as much
--          * as land area.
--          */

--         if (ncity == nshore) val = (nshore - 2) * 3;
--         else val = (nshore-1) * 3 + (ncity - nshore - 1) * 2;

--         val *= 1000; /* cities are worth a lot */
--         val += nland;
--         cont_tab[ncont].value = val;
--         cont_tab[ncont].ncity = ncity;
--         return (TRUE);
-- }

-- /*
--  * Mark a continent.  This recursive algorithm marks the current square
--  * and counts it if it is land or city.  If it is city, we also check
--  * to see if it is a shore city, and we install it in the list of
--  * cities for the continent.  We then examine each surrounding cell.
--  */

-- static void
-- mark_cont (long mapi)
-- {
--         int i;

--         if (marked[mapi] || map[mapi].contents == '.'
--                 || !map[mapi].on_board) return;

--         marked[mapi] = 1;       /* mark this cell seen          */
--         nland++;                /* count land on continent      */

--         if (map[mapi].contents == '*')
--         { /* a city? */
--                 cont_tab[ncont].cityp[ncity] = map[mapi].cityp;
--                 ncity++;
--                 if (rmap_shore(mapi))
--                         nshore++;
--         }

--         for (i = 0; i < 8; i++) /* look at surrounding squares */
--                 mark_cont (mapi + dir_offset[i]);
-- }

-- /*
--  * Create a list of pairs of continents in a ranked order.  The first
--  * element in the list is the pair which is easiest for the user to
--  * win with.  Our ranking is simply based on the difference in value
--  * between the user's continent and the computer's continent.
--  */

-- static void
-- make_pair (void)
-- {
--         int i, j, k, npair;
--         long val;

--         npair = 0; /* none yet */

--         for (i = 0; i < ncont; i++)
--         for (j = 0; j < ncont; j++) { /* loop through all continents */
--                 val = cont_tab[i].value - cont_tab[j].value;
--                 pair_tab[npair].value = val;
--                 pair_tab[npair].user_cont = i;
--                 pair_tab[npair].comp_cont = j;

--                 for (k = npair; k > 0; k--) { /* bubble up new rank */
--                         if (val > pair_tab[k-1].value) {
--                                 pair_tab[k] = pair_tab[k-1];
--                                 pair_tab[k-1].user_cont = i;
--                                 pair_tab[k-1].comp_cont = j;
--                         }
--                         else break;
--                 }
--                 npair++; /* count pairs */
--         }
-- }

-- /*
--  * Save a game.  We save the game in emp_save.dat.  Someday we may want
--  * to ask the user for a file name.  If we cannot save the game, we will
--  * tell the user why.
--  */

-- /* macro to save typing; write an array, return if it fails */
-- #define wbuf(buf) if (!xwrite (f, (void *)buf, sizeof (buf))) return
-- #define wval(val) if (!xwrite (f, (void *)&val, sizeof (val))) return

-- void
-- save_game (void)
-- {
-- #ifndef USE_ZLIB
--         FILE *f; /* file to save game in */
-- #else
--         gzFile f; /* compressing file to save game in */
-- #endif

-- #ifndef USE_ZLIB
--         f = fopen("empsave.dat", "w"); /* open for output */
-- #else
--         f = gzopen("empsave.dat", "w"); /* open for compressing output */
-- #endif
--         if (f == NULL) {
--                 error ("Cannot save empsave.dat");
--                 return;
--         }
--         wbuf (map);
--         wbuf (comp_map);
--         wbuf (user_map);
--         wbuf (city);
--         wbuf (object);
--         wbuf (user_obj);
--         wbuf (comp_obj);
--         wval (free_list);
--         wval (date);
--         wval (automove);
--         wval (resigned);
--         wval (debug);
--         wval (win);
--         wval (save_movie);
--         wval (user_score);
--         wval (comp_score);

-- #ifndef USE_ZLIB
--         fclose(f);
-- #else
--         gzclose(f);
-- #endif
--         info("Game saved.");
-- }

-- /*
--  * Recover a saved game from emp_save.dat.
--  * We return TRUE if we succeed, otherwise FALSE.
--  */

-- #define rbuf(buf) if (!xread (f, (void *)buf, sizeof(buf))) return (FALSE);
-- #define rval(val) if (!xread (f, (void *)&val, sizeof(val))) return (FALSE);

-- int
-- restore_game (void)
-- {
-- #ifndef USE_ZLIB
--         FILE *f; /* file to save game in */
-- #else
--         gzFile f; /* compressing file to save game in */
-- #endif
--         long i;
--         piece_type_t j;
--         piece_info_t **list;
--         piece_info_t *obj;

-- #ifndef USE_ZLIB
--         f = fopen("empsave.dat", "r"); /* open for input */
-- #else
--         f = gzopen("empsave.dat", "r"); /* open for decompressing input */
-- #endif
--         if (f == NULL) {
--                 error("Cannot open empsave.dat");
--                 return (FALSE);
--         }
--         rbuf (map);
--         rbuf (comp_map);
--         rbuf (user_map);
--         rbuf (city);
--         rbuf (object);
--         rbuf (user_obj);
--         rbuf (comp_obj);
--         rval (free_list);
--         rval (date);
--         rval (automove);
--         rval (resigned);
--         rval (debug);
--         rval (win);
--         rval (save_movie);
--         rval (user_score);
--         rval (comp_score);

--         /* Our pointers may not be valid because of source
--         changes or other things.  We recreate them. */

--         free_list = NULL; /* zero all ptrs */
--         for (i = 0; i < MAP_SIZE; i++)
--         {
--                 map[i].cityp = NULL;
--                 map[i].objp = NULL;
--         }
--         for (i = 0; i < LIST_SIZE; i++)
--         {
--                 object[i].loc_link.next = NULL;
--                 object[i].loc_link.prev = NULL;
--                 object[i].cargo_link.next = NULL;
--                 object[i].cargo_link.prev = NULL;
--                 object[i].piece_link.next = NULL;
--                 object[i].piece_link.prev = NULL;
--                 object[i].ship = NULL;
--                 object[i].cargo = NULL;
--         }
--         for (j = 0; j < NUM_OBJECTS; j++)
--         {
--                 comp_obj[j] = NULL;
--                 user_obj[j] = NULL;
--         }
--         /* put cities on map */
--         for (i = 0; i < NUM_CITY; i++)
--                 map[city[i].loc].cityp = &(city[i]);

--         /* put pieces in free list or on map and in object lists */
--         for (i = 0; i < LIST_SIZE; i++)
--         {
--                 obj = &(object[i]);
--                 if (object[i].owner == UNOWNED || object[i].hits == 0)
--                 {
--                         LINK (free_list, obj, piece_link);
--                 } else {
--                         list = LIST (object[i].owner);
--                         LINK (list[object[i].type], obj, piece_link);
--                         LINK (map[object[i].loc].objp, obj, loc_link);
--                 }
--         }

--         /* Embark armies and fighters. */
--         read_embark (user_obj[TRANSPORT], ARMY);
--         read_embark (user_obj[CARRIER], FIGHTER);
--         read_embark (comp_obj[TRANSPORT], ARMY);
--         read_embark (comp_obj[CARRIER], FIGHTER);

-- #ifndef USE_ZLIB
--         fclose(f);
-- #else
--         gzclose(f);
-- #endif
--         kill_display (); /* what we had is no longer good */
--         info("Game restored from empsave.dat.");
--         return (TRUE);
-- }

-- /*
--  * Embark cargo on a ship.  We loop through the list of ships.
--  * We then loop through the pieces at the ship's location until
--  * the ship has the same amount of cargo it previously had.
--  */

-- static void
-- read_embark (piece_info_t *list, piece_type_t piece_type)
-- {
--         piece_info_t *ship;
--         piece_info_t *obj;
--         int count;

--         for (ship = list; ship != NULL; ship = ship->piece_link.next) {
--                 count = ship->count; /* get # of pieces we need */
--                 if (count < 0) inconsistent ();
--                 ship->count = 0; /* nothing on board yet */
--                 for (obj = map[ship->loc].objp; obj && count;
--                     obj = obj->loc_link.next) {
--                         if (obj->ship == NULL && obj->type == piece_type) {
--                                 embark (ship, obj);
--                                 count -= 1;
--                         }
--                 }
--                 if (count) inconsistent ();
--         }
-- }

-- in ada version, callers should instead `raise corrupt_saved_game'.  kill this func.

-- static void
-- inconsistent (void)
-- {
--         error("empsave.dat is inconsistent.  Please remove it.\n");
--         exit (1);
-- }

-- #ifndef USE_ZLIB
-- /*
--  * Write a buffer to a file.  If we cannot write everything, return FALSE.
--  * Also, tell the user why the write did not work if it didn't.
--  */

-- static int
-- xwrite (FILE *f, void *buf, int size)
-- {
--         int bytes;

--         bytes = fwrite (buf, 1, size, f);
--         if (bytes == -1) {
--                 error ("Write to save file failed");
--                 return (FALSE);
--         }
--         if (bytes != size) {
--                 error ("Cannot complete write to save file.\n");
--                 return (FALSE);
--         }
--         return (TRUE);
-- }

-- #else
-- /*
--  * Write a buffer to a file, compressing as we go.  If we cannot write
--  * everything, return FALSE.  Also, tell the user why the write did not
--  * work if it didn't.
--  */

-- static int
-- xwrite (gzFile *f, void *buf, int size)
-- {
--         int bytes;

--         bytes = gzwrite (f, buf, size);
--         if (bytes == 0) {
--                 error ("Write to save file failed");
--                 return (FALSE);
--         }
--         if (bytes != size) {
--                 error ("Cannot complete write to save file.\n");
--                 return (FALSE);
--         }
--         return (TRUE);
-- }

-- #endif

-- #ifndef USE_ZLIB
-- /*
--  * Read a buffer from a file.  If the read fails, we tell the user why
--  * and return FALSE.
--  */

-- static int
-- xread (FILE *f, void *buf, int size)
-- {
--         int bytes;

--         bytes = fread (buf, 1, size, f);
--         if (bytes == 0) {
--                 error ("Read from save file failed");
--                 return (FALSE);
--         }
--         if (bytes != size) {
--                 error ("Saved file is too short.\n");
--                 return (FALSE);
--         }
--         return (TRUE);
-- }

-- #else
-- /*
--  * Read a buffer from a file, decompressing as we go.  If the read fails,
--  * we tell the user why and return FALSE.
--  */

-- static int
-- xread (gzFile *f, void *buf, int size)
-- {
--         int bytes;

--         bytes = gzread (f, buf, size);
--         if (bytes == -1) {
--                 error ("Read from save file failed");
--                 return (FALSE);
--         }
--         if (bytes != size) {
--                 error ("Saved file is too short.\n");
--                 return (FALSE);
--         }
--         return (TRUE);
-- }
-- #endif

-- /*
--  * Save a movie screen.  For each cell on the board, we write out
--  * the character that would appear on either the user's or the
--  * computer's screen.  This information is appended to 'empmovie.dat'.
--  */

-- static char mapbuf[MAP_SIZE];

-- void
-- save_movie_screen (void)
-- {
-- #ifndef USE_ZLIB
--         FILE *f; /* file to save movie in */
-- #else
--         gzFile f; /* compressing file to save movie in */
-- #endif
--         long i;
--         piece_info_t *p;

-- #ifndef USE_ZLIB
--         f = fopen("empmovie.dat", "a"); /* open for append */
-- #else
--         f =gzopen("empmovie.dat", "a"); /* open for append */
-- #endif
--         if (f == NULL)
--         {
--                 error ("Cannot open empmovie.dat");
--                 return;
--         }

--         for (i = 0; i < MAP_SIZE; i++)
--         {
--                 if (map[i].cityp)
--                         mapbuf[i] = city_char[map[i].cityp->owner];
--                 else
--                 {
--                         p = find_obj_at_loc (i);

--                         if (!p)
--                                 mapbuf[i] = map[i].contents;
--                         else if (p->owner == USER)
--                                 mapbuf[i] = piece_attr[p->type].sname;
--                         else
--                           mapbuf[i] = tolower((int)piece_attr[p->type].sname);
--                 }
--         }
--         wbuf (mapbuf);
-- #ifndef USE_ZLIB
--         fclose(f);
-- #else
--         gzclose(f);
-- #endif
-- }

-- /*
--  * Replay a movie.  We read each buffer from the file and
--  * print it using a zoomed display.
--  */

-- void
-- replay_movie (void)
-- {
-- #ifndef USE_ZLIB
--         FILE *f; /* file to read movie from */
-- #else
--         gzFile f; /* compressing file to read movie from */
-- #endif
--         int round;

-- #ifndef USE_ZLIB
--         f = fopen("empmovie.dat", "r"); /* open for input */
-- #else
--         f = gzopen("empmovie.dat", "r");
-- #endif
--         if (f == NULL)
--         {
--                 error ("Cannot open empmovie.dat");
--                 return;
--         }
--         round = 0;

--         term_clear();

--         while (1)
--         {
--                 if (!xread (f, mapbuf, sizeof(mapbuf)))
--                         break;

--                 round += 1;

--                 stat_display(mapbuf, round);

--                 print_movie_screen(mapbuf);
--         }

-- #ifndef USE_ZLIB
--         fclose(f);
-- #else
--         gzclose(f);
-- #endif
-- }

-- /*
--  * Display statistics about the game.  At the top of the screen we
--  * print:
--  *
--  * nn O  nn A  nn F  nn P  nn D  nn S  nn T  nn C  nn B  nn Z  xxxxx
--  * nn X  nn a  nn f  nn p  nn d  nn s  nn t  nn c  nn b  nn z  xxxxx
--  *
--  * There may be objects in cities and boats that aren't displayed.
--  * The "xxxxx" field is the cumulative cost of building the hardware.
--  */

-- /* in declared order, with city first */
-- static const char *pieces = "OAFPDSTCBZXafpdstcbz";

-- static void
-- stat_display (char *mbuf, int round)
-- {
--         long i;
--         int counts[2*NUM_OBJECTS+2];
--         int user_cost, comp_cost;
--         char *p;
--         char buf1[STRSIZE], buf2[STRSIZE];

--         memset(counts, 0, sizeof(counts));

--         for (i = 0; i < MAP_SIZE; i++)
--         {
--                 p = strchr (pieces, mbuf[i]);
--                 if (p) counts[p-pieces] += 1;
--         }

--         user_cost = 0;
--         for (i = 1; i <= NUM_OBJECTS; i++)
--                 user_cost += counts[i] * piece_attr[i-1].build_time;

--         comp_cost = 0;
--         for (i = NUM_OBJECTS+2; i <= 2*NUM_OBJECTS+1; i++)
--                 comp_cost += counts[i] * piece_attr[i-NUM_OBJECTS-2].build_time;

--         for (i = FIRST_OBJECT; i < NUM_OBJECTS+1; i++)
--         {
--                 snprintf(buf1 + (i*6), STRSIZE - (i*6), "%2d %c  ", counts[i], pieces[i]);
--                 snprintf(buf2 + (i*6), STRSIZE - (i*6), "%2d %c  ", counts[i+NUM_OBJECTS+1], pieces[i+NUM_OBJECTS+1]);
--         }

--         snprintf(buf1 + (i*6), STRSIZE - (i*6), "%5d", user_cost);
--         snprintf(buf2 + (i*6), STRSIZE - (i*6), "%5d", comp_cost);

--         info(buf1);
--         info(buf2);
--         info("");

--         prompt("Round %3d", (round + 1) / 2);
-- }

end Empire.Game;
