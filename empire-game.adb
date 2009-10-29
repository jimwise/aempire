-- Routines to initialize, save, and restore a game

-- XXX This produces an utterly unportable save file, and thus clearly
-- XXX needs massive rework C version had been updated by me to use zlib, but I
-- XXX am dodging doing so with the Ada version until I get around to reworking
-- XXX the save file format

with Ada.Text_IO;
use Ada.Text_IO;

with Empire.Lists;
with Empire.Locations;
with Empire.Mapping;
with Empire.Math;
with Empire.Objects;

package body Empire.Game is

-- Initialize a new game.  Here we generate a new random map, put cities
-- on the map, select cities for each opponent, and zero out the lists of
-- pieces on the board.

   procedure Init_Game is
   begin
      Ui.Kill_Display;                     -- nothing on screen
      Automove := FALSE;
      Resigned := FALSE;
      Save_Movie := FALSE;
      Win := UNOWNED;
      Date := 0;                            -- no date yet
      User_Score := 0;
      Comp_Score := 0;

      View(USER) := (others => (Contents => ' ', Seen => 0));
      View(COMP) := (others => (Contents => ' ', Seen => 0));
      User_Obj := (others => null);
      Comp_Obj := (others => null);

      Free_List := null;

      for I in Object'Range
      loop
         declare
            Objp : Piece_Info_P;
         begin
            Object(I).Hits := 0;
            Objp := Object(I);
            Lists.Link(Free_List, Objp, PIECE_LINK);
         end;
      end loop;

      Make_Map;                            --  fills in land and water

      Place_Cities;
      while not Select_Cities              --  try to choose a city for each player
      loop
         for I in Map'Range
         loop
            -- remove cities placed in last pass, and try again
            if Map(I).Contents = '*'
            then
               Map(I).Contents := '+';
            end if;
         end loop;
         Place_Cities;
      end loop;
   end Init_Game;


--  Create a map.  To do this, we first randomly assign heights to each
--  map location.  Then we smooth these heights.  The more we smooth,
--  the better the land and water will clump together.  Then we decide
--  how high the land will be.  We attempt to choose enough land to meet
--  some required percentage.
--
--  There are two parameters to this algorithm:  the amount we will smooth,
--  and the ratio of land to water.  The user can provide these numbers
--  at program start up.

   procedure Make_Map is
      From : Integer;
      To : Integer;
      Tmp : Integer;
      Sum : Integer;
      Loc : Location_T;
      Waterline : Integer;
   begin
      for I in Location_T'Range
      loop
         Map(I).On_Board := True;       --  start with the assumption that we're on board
         --  mark disallowed `edge-of-the-world' cells as such
         case Locations.Loc_Col(I) is
            when Column_T'First | Column_T'Last =>
               Map(I).On_Board := False;
            when others =>
               null;
         end case;

         case Locations.Loc_Row(I) is
            when Row_T'First | Row_T'Last =>
               Map(I).On_Board := False;
            when others =>
               null;
         end case;

         --  fill map with random `sand'
         Height(1, I) := Math.Rand_Long(Max_Height);
      end loop;

      From := 1;
      To := 2;
      for I in 1 .. Smooth
      loop
         for J in Location_T'Range
         loop
            if Map(J).On_Board
            then
               Sum := Height(From, J);
               for K in Direction_T'Range
               loop
                  begin
                     Loc := J + Dir_Offset(K);
                     Sum := Sum + Height(From, Loc);
                  exception
                     when Constraint_Error =>
                        --  Ui.Error("Current Location is " & Location_T'Image(J));
                        --  Ui.Error("Current direction is " & Direction_T'Image(K));
                        --  Ui.Error("Row is " & Row_T'Image(Locations.Loc_Row(J)));
                        --  Ui.Error("Col is " & Column_T'Image(Locations.Loc_Col(J)));
                        --  Ui.Error("About to offset by " & Integer'Image(Dir_Offset(K)));
                        null;
                  end;
               end loop;
               Height(To, J) := Sum / 9;
            else
               --  Ui.Error("Skiping off-board location " & Location_T'Image(J));
               null;
            end if;
         end loop;
         --  swap to and from
         Tmp := To;
         To := From;
         From := Tmp;
      end loop;

      --  count the number of cells at each height
      for I in Map'Range
      loop
         Height_Count(Height(From, I)) := Height_Count(Height(From, I)) + 1;
      end loop;

      --  find the water line
      Waterline := MAX_HEIGHT;          --  default to all water
      sum := 0;
      for I in Height_Count'Range
      loop
         Sum := Sum + Height_Count(I);
         if ((Sum * 100 / Map_Size) > Water_Ratio) and (Sum >= NUM_CITY)
         then
            Waterline := I;            --  this is the last height that is under water
            exit;
         end if;
      end loop;

      --  mark the land and water
      for I in Map'Range
      loop
         if Height(From, I) > Waterline
         then
            Map(I).Contents := '+';
         else
            Map(I).Contents := '.';
         end if;

         Map(I).Objp := null;           --  nothing in cell yet
         Map(I).Cityp := null;
      end loop;
      Ui.Debug_Info("Generated map");
   end Make_Map;

--  Randomly place cities on the land.  There is a minimum distance that
--  should exist between cities.  We maintain a list of acceptable land cells
--  on which a city may be placed.  We randomly choose elements from this
--  list until all the cities are placed.  After each choice of a land cell
--  for a city, we remove land cells which are too close to the city.

   procedure Place_Cities is
      Placed : Location_T := 0;         --  count of cities placed
      I : Location_T;
      Loc : Location_T;
      Num_Land : Integer := 0;          --  count of entries in land array
   begin

      while Placed < NUM_CITY
      loop
         while Num_Land = 0
         loop
            Regen_Land(Placed, Num_Land);
         end loop;

         I := Math.Rand_Long(Num_Land - 1);  --  pick a random plot of land
         Loc := Land(I);

         Placed := Placed + 1;
         City(Placed) := (Loc => Loc,
                          Owner => UNOWNED,
                          Work => 0,
                          Prod => NOPIECE,
                          Func => (others => NOFUNC),
                          Dest => (others => 0)
                         );

         Map(Loc).Contents := '*';
         Map(Loc).Cityp := City(Placed)'Access;

         -- remove any land too close to plot we selected from land eligible to be a city
         Remove_Land(Loc, Num_Land);
      end loop;
      Ui.Debug_Info("Placed cities");
   end Place_Cities;

--  When we run out of available land, we recreate our land list.  We
--  put all land in the list, decrement the min_city_dist, and then
--  remove any land which is too close to a city.

   procedure Regen_Land (Placed : in Natural; Num_Land : in out Natural) is
   begin
      for I in Map'Range
      loop
         if Map(I).On_Board and (Map(I).Contents = '+')
         then
            Land(Num_land) := I;
            Num_Land := Num_Land + 1;
         end if;
      end loop;
      if Placed > 0                     --  don't decrement the first time through
      then
         Min_City_Dist := Min_City_Dist - 1;
      end if;
      for I in 0 .. Placed
      loop
         Remove_Land (City(I+1).Loc, Num_Land);
      end loop;
   end Regen_Land;


-- Remove land that is too close to a city

   procedure Remove_Land (Loc : in Location_T; Num_Land : in out Natural) is
      New_Land : Integer := 0;               --  nothing kept yet
   begin
      for I in 0 .. Num_Land
      loop
         if Math.Dist(Loc, Land(I)) >= MIN_CITY_DIST
         then
            Land(New_Land) := Land(I);
            New_Land := New_Land + 1;
         end if;
      end loop;
      Num_Land := New_Land;
   end Remove_Land;

-- Here we select the cities for the user and the computer.  Our choice of
-- cities will be heavily dependent on the difficulty level the user desires.
--
-- Our algorithm will not guarantee that either player will eventually be
-- able to move armies to any continent on the map.  There may be continents
-- which are unreachable by sea.  Consider the case of an island in a lake.
-- If the lake has no shore cities, then there is no way for a boat to reach
-- the island.  Our hope is that there will be enough water on the map, or enough
-- land, and that there will be enough cities, to make this case extremely rare.
--
-- First we make a list of continents which contain at least two cities, one
-- or more of which is on the coast.  If there are no such continents, we return
-- FALSE, and our caller should decide again where cities should be placed
-- on the map.  While making this list, we will rank the continents.  Our ranking
-- is based on the thought that shore cities are better than inland cities,
-- that any city is very important, and that the land area of a continent
-- is mildly important.  Usually, we expect every continent to have a different
-- ranking.  It will be unusual to have two continents with the same land area,
-- the same number of shore cities, and the same number of inland cities.  When
-- this is not the case, the first city encountered will be given the highest
-- rank.
--
-- We then rank pairs of continents.  We tell the user the number of different
-- ranks, and ask the user what rank they want to use.  This is how the
-- user specifies the difficulty level.  Using that pair, we have now decided
-- on a continent for each player.  We now choose a random city on each continent,
-- making sure the cities are not the same.
--
-- We return whether we succeeded, which allows us to loop until we do.

   function Select_Cities return Boolean is
      CompI, UserI : City_Index_T;
      CompP, UserP : City_Info_P;
      Comp_Cont, User_Cont : Cont_Index_T;
      Pair : Integer;
   begin
      Find_Cont;                        --  find and rank the continents
      if (NCont = 0)
      then
         Ui.Debug_Info("no usable continents found");
         return FALSE;                  --  no good continents
      end if;

      Make_Pair;                        --  create list of ranked pairs

      -- prompt user to choose N'th easiest pair
      Pair := Ui.Get_Int("Choose a difficulty level where 0 is easy and " & Integer'Image(Ncont*Ncont-1) & " is hard: ",
                      0, NCont*NCont-1);

      Comp_Cont := Pair_Tab(Pair).Comp_Cont;
      User_Cont := Pair_Tab(Pair).User_Cont;

      CompI := Math.Rand_Long(Cont_Tab(Comp_Cont).Ncity);
      CompP := Cont_Tab(Comp_Cont).Cityp(CompI);

      loop
         UserI := Math.Rand_Long(Cont_Tab(User_Cont).NCity);
         Userp := Cont_Tab(User_Cont).CityP(UserI);
         exit when UserP /= CompP;
      end loop;

      Ui.Info("Your city is at " & Location_T'Image(UserP.Loc) & ".");

      -- update city and map
      CompP.Owner := COMP;
      CompP.Prod := ARMY;
      CompP.Work := 0;
      Objects.Scan(COMP, CompP.Loc);

      UserP.Owner := USER;
      UserP.Work := 0;
      Objects.Scan(USER, UserP.Loc);
      Objects.Ask_Prod(UserP.all);

      Ui.Debug_Info("usable city pair found");
      return TRUE;
   end Select_Cities;

-- Find all continents with 2 cities or more, one of which must be a port.
-- city.  While at it,  rank the continents.

   procedure Find_Cont is
      Mapi : Location_T;
      Found : Boolean;
   begin
      Marked := (others => False);
      Ncont := 0;
      Mapi := 0;

      while Ncont < MAX_CONT
      loop
         Find_Next(Mapi, Found);
         exit when not Found;           --  all found
      end loop;
   end Find_Cont;

-- Find the next continent and insert it in the rank table.
-- If there are no more continents, we return false.

   procedure Find_Next (Mapi : in out Location_T; Found : out Boolean) is
      Val : Integer;
   begin
      Found := False;
      loop
         -- first, make sure to break out if we're past the end of the board
         if  Locations.Loc_Row(Mapi) = (Map_Height - 1)
         then
            exit;
         end if;

         if (not Map(Mapi).On_Board) or Marked(Mapi) or (Map(Mapi).Contents = '.')
         then
            Mapi := Mapi + 1;
         elsif Good_Cont(Mapi)
         then
            Rank_Tab(Ncont) := Ncont;   --  insert cont into rank table
            Val := Cont_Tab(Ncont).Value;

            for I in reverse 1 .. Ncont - 1
            loop
               if Val > Cont_Tab(Rank_Tab(I-1)).Value
               then
                  Rank_Tab(I) := Rank_Tab(I-1);
                  Rank_Tab(I-1) := Ncont;
               else
                  exit;
               end if;
            end loop;
            Ncont := Ncont + 1; --  count continents
            Found := True;      --  found at least one
            exit;
         end if;
      end loop;

      --     if (not Map(Mapi).On_Board) or Marked(Mapi) or (Map(Mapi).Contents = '.')
      --     then
      --        if Mapi = Location_T'Last
      --        then
      --           Found := False;
      --           return;
      --        end if;
      --        Mapi := Mapi + 1;
      --     elsif Good_Cont(Mapi)
      --     then
      --        Rank_Tab(Ncont) := Ncont;    --  insert cont in rank table
      --        Val := Cont_Tab(Ncont).Value;

      --        -- bubble sort
      --        for I in reverse 1 .. Ncont
      --        loop
      --           if Val > Cont_Tab(Rank_Tab(I-1)).Value
      --           then
      --              Rank_Tab(I) := Rank_Tab(I-1);
      --              Rank_Tab(I-1) := Ncont;
      --           else
      --              exit;
      --           end if;
      --        end loop;

      --        Ncont := Ncont + 1;
      --        Found := True;
      --        return;
      --     end if;
      --  end loop;
   end Find_Next;

-- Map out the current continent.  We mark every piece of land on the continent,
-- count the cities, shore cities, and land area of the continent.  If the
-- continent contains 2 cities and a shore city, we set the value of the
-- continent and return true.  Otherwise we return false.

   function Good_Cont (Mapi : in Location_T) return Boolean is
      Val : Integer;
   begin
      Ncity := 0;
      Nland := 0;
      Nshore := 0;

      Mark_Cont(Mapi);

      if (Nshore < 1) or (Ncity < 2)
      then
         return False;
      end if;

      -- The first two cities, one of which must be a shore city,
      -- don't contribute to the value.  Otherwise shore cities are
      -- worth 3/2 an inland city.  A city is worth 1000 times as much
      -- as land area.

      if Ncity = Nshore
      then
         Val := (Nshore - 2) * 3;
      else
         Val := ((Nshore - 1) * 3) + ((Ncity - Nshore - 1) * 2);
      end if;

      Val := Val * 1000;                --  cities are worth a lot
      Val := Val + Nland;

      Cont_Tab(Ncont).Value := Val;
      Cont_Tab(Ncont).Ncity := Ncity;
      return True;
   end Good_Cont;

-- Mark a continent.  This recursive algorithm marks the current square
-- and counts it if it is land or city.  If it is city, we also check
-- to see if it is a shore city, and we install it in the list of
-- cities for the continent.  We then examine each surrounding cell.

   -- XXX see vmap_land and vmap_cont_scan in comp_move for another approach.

   procedure Mark_Cont (Loc : in Location_T) is
   begin
      if Marked(Loc) or (Map(Loc).Contents = '.') or (not Map(Loc).On_Board)
      then
         return;                        --  don't recurse if we've hit the water
      end if;

      Marked(Loc) := True;             --  mark this cell seen
      Nland := Nland + 1;              --  count land on continent

      if Map(Loc).Contents = '*'
      then                              --  a city
         Cont_Tab(Ncont).Cityp(Ncity) := Map(Loc).Cityp;
         Ncity := Ncity + 1;
         if Empire.Mapping.Rmap_Shore(Loc)
         then
            Nshore := Nshore + 1;
         end if;
      end if;

      for I in Direction_T'Range        --  remember, we already checked if we're on_board
      loop
         Mark_Cont(Loc + Dir_Offset(I));
      end loop;
   end Mark_Cont;


   -- Create a list of pairs of continents in a ranked order.  The first
   -- element in the list is the pair which is easiest for the user to
   -- win with.  Our ranking is simply based on the difference in value
   -- between the user's continent and the computer's continent.

   procedure Make_Pair is
      Npair, val : Integer := 0;
   begin
      for I in 0 .. Ncont - 1
      loop
         for J in 0 .. Ncont - 1
         loop
            Val := Cont_Tab(I).Value - Cont_Tab(J).Value;
            Pair_Tab(Npair).Value := Val;
            Pair_Tab(Npair).User_Cont := I;
            Pair_Tab(Npair).Comp_Cont := J;

            for K in reverse 1 .. Npair --  bubble up new rank
            loop
               if Val > Pair_Tab(K).Value
               then
                  Pair_Tab(K) := Pair_Tab(K-1);
                  Pair_Tab(K-1).User_Cont := I;
                  Pair_Tab(K-1).Comp_Cont := J;
               else
                  exit;
               end if;
            end loop;
            Npair := Npair + 1;
         end loop;
      end loop;
   end Make_Pair;

   -- Save a game.  We save the game in emp_save.dat.  Someday we may want
   -- to ask the user for a file name.  If we cannot save the game, we will
   -- tell the user why.

   -- note that cempire uses zlib support here if defined(USE_ZLIB).  This
   -- support will be re-added in due time


-- /* macro to save typing; write an array, return if it fails */
-- #define wbuf(buf) if (!xwrite (f, (void *)buf, sizeof (buf))) return
-- #define wval(val) if (!xwrite (f, (void *)&val, sizeof (val))) return

   procedure Save_Game  is
      File: File_Type;
   begin
      Ui.Info("XXX XXX XXX File save is not yet supported");
      Create(File, Out_File, "empsave.dat");
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

      Close(File);
      Ui.Info("Game (NOT) saved");
   end Save_Game;

   -- ecover a saved game from emp_save.dat.
   -- We return TRUE if we succeed, otherwise FALSE.

   -- #define rbuf(buf) if (!xread (f, (void *)buf, sizeof(buf))) return (FALSE);
   -- #define rval(val) if (!xread (f, (void *)&val, sizeof(val))) return (FALSE);

   procedure Restore_Game is
      -- File: File_Type;
   begin
      Ui.Info("XXX XXX XXX File restore is not yet supported");
      raise Game.No_Saved_Game;

      -- Open(File, In_File, "empsave.dat");

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
      -- Close(File);

      -- Ui.Kill_Display;                  --  need to force a full refresh
      -- Ui.Info("Game (NOT) restored from empsave.dat.");
   end Restore_Game;

   -- Embark cargo on a ship.  We loop through the list of ships.
   -- We then loop through the pieces at the ship's location until
   -- the ship has the same amount of cargo it previously had.

   procedure Read_Embark (List : in Piece_Info_P; Ptype : in Piece_Type_T) is
      Ship : Piece_Info_P;
      Obj : Piece_Info_P;
      Count : Natural;
   begin
      Ship := List;

      while Ship /= null
      loop
         Count := Ship.Count;           --  expected number of pieces
         Ship.Count := 0;
         Obj := Map(Ship.Loc).Objp;

         while Count > 0 and Obj /= null
         loop

            if (Obj.Ship = null) and (Obj.Piece_Type = Ptype)
            then
               Objects.Embark(Ship, Obj);
               Count := Count - 1;
            end if;
            Obj := Obj.Links(Loc_Link).Next;
         end loop;
         if Count > 0
         then
            raise Corrupt_Saved_Game;
         end if;

         Ship := Ship.Links(Piece_Link).Next;
      end loop;
   end Read_Embark;

   -- Save a movie screen.  For each cell on the board, we write out
   -- the character that would appear on either the user's or the
   -- computer's screen.  This information is appended to 'empmovie.dat'.

   -- static char mapbuf[MAP_SIZE];

   procedure Save_Movie_Screen is
      File : File_Type;
      -- P : Piece_Info_P;
   begin
      -- XXX need to APPEND
      Open(File, Out_File, "empmovie.dat");
      Ui.Info("XXX XXX XXX Movie creation not yet supported");

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
      Close(File);
   end;

   -- Replay a movie.  We read each buffer from the file and
   -- print it using a zoomed display.

   procedure Replay_Movie is
      File : File_Type;
      -- Round : Integer;
   begin
      Open(File, In_File, "empmovie.dat");
      Ui.Info("XXX XXX XXX Movie playback not yet supported");

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

      Close(File);
   end Replay_Movie;

   -- Display statistics about the game.  At the top of the screen we
   -- print:
   --   nn O  nn A  nn F  nn P  nn D  nn S  nn T  nn C  nn B  nn Z  xxxxx
   --   nn X  nn a  nn f  nn p  nn d  nn s  nn t  nn c  nn b  nn z  xxxxx
   -- There may be objects in cities and boats that aren't displayed.
   -- The "xxxxx" field is the cumulative cost of building the hardware.

-- /* in declared order, with city first */
-- static const char *pieces = "OAFPDSTCBZXafpdstcbz";

   procedure Stat_Display(Frame : in Ui.Movie_Screen; Round : in Natural) is
--      Counts : Content_Value_Array := (others => 0);
--      User_Cost, Comp_Cost : Natural := 0;
   begin
      null;
      -- needs to be reworked to match how movie replay is reworked -- main issue is
      -- that we can't work backward easily from content_display_t to piece_type_t in
      -- order to calculate cost...

--        for I in Location_T'Range
--        loop
--           Counts(Frame(I)) := Counts(Frame(I)) + 1;
--        end loop;

--        for I in Counts'Range
--        loop
--           if User_Content(I)
--           then
--              User_Cost := User_Cost +

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
   end Stat_Display;

   -- Check to see if the game is over.  We count the number of cities
-- owned by each side.  If either side has no cities and no armies, then
-- the game is over.  If the computer has less than one third as many cities
-- and armies as the user, then the computer will offer to resign.
--
-- The computer will only offer to resign once per session, and the game continues
-- normally if the player refuses the computers offer.

   procedure Check_Endgame is
      Nuser_City : Integer := 0;
      Ncomp_City : Integer := 0;
      Nuser_Army : Integer := 0;
      Ncomp_Army : Integer := 0;
      P : Piece_Info_P;
   begin

      if Win /= UNOWNED
      then
         return;     -- we already know game is over
      end if;

      for I in City'Range
      loop
         case City(I).Owner is
            when USER =>
               Nuser_City := Nuser_City + 1;
            when COMP =>
               Ncomp_City := Ncomp_City + 1;
            when UNOWNED =>
               null;
         end case;
      end loop;

      P := User_Obj(ARMY);
      while P /= null
      loop
         Nuser_Army := Nuser_Army + 1;
         P := P.Links(Piece_Link).Next;
      end loop;

      P := Comp_Obj(ARMY);
      while P /= null
      loop
         Ncomp_Army := Ncomp_Army + 1;
         P := P.Links(Piece_Link).Next;
      end loop;

      if (Ncomp_City < Nuser_City / 3) and (Ncomp_Army < Nuser_Army / 3)
      then
         if not To_The_Death
         then
            if Ui.Get_Yn("The enemy acknowledges defeat.  Do you accept?")
            then
               Ui.Info("The enemy inadvertantly revealed the code they use for");
               Ui.Info("receiving battle information. You can display what");
               Ui.Info("they've learned with the 'Examine' command.");

               Resigned := TRUE;
               Win := USER;
               Automove := FALSE;
            else
               To_The_Death := TRUE;
            end if;
         end if;
      elsif (Ncomp_City = 0) and (Ncomp_Army = 0)
      then
         -- given the above condition, this can only happen if the computer is defeated while
         -- the user is also very weak.

         Ui.Info("The enemy is incapable of defeating you.");
         Ui.Info("There may be, however, remnants of the enemy fleet");
         Ui.Info("to be routed out and destroyed.");

         Win := USER;
         Automove := FALSE;
      elsif (Nuser_City = 0) and (Nuser_Army = 0)
      then
         Ui.Info("You have been rendered incapable of defeating");
         Ui.Info("the rampaging enemy. The empire is lost. If you");
         Ui.Info("have any ships left, you may hold out at sea.");

         Win := COMP;
         Automove := FALSE;
      elsif (Nuser_City < Ncomp_City / 3) and (Nuser_Army < Ncomp_Army / 3)
      then
         -- XXX Not in the original, but may be helpful.  Let's see how it plays out.
         Ui.Info("Intelligence reports suggest that the enemy are becoming");
         Ui.Info("much more powerful than your empire.  Your advisers");
         Ui.Info("recommend immediate steps to address this military gap.");
      end if;

   end Check_Endgame;

end Empire.Game;
