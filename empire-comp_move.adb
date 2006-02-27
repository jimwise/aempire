-- Make a move for the computer.
--
-- For each move the user wants us to make, we do the following:
--
--     1)  Handle city production;
--     2)  Move computer's pieces;
--     3)  Check to see if the game is over.

with Empire.Attack;
with Empire.Game;
with Empire.Mapping;
with Empire.Objects;
with Empire.Ui;
with Empire.Utility;

package body Empire.Comp_Move is

   procedure Comp_Move is
      Obj : Piece_Info_P;
   begin

      -- update our view of the world
      for I in Piece_Type_T'Range
      loop
         Obj := Comp_Obj(I);
         while Obj /= null
         loop
            Objects.Scan(Comp_Map, Obj.Loc);
         end loop;
      end loop;

      -- for each move we get...
      Ui.Prompt("Thinking...");

      Emap := Comp_Map;
      Mapping.Vmap_Prune_Explore_Locs(Emap);

      Do_Cities;                        -- handle city production
      Do_Pieces;                        -- move pieces

      if Save_Movie
      then
         Game.Save_Movie_Screen;
      end if;

      Check_Endgame;

   end Comp_Move;

-- Handle city production.  First, we set production for new cities.
-- Then we produce new pieces.  After producing a piece, we will see
-- if we should change our production.
--
-- Our goals for city production are first, not to change production
-- while something is in the process of being built.  Second, we attempt
-- to have enough city producing armies on a continent to counter any
-- threat on the continent, and to adequately explore and control the
-- continent.  Third, we attempt to always have at least one transport
-- producer.  Fourth, we attempt to maintain a good ratio between the
-- number of producers we have of each type of piece.  Fifth, we never
-- build carriers, as we don't have a good strategy for moving these.

   procedure  Do_Cities is
      Is_Land_Locked : Boolean;
   begin
      for I in City'Range
      loop
         if City(I).Owner = Comp
         then
            Objects.Scan(Comp_Map, City(I).Loc);

            if City(I).Prod = NOPIECE
            then
               Comp_Prod (City(I), Land_Locked(City(I).Loc));
            end if;
         end if;
      end loop;

      for I in City'Range         -- produce and change production
      loop
         if City(I).Owner = COMP
         then
            Is_Land_Locked := Land_Locked(City(I).Loc);
            City(I).Work := City(I).Work + 1; -- was post-increment in c, but this was wrong (?)
            if City(I).Work >= Piece_Attr(City(I).Prod).Build_time
            then
               Objects.Produce(City(I));
               Comp_Prod(City(I), Is_Land_Locked);
            else                     -- if we're making a ship in a land-locked city, stop!
               if Piece_Attr(City(I).Prod).Class = SHIP
               then
                 Comp_Prod(City(I), Is_Land_Locked);
               end if;
            end if;
         end if;
      end loop;

   end Do_Cities;

--  Set city production if necessary.
--
-- The algorithm below contains three parts:
--
-- 1)  Defend continents we own.
--
-- 2)  Produce a TT and a Satellite.
--
-- 3)  Meet the ratio requirements given above.

   procedure Comp_Prod (Cityp : in out City_Info_T; Is_Land_Locked : in Boolean) is
      City_Count : Piece_Value_Array := (others => 0); -- # of cities producing each piece
      Cont_Map : Continent_Map := (others => FALSE);
      Total_Cities : Integer := 0;
      Comp_Army_Count : Integer := 0;   -- # of army-producing cities
      P : City_Info_P;
      Need_Count : Integer := 0;
      Interest : Boolean := FALSE;
      Counts : Scan_Counts_T;

   begin
      -- Make sure we have army producers for current continent

      -- map out city's continent
      Mapping.Vmap_Cont(Cont_Map, Comp_Map, Cityp.Loc, '.');

      -- count items of interest on the continent
      Counts := Mapping.Vmap_Cont_Scan (Cont_Map, Comp_Map);

      for I in Comp_Map'Range
      loop
         if Cont_Map(I)                 -- for each cell of continent
         then
            if Comp_Map(I).Contents = 'X'
            then
               P := Objects.Find_City(I);
               if P.Prod = ARMY
               then
                  Comp_Army_Count := Comp_Army_Count + 1;
               end if;
            end if;
         end if;
      end loop;

      -- see if there's anything of interest on the continent
      Interest := (Counts.Unexplored > 0) or (Counts.User_Cities > 0) or
        (Counts.User_Objects(ARMY) > 0) or (Counts.Unowned_Cities > 0);

      --  we want one more army producer than enemy has cities
      --  and one more if anything of interest on continent
      Need_Count := Counts.User_Cities - Comp_Army_Count;
      if Interest
      then
         Need_Count := Need_Count + 1;
      end if;

      if Need_Count > 0                 -- need an army producer?
      then
         Comp_Set_Prod(Cityp, ARMY);
         return;
      end if;

      -- Produce armies in new cities if there is a city to attack
      if (Counts.User_Cities > 0) and (Cityp.Prod = NOPIECE)
      then
         Comp_Set_Prod(Cityp, ARMY);
         return;
      end if;

      -- Produce a TT if we don't have one

      -- count # of cities producing each piece
      for I in City'Range
      loop
         if (City(I).Owner = COMP) and (City(I).Prod /= NOPIECE)
         then
            City_Count(City(I).Prod) := City_Count(City(I).Prod) + 1;
            Total_Cities := Total_Cities + 1;
         end if;
      end loop;

      if Total_Cities <= 10
      then
         Ratio := 1;
      elsif Total_Cities <= 20
      then
         Ratio := 2;
      elsif Total_Cities <= 30
      then
         Ratio := 3;
      else
         Ratio := 4;
      end if;

      -- if we have one army producer, and this is it, return
      if (City_Count(ARMY) = 1) and (Cityp.Prod = ARMY)
      then
         return;
      end if;

      -- first available non-land-locked city becomes a tt producer
      if City_Count(TRANSPORT) = 0
      then
         if not Is_Land_Locked
         then
            Comp_Set_Prod(Cityp, TRANSPORT);
            return;
         end if;

         -- if we have one army producer that is not on a land-locked,
         -- produce armies here instead
         if City_Count(ARMY) = 1
         then
            for I in City'Range
            loop
               if (City(I).Owner = COMP) and (City(I).Prod = ARMY)
               then
                  if not Land_Locked(City(I).Loc)
                  then
                     Comp_Set_Prod(Cityp, ARMY);
                     return;
                  end if;
                  exit;                 -- only one, so stop at first
               end if;
            end loop;
         end if;
      end if;

      -- don't change prod from armies if there's something on continent
      if (Cityp.Prod = ARMY) and Interest
      then
         return;
      end if;

      -- Produce armies in new cities if there is a city to attack
      if (Counts.Unowned_Cities > 0) and Cityp.Prod = NOPIECE
      then
         Comp_Set_Prod(Cityp, ARMY);
         return;
      end if;

      -- Set production to item most needed.  Continents with one
      -- city and nothing interesting may not produce armies.  We
      -- set production for unset cities, and change production for
      -- cities that produce objects for which we have many city producers.
      -- Ship producers on lakes also get there production changed.

      Interest := (Counts.Comp_Cities /= 1) or Interest;

      case Cityp.Prod is
         when NOPIECE =>
            Comp_Set_Needed(Cityp, City_Count, Interest, Is_Land_Locked);
         when ARMY =>
            if Counts.Comp_Cities = 1
            then
               Comp_Set_Needed(Cityp, City_Count, Interest, Is_Land_Locked);
            end if;
         when PATROL|DESTROYER|SUBMARINE|TRANSPORT|CARRIER|BATTLESHIP =>
            if Is_Land_locked           -- separate from Do_Cities case so that doesn't loop with this
            then
               Comp_Set_Needed(Cityp, City_Count, Interest, Is_Land_Locked);
            end if;
         when others =>
            if Overproduced(Cityp, City_Count)
            then
               Comp_Set_Needed(Cityp, City_Count, Interest, Is_Land_Locked);
            end if;
      end case;

      end Comp_Prod;

-- Set production for a computer city to a given type.  Don't
-- reset production if it is already correct.

   procedure Comp_Set_Prod (Cityp : in out City_Info_T; Ptype : in Piece_Type_T) is
   begin
      if Cityp.Prod = Ptype
      then
         return;
      end if;

      if Print_Debug
      then
         Ui.Info("Changing city production at " & Location_T'Image(Cityp.Loc) &
                 " from " & Piece_Type_T'Image(Cityp.Prod) & " to " &
                 Piece_Type_T'Image(Ptype));
      end if;

      Cityp.Prod := Ptype;
      -- re-tooling time of 20% of new production cost.
      Cityp.Work := -(Piece_Attr(Ptype).Build_Time / RETOOLING_DENOMINATOR);
   end Comp_Set_Prod;


-- See if a city is producing an object which is being overproduced

   function Overproduced (Cityp : in City_Info_T; City_Count : in Piece_Value_Array) return Boolean is
   begin
      for I in Piece_Type_T'Range
      loop
         -- return true if changing production would improve balance
         if (Cityp.Prod /= I) and
           ( ((City_Count(Cityp.Prod) - 1) * Ratios(Ratio)(I)) > ((City_Count(I) + 1) * Ratios(Ratio)(I)) )
         then
            return TRUE;
         end if;
      end loop;

      return FALSE;
   end Overproduced;

-- See if one type of production is needed more than another type.
-- Return the most-needed type of production.

   function Need_More (City_Count : in Piece_Value_Array; Prod1 : in Piece_Type_T; Prod2 : in Piece_Type_T) return Piece_Type_T is
   begin
      if (City_Count(Prod1) * Ratios(Ratio)(Prod2)) <= (City_Count(Prod2) * Ratios(Ratio)(Prod1))
      then
         return Prod1;
      else
         return Prod2;
      end if;
   end Need_More;

-- Figure out the most needed type of production.  We are passed
-- a flag telling us if armies are ok to produce.

   procedure Comp_Set_Needed (Cityp : in out City_Info_T; City_Count : in out Piece_Value_Array; Army_Ok : in Boolean; Is_Land_Locked : in Boolean) is
      Best_Prod : Piece_Type_T;
   begin
      if not Army_Ok
      then
         City_Count(ARMY) := INFINITY;
      end if;

      if Is_Land_Locked
      then
         -- choose fighter or army
         Comp_Set_Prod (Cityp, Need_More (City_Count, ARMY, FIGHTER));
         return;
      end if;

      -- don't choose fighter
      City_Count(FIGHTER) := INFINITY;

      Best_Prod := ARMY;                -- default
      for Prod in Piece_Type_T'Range
      loop
         Best_Prod := Need_More(City_Count, Best_Prod, Prod);
      end loop;

      Comp_Set_Prod(Cityp, Best_Prod);
   end Comp_Set_Needed;

-- See if a city is land-locked (not next to water or only next to a
-- lake).  We define a lake to be a body of water (where cities are
-- considered to be water) that does not touch either an attackable
-- city or unexplored territory.

-- Be careful, because we use the 'emap'.  This predicts whether
-- unexplored territory will be land or water.  The prediction should be
-- helpful, because small bodies of water that enclose unexplored
-- territory will appear as solid water.  Big bodies of water should
-- have unexplored territory on the edges.

   function Land_Locked (Loc : in Location_T) return Boolean is
      Cont_Map : Continent_Map;
      Counts : Scan_Counts_T;
   begin
      Mapping.Vmap_Cont(Cont_Map, Emap, Loc, '+');
      Counts := Mapping.Vmap_Cont_Scan(Cont_Map, Emap);

      return (Counts.Unowned_Cities > 0) or (Counts.User_Cities > 0) or (Counts.Unexplored > 0);
   end Land_Locked;


-- Move all computer pieces

   procedure Do_Pieces is
      Obj : Piece_Info_P;
      Next_Obj : Piece_Info_P;
   begin
      for I in Move_Order'Range
      loop
         -- loop through object lists
         Obj := Comp_Obj(Move_Order(I));
         while Obj /= null
         loop
            Next_Obj := Obj.Piece_Link.Next; -- set now, in case obj is destroyed
            Cpiece_Move(Obj.all);           -- actually move the piece
            Obj := Next_Obj;
         end loop;
      end loop;
   end Do_Pieces;

-- Move a piece.  We loop until all the moves of a piece are made.  Within
-- the loop, we find a direction to move that will take us closer to an
-- objective.

   procedure Cpiece_Move (Obj : in out Piece_Info_T) is
      Changed_Loc : Boolean;
      Saved_Loc : Location_T;
   begin

      if Obj.Piece_Type = SATELLITE
      then
         Objects.Move_Sat(Obj);
         return;
      end if;

      Obj.Moved := 0;                   -- not moved yet
      Changed_Loc := FALSE;             -- location not changed yet

      if Piece_Attr(Obj.Piece_Type).Piece_Range /= INFINITY
      then
         -- if we have a range limit, initialize our range
         if Comp_Map(Obj.Loc).Contents = 'X'  -- if we start in a city
         then
            Obj.Piece_range := Piece_Attr(Obj.Piece_Type).Piece_Range;
         end if;
      end if;

      while Obj.Moved < Objects.Obj_Moves(Obj)
      loop
         Saved_Loc := Obj.Loc;          -- remember starting location
         Move1(Obj);
         if Saved_Loc /= Obj.Loc
         then
            Changed_Loc := TRUE;
         end if;

         if (Piece_Attr(Obj.Piece_Type).Piece_Range /= INFINITY) and
           Obj.Hits > 0                 -- if we're range-limited
         then
            if Comp_Map(Obj.Loc).Contents = 'X'
            then
              -- remember, we've already move1'ed above.  so if we ended in
              -- a city, just end
              Obj.Moved := Piece_Attr(Obj.Piece_Type).Speed;
            end if;
         elsif Obj.Piece_Range = 0
         then
            if Print_Debug
            then
               -- XXX spaces correctly?
               Ui.Info("Fighter at " & Integer'Image(Obj.Loc) & " crashed and burned");
            end if;
            Objects.Kill_Obj(Obj, Obj.Loc);
         end if;
      end loop;

      -- if a boat is in port, damaged, and never moved, fix some damage

      if Obj.Hits > 0 and
        not Changed_loc and             -- we didn't move
        Piece_Attr(Obj.Piece_Type).Class = SHIP and
        Obj.Hits < Piece_Attr(Obj.Piece_Type).Max_Hits and
        Comp_Map(Obj.Loc).Contents = 'X'
      then
         Obj.Hits := Obj.Hits + 1;
      end if;
   end Cpiece_Move;

-- /* Move a piece one square. */

   procedure Move1 (Obj : in out Piece_Info_T) is
   begin
      case Obj.Piece_Type is
         when ARMY =>
            Army_Move(Obj);
         when TRANSPORT =>
            Transport_Move(Obj);
         when FIGHTER =>
            Fighter_Move(Obj);
         when PATROL|DESTROYER|SUBMARINE|CARRIER|BATTLESHIP =>
            Ship_Move(Obj);
         when others =>                 -- sats are moved before, we know no other
            raise Program_Error;
      end case;
   end Move1;

-- Move an army.
--
-- This is a multi-step algorithm:
--
-- 1) See if there is an object we can attack immediately.
--    If so, attack it.
--
-- 2) Look for the nearest land objective.
--
-- 3) If we find an objective reachable by land, figure out
--    how far away that objective is.  Based on the found objective,
--    also figure out how close a loadable tt must be to be of
--    interest.  If the objective is closer than the tt must be,
--    head towards the objective.
--
-- 4) Otherwise, look for the nearest loading tt (or tt producing
--    city).  If the nearest loading tt is farther than our land objective,
--    head towards the land objective.
--
--  5)  Otherwise, head for the tt.
--
-- 6)  If we still have no destination and we are in a city,
--    attempt to leave the city.
--
-- 7)  Once we have a destination, find the best move toward that
--     destination.  (If there is no destination, sit around and wait.)

   procedure Army_Move (Obj : in out Piece_Info_T) is
      New_Loc : Location_T;
      New_Loc2 : Location_T;
      Pathmap : Path_Map;               -- file-global in original
      Pathmap2 : Path_Map;
      Cross_Cost : Integer;             -- cost to enter water
      Amap: View_Map;                   -- file-global in original
   begin
      if Obj.Piece_Type /= ARMY
      then
         raise Program_Error;
      end if;

      Obj.Func := NOFUNC;               -- army doesn't want a tt

      if Mapping.Vmap_At_Sea(Comp_Map, Obj.Loc) -- army can't move?
      then
         Load_Army(Obj);                -- make sure we're on the best ship present
         Obj.Moved := Piece_Attr(ARMY).Speed;
         -- old code checked obj.ship for null, but we would have excepted in that case
         return;
      end if;

      if Obj.Ship /= null               -- is army on a transport?
      then
         -- if we're on a ship, only attack targets on land
         New_Loc := Find_Attack(Obj.Loc, Army_Attack, ('+'|'*' => TRUE, others => FALSE));
      else
         -- otherwise, sacrifice ourselves to kill nearby ships/planes if needed
         New_Loc := Find_Attack(Obj.Loc, Army_Attack, ('.'|'+'|'*' => TRUE, others => FALSE));
      end if;

      if New_Loc /= Obj.Loc             -- something to attack?
      then
         Attack.Attack(Obj, New_Loc);
         if Comp_Map(New_Loc).Contents = '.' and Obj.Hits > 0 -- moved to ocean and survived
         then
            Objects.Kill_Obj(Obj, New_Loc);     -- sacrificed to defend land
            Objects.Scan(User_Map, New_Loc);    -- rescan for user, since army is gone
         end if;

         return;
      end if;

      if Obj.Ship /= null               -- otherwise, if on a ship
      then
         if Obj.Ship.Func = NOFUNC
         then
            Load_Army(Obj);             -- original code paniced on fail, we except
            return;                     -- armies stay on a loading ship
         end if;

         Make_Unload_Map(Amap, Comp_Map);
         Mapping.Vmap_Find_Wlobj(New_Loc, Pathmap, Amap, Obj.Loc, Tt_Unload);
         Move_Objective(Obj, Pathmap, New_Loc, (' ' => TRUE, others => False));
         return;
      end if;

      -- otherwise (not on a ship)
      Mapping.Vmap_Find_Lobj(New_Loc, Pathmap, Comp_Map, Obj.Loc, Army_Fight);

      if New_Loc /= Obj.Loc
      then
         -- something interesting on land?
         case Comp_Map(New_Loc).Contents is
            -- by setting cross_cost, we argue against going elsewhere on the map
            when 'A'|'O' =>
               Cross_Cost := 60;        -- highest cost if enemy present
            when '*' =>
               Cross_Cost := 30;        -- medium cost for attackable city
            when ' ' =>
               Cross_Cost := 14;        -- low cost for exploring
            when others =>
               Utility.Panic("unrecognized objective");
         end case;
         Cross_Cost := Pathmap(New_Loc).Cost * 2 - Cross_Cost;
      else
         Cross_Cost := INFINITY;
      end if;

      -- see if there is something interesting to go to by water
      if Cross_Cost > 0                 -- possible to be false only due to normalization after case above
      then
         Make_Army_Load_Map(Obj, Amap, Comp_Map);
         Mapping.Vmap_Find_Lwobj(New_Loc2, Pathmap2, Amap, Obj.Loc, Army_Load, Cross_Cost);
         if New_Loc2 /= Obj.loc         -- found something?
         then
            Board_Ship(Obj, Pathmap2, New_Loc2);
            return;
         end if;
      end if;

      Move_Objective(Obj, Pathmap, New_Loc, (' ' => TRUE, others => FALSE));

   end Army_Move;

-- /* Remove pruned explore locs from a view map. */

-- static void
-- unmark_explore_locs (view_map_t *xmap)
-- {
--         long i;

--         for (i = 0; i < MAP_SIZE; i++)
--         if (map[i].on_board && xmap[i].contents == ' ')
--                 xmap[i].contents = emap[i].contents;
-- }

-- /*
--  * Make a load map.  We copy the view map and mark each loading
--  * transport and tt producing city with a '$'.
--  */

-- static void
-- make_army_load_map (piece_info_t *obj, view_map_t *xmap, view_map_t *vmap)
-- {
--         piece_info_t *p;
--         int i;

--         memcpy (xmap, vmap, sizeof (view_map_t) * MAP_SIZE);

--         /* mark loading transports or cities building transports */
--         for (p = comp_obj[TRANSPORT]; p; p = p->piece_link.next)
--         if (p->func == 0) /* loading tt? */
--         xmap[p->loc].contents = '$';

--         for (i = 0; i < NUM_CITY; i++)
--         if (city[i].owner == COMP && city[i].prod == TRANSPORT) {
--                 if (nearby_load (obj, city[i].loc))
--                         xmap[city[i].loc].contents = 'x'; /* army is nearby so it can load */
--                 else if (nearby_count (city[i].loc) < piece_attr[TRANSPORT].capacity)
--                         xmap[city[i].loc].contents = 'x'; /* city needs armies */
--         }

--         if (print_vmap == 'A')
--                 print_zoom(xmap);
-- }

-- /* Return true if an army is considered near a location for loading. */

-- static int
-- nearby_load (piece_info_t *obj, long loc)
-- {
--         return obj->func == 1 && dist (obj->loc, loc) <= 2;
-- }

-- /* Return number of nearby armies. */

-- static int
-- nearby_count (long loc)
-- {
--         piece_info_t *obj;
--         int count;

--         count = 0;
--         for (obj = comp_obj[ARMY]; obj; obj = obj->piece_link.next) {
--                 if (nearby_load (obj, loc)) count += 1;
--         }
--         return count;
-- }

-- /* Make load map for a ship. */

-- static void
-- make_tt_load_map (view_map_t *xmap, view_map_t *vmap)
-- {
--         piece_info_t *p;

--         memcpy (xmap, vmap, sizeof (view_map_t) * MAP_SIZE);

--         /* mark loading armies */
--         for (p = comp_obj[ARMY]; p; p = p->piece_link.next)
--         if (p->func == 1) /* loading army? */
--         xmap[p->loc].contents = '$';

--         if (print_vmap == 'L')
--                 print_zoom(xmap);
-- }

-- /*
--  * Make an unload map.  We copy the view map.  We then create
--  * a continent map.  For each of our cities, we mark out the continent
--  * that city is on.  Then, for each city that we don't own and which
--  * doesn't appear on our continent map, we set that square to a digit.
--  *
--  * We want to assign weights to each attackable city.
--  * Cities are more valuable if they are on a continent which
--  * has lots of cities.  Cities are also valuable if either it
--  * will be easy for us to take over the continent, or if we
--  * need to defend that continent from an enemy.
--  *
--  * To implement the above, we assign numbers to each city as follows:
--  *
--  * a)  if unowned_cities > user_cities && comp_cities == 0
--  *        set number to min (total_cities, 9)
--  *
--  * b)  if comp_cities != 0 && user_cities != 0
--  *        set number to min (total_cities, 9)
--  *
--  * c)  if enemy_cities == 1 && total_cities == 1, set number to 2.
--  *     (( taking the sole enemy city on a continent is as good as
--  *       getting a two city continent ))
--  *
--  * d)  Any other attackable city is marked with a '0'.
--  */

-- static int owncont_map[MAP_SIZE];
-- static int tcont_map[MAP_SIZE];

-- static void
-- make_unload_map (view_map_t *xmap, view_map_t *vmap)
-- {
--         long i;
--         scan_counts_t counts;

--         memcpy (xmap, vmap, sizeof (view_map_t) * MAP_SIZE);
--         unmark_explore_locs (xmap);

--         for (i = 0; i < MAP_SIZE; i++)
--                 owncont_map[i] = 0; /* nothing marked */

--         for (i = 0; i < NUM_CITY; i++)
--         if (city[i].owner == COMP)
--         vmap_mark_up_cont (owncont_map, xmap, city[i].loc, '.');

--         for (i = 0; i < MAP_SIZE; i++)
--         if (strchr ("O*", vmap[i].contents)) {
--                 int total_cities;

--                 vmap_cont (tcont_map, xmap, i, '.'); /* map continent */
--                 counts = vmap_cont_scan (tcont_map, xmap);

--                 total_cities = counts.unowned_cities
--                              + counts.user_cities
--                              + counts.comp_cities;

--                 if (total_cities > 9) total_cities = 0;

--                 if (counts.user_cities && counts.comp_cities)
--                         xmap[i].contents = '0' + total_cities;

--                 else if (counts.unowned_cities > counts.user_cities
--                          && counts.comp_cities == 0)
--                         xmap[i].contents = '0' + total_cities;

--                 else if (counts.user_cities == 1 && counts.comp_cities == 0)
--                         xmap[i].contents = '2';

--                 else xmap[i].contents = '0';
--         }
--         if (print_vmap == 'U')
--                 print_zoom(xmap);
-- }

-- /*
--  * Load an army onto a ship.  First look for an adjacent ship.
--  * If that doesn't work, move to the objective, trying to be
--  * close to the ocean.
--  */

-- static void
-- board_ship (piece_info_t *obj, path_map_t *pmap, long dest)
-- {
--         if (!load_army (obj)) {
--                 obj->func = 1; /* loading */
--                 move_objective (obj, pmap, dest, "t.");
--         }
-- }

-- /*
--  * Look for the most full, non-full transport at a location.
--  * refer switching to staying.  If we switch, we force
--  * one of the ships to become more full.
--  */

-- static piece_info_t *
-- find_best_tt (piece_info_t *best, long loc)
-- {
--         piece_info_t *p;

--         for (p = map[loc].objp; p != NULL; p = p->loc_link.next)
--         if (p->type == TRANSPORT && obj_capacity (p) > p->count) {
--                 if (!best) best = p;
--                 else if (p->count >= best->count) best = p;
--         }
--         return best;
-- }

-- /* Load an army onto the most full non-full ship. */

-- static int
-- load_army (piece_info_t *obj)
-- {
--         piece_info_t *p;
--         int i;
--         long x_loc;

--         p = find_best_tt (obj->ship, obj->loc); /* look here first */

--         for (i = 0; i < 8; i++) { /* try surrounding squares */
--                 x_loc = obj->loc + dir_offset[i];
--                 if (map[x_loc].on_board)
--                         p = find_best_tt (p, x_loc);

--         }
--         if (!p) return FALSE; /* no tt to be found */

--         if (p->loc == obj->loc) { /* stay in same place */
--                 obj->moved = piece_attr[ARMY].speed;
--         }
--         else move_obj (obj, p->loc); /* move to square with ship */

--         if (p->ship != obj->ship) { /* reload army to new ship */
--                 disembark (obj);
--                 embark (p, obj);
--         }
--         return TRUE;
-- }

-- /*
--  * Return the first location we find adjacent to the current location of
--  * the correct terrain.
--  */

-- static long
-- move_away (view_map_t *vmap, long loc, const char *terrain)
-- {
--         long new_loc;
--         int i;

--         for (i = 0; i < 8; i++) {
--                 new_loc = loc + dir_offset[i];
--                 if (map[new_loc].on_board
--                  && strchr (terrain, vmap[new_loc].contents))
--                         return (new_loc);
--         }
--         return (loc);
-- }

-- /*
--  * Look to see if there is an adjacent object to attack.  We are passed
--  * a location and a list of items we attack sorted in order of most
--  * valuable first.  We look at each surrounding on board location.
--  * If there is an object we can attack, we return the location of the
--  * best of these.
--  */

-- static long
-- find_attack (long loc, const char *obj_list, const char *terrain)
-- {
--         long new_loc, best_loc;
--         int i, best_val;
--         char *p;

--         best_loc = loc; /* nothing found yet */
--         best_val = INFINITY;
--         for (i = 0; i < 8; i++) {
--                 new_loc = loc + dir_offset[i];

--                 if (map[new_loc].on_board /* can we move here? */
--                     && strchr (terrain, map[new_loc].contents)) {
--                         p = strchr (obj_list, comp_map[new_loc].contents);
--                         if (p != NULL && p - obj_list < best_val) {
--                                 best_val = p - obj_list;
--                                 best_loc = new_loc;
--                         }
--                 }
--         }
--         return (best_loc);
-- }

-- /*
--  * Move a transport.
--  *
--  * There are two kinds of transports:  loading and unloading.
--  *
--  * Loading transports move toward loading armies.  Unloading
--  * transports move toward attackable cities on unowned continents.
--  *
--  * An empty transport is willing to attack adjacent enemy transports.
--  * Transports become 'loading' when empty, and 'unloading' when full.
--  */

-- static void
-- transport_move (piece_info_t *obj)
-- {
--         long new_loc;

--         /* empty transports can attack */
--         if (obj->count == 0) { /* empty? */
--                 obj->func = 0; /* transport is loading */
--                 new_loc = find_attack (obj->loc, tt_attack, ".");
--                 if (new_loc != obj->loc) { /* something to attack? */
--                         attack (obj, new_loc); /* attack it */
--                         return;
--                 }
--         }

--         if (obj->count == obj_capacity (obj)) /* full? */
--                 obj->func = 1; /* unloading */

--         if (obj->func == 0) { /* loading? */
--                 make_tt_load_map (amap, comp_map);
--                 new_loc = vmap_find_wlobj (path_map, amap, obj->loc, &tt_load);

--                 if (new_loc == obj->loc) { /* nothing to load? */
--                         memcpy (amap, comp_map, MAP_SIZE * sizeof (view_map_t));
--                         unmark_explore_locs (amap);
--                         if (print_vmap == 'S')
--                                 print_zoom(amap);
--                         new_loc = vmap_find_wobj (path_map, amap, obj->loc, &tt_explore);
--                 }

--                 move_objective (obj, path_map, new_loc, "a ");
--         }
--         else {
--                 make_unload_map (amap, comp_map);
--                 new_loc = vmap_find_wlobj (path_map, amap, obj->loc, &tt_unload);
--                 move_objective (obj, path_map, new_loc, " ");
--         }
-- }

-- /*
--  * Move a fighter.
--  *
--  * 1)  See if there is an object we can attack immediately.
--  * If so, attack it.
--  *
--  * 2)  Otherwise, if fighter is low on fuel, move toward nearest city
--  * if there is one in range.
--  *
--  * 3)  Otherwise, look for an objective.
--  */

-- static void
-- fighter_move (piece_info_t *obj)
-- {
--         long new_loc;

--         new_loc = find_attack (obj->loc, fighter_attack, ".+");
--         if (new_loc != obj->loc) { /* something to attack? */
--                 attack (obj, new_loc); /* attack it */
--                 return;
--         }
--         /* return to base if low on fuel */
--         if (obj->range <= find_nearest_city (obj->loc, COMP, &new_loc) + 2) {
--                 if (new_loc != obj->loc)
--                         new_loc = vmap_find_dest (path_map, comp_map, obj->loc,
--                                                   new_loc, COMP, T_AIR);
--         }
--         else new_loc = obj->loc;

--         if (new_loc == obj->loc) { /* no nearby city? */
--                 new_loc = vmap_find_aobj (path_map, comp_map, obj->loc,
--                                                &fighter_fight);
--         }
--         move_objective (obj, path_map, new_loc, " ");
-- }

-- /*
--  * Move a ship.
--  *
--  * Attack anything adjacent.  If nothing adjacent, explore or look for
--  * something to attack.
--  */

-- static void
-- ship_move (piece_info_t *obj)
-- {
--         long new_loc;
--         const char *adj_list;

--         if (obj->hits < piece_attr[obj->type].max_hits) { /* head to port */
--                 if (comp_map[obj->loc].contents == 'X') { /* stay in port */
--                         obj->moved = piece_attr[obj->type].speed;
--                         return;
--                 }
--                 new_loc = vmap_find_wobj (path_map, comp_map, obj->loc,
--                                                &ship_repair);
--                 adj_list = ".";

--         }
--         else {
--                 new_loc = find_attack (obj->loc, ship_attack, ".");
--                 if (new_loc != obj->loc) { /* something to attack? */
--                         attack (obj, new_loc); /* attack it */
--                         return;
--                 }
--                 /* look for an objective */
--                 memcpy (amap, comp_map, MAP_SIZE * sizeof (view_map_t));
--                 unmark_explore_locs (amap);
--                 if (print_vmap == 'S')
--                         print_zoom (amap);

--                 new_loc = vmap_find_wobj (path_map, amap, obj->loc,
--                                                &ship_fight);
--                 adj_list = ship_fight.objectives;
--         }

--         move_objective (obj, path_map, new_loc, adj_list);
-- }

-- /* Move to an objective. */

-- static void
-- move_objective (piece_info_t *obj, path_map_t pathmap[], long new_loc, const char *adj_list)
-- {
--         const char *terrain;
--         const char *attack_list;
--         int d;
--         int reuse; /* true iff we should reuse old map */
--         long old_loc;
--         long old_dest;

--         if (new_loc == obj->loc) {
--                 obj->moved = piece_attr[obj->type].speed;
--                 obj->range -= 1;

--                 if (print_debug)
--                         info("No destination found for %d at %d; func=%d\n", obj->type, obj->loc, obj->func);

--                 return;
--         }
--         old_loc = obj->loc; /* remember where we are */
--         old_dest = new_loc; /* and where we're going */

--         d = dist (new_loc, obj->loc);
--         reuse = 1; /* try to reuse unless we learn otherwise */

--         if (comp_map[new_loc].contents == ' ' && d == 2) { /* are we exploring? */
--                 vmap_mark_adjacent (pathmap, obj->loc);
--                 reuse = 0;
--         }
--         else vmap_mark_path (pathmap, comp_map, new_loc); /* find routes to destination */

--         /* path terrain and move terrain may differ */
--         switch (obj->type) {
--         case ARMY: terrain = "+"; break;
--         case FIGHTER: terrain = "+.X"; break;
--         default: terrain = ".X"; break;
--         }

--         new_loc = vmap_find_dir (pathmap, comp_map, obj->loc,
--                                  terrain, adj_list);

--         if (new_loc == obj->loc /* path is blocked? */
--             && (obj->type != ARMY || !obj->ship)) { /* don't unblock armies on a ship */
--                 vmap_mark_near_path (pathmap, obj->loc);
--                 reuse = 0;
--                 new_loc = vmap_find_dir (pathmap, comp_map, obj->loc,
--                                          terrain, adj_list);
--         }

--         /* encourage army to leave city */
--         if (new_loc == obj->loc && map[obj->loc].cityp != NULL
--                                 && obj->type == ARMY) {
--                 new_loc = move_away (comp_map, obj->loc, "+");
--                 reuse = 0;
--         }
--         if (new_loc == obj->loc) {
--                 obj->moved = piece_attr[obj->type].speed;

--                 if (!(obj->type == ARMY && obj->ship))
--                         if (print_debug)
--                                 info("Cannot move %d at %d toward objective; func=%d\n", obj->type, obj->loc, obj->func);
--         }
--         else move_obj (obj, new_loc);

--         /* Try to make more moves using same path map. */
--         if (reuse && obj->moved < obj_moves (obj) && obj->loc != old_dest) {
--                 /* check for immediate attack */
--                 switch (obj->type) {
--                 case FIGHTER:
--                         if (comp_map[old_dest].contents != 'X' /* watch fuel */
--                                 && obj->range <= piece_attr[FIGHTER].range / 2)
--                                         return;
--                         attack_list = fighter_attack;
--                         terrain = "+.";
--                         break;
--                 case ARMY:
--                         attack_list = army_attack;
--                         if (obj->ship) terrain = "+*";
--                         else terrain = "+.*";
--                         break;
--                 case TRANSPORT:
--                         terrain = ".*";
--                         if (obj->cargo) attack_list = tt_attack;
--                         else attack_list = "*O"; /* causes tt to wake up */
--                         break;
--                 default:
--                         attack_list = ship_attack;
--                         terrain = ".";
--                         break;
--                 }
--                 if (find_attack (obj->loc, attack_list, terrain) != obj->loc)
--                         return;

--                 /* clear old path */
--                 pathmap[old_loc].terrain = T_UNKNOWN;
--                 for (d = 0; d < 8; d++) {
--                         new_loc = old_loc + dir_offset[d];
--                         pathmap[new_loc].terrain = T_UNKNOWN;
--                 }
--                 /* pathmap is already marked, but this should work */
--                 move_objective (obj, pathmap, old_dest, adj_list);
--         }
-- }

-- /*
--  * Check to see if the game is over.  We count the number of cities
--  * owned by each side.  If either side has no cities and no armies, then
--  * the game is over.  If the computer has less than one third as many cities
--  * and armies as the user, then the computer will offer to resign.
--  *
--  * The computer will only offer to resign once per session, and the game continues
--  * normally if the player refuses the computers offer.
--  */

-- static int      to_the_death = FALSE;

-- static void
-- check_endgame (void)
-- {
--         /* see if game is over */
--         int nuser_city, ncomp_city;
--         int nuser_army, ncomp_army;
--         piece_info_t *p;
--         int i;

--         date += 1; /* one more turn has passed */
--         if (win != UNOWNED)
--                 return; /* we already know game is over */

--         nuser_city = 0; /* nothing counted yet */
--         ncomp_city = 0;
--         nuser_army = 0;
--         ncomp_army = 0;

--         for (i = 0; i < NUM_CITY; i++)
--         {
--                 if (city[i].owner == USER)
--                         nuser_city++;
--                 else if (city[i].owner == COMP)
--                         ncomp_city++;
--         }

--         for (p = user_obj[ARMY]; p != NULL; p = p->piece_link.next)
--                 nuser_army++;

--         for (p = comp_obj[ARMY]; p != NULL; p = p->piece_link.next)
--                 ncomp_army++;

--         if ((ncomp_city < nuser_city / 3) && (ncomp_army < nuser_army / 3))
--         {
--                 if (!to_the_death)
--                 {
--                         if (getyn("The computer acknowledges defeat. Do you accept?"))
--                         {
--                                 info("The enemy inadvertantly revealed its code used for");
--                                 info("receiving battle information. You can display what");
--                                 info("they've learned with the ''E'' command.");

--                                 resigned = TRUE;
--                                 win = USER;
--                                 automove = FALSE;
--                         }
--                         else
--                                 to_the_death = TRUE;
--                 }
--         }
--         else if ((ncomp_city == 0) && (ncomp_army == 0))
--         {
--                 info("The enemy is incapable of defeating you.");
--                 info("There may be, however, remnants of the enemy fleet");
--                 info("to be routed out and destroyed.");

--                 win = USER;
--                 automove = FALSE;
--         }
--         else if ((nuser_city == 0) && (nuser_army == 0))
--         {
--                 info("You have been rendered incapable of defeating");
--                 info("the rampaging enemy. The empire is lost. If you");
--                 info("have any ships left, you may hold out at sea.");

--                 win = COMP;
--                 automove = FALSE;
--         }
-- }

end Empire.Comp_Move;
