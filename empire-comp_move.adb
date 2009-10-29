-- Make a move for the computer.
--
-- For each move the user wants us to make, we do the following:
--
--     1)  Handle city production;
--     2)  Move computer's pieces;
--     3)  Check to see if the game is over.

with Empire.Attack;
with Empire.Mapping;
with Empire.Math;
with Empire.Objects;
with Empire.Ui;
with Empire.Utility;

package body Empire.Comp_Move is

   procedure Comp_Move is
      Obj : Piece_Info_P;
   begin

      Ui.Prompt("Updating...");
      -- update our view of the world
      Ui.Debug_Info("Updating worldview");
      for I in Piece_Type_T'Range
      loop
         Obj := Comp_Obj(I);
         while Obj /= null
         loop
            Ui.Prompt("ENEMY: UPDATING PIECE AT " & Location_T'Image(Obj.Loc));
            Objects.Scan(COMP, Obj.Loc);
            Ui.Prompt("");
            Obj := Obj.Links(PIECE_LINK).Next;
         end loop;
      end loop;

      -- for each move we get...
      Ui.Prompt("Thinking...");

      Emap := View(COMP);
      Ui.Debug_Info("calling vmap_prune_explore_locs");
      Vmap_Prune_Explore_Locs(Emap);

      Ui.Prompt("Moving...");
      Do_Cities;                        -- handle city production
      Ui.Debug_Info("handling pieces");
      Do_Pieces;                        -- move pieces
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
--
-- XXX user_move can now use carriers to refuel planes, once we add
-- XXX same code here, we just need a strategy to position and guard
-- XXX carriers

   procedure  Do_Cities is
      Is_Land_Locked : Boolean;
   begin
      for I in City'Range
      loop
         if City(I).Owner = Comp
         then
            Objects.Scan(COMP, City(I).Loc);

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
      Cont_Map : Continent_Map; --  := (others => FALSE);
      Total_Cities : Integer := 0;
      Comp_Army_Count : Integer := 0;   -- # of army-producing cities
      P : City_Info_P;
      Need_Count : Integer := 0;
      Interest : Boolean := FALSE;
      Counts : Scan_Counts_T;

   begin
      -- Make sure we have army producers for current continent

      -- map out city's continent
      -- Mapping.Vmap_Cont(Cont_Map, View(COMP), Cityp.Loc, '.');
      Cont_Map := Vmap_Land(View(COMP), Cityp.Loc);

      -- count items of interest on the continent
      Counts := Vmap_Cont_Scan (Cont_Map, View(COMP));

      for I in View(COMP)'Range
      loop
         if Cont_Map(I)                 -- for each cell of continent
         then
            P := Objects.Find_City_At_Loc(I, Owners => (COMP => TRUE, others => FALSE));
            if (P /= null) and then (P.Prod = ARMY)
            then
               Comp_Army_Count := Comp_Army_Count + 1;
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
         -- XXX spaces right?
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
      Cont_Map : Continent_Map; -- := (others => false)
      Counts : Scan_Counts_T;
   begin
      -- Mapping.Vmap_Cont(Cont_Map, Emap, Loc, '+');
      Cont_Map := Vmap_Water(Emap, Loc);
      Counts := Vmap_Cont_Scan(Cont_Map, Emap);

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
            Next_Obj := Obj.Links(Piece_Link).Next; -- set now, in case obj is destroyed
            Cpiece_Move(Obj);           -- actually move the piece
            Obj := Next_Obj;
         end loop;
      end loop;
   end Do_Pieces;

-- Move a piece.  We loop until all the moves of a piece are made.  Within
-- the loop, we find a direction to move that will take us closer to an
-- objective.

   procedure Cpiece_Move (Obj : in out Piece_Info_P) is
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
         if View(COMP)(Obj.Loc).Contents = 'X'  -- if we start in a city
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

         -- XXX why the check for hits > 0?
         if (Piece_Attr(Obj.Piece_Type).Piece_Range /= INFINITY) and
           Obj.Hits > 0                 -- if we're range-limited
         then
            if View(COMP)(Obj.Loc).Contents = 'X'
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
               Ui.Info("Fighter at " & Location_T'Image(Obj.Loc) & " crashed and burned");
            end if;
            Objects.Kill_Obj(Obj, Obj.Loc);
         end if;
      end loop;

      -- if a boat is in port, damaged, and never moved, fix some damage

      if Obj.Hits > 0 and
        not Changed_loc and             -- we didn't move
        Piece_Attr(Obj.Piece_Type).Class = SHIP and
        Obj.Hits < Piece_Attr(Obj.Piece_Type).Max_Hits and
        View(COMP)(Obj.Loc).Contents = 'X'
      then
         Obj.Hits := Obj.Hits + 1;
      end if;
   end Cpiece_Move;

-- /* Move a piece one square. */

   procedure Move1 (Obj : in out Piece_Info_P) is
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

   procedure Army_Move (Obj : in out Piece_Info_P) is
      New_Loc : Location_T;
      New_Loc2 : Location_T;
      Pmap : Path_Map;                  -- file-global in original
      Pmap2 : Path_Map;
      Cross_Cost : Integer;             -- cost to enter water
      Amap: View_Map;                   -- file-global in original
   begin
      if Obj.Piece_Type /= ARMY
      then
         raise Program_Error;
      end if;

      Obj.Func := COMP_UNLOADING;               -- army doesn't want a tt

      if Mapping.Vmap_At_Sea(View(COMP), Obj.Loc) -- army can't move?
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
         if View(COMP)(New_Loc).Contents = '.' and Obj.Hits > 0 -- moved to ocean and survived
         then
            Objects.Kill_Obj(Obj, New_Loc);     -- sacrificed to defend land
            Objects.Scan(USER, New_Loc);    -- rescan for user, since army is gone
         end if;

         return;
      end if;

      if Obj.Ship /= null               -- otherwise, if on a ship
      then
         if Obj.Ship.Func = COMP_LOADING
         then
            Load_Army(Obj);             -- original code paniced on fail, we except
            return;                     -- armies stay on a loading ship
         end if;

         Make_Unload_Map(Amap, View(COMP));
         Mapping.Vmap_Find_SeaLand_Obj(New_Loc, Pmap, Amap, Obj.Loc, Tt_Unload);
         Move_Objective(Obj, Pmap, New_Loc, (' ' => 1, others => 0));
         return;
      end if;

      -- otherwise (not on a ship)
      Mapping.Vmap_Find_Ground_Obj(New_Loc, Pmap, View(COMP), Obj.Loc, Army_Fight);

      if New_Loc /= Obj.Loc
      then
         -- something interesting on land?
         case View(COMP)(New_Loc).Contents is
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
         Cross_Cost := Pmap(New_Loc).Cost * 2 - Cross_Cost;
      else
         Cross_Cost := INFINITY;
      end if;

      -- see if there is something interesting to go to by water
      if Cross_Cost > 0                 -- possible to be false only due to normalization after case above
      then
         Make_Army_Load_Map(Obj, Amap, View(COMP));
         Mapping.Vmap_Find_Landsea_Obj(New_Loc2, Pmap2, Amap, Obj.Loc, Army_Load, Cross_Cost);
         if New_Loc2 /= Obj.loc         -- found something?
         then
            -- Load an army onto a ship.  First look for an adjacent ship.
            -- If that doesn't work, move to the objective, trying to be
            -- close to the ocean.
            -- Note:  this was a function in the original, but was only called here.
            begin
               Load_Army(Obj);
            exception
               when Could_Not_Load =>
                  Obj.Func := COMP_LOADING;
                  Move_Objective(Obj, Pmap2, New_Loc2, ('t' => 2, '.' => 1, others => 0));
            end;
            return;
         end if;
      end if;

      Move_Objective(Obj, Pmap, New_Loc, (' ' => 1, others => 0));

   end Army_Move;

-- Remove pruned explore locs from a view map

   procedure Unmark_Explore_Locs (Xmap : in out View_Map) is
   begin
      for I in Xmap'Range
      loop
         if Map(I).On_Board and (Xmap(I).Contents = ' ')
         then
            Xmap(I).Contents := Emap(I).Contents;
         end if;
      end loop;
   end Unmark_Explore_Locs;

-- Make a load map.  We copy the view map and mark each loading
-- transport and transport-producing city with a '$'.

   procedure Make_Army_Load_Map (Obj : in Piece_Info_P; Xmap : in out View_Map; Vmap : in View_Map) is
      P : Piece_Info_P;
   begin
      Xmap := Vmap;

      P := Comp_Obj(TRANSPORT);
      while P /= null
      loop
         if P.Func = COMP_LOADING
         then
            Xmap(P.Loc).Contents := '$';
         end if;

         P := P.Links(Piece_Link).Next;
      end loop;

      for I in City'Range
      loop
         if (City(I).Owner = COMP) and (City(I).Prod = TRANSPORT)
         then
            if Nearby_Load(Obj, City(I).Loc)
            then
               Xmap(City(I).Loc).Contents := 'x'; -- army is nearby so it can load
            elsif Nearby_Count(City(I).Loc) < Piece_Attr(TRANSPORT).Capacity
            then
              Xmap(City(I).Loc).Contents := 'x'; -- city neads armies
            end if;
         end if;
      end loop;

      if Print_Vmap = 'A'
      then
         Ui.Print_Zoom(Xmap);
      end if;

   end Make_Army_Load_Map;

-- Return true if an army is considered near a location for loading

   function Nearby_Load (Obj : in Piece_Info_P; Loc : in Location_T) return Boolean is
   begin
      return (Obj.Func = COMP_LOADING) and (Math.Dist(Obj.Loc, Loc) <= LOAD_RADIUS);
   end Nearby_Load;


-- Return number of nearby armies. */

   function Nearby_Count (Loc : in Location_T) return Integer is
      Obj : Piece_Info_P;
      Count : Integer := 0;
   begin
      Obj := Comp_Obj(ARMY);
      while Obj /= null
      loop
         if Nearby_Load(Obj, Loc)
         then
            Count := Count + 1;
         end if;

         Obj := Obj.Links(Piece_Link).Next;
      end loop;

      return Count;
   end Nearby_Count;

-- Make load map for a ship

   procedure Make_Tt_Load_Map (Xmap : in out View_Map; Vmap : in View_Map) is
      P : Piece_Info_P;
   begin
      Xmap := Vmap;

      P := Comp_Obj(ARMY);
      while P /= null
      loop
         if P.Func = COMP_LOADING
         then
           Xmap(P.Loc).Contents := '$';
         end if;
      end loop;

      if Print_Vmap = 'L'
      then
         Ui.Print_Zoom(Xmap);
      end if;
   end Make_Tt_Load_Map;

-- Make an unload map.  We copy the view map.  We then create
-- a continent map.  For each of our cities, we mark out the continent
-- that city is on.  Then, for each city that we don't own and which
-- doesn't appear on our continent map, we set that square to a digit.
--
-- We want to assign weights to each attackable city.
-- Cities are more valuable if they are on a continent which
-- has lots of cities.  Cities are also valuable if either it
-- will be easy for us to take over the continent, or if we
-- need to defend that continent from an enemy.
--
-- To implement the above, we assign numbers to each city as follows:
--
-- a)  if unowned_cities > user_cities && comp_cities == 0
--     set number to min (total_cities, 9)
--
-- b)  if comp_cities != 0 && user_cities != 0
--     set number to min (total_cities, 9)
--
-- c)  if enemy_cities == 1 && total_cities == 1, set number to 2.
--     (( taking the sole enemy city on a continent is as good as
--     getting a two city continent ))
--
--  d)  Any other attackable city is marked with a '0'.

   procedure Make_Unload_Map (Xmap : in out View_Map; Vmap : in View_Map) is
      --  Owncont_Map : Continent_Map := (others => FALSE);
      Tcont_Map : Continent_Map; --  := (others => false);
      Counts : Scan_Counts_T;
      Total_Cities : Integer;
   begin

      Xmap := Vmap;
      Unmark_Explore_Locs(Xmap);

      -- Owncont_Map := (others => False);

      -- XXX XXX XXX this was here from long ago in the c, but it's a no-op, as
      -- XXX XXX XXX it fills in owncont_map, which is never used!
      --  for I in City'Range
      --  loop
      --     if City(I).Owner = COMP
      --     then
      --        -- can't easily switch to vmap_land, because we want to map multiple continents
      --        Mapping.Vmap_Mark_Up_Cont(Owncont_Map, Xmap, City(I).Loc, '.');
      --     end if;
      --  end loop;

      for I in Vmap'Range
      loop
         if (Vmap(I).Contents = 'O') or (Vmap(I).Contents = '*')
         then
            -- Mapping.Vmap_Cont(Tcont_Map, Xmap, I, '.');
            Tcont_Map := Vmap_Water(Xmap, I);
            Counts := Vmap_Cont_Scan(Tcont_Map, Xmap);

            Total_Cities := Counts.Unowned_Cities + Counts.User_Cities + Counts.Comp_Cities;

            if (Counts.User_Cities > 0) and (Counts.Comp_Cities > 0)
            then
               Xmap(I).Contents := Comp_Map_Weight(Total_Cities);
            elsif (Counts.Unowned_Cities > Counts.User_Cities) and (Counts.Comp_Cities = 0)
            then
               Xmap(I).Contents := Comp_Map_Weight(Total_Cities);
            elsif (Counts.User_Cities = 1) and (Counts.Comp_Cities = 0)
            then
               Xmap(I).Contents := Comp_Map_Weight(SINGLE_USER_CITY_WEIGHT);
            else
               Xmap(I).Contents := Comp_Map_Weight(0);
            end if;
         end if;
      end loop;

      if Print_Vmap = 'U'
      then
         Ui.Print_Zoom(Xmap);
      end if;

   end Make_Unload_Map;

   function Comp_Map_Weight (Weight : in Natural) return Content_Display_T is
   begin
      case Weight is
         when 0 =>
            return '0';
         when 1 =>
            return '1';
         when 2 =>
            return '2';
         when 3 =>
            return '3';
         when 4 =>
            return '4';
         when 5 =>
            return '5';
         when 6 =>
            return '6';
         when 7 =>
            return '7';
         when 8 =>
            return '8';
         when 9 =>
            return '9';
         when others =>
            -- perhaps this should change, but this is what the old code did, in effect
            return '0';
      end case;
   end Comp_Map_Weight;


-- Look for the most full, non-full transport at a location.
-- prefer switching to staying - if we switch, we force
-- one of the ships to become more full.

   procedure Find_Best_Tt (Best : in out Piece_Info_P; Loc : in Location_T) is
      P : Piece_Info_P;
   begin
      P := Map(Loc).Objp;
      while P /= null
      loop
         if (P.Piece_Type = TRANSPORT) and (Objects.Obj_Capacity(P) > P.Count)
         then
            if (Best = null) or else (P.Count >= Best.Count)
            then
               Best := P;
            end if;
         end if;
      end loop;

   end Find_Best_Tt;

-- Load an army onto the most full non-full ship

   procedure Load_Army (Obj : in Piece_Info_P) is
      X_Loc : Location_T;
      P : Piece_Info_P := Obj.Ship;
   begin
      if Obj.Piece_Type /= ARMY
      then
         raise Program_Error;
      end if;

      Find_Best_Tt(P, Obj.Loc);  -- look here first

      -- try surrounding squares
      for I in Dir_Offset'Range
      loop
         X_Loc := Obj.Loc + Dir_Offset(I);
         if Map(X_Loc).On_Board
         then
            Find_Best_Tt(P, X_Loc);
         end if;
      end loop;

      if P = null                       -- no tt to be found
      then
         raise Could_Not_Load;
      end if;

      if P.Loc = Obj.Loc                -- stay in same place
      then
         Obj.Moved := Piece_Attr(ARMY).Speed;
      else
         Objects.Move_Obj(Obj, P.Loc);             -- move to square with ship
      end if;

      if P /= Obj.Ship
      then
         Objects.Disembark(Obj);
         Objects.Embark(P, Obj);
      end if;

   end Load_Army;

-- Return the first location we find adjacent to the current location of
-- the correct terrain.

   function Move_Away (Vmap : in View_Map; Loc : in Location_T; Terrain : in Acceptable_Content_Array) return Location_T is
      New_Loc : Location_T;
   begin
      for I in Dir_Offset'Range
      loop
         New_Loc := Loc + Dir_Offset(I);
         if Map(New_Loc).On_Board and Terrain(Vmap(New_Loc).Contents)
         then
            return New_Loc;
         end if;
      end loop;

      return Loc;
   end Move_Away;

-- Look to see if there is an adjacent object to attack.  We are passed
-- a location and a list of items we attack sorted in order of most
-- valuable first.  We look at each surrounding on board location.
-- If there is an object we can attack, we return the location of the
-- best of these.

   function Find_Attack (Loc : in Location_T; Obj_List : in Content_Value_Array; Terrain : Acceptable_Terrain_Array) return Location_T is
      New_Loc : Location_T;
      Best_Loc : Location_T := Loc;     -- nothing better found yet
      Best_Val : Integer := 0;   -- higher is better
      P : Integer;
   begin
      for I in Dir_Offset'Range
      loop
         New_Loc := Loc + Dir_Offset(I);

         if Map(New_Loc).On_Board and Terrain(Map(New_Loc).Contents)
         then
            P := Obj_List(View(COMP)(New_Loc).Contents);
            if (P /= 0) and (P > Best_Val)
            then
               Best_Val := P;
               Best_Loc := New_Loc;
            end if;
         end if;
      end loop;

      return Best_Loc;
   end Find_Attack;

-- Move a transport.
--
-- There are two kinds of transports:  loading and unloading.
--
-- Loading transports move toward loading armies.  Unloading
-- transports move toward attackable cities on unowned continents.
--
-- An empty transport is willing to attack adjacent enemy transports.
-- Transports become 'loading' when empty, and 'unloading' when full.

   procedure Transport_Move (Obj : in out Piece_Info_P) is
      New_Loc : Location_T;
      Amap: View_Map;                   -- file-global in original
      Pmap : Path_Map;               -- file-global in original
   begin

      if Obj.Count = 0                  -- empty
      then
         Obj.Func := COMP_LOADING;           -- transport is loading
         New_Loc := Find_Attack(Obj.Loc, Tt_Attack, ('.' => TRUE, others => FALSE));

         if New_Loc /= Obj.loc          -- something to attack?
         then
            Attack.Attack(Obj, New_Loc);
            return;
         end if;
      end if;

      if Obj.Count = Objects.Obj_Capacity(Obj) -- full?
      then
        Obj.Func := COMP_UNLOADING;
      end if;

      if Obj.Func = COMP_LOADING
      then
         Make_Tt_Load_Map(Amap, View(COMP));
         Mapping.Vmap_Find_Sealand_Obj(New_Loc, Pmap, Amap, Obj.Loc, Tt_Load);

         if New_Loc = Obj.loc           -- nothing to load?
         then
            Amap := View(COMP);
            Unmark_Explore_Locs(Amap);
            if Print_Vmap = 'S'
            then
               Ui.Print_Zoom(Amap);
            end if;
            Mapping.Vmap_Find_Ship_Obj(New_Loc, Pmap, Amap, Obj.Loc, Tt_Explore);
         end if;

         Move_Objective(Obj, Pmap, New_Loc, ('a' => 2, ' ' => 1, others => 0));
      else
         Make_Unload_Map(Amap, View(COMP));
         Mapping.Vmap_Find_Sealand_Obj(New_Loc, Pmap, Amap, Obj.Loc, Tt_Unload);
         Move_Objective(Obj, Pmap, New_Loc, (' ' => 1, others => 0));
      end if;
   end Transport_Move;

-- Move a fighter.
--
-- 1) See if there is an object we can attack immediately.
--    If so, attack it.
--
-- 2) Otherwise, if fighter is low on fuel, move toward nearest city
--    if there is one in range.
--
-- 3)  Otherwise, look for an objective.

   procedure Fighter_Move (Obj : in out Piece_Info_P) is
      New_Loc : Location_T;
      Pmap : Path_Map;
      City_Found : Boolean;
      City_Cost : Integer;
   begin
      New_Loc := Find_Attack(Obj.Loc, Fighter_Attack, ('.'|'+' => TRUE, others => FALSE));
      if New_Loc /= Obj.Loc             -- something to attack?
      then
         Attack.Attack(Obj, New_Loc);
         return;
      end if;

      -- return to base if low on fuel
      -- XXX XXX XXX should check for carriers -- compare user_move, which now does.
      Objects.Find_Nearest_City(Obj.Loc, COMP, City_Found, New_Loc, City_Cost);
      if City_Found and Obj.Piece_range <= City_Cost + 2
      then
         if New_Loc /= Obj.Loc
         then
            Mapping.Vmap_Find_Dest(New_Loc, Pmap, View(COMP), Obj.Loc, New_Loc, COMP, T_AIR);
         end if;
      else
           New_Loc := Obj.Loc;
      end if;

      if New_Loc = Obj.Loc              -- no nearby city?
      then
         Mapping.Vmap_Find_Aircraft_Obj(New_Loc, Pmap, View(COMP), Obj.Loc, Fighter_Fight);
      end if;

      Move_Objective(Obj, Pmap, New_Loc, (' ' => 1, others => 0));
   end Fighter_Move;

-- Move a ship.
--
-- Attack anything adjacent.  If nothing adjacent, explore or look for
-- something to attack.
--
-- XXX carriers should probably have their own routine -- they shouldn't be attacking
-- XXX things, and they should be making themselves more useful

   procedure Ship_Move (Obj : in out Piece_Info_P) is
      New_Loc : Location_T;
      Adj_List : Content_Value_Array;
      Amap : View_Map;                  -- file-global in original
      Pmap : Path_Map;                  -- file-global in original
   begin
      -- if we're damaged, head to port (or stay there)
      if Obj.Hits < Piece_Attr(Obj.Piece_Type).Max_Hits
      then
         -- if in port, stay there
         if View(COMP)(Obj.Loc).Contents = 'X'
         then
            Obj.Moved := Piece_Attr(Obj.Piece_Type).Speed;
            return;
         end if;
         -- otherwise, go there
         Mapping.Vmap_Find_Ship_Obj(New_Loc, Pmap, View(COMP), Obj.Loc, Ship_Repair);
         Adj_List := ('.' => 1, others => 0);
      else
         New_Loc := Find_Attack(Obj.Loc, Ship_Attack, ('.' => TRUE, others => FALSE));
         if New_Loc /= Obj.loc          -- something to attack?
         then
            Attack.Attack(Obj, New_Loc);
            return;
         end if;
         -- look for an objective
         Amap := View(COMP);
         Unmark_Explore_Locs(Amap);
         if Print_Vmap = 'S'
         then
            Ui.Print_Zoom(Amap);
         end if;

         Mapping.Vmap_Find_Ship_Obj(New_Loc, Pmap, Amap, Obj.Loc, Ship_Fight);
         Adj_List := Ship_Fight.Objective_Weights;
      end if;

      Move_Objective(Obj, Pmap, New_Loc, Adj_List);
   end Ship_Move;

-- Move to an objective

   procedure Move_Objective (Obj : in out Piece_Info_P; Pathmap : in out Path_Map; Loc : in Location_T; Adj_List : in Content_Value_Array) is
      PTerrain : Acceptable_Content_Array;
      Terrain : Acceptable_Terrain_Array;
      Attack_List : Content_Value_Array;
      D : Integer;
      -- XXX XXX not clear if map reuse saves us much.  To find out, set this to FALSE, and compare
      -- XXX XXX profiling output.
      Reuse : Boolean := TRUE;          -- try to reuse unless we learn otherwise
      Old_Loc : Location_T;
      Old_Dest : Location_T;
      Pmap : Path_Map;
      New_Loc : Location_T := Loc;
   begin
      if New_Loc = Obj.Loc              -- standing still to burn a turn?
      then
         Obj.Moved := Piece_Attr(Obj.Piece_Type).Speed;
         if Piece_Attr(Obj.Piece_Type).Piece_Range /= INFINITY
         then
            Obj.Piece_Range := Obj.Piece_Range - 1;
         end if;

         if Print_Debug
         then
            -- XXX spaces right?
            Ui.Info("No destination found for" & Piece_Type_T'Image(Obj.Piece_Type) & " at " & Location_T'Image(Obj.Loc) &
                    "; func=" & Strings.To_String(Function_Name(Obj.Func)));
         end if;

         return;
      end if;

      Old_Loc := Obj.Loc;               -- remember where we are
      Old_Dest := New_Loc;              -- and where we're going

      D := Math.Dist(Old_Loc, Old_Dest);

      if (View(COMP)(New_Loc).Contents = ' ') and (D = 2) -- are we exploring?
      then
         Mapping.Vmap_Mark_Adjacent(Pmap, Obj.Loc);
         Reuse := FALSE;
      else
         Mapping.Vmap_Mark_Path(Pmap, View(COMP), New_Loc); -- find routes to destination
      end if;

      case Piece_Attr(Obj.Piece_Type).Class is
         when GROUND =>
            PTerrain := ('+' => TRUE, others => FALSE);
         when AIRCRAFT =>
            PTerrain := ('+'|'.'|'X' => TRUE, others => FALSE);
         when SHIP =>
            PTerrain := ('.'|'X' => TRUE, others => FALSE);
         when SPACECRAFT =>
            -- can't happen -- we don't do controlled movement for SAT
            raise Program_Error;
      end case;

      Mapping.Vmap_Find_Dir(New_Loc, Pmap, View(COMP), Obj.Loc, PTerrain, Adj_List);

      if New_Loc = Obj.Loc and          -- path is blocked?
        (Piece_Attr(Obj.Piece_Type).Class /= GROUND) and (Obj.Ship /= null) -- don't unblock armies on a ship
      then
         Mapping.Vmap_Mark_Near_Path(Pmap, Obj.Loc);
         Reuse := FALSE;
         Mapping.Vmap_Find_Dir(New_Loc, Pmap, View(COMP), Obj.Loc, PTerrain, Adj_List);
      end if;

      -- encourage army to leave city
      if (New_Loc = Obj.Loc) and (Map(Obj.Loc).Cityp /= null) and (Piece_Attr(Obj.Piece_Type).Class = GROUND)
      then
         New_Loc := Move_Away(View(COMP), Obj.Loc, ('+' => TRUE, others => FALSE));
         Reuse := FALSE;
      end if;

      -- if we still have nowhere to go, burn the turn
      if New_Loc = Obj.Loc
      then
         Obj.Moved := Piece_Attr(Obj.Piece_Type).Speed;

         if not ((Piece_Attr(Obj.Piece_Type).Class = GROUND) and (Obj.Ship /= null))
         then
            if Print_Debug
            then
               -- XXX spaces right?
               Ui.Info("Cannot move " & Piece_Type_T'Image(Obj.Piece_Type) & " at " & Location_T'Image(Obj.Loc) &
                       " toward objective at " & Location_T'Image(New_Loc) & "; func=" & Strings.To_String(Function_Name(Obj.Func)));
            end if;
         end if;
      else
         Objects.Move_Obj (Obj, New_Loc);
      end if;

      -- now, if we have not made the Path_Map un-reusable, immediately try to chain
      -- a second move onto the first, to save us an iteration.  It is _not_ clear how
      -- much this saves at this point, so given its complexity, we should profile and
      -- (perhaps) eliminate this.  Way to check would be to set default value of `Reuse'
      -- (above) to `FALSE' and see how profile data changes
      --
      -- Note also that this is error prone in that behavior of a chained move may not
      -- exactly match what we would have done if not chaining.

      -- if we can reuse the path map, and we have more moves, and we haven't reached
      -- our objective yet

      if Reuse and (Obj.Moved < Objects.Obj_Moves(Obj)) and (Obj.Loc /= Old_Dest)
      then
         -- check for immediate attack
         -- XXX XXX XXX split into an if for transport, and a case on piece_attr(obj.piece_type).class
         case Obj.Piece_Type is

            when FIGHTER =>
               -- watch fuel
               if (View(COMP)(Old_Dest).Contents /= 'X') and
                 (Obj.Piece_Range <= Piece_Attr(FIGHTER).Piece_Range / 2)
               then
                  return;               -- don't risk chaining - start over so fuel is recalculated
               end if;
               Attack_List := Fighter_Attack;
               Terrain := ('+'|'.' => TRUE, others => FALSE);

            when ARMY =>
               Attack_List := Army_Attack;
               -- IMPORTANT (XXX):  in the original code, these had 'X' instead of '*', which
               -- could _never_ match (since Find_Attack uses Map, not View(COMP)) (!).  This means
               -- that in the original code, each of these acted as if the city was _not_ included.
               -- THIS IS A BEHAVIOR CHANGE!  (and skill of Ai should be tested accordingly!)
               if Obj.Ship /= null
               then
                  Terrain := ('+'|'*' => TRUE, others => FALSE);
               else
                  Terrain := ('+'|'.'|'*' => TRUE, others => FALSE);
               end if;

            when TRANSPORT =>
               Terrain := ('.'|'*' => TRUE, others => FALSE);
               if Obj.Cargo /= null
               then
                  Attack_List := Tt_Attack;
               else
                  Attack_List := ('*' => 2, 'O' => 1, others => 0); -- cause TT to wake up
               end if;

            when SATELLITE =>
               raise Program_Error;

            when others => -- XXX assumes others are ships.  safe for anything comp will intentionally build
                           -- XXX until reworking, check that assumption:
               if Piece_Attr(Obj.Piece_Type).Class /= SHIP
               then
                   raise Program_Error;
               end if;
               Attack_List := Ship_Attack;
               Terrain := ('.' => TRUE, others => FALSE);
         end case;

         if Find_Attack(Obj.Loc, Attack_List, Terrain) /= Obj.Loc
         then
            return;
         end if;

         -- clear old path before moving, so we don't oscillate
         Pmap(Old_Loc).Terrain := T_UNKNOWN;
         for I in Dir_Offset'Range
         loop
            New_Loc := Old_Loc + Dir_Offset(I);
            Pathmap(New_Loc).Terrain := T_UNKNOWN;
         end loop;

         -- pathmap is already marked, but that should be harmless (XXX)
         Move_Objective(Obj, Pathmap, Old_Dest, Adj_List);
      end if;

   end Move_Objective;

   -- eventual replacements for Vmap_{Mark_Up_}Cont -- explicitly floodfill
   -- land or water, resulting in simpler calls and simpler functions

   function Vmap_Water (Vmap : in View_Map; Loc : in Location_T) return Continent_Map is
      Water_Chars : constant Acceptable_Content_Array := ('.'|'*' => True, others => False);
   begin
      return Vmap_Flood_Fill(Vmap, Loc, Water_Chars);
   end Vmap_Water;

   function Vmap_Land (Vmap : in View_Map; Loc : in Location_T) return Continent_Map is
      Land_Chars : constant Acceptable_Content_Array := ('+'|'*' => True, others => False);
   begin
      return Vmap_Flood_Fill(Vmap, Loc, Land_Chars);
   end Vmap_Land;

   function Vmap_Flood_Fill (Vmap : in View_Map; Loc : in Location_T; Good_Terrain : in Acceptable_Content_Array) return Continent_Map is
      Cont_Map : Continent_Map := (others => False);
      Seen : array (Location_T) of Boolean := (others => False); --  cells we've examined
      Workset : Location_Vectors.Vector;
      Cur, New_Loc : Location_T;
   begin
      -- test if loc is acceptable, or raise
      if not Good_Terrain(Map(Loc).Contents)
      then
         raise Program_Error;
      end if;

      -- push loc into a collection
      Cont_Map(Loc) := True;
      Workset.Prepend(Loc);

      -- while collection not empty
      while not Workset.Is_Empty
      loop
         Cur := Workset.First_element;
         Workset.Delete_First;

         -- FOREACH New_Loc Next To Cur
         for D in Direction_T'range
         loop
            New_Loc := Cur + Dir_Offset(D);
         --     if unexplored (per vmap) or desired_terrain (per vmap) or desired terrain (per map)
         --   we check desired terrain in two places, so that we can use the pruned Emap if desired
            if Map(New_Loc).On_Board and not Seen(New_Loc)
            then
               if Vmap(New_Loc).Contents = ' ' or else
                 Good_Terrain(Vmap(New_loc).Contents) or else
                 Good_Terrain(Map(New_Loc).Contents)
               then
                  Cont_Map(New_Loc) := True;
                  Workset.Prepend(New_Loc);
                  Seen(New_Loc) := True;
               end if;
            end if;
         end loop;
      end loop;

      return Cont_Map;
   end Vmap_Flood_Fill;

   -- Scan a continent recording items of interest on the continent.
   -- XXX XXX XXX This could be done as we flood-fill the continent.

   function Vmap_Cont_Scan (Cont_Map : in Continent_Map; Vmap : in View_Map) return Scan_Counts_T is
      procedure Incr (I : in out Integer) is
      begin
         I := I + 1;
      end;
      Counts : Scan_Counts_T;
   begin
      for I in Location_T'Range
      loop
         if Cont_Map(I)
         then
            Counts.Size := Counts.Size + 1;

            case Vmap(I).Contents is
               when ' ' => Incr(Counts.Unexplored);

               when 'O' => Incr(Counts.User_Cities);

               when 'A' => Incr(Counts.User_Objects(ARMY));
               when 'F' => Incr(Counts.User_Objects(FIGHTER));
               when 'P' => Incr(Counts.User_Objects(PATROL));
               when 'D' => Incr(Counts.User_Objects(DESTROYER));
               when 'S' => Incr(Counts.User_Objects(SUBMARINE));
               when 'T' => Incr(Counts.User_Objects(TRANSPORT));
               when 'C' => Incr(Counts.User_Objects(CARRIER));
               when 'B' => Incr(Counts.User_Objects(BATTLESHIP));

               when 'X' => Incr(Counts.Comp_Cities);

               when 'a' => Incr(Counts.Comp_Objects(ARMY));
               when 'f' => Incr(Counts.Comp_Objects(FIGHTER));
               when 'p' => Incr(Counts.Comp_Objects(PATROL));
               when 'd' => Incr(Counts.Comp_Objects(DESTROYER));
               when 's' => Incr(Counts.Comp_Objects(SUBMARINE));
               when 't' => Incr(Counts.Comp_Objects(TRANSPORT));
               when 'c' => Incr(Counts.Comp_Objects(CARRIER));
               when 'b' => Incr(Counts.Comp_Objects(BATTLESHIP));

               when '*' => Incr(Counts.Unowned_Cities);

               when '+'|'.' => null;

               when 'Z'|'z' =>                --  check for city underneath satellite
                 if Map(I).Contents = '*'
                 then
                    case Map(I).Cityp.Owner is
                        when USER => Incr(Counts.User_Cities);
                        when COMP => Incr(Counts.Comp_Cities);
                        when UNOWNED => Incr(Counts.Unowned_Cities);
                     end case;
                 end if;
               when others =>           --  XXX XXX XXX special markers '$' .. '9'
                  raise Program_Error;
            end case;
         end if;
      end loop;

      return Counts;
   end Vmap_Cont_Scan;

   -- Prune unexplored territory.  We take a view map and we modify it
   -- so that unexplored territory that is adjacent to a lot of land
   -- or a lot of water is marked as being either that land or water.
   -- So basically, we are making a predicition about what we expect
   -- for land and water.  We iterate this algorithm until either
   -- the next iteration would remove all unexplored territory, or
   -- there is nothing more about which we can make an assumption.
   --
   -- First, we use a pathmap to save the number of adjacent land
   -- and water cells for each unexplored cell.  Cells which have
   -- adjacent explored territory are placed in a perimeter list.
   -- We also count the number of cells that are not unexplored.
   --
   -- We now take this perimeter list and make high-probability
   -- predictions.
   --
   -- Then we round things off by making one pass of medium
   -- probability predictions.
   --
   -- Then we make multiple passes extending our predictions.
   --
   -- We stop if at any point all remaining unexplored cells are
   -- in a perimeter list, or if no predictions were made during
   -- one of the final passes.
   --
   -- Unlike other algorithms, here we deal with "off board" locations.
   -- So be careful. XXX XXX XXX is this necessary?!  would be cleaner
   -- to code without this caveat...

   -- XXX used only in comp_move, maybe should move there?

   procedure Vmap_Prune_Explore_Locs (Vmap : in out View_Map)
   is
      Pmap : Path_Map := (others => (0, 0, T_UNKNOWN));
      From, To, Tmp : Perimeter_T;
      Explored : Integer := 0;
      New_Loc, Loc : Location_T;
      Copied : Integer;
   begin
      From.Len := 0;

      -- build initial path map and perimeter list
      for L in Location_T'Range
      loop
         if Vmap(L).Contents /= ' '
         then
            Explored := Explored + 1;
         else                           --  add unexplored cell to perim
            for D in Direction_T'Range
            loop
               begin
                  New_Loc := L + Dir_Offset(D);
                  case Vmap(New_Loc).Contents is
                     when ' ' => null;  --  ignore adjacent unexplored
                     when '.' => Pmap(L).Inc_Cost := Pmap(L).Inc_Cost + 1; --  count water
                                                           -- XXX does this count ships as land?
                     when others => Pmap(L).Cost := Pmap(L).Cost + 1; --  count_land
                  end case;
               exception
                  when Constraint_Error => null; --  went off map via dir_offset (remember, we're working with all map locs)
               end;
            end loop;
            if Pmap(L).Cost > 0 or Pmap(L).Inc_Cost > 0
            then
               From.List(From.Len) := L;
               From.Len := From.Len + 1;
            end if;
         end if;
      end loop;

      if Print_Vmap = 'I'
      then
         Ui.Print_Zoom(Vmap);
      end if;

      loop    --  do high-probability predictions
         if From.Len + Explored = MAP_SIZE
         then
            return;                     --  nothing left to guess
         end if;

         To.Len := 0;
         Copied := 0;

         for I in 0 .. From.Len - 1
         loop
            Loc := From.List(I);
            if Pmap(Loc).Cost >= 5
            then
               Expand_Prune(Vmap, Pmap, Loc, T_LAND, To, Explored);
            elsif Pmap(Loc).Inc_Cost >= 5
            then
               Expand_Prune(Vmap, Pmap, Loc, T_WATER, To, Explored);
            elsif (Loc < MAP_WIDTH or Loc >= MAP_SIZE-MAP_WIDTH) and Pmap(Loc).Cost >= 3
            then
               Expand_Prune(Vmap, Pmap, Loc, T_LAND, To, Explored);
            elsif (Loc < MAP_WIDTH or Loc >= MAP_SIZE-MAP_WIDTH) and Pmap(Loc).Inc_Cost >= 3
            then
               Expand_Prune(Vmap, Pmap, Loc, T_WATER, To, Explored);
            elsif (Loc = 0 or Loc = Map_Size-1) and Pmap(Loc).Cost >= 2
            then
               Expand_Prune(Vmap, Pmap, Loc, T_LAND, To, Explored);
            elsif (Loc = 0 or Loc = MAP_SIZE-1) and Pmap(Loc).Inc_Cost >= 2
            then
               Expand_Prune(Vmap, Pmap, Loc, T_WATER, To, Explored);
            else                        --  copy perimeter cell as is
               To.List(To.Len) := Loc;
               To.Len := To.Len + 1;
               Copied := Copied + 1;
            end if;
         end loop;
         if Copied = From.Len
         then
            exit;                       --  nothing expanded
         end if;
         Tmp := From;
         From := To;
         To := Tmp;
      end loop;

      if Print_Vmap = 'I'
      then
         Ui.Print_Zoom(Vmap);
      end if;

      -- one pass for medium probability predictions
      if From.Len + Explored = Map_Size
      then
         return;                        --  nothing left to guess
      end if;
      To.Len := 0;

      for I in 0 .. From.Len - 1
      loop
         Loc := From.List(I);
         if Pmap(Loc).Cost > Pmap(Loc).Inc_Cost
         then
            Expand_Prune(Vmap, Pmap, Loc, T_LAND, To, Explored);
         elsif Pmap(Loc).Cost < Pmap(Loc).Inc_Cost
         then
            Expand_Prune(Vmap, Pmap, Loc, T_WATER, To, Explored);
         else         --  copy perimeter cell as is
            To.List(To.Len) := Loc;
            To.Len := To.Len + 1;
            -- note we don't track `Copied' here, as it is not used in this loop
            -- (it will be zero'ed in next loop)
         end if;
      end loop;

      Tmp := From;
      From := To;
      To := Tmp;

      if Print_Vmap = 'I'
      then
         Ui.Print_Zoom(Vmap);
      end if;

      -- multiple low probability passes
      loop
      -- return if very little left to explore
      if From.Len + Explored >= MAP_SIZE - MAP_HEIGHT --  XXX XXX arbitrary, should probably be a tunable
      then
         if Print_Vmap = 'I'
         then
            Ui.Print_Zoom(Vmap);
         end if;
         return;
      end if;

      To.Len := 0;
      Copied := 0;

      for I in 0 .. From.Len - 1
      loop
         Loc := From.List(I);
         if Pmap(Loc).Cost >= 4 and Pmap(Loc).Inc_Cost < 4
         then
            Expand_Prune(Vmap, Pmap, Loc, T_LAND, To, Explored);
         elsif Pmap(Loc).Inc_Cost >= 4 and Pmap(Loc).Cost < 4
         then
            Expand_Prune(Vmap, Pmap, Loc, T_WATER, To, Explored);
         elsif (Loc < MAP_WIDTH or Loc >= MAP_SIZE - MAP_WIDTH) and Pmap(Loc).Cost > Pmap(Loc).Inc_Cost
         then
            Expand_Prune(Vmap, Pmap, Loc, T_LAND, To, Explored);
         elsif (Loc < MAP_WIDTH or Loc >= MAP_SIZE - MAP_WIDTH) and Pmap(Loc).Inc_Cost > Pmap(Loc).Cost
         then
            Expand_Prune(Vmap, Pmap, Loc, T_WATER, To, Explored);
         else --  copy perimeter cell as-is
            To.List(To.Len) := Loc;
            To.Len := To.Len + 1;
            Copied := Copied + 1;
         end if;
      end loop;
      if Copied = From.Len
      then
         exit;                          --  nothing expanded
      end if;
      Tmp := From;
      From := To;
      To := Tmp;
   end loop;

   if Print_Vmap = 'I'
   then
      Ui.Print_Zoom(Vmap);
   end if;
   end Vmap_Prune_Explore_Locs;


   -- Expand an unexplored cell.  We increment the land or water count
   -- of each neighbor.  Any neighbor that acquires a non-zero count
   -- is added to the 'to' perimiter list.  The count of explored
   -- territory is incremented.
   --
   -- Careful:  'loc' may be "off board" XXX XXX as above, is this necessary?

   procedure Expand_Prune
     (Vmap     : in out View_Map;
      Pmap     : in out Path_Map;
      Loc      : in     Location_T;
      Ttype    : in     Terrain_T;
      To       : in out Perimeter_T;
      Explored : in out Integer)
   is
      New_Loc : Location_T;
   begin
      Explored := Explored + 1;

      if Ttype = T_LAND
      then
         Vmap(Loc).Contents := '+';
      else
         Vmap(Loc).Contents := '.';
      end if;

      for D in Direction_T'Range
        loop
           begin
              New_Loc := Loc + Dir_Offset(D);
              if Vmap(New_Loc).Contents = ' '
              then
                 if Pmap(New_Loc).Cost = 0 and Pmap(New_Loc).Inc_Cost = 0
                 then
                    To.List(To.Len) := New_Loc;
                    To.Len := To.Len + 1;
                 end if;
                 if Ttype = T_LAND
                 then
                    Pmap(New_Loc).Cost := Pmap(New_Loc).Cost + 1;
                 else
                    Pmap(New_Loc).Inc_Cost := Pmap(New_Loc).Inc_Cost + 1;
                 end if;
              end if;
           exception
              when Constraint_Error => null; --  skips for off-board locations. XXX XXX
           end;
      end loop;
   end Expand_Prune;

end Empire.Comp_Move;
