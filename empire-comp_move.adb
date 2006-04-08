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
with Empire.Math;
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

      -- originally in Check_Endgame, but without good reason
      Date := Date + 1;

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
            P := Objects.Find_City_At_Loc(I, Owners => (COMP => TRUE, others => FALSE));
            if (P /= null) and (P.Prod = ARMY)
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
         if Obj.Ship.Func = COMP_LOADING
         then
            Load_Army(Obj);             -- original code paniced on fail, we except
            return;                     -- armies stay on a loading ship
         end if;

         Make_Unload_Map(Amap, Comp_Map);
         Mapping.Vmap_Find_Wlobj(New_Loc, Pmap, Amap, Obj.Loc, Tt_Unload);
         Move_Objective(Obj, Pmap, New_Loc, (' ' => 1, others => 0));
         return;
      end if;

      -- otherwise (not on a ship)
      Mapping.Vmap_Find_Lobj(New_Loc, Pmap, Comp_Map, Obj.Loc, Army_Fight);

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
         Cross_Cost := Pmap(New_Loc).Cost * 2 - Cross_Cost;
      else
         Cross_Cost := INFINITY;
      end if;

      -- see if there is something interesting to go to by water
      if Cross_Cost > 0                 -- possible to be false only due to normalization after case above
      then
         Make_Army_Load_Map(Obj, Amap, Comp_Map);
         Mapping.Vmap_Find_Lwobj(New_Loc2, Pmap2, Amap, Obj.Loc, Army_Load, Cross_Cost);
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

   procedure Make_Army_Load_Map (Obj : in out Piece_Info_T; Xmap : in out View_Map; Vmap : in View_Map) is
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

         P := P.Piece_Link.Next;
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

   function Nearby_Load (Obj : in Piece_Info_T; Loc : in Location_T) return Boolean is
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
         if Nearby_Load(Obj.all, Loc)
         then
            Count := Count + 1;
         end if;

         Obj := Obj.Piece_Link.Next;
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
      Owncont_Map : Continent_Map := (others => FALSE);
      Tcont_Map : Continent_Map;
      Counts : Scan_Counts_T;
      Total_Cities : Integer;
   begin

      Xmap := Vmap;
      Unmark_Explore_Locs(Xmap);

      for I in City'Range
      loop
         if City(I).Owner = COMP
         then
            Mapping.Vmap_Mark_Up_Cont(Owncont_Map, Xmap, City(I).Loc, '.');
         end if;
      end loop;

      for I in Vmap'Range
      loop
         if (Vmap(I).Contents = 'O') or (Vmap(I).Contents = '*')
         then
            Mapping.Vmap_Cont(Tcont_Map, Xmap, I, '.');
            Counts := Mapping.Vmap_Cont_Scan(Tcont_Map, Xmap);

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
         if (P.Piece_Type = TRANSPORT) and (Objects.Obj_Capacity(P.all) > P.Count)
         then
            if (Best = null) or (P.Count >= Best.Count)
            then
               Best := P;
            end if;
         end if;
      end loop;

   end Find_Best_Tt;

-- Load an army onto the most full non-full ship

   procedure Load_Army (Obj : in out Piece_Info_T) is
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
         Objects.Embark(P.all, Obj);
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
            P := Obj_List(Comp_Map(New_Loc).Contents);
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

   procedure Transport_Move (Obj : in out Piece_Info_T) is
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
         Make_Tt_Load_Map(Amap, Comp_Map);
         Mapping.Vmap_Find_Wlobj(New_Loc, Pmap, Amap, Obj.Loc, Tt_Load);

         if New_Loc = Obj.loc           -- nothing to load?
         then
            Amap := Comp_Map;
            Unmark_Explore_Locs(Amap);
            if Print_Vmap = 'S'
            then
               Ui.Print_Zoom(Amap);
            end if;
            Mapping.Vmap_Find_Wobj(New_Loc, Pmap, Amap, Obj.Loc, Tt_Explore);
         end if;

         Move_Objective(Obj, Pmap, New_Loc, ('a' => 2, ' ' => 1, others => 0));
      else
         Make_Unload_Map(Amap, Comp_Map);
         Mapping.Vmap_Find_Wlobj(New_Loc, Pmap, Amap, Obj.Loc, Tt_Unload);
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

   procedure Fighter_Move (Obj : in out Piece_Info_T) is
      New_Loc : Location_T;
      Pmap : Path_Map;
      City_Cost : Integer;
   begin
      New_Loc := Find_Attack(Obj.Loc, Fighter_Attack, ('.'|'+' => TRUE, others => FALSE));
      if New_Loc /= Obj.Loc             -- something to attack?
      then
         Attack.Attack(Obj, New_Loc);
         return;
      end if;

      -- return to base if low on fuel
      Objects.Find_Nearest_City(Obj.Loc, COMP, New_Loc, City_Cost);
      if Obj.Piece_range <= City_Cost + 2
      then
         if New_Loc /= Obj.Loc
         then
            Mapping.Vmap_Find_Dest(New_Loc, Pmap, Comp_Map, Obj.Loc, New_Loc, COMP, T_AIR);
         end if;
      else
           New_Loc := Obj.Loc;
      end if;

      if New_Loc = Obj.Loc              -- no nearby city?
      then
         Mapping.Vmap_Find_Aobj(New_Loc, Pmap, Comp_Map, Obj.Loc, Fighter_Fight);
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

   procedure Ship_Move (Obj : in out Piece_Info_T) is
      New_Loc : Location_T;
      Adj_List : Content_Value_Array;
      Amap : View_Map;                  -- file-global in original
      Pmap : Path_Map;                  -- file-global in original
   begin
      -- if we're damaged, head to port (or stay there)
      if Obj.Hits < Piece_Attr(Obj.Piece_Type).Max_Hits
      then
         -- if in port, stay there
         if Comp_Map(Obj.Loc).Contents = 'X'
         then
            Obj.Moved := Piece_Attr(Obj.Piece_Type).Speed;
            return;
         end if;
         -- otherwise, go there
         Mapping.Vmap_Find_Wobj(New_Loc, Pmap, Comp_Map, Obj.Loc, Ship_Repair);
         Adj_List := ('.' => 1, others => 0);
      else
         New_Loc := Find_Attack(Obj.Loc, Ship_Attack, ('.' => TRUE, others => FALSE));
         if New_Loc /= Obj.loc          -- something to attack?
         then
            Attack.Attack(Obj, New_Loc);
            return;
         end if;
         -- look for an objective
         Amap := Comp_Map;
         Unmark_Explore_Locs(Amap);
         if Print_Vmap = 'S'
         then
            Ui.Print_Zoom(Amap);
         end if;

         Mapping.Vmap_Find_Wobj(New_Loc, Pmap, Amap, Obj.Loc, Ship_Fight);
         Adj_List := Ship_Fight.Objective_Weights;
      end if;

      Move_Objective(Obj, Pmap, New_Loc, Adj_List);
   end Ship_Move;

-- Move to an objective

   procedure Move_Objective (Obj : in out Piece_Info_T; Pathmap : in out Path_Map; Loc : in Location_T; Adj_List : in Content_Value_Array) is
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
                    "; func=" & Function_Name(Obj.Func));
         end if;

         return;
      end if;

      Old_Loc := Obj.Loc;               -- remember where we are
      Old_Dest := New_Loc;              -- and where we're going

      D := Math.Dist(Old_Loc, Old_Dest);

      if (Comp_Map(New_Loc).Contents = ' ') and (D = 2) -- are we exploring?
      then
         Mapping.Vmap_Mark_Adjacent(Pmap, Obj.Loc);
         Reuse := FALSE;
      else
         Mapping.Vmap_Mark_Path(Pmap, Comp_Map, New_Loc); -- find routes to destination
      end if;


      -- path terrain and move terrain may differ
      case Piece_Attr(Obj.Piece_Type).Class is
         when GROUND =>
            Terrain := ('+' => TRUE, others => FALSE);
         when AIRCRAFT =>
            Terrain := ('+'|'.'|'*' => TRUE, others => FALSE);
         when SHIP =>
            Terrain := ('.'|'*' => TRUE, others => FALSE);
         when SPACECRAFT =>
            -- can't happen -- we don't do controlled movement for SAT
            raise Program_Error;
      end case;

      Mapping.Vmap_Find_Dir(New_Loc, Pmap, Comp_Map, Obj.Loc, Terrain, Adj_List);

      if New_Loc = Obj.Loc and          -- path is blocked?
        (Piece_Attr(Obj.Piece_Type).Class /= GROUND) and (Obj.Ship /= null) -- don't unblock armies on a ship
      then
         Mapping.Vmap_Mark_Near_Path(Pmap, Obj.Loc);
         Reuse := FALSE;
         Mapping.Vmap_Find_Dir(New_Loc, Pmap, Comp_Map, Obj.Loc, Terrain, Adj_List);
      end if;

      -- encourage army to leave city
      if (New_Loc = Obj.Loc) and (Map(Obj.Loc).Cityp /= null) and (Piece_Attr(Obj.Piece_Type).Class = GROUND)
      then
         New_Loc := Move_Away(Comp_Map, Obj.Loc, ('+' => TRUE, others => FALSE));
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
                       " toward objective at " & Location_T'Image(New_Loc) & "; func=" & Function_Name(Obj.Func));
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
         case Obj.Piece_Type is

            when FIGHTER =>
               -- watch fuel
               if (Comp_Map(Old_Dest).Contents /= 'X') and
                 (Obj.Piece_Range <= Piece_Attr(FIGHTER).Piece_Range / 2)
               then
                  return;               -- don't risk chaining - start over so fuel is recalculated
               end if;
               Attack_List := Fighter_Attack;
               Terrain := ('+'|'.' => TRUE, others => FALSE);

            when ARMY =>
               Attack_List := Army_Attack;
               -- IMPORTANT (XXX):  in the original code, these had 'X' instead of '*', which
               -- could _never_ match (since Find_Attack uses Map, not Comp_Map) (!).  This means
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
         P := P.Piece_Link.Next;
      end loop;

      P := Comp_Obj(ARMY);
      while P /= null
      loop
         Ncomp_Army := Ncomp_Army + 1;
         P := P.Piece_Link.Next;
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

end Empire.Comp_Move;
