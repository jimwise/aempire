with Empire.Objects;
with Empire.Ui;

package body Empire.Mapping is

   -- This file contains routines for playing around with view_maps,
   -- real_maps, path_maps, and cont_maps.

   -- XXX XXX XXX the old version of this kept four statically allocated
   -- XXX XXX XXX perimeter_t's, and passed around pointers to these
   -- XXX XXX XXX then, at numerous points, it `swapped' these by
   -- XXX XXX XXX updating the pointers.
   --
   -- XXX XXX XXX we do structure copies, which is much more expensive
   -- XXX XXX XXX for now, but will fix this when we move to less static
   -- XXX XXX XXX structures, in the short term.
   --
   -- XXX XXX XXX (As an idea of scale, each perimeter_t contains one integer
   -- XXX XXX XXX per location_t value.

      -- Find the nearest objective for a piece.  This routine actually does
      -- some real work.  This code represents my fourth rewrite of the
      -- algorithm.  This algorithm is central to the strategy used by the
      -- computer.
      --
      -- Given a view_map, we create a path_map.  On the path_map, we record
      -- the distance from a location to the nearest objective.  We are
      -- given information about what the interesting objectives are, and
      -- how interesting each objective is.
      --
      -- We use a breadth first search to find the nearest objective.
      -- We maintain something called a "perimeter list".  This list
      -- initially contains a list of squares that we can reach in 'n' moves.
      -- On each pass through our loop, we add all squares that are adjacent
      -- to the perimeter list and which lie outside the perimeter to our
      -- list.  (The loop is only slightly more complicated for armies and
      -- transports.)
      --
      -- When our perimeter list becomes empty, or when the distance to
      -- the current perimeter is at least as large as the weighted distance
      -- to the best objective, we return the location of the best objective
      -- found.
      --
      -- The 'cost' field in a path_map must be INFINITY if the cell lies
      -- outside of the current perimeter.  The cost for cells that lie
      -- on or within the current perimeter doesn't matter, except that
      -- the information must be consistent with the needs of 'vmap_mark_path'.

      -- Find an objective over a single type of terrain

      procedure Vmap_Find_Xobj
        (Objective :    out Location_T;
         Pmap      : in out Path_Map;
         Vmap      : in     View_Map;
         Loc       : in     Location_T;
         Move_Info : in     Move_Info_T;
         Start     : in     Terrain_T;
         Expand    : in     Terrain_T)
      is
         From, To, Tmp : Perimeter_T;
         Cur_Cost : Integer := 0;      --  const to reach current perimeter
      begin
         Start_Perimeter(Pmap, From, Loc, Start);

         loop
            To.Len := 0;                --  nothing in perim yet
            Expand_Perimeter(Pmap, Vmap, Move_Info, From, Expand,
                             Cur_Cost, 1, 1, To, To);

            if Trace_Pmap
            then
               Ui.Print_Pzoom("After xobj loop:", Pmap, Vmap);
            end if;

              Cur_Cost := Cur_Cost + 1;
              if To.Len = 0 or Best_Cost <= Cur_Cost
              then
                 Objective := Best_Loc;
                 return;
              end if;

              Tmp := From;
              From := To;
              To := Tmp;
         end loop;
      end Vmap_Find_Xobj;

      -- Find an objective for a piece that crosses land and water
      procedure Vmap_Find_Aircraft_Obj
        (Objective :    out Location_T;
         Pmap      :    out Path_Map;
         Vmap      : in     View_Map;
         Loc       : in     Location_T;
         Move_Info : in     Move_Info_T)
      is
      begin
         Vmap_Find_Xobj(Objective, Pmap, Vmap, Loc, Move_Info, T_LAND, T_AIR);
      end Vmap_Find_Aircraft_Obj;

      -- Find an objective for a piece that crosses only water
      procedure Vmap_Find_Ship_Obj
        (Objective :    out Location_T;
         Pmap      :    out Path_Map;
         Vmap      : in     View_Map;
         Loc       : in     Location_T;
         Move_Info : in     Move_Info_T)
      is
      begin
         Vmap_Find_Xobj(Objective, Pmap, Vmap, Loc, Move_Info, T_WATER, T_WATER);
      end Vmap_Find_Ship_obj;

      -- Find an objective for a piece that crosses only land
      procedure Vmap_Find_Ground_Obj
        (Objective :    out Location_T;
         Pmap      :    out Path_Map;
         Vmap      : in     View_Map;
         Loc       : in     Location_T;
         Move_Info : in     Move_Info_T)
      is
      begin
         Vmap_Find_Xobj(Objective, Pmap, Vmap, Loc, Move_Info, T_LAND, T_LAND);
      end Vmap_Find_Ground_Obj;


               -- Find an objective moving from land to water.
      --
      -- With vmap_find_wlobj, this is a complex path-finding routine used only by Empire.Comp_Move
      --
      -- This is mildly complicated.  It costs 2 to move on land
      -- and one to move on water.  To handle this, we expand our current
      -- perimeter by one cell, where land can be expanded to either
      -- land or water, and water is only expanded to water.  Then
      -- we expand any water one more cell.
      --
      -- We have different objectives depending on whether the objective
      -- is being approached from the land or the water.

      procedure Vmap_Find_Landsea_Obj
        (Objective :    out Location_T;
         Pmap      :    out Path_Map;
         Vmap      : in     View_Map;
         Loc       : in     Location_T;
         Move_Info : in     Move_Info_T;
         Beat_Cost :        Integer)
      is
         Cur_Land : Perimeter_T;
         Cur_Water : Perimeter_T;
         New_Land : Perimeter_T;
         New_Water : Perimeter_T;
         Tmp : Perimeter_T;
         Cur_Cost : Integer := 0;       --  cost to reach current perimeter
      begin
         Start_Perimeter(Pmap, Cur_Land, Loc, T_LAND);
         Cur_Water.Len := 0;
         Best_Cost := Beat_Cost;        --  we can do this well

         loop
            -- expand current perimeter one cell
            New_Water.Len := 0;
            New_Land.Len := 0;
            Expand_Perimeter(Pmap, Vmap, Move_Info, Cur_Water, T_WATER,
                             Cur_Cost, 1, 1, New_Water, Tmp); --  tmp ignored
            Expand_Perimeter(Pmap, Vmap, Move_Info, Cur_Land, T_AIR,
                             Cur_Cost, 1, 2, New_Water, New_Land);

            -- expand new water one cell
            Cur_Water.Len := 0;
            Expand_Perimeter(Pmap, Vmap, Move_Info, New_Water, T_WATER,
                             Cur_Cost+1, 1, 1, Cur_Water, Tmp); --  tmp ignored

            if Trace_Pmap
            then
               Ui.Print_Pzoom("After landsea_obj loop:", Pmap, Vmap);
            end if;

            Cur_Cost := Cur_Cost + 2;

            if (Cur_Water.Len = 0 and New_Land.Len = 0) or Best_Cost <= Cur_Cost
            then
              Objective := Best_Loc;
              return;
            end if;

            Tmp := Cur_Land;
            Cur_Land := New_Land;
            New_Land := Tmp;
         end loop;
      end Vmap_Find_Landsea_Obj;

      -- Find an objective moving from water to land.
      --
      -- With vmap_find_wlobj, this is a complex path-finding routine used only by Empire.Comp_Move
      --
      -- Here, we expand water to either land or water.
      -- We expand land only to land.
      --
      -- We cheat ever so slightly, but this cheating accurately reflects
      -- the mechanics of moving.  The first time we expand water we can
      -- expand to land or water (army moving off tt or tt moving on water),
      -- but the second time, we only expand water (tt taking its second move).

      procedure Vmap_Find_Sealand_Obj
        (Objective :    out Location_T;
         Pmap      :    out Path_Map;
         Vmap      : in     View_Map;
         Loc       : in     Location_T;
         Move_Info : in     Move_Info_T)
      is
         Cur_Land : Perimeter_T;
         Cur_Water : Perimeter_T;
         New_Land : Perimeter_T;
         New_Water : Perimeter_T;
         Tmp : Perimeter_T;
         Cur_Cost : Integer := 0;  --  cost to reach current perimeter
      begin
         Start_Perimeter(Pmap, Cur_Water, Loc, T_WATER);
         Cur_Land.Len := 0;

         loop
            -- expand current perimeter one cell
            New_Water.Len := 0;
            New_Land.Len := 0;
            Expand_Perimeter(Pmap, Vmap, Move_Info, Cur_Water, T_AIR,
                             Cur_Cost, 1, 2, New_Water, New_Land);
            Expand_Perimeter(Pmap, Vmap, Move_Info, Cur_Land, T_LAND,
                             Cur_Cost, 1, 2, Tmp, New_Land);

            -- expand new water one cell to water
            Cur_Water.Len := 0;
            Expand_Perimeter(Pmap, Vmap, Move_Info, New_Water, T_WATER,
                             Cur_Cost + 1, 1, 1, Cur_Water, Tmp);

            if Trace_Pmap
            then
               Ui.Print_Pzoom("After sealand obj loop:", Pmap, Vmap);
            end if;

            Cur_Cost := Cur_Cost + 2;

            if (Cur_Water.Len = 0 and New_Land.Len = 0) or Best_Cost <= Cur_Cost
            then
               Objective := Best_Loc;
               return;
            end if;

               Tmp := Cur_Land;
               Cur_Land := New_Land;
               New_Land := Tmp;
         end loop;
      end Vmap_Find_Sealand_Obj;

      -- Initialize perimeter searching
      --
      -- XXX C routine had this optimization:
      --   This routine was taking a significant amount of the program time (10%)
      --   doing the initialization of the path map.  We now use an external
      --   constant and 'memcpy'.
      -- XXX we skip this and let the compiler/optimizer fill in for now.

   procedure Start_Perimeter
     (Pmap    : in out Path_Map;
      Perim   : in out Perimeter_T;
      Loc     : in     Location_T;
      Terrain : in     Terrain_T)
   is
   begin
      -- XXX XXX XXX this isn't right -- it worked in the original, where we had a low value of `INFINITY', with the
      -- XXX XXX XXX result that we could sum INFINITY without overflowing int soon.
      Pmap := (others => (Cost => INFINITY, Inc_Cost => INFINITY, Terrain => T_UNKNOWN));

      -- put first location in perimeter
      Pmap(Loc).Cost := 0;
      Pmap(Loc).Inc_Cost := 0;
      Pmap(Loc).Terrain := Terrain;

      Perim.Len := 1;
      Perim.List(0) := Loc;

      --  XXX XXX XXX yes, these are package-global.  they should be come a 'search_t' record
      --  XXX XXX XXX which will be passed around the same way the pmap is...
      Best_Cost := INFINITY;            --  no best yet
      Best_Loc := Loc;                  --  if nothing found, we return current loc
   end Start_Perimeter;

   -- Expand the perimeter.
   --
   -- Note that 'waterp' and 'landp' may be the same.
   --
   -- For each cell of the current perimeter, we examine each
   -- cell adjacent to that cell which lies outside of the current
   -- perimeter.  If the adjacent cell is an objective, we update
   -- best_cost and best_loc.  If the adjacent cell is of the correct
   -- type, we turn place the adjacent cell in either the new water perimeter
   -- or the new land perimeter.
   --
   -- We set the cost to reach the current perimeter.
   --
   -- pmap == path map to update
   -- move_info == objectives and weights
   -- curp == perimeter to expand
   --  type == type of terrain to expand
   -- cur_cost == cost to reach cells on perimeter
   -- inc_wcost == cost to enter new water cells
   -- inc_lcost == cost to enter new land cells
   -- waterp == pointer to new water perimeter
   -- landp == pointer to new land perimeter

   procedure Expand_Perimeter
     (Pmap      : in out Path_Map;
      Vmap      : in     View_Map;
      Move_Info : in     Move_Info_T;
      Perim     : in     Perimeter_T;
      Ttype     : in     Terrain_T;
      Cur_Cost  : in     Integer;
      Inc_Wcost : in     Integer;
      Inc_Lcost : in     Integer;
      Waterp    : in out Perimeter_T;
      Landp     : in out Perimeter_T)
   is
      New_Loc : Location_T;
      Obj_Cost : Integer;
      New_Type : Terrain_T;
   begin
      for I in 0 .. Perim.Len - 1
      loop
         for D in Direction_T'Range
         loop
            New_Loc := Perim.List(I) + Dir_Offset(D);
            if Map(New_Loc).On_Board
            then
               if Pmap(New_Loc).Cost = INFINITY
               then
                  New_Type := Terrain_Type(Pmap, Vmap, Move_Info, Perim.List(I), New_Loc);
                  if New_Type = T_LAND and (Ttype = T_LAND or Ttype = T_AIR)
                  then
                     Add_Cell(Pmap, New_Loc, Landp, New_Type, Cur_Cost, Inc_Lcost);
                  elsif New_Type = T_WATER and (Ttype = T_WATER or Ttype = T_AIR)
                  then
                     Add_Cell(Pmap, New_Loc, Waterp, New_Type, Cur_Cost, Inc_Wcost);
                  elsif New_Type = T_UNKNOWN
                  then                  --  unreachable cell
                     Pmap(New_Loc).Terrain := New_Type;
                     Pmap(New_Loc).Cost := Cur_Cost + (INFINITY/2);
                     Pmap(New_Loc).Inc_Cost := INFINITY/2;
                  end if;

                  if Pmap(New_Loc).Cost /= INFINITY
                  then                  --  we expanded
                     Obj_Cost := Objective_Cost(Vmap, Move_Info, New_Loc, Cur_Cost);
                     if Obj_Cost < Best_Cost
                     then
                        Best_Cost := Obj_Cost;
                        Best_Loc := New_Loc;
                        if New_Type = T_UNKNOWN
                        then
                           Pmap(New_Loc).Cost := Cur_Cost + 2;
                           Pmap(New_Loc).Inc_Cost := 2;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end loop;
      end loop;

   end Expand_Perimeter;

   -- Add a cell to a perimeter list
   -- XXX XXX this is partially unrolled in at least one place.  consider unrolling throughout
   procedure Add_Cell
     (Pmap     : in out Path_Map;
      New_Loc  : in     Location_T;
      Perim    : in out Perimeter_T;
      Terrain  : in     Terrain_T;
      Cur_Cost : in     Integer;
      Inc_Cost : in     Integer)
   is
   begin
      Pmap(New_Loc).Terrain := Terrain;
      Pmap(New_Loc).Inc_Cost := Inc_Cost;
      Pmap(New_Loc).Cost := Cur_Cost + Inc_Cost;

      Perim.List(Perim.Len) := New_Loc;
      Perim.Len := Perim.Len + 1;
   end Add_Cell;

   -- Compute the cost to move to an objective

   function Objective_Cost
     (Vmap      : in View_Map;
      Move_Info : in Move_Info_T;
      Loc       : in Location_T;
      Base_Cost : in Integer)
     return Integer
   is
      W : Integer;
      Cityp : City_Info_P;
   begin
      W := Move_Info.Objective_Weights(Vmap(Loc).Contents);

      if W = 0
      then
         return INFINITY;
      end if;

      if W /= W_TT_BUILD
      then
         return W + Base_Cost;
      else
         -- handle special case of moving to tt building city
         Cityp := Objects.Find_City_At_Loc(Loc);
         if Cityp = null
         then
            return Base_Cost +2;        --  tt is already here
         end if;
         if Cityp.Prod /= TRANSPORT
         then
            return Base_Cost +2;        --  just finished a tt
         end if;

         -- compute time to wait for tt to be built
         W := Piece_Attr(TRANSPORT).Build_Time - Cityp.Work;
         W := W * 2;                  -- Had To Cross Land To Get here
         if W < Base_Cost + 2
         then
            W := Base_Cost + 2;
         end if;

         return W;
      end if;
   end Objective_Cost;

   -- Return the type of terrain at a vmap location

   function Terrain_Type
     (Pmap      : in Path_Map;
      Vmap      : in View_Map;
      Move_Info : in Move_Info_T;
      From_Loc  : in Location_T;
      To_Loc    : in Location_T)
     return Terrain_T
   is
   begin
      case Vmap(To_Loc).Contents is
         when '+' => return T_LAND;
         when '.' => return T_WATER;
         when '%' => return T_UNKNOWN;  --  magic objective
         when ' ' => return Pmap(From_Loc).Terrain;
         when others => null;
      end case;

      case Map(To_Loc).Contents is
         when '.' => return T_WATER;
         when '+' => return T_LAND;
         when '*' =>
            if Map(To_Loc).Cityp.Owner = Move_Info.Owner
            then
               return T_WATER;       --  well, a ship can enter it, and an army can't...
            else
               return T_UNKNOWN;     --  cannot cross
            end if;
      end case;
   end Terrain_Type;

   -- Find the shortest path from the current location to the
   -- destination which passes over valid terrain.  We return
   -- the destination if a path exists.  Otherwise we return the
   -- origin.
   --
   -- This is similar to 'find_objective' except that we know our destination.

   procedure Vmap_Find_Dest
     (New_Loc  :    out Location_T;
      Pmap     :    out Path_Map;
      OVmap    : in     View_Map;
      Cur_Loc  : in     Location_T;
      Dest_Loc : in     Location_T;
      Owner    : in     Owner_T;
      Terrain  : in     Terrain_T)
   is
      From, To, Tmp : Perimeter_T;
      Cur_Cost : Integer := 0;     --  cost to reach current perimeter

      Vmap : View_Map := OVmap;         --  XXX XXX XXX this is expensive, but ensures we don't
                                        --  XXX XXX XXX accidentally modify passed vmap
      Start_Terrain : Terrain_T;
      Move_Info : Move_Info_T;
      Old_Contents : Content_Display_T;
   begin
      Old_Contents := Vmap(Dest_Loc).Contents;
      Vmap(Dest_Loc).Contents := '%';   --  `magic' objective marker
      Move_Info.Owner := Owner;
      Move_Info.Objective_weights := ('%' => 1, others => 0);

      if Terrain = T_AIR
      then
         Start_Terrain := T_LAND;
      else
         Start_Terrain := Terrain;
      end if;

      Start_Perimeter(Pmap, From, Cur_Loc, Start_Terrain);

      loop
         To.Len := 0;                   --  nothing in perim yet
         Expand_Perimeter(Pmap, Vmap, Move_Info, From, Terrain, Cur_Cost, 1, 1, To, To);
         Cur_Cost := Cur_Cost + 1;
         if To.Len = 0 or Best_Cost <= Cur_Cost
         then
            Vmap(Dest_Loc).Contents := Old_Contents;
            New_Loc := Best_Loc;
            return;
         end if;
         Tmp := From;
         From := To;
         To := Tmp;
      end loop;
   end Vmap_Find_Dest;

   -- Starting with the destination, we recursively back track toward the source
   -- marking all cells which are on a shortest path between the start and the
   -- destination.  To do this, we know the distance from the destination to
   -- the start.  The destination is on a path.  We then find the cells adjacent
   -- to the destination and nearest to the source and place them on the path.
   --
   -- If we know square P is on the path, then S is on the path if S is
   -- adjacent to P, the cost to reach S is less than the cost to reach P,
   -- and the cost to move from S to P is the difference in cost between
   -- S and P.
   --
   -- Someday, this routine should probably use perimeter lists as well.

   procedure Vmap_Mark_Path
     (Pmap : in out Path_Map;
      Vmap : in     View_Map;
      Dest : in     Location_T)
   is
      New_Dest : Location_T;
   begin
      if Pmap(Dest).Cost = 0
      then
         return;                        --  reached end of path (recursive exit condition)
      end if;

      if Pmap(Dest).Terrain = T_PATH
      then
         return;                        --  already marked (other recursive exit condition)
      end if;

      Pmap(Dest).Terrain := T_PATH;     --  this square is on path

      -- loop to mark adjacent squares on shortest path
      for D in Direction_T'Range
      loop
         New_Dest := Dest + Dir_Offset(D);
         if Map(New_Dest).On_Board and Pmap(New_Dest).Cost = (Pmap(Dest).Cost - Pmap(Dest).Inc_Cost)
         then
            Vmap_Mark_Path(Pmap, Vmap, New_Dest);
         end if;
      end loop;
   end Vmap_Mark_Path;

   -- Create a marked path map.  We mark those squares adjacent to the
   -- starting location which are on the board.  'find_dir' must be
   -- invoked to decide which squares are actually valid.

   procedure Vmap_Mark_Adjacent
     (Pmap : in out Path_Map;
      Loc  : in     Location_T)
   is
      New_Loc : Location_T;
   begin
      for D in Direction_T'Range
      loop
         New_Loc := Loc + Dir_Offset(D);
         if Map(New_Loc).On_Board
         then
            Pmap(New_Loc).Terrain := T_PATH;
         end if;
      end loop;
   end Vmap_Mark_Adjacent;

   -- Modify a marked path map.  We mark those squares adjacent to the
   -- starting location which are on the board and which are adjacent
   -- to a location on the existing shortest path.

   procedure Vmap_Mark_Near_Path
     (Pmap : in out Path_Map;
      Loc  : in     Location_T)
   is
      New_Loc, Xloc : Location_T;
      Hit_Loc : array (Direction_T) of Boolean := (others => True);
   begin
      for D in Direction_T'Range
      loop
         New_Loc := Loc + Dir_Offset(D);
         if Map(New_Loc).On_Board
         then
            for E in Direction_T'Range
            loop
               Xloc := New_Loc + Dir_Offset(E);
               if Map(New_Loc).On_Board
               then
                  if Xloc /= Loc and Pmap(Xloc).Terrain = T_PATH
                  then
                     Hit_Loc(D) := True;
                     exit;
                  end if;
               end if;
            end loop;
         end if;
      end loop;
      for I in Direction_T'Range
      loop
         if Hit_Loc(I)
         then
            Pmap(Loc + Dir_Offset(I)).Terrain := T_PATH;
         end if;
      end loop;
   end Vmap_Mark_Near_Path;

   -- Look at each neighbor of 'loc'.  Select the first marked cell which
   -- is on a short path to the desired destination, and which holds a valid
   -- terrain.  Note that while this terrain is matched against a 'vmap',
   -- it differs slightly from terrains used above.  This terrain is the
   -- terrain to which we can move immediately, and does not include terrain
   -- for which we would have to wait for another piece to move off of.
   --
   -- We prefer diagonal moves, and we try to have as many squares
   -- as possible containing something in 'adj_char'.
   --
   -- For tie-breaking, we prefer moving to cells that are adjacent to
   -- as many other squares on the path.  This should have a few benefits:
   --
   -- 1)  Fighters are less likely to be blocked from reaching a city
   -- because they stay in the center of the path and increase the number
   -- of options for subsequent moves.
   --
   -- 2)  Transports will approach a city so that as many armies
   -- as possible can hop off the tt on one turn to take a valid
   -- path toward the city.
   --
   -- 3)  User pieces will move more intuitively by staying in the
   -- center of the best path.

   procedure Vmap_Find_Dir
     (Found_Loc :    out Location_T;
      Pmap      : in     Path_Map;
      Vmap      : in     View_Map;
      Loc       : in     Location_T;
      Terrain   : in     Acceptable_Content_Array;
      Adj_Char  : in     Content_Value_Array)
   is
      Count, Best_Count : Integer;
      Best_Loc, New_Loc : Location_T;
      Path_Count, Best_Path : Integer;

      Order : constant array (1 .. 8) of Direction_T := (NORTHWEST, NORTHEAST,
                                                         SOUTHWEST, SOUTHEAST,
                                                         WEST, EAST, NORTH, SOUTH);

   begin
      if Trace_Pmap
      then
         Ui.Print_Pzoom("Before Vmap_Find_Dir:", Pmap, Vmap);
      end if;

      -- no best yet
      Best_Count := -2;                 --  was -INFINITY (where INFINITY was 100,000), but this should do
      Best_Path := -1;
      Best_Loc := Loc;

      for I in Order'Range
      loop
         New_Loc := Loc + Dir_Offset(Order(I)); --  shuffle order to chosen order
         if Pmap(New_Loc).Terrain = T_PATH
         then
            if Terrain(Vmap(New_Loc).Contents) --  desirable square?
            then
               Count := Vmap_Count_Adjacent(Vmap, New_Loc, Adj_Char);
               Path_Count := Vmap_Count_Path(Pmap, New_Loc);

               --  remember best location
               if Count > Best_Count or (Count = Best_Count and Path_Count > Best_Path)
               then
                  Best_Count := Count;
                  Best_Path := Path_Count;
                  Best_Loc := New_Loc;
               end if;
            end if;
         end if;
      end loop;

      Found_Loc := Best_Loc;
      return;
   end Vmap_Find_Dir;

   -- Count the number of adjacent squares of interest.
   -- Squares are weighted based on value in the passed
   -- Content_Value_Array (ordering was used in C version)

   function Vmap_Count_Adjacent
     (Vmap     : in View_Map;
      Loc      : in Location_T;
      Adj_Type : in Content_Value_Array)
     return Integer
   is
      Count : Integer := 0;
      New_Loc : Location_T;
   begin
      for D in Direction_T'Range
      loop
         New_Loc := Loc + Dir_Offset(D);
         if Map(New_Loc).On_Board
         then
            -- was an if in c version, but here just works (non-included types will have value of '0')
            Count := Count + 8 * Adj_Type(Vmap(New_Loc).Contents);
         end if;
      end loop;

      return Count;
   end Vmap_Count_Adjacent;

   -- Count the number of adjacent cells that are on the path

   function Vmap_Count_Path
     (Pmap : in Path_Map;
      Loc  : in Location_T)
     return Integer
   is
      Count : Integer := 0;
      New_Loc : Location_T;
   begin

      for D in Direction_T'Range
      loop
         New_Loc := Loc + Dir_Offset(D);
         if Map(New_Loc).On_Board
         then
            if Pmap(New_Loc).Terrain = T_PATH
            then
               Count := Count + 1;
            end if;
         end if;
      end loop;

      return Count;
   end Vmap_Count_Path;

   -- See if a location is on the shore.  We return true if a surrounding
   -- cell contains water and is on the board.
   -- XXX why 'R'?

   function Rmap_Shore (Loc : in Location_T) return Boolean
   is
      New_Loc : Location_T;
   begin
      for D in Direction_T'Range
      loop
         New_Loc := Loc + Dir_Offset(D);
         if Map(New_Loc).On_Board
         then
            if Map(New_Loc).Contents = '.'
            then
               return True;
            end if;
         end if;
      end loop;

      return False;
   end Rmap_Shore;

   -- Return true if a location is surrounded by ocean.  Off board locations
   -- are treated as ocean.

   function Vmap_At_Sea (Vmap : in View_Map; Loc : in Location_T) return Boolean
   is
      New_Loc : Location_T;
   begin
      for D in Direction_T'Range
      loop
         New_Loc := Loc + Dir_Offset(D);
         if Map(New_Loc).On_Board
         then
            if Vmap(New_Loc).Contents = ' ' or Vmap(New_Loc).Contents = '+' or Map(New_Loc).Contents /= '.'
            then
               return False;
            end if;
         end if;
      end loop;

      return True;
   end Vmap_At_Sea;

end Empire.Mapping;
