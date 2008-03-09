with Empire.Objects;
with Empire.Ui;

package body Empire.Mapping is

   -- This file contains routines for playing around with view_maps,
   -- real_maps, path_maps, and cont_maps.

   -- Map out a continent.  We are given a location on the continent.
   -- We mark each square that is part of the continent and unexplored
   -- territory adjacent to the continent.  By adjusting the value of
   -- 'bad_terrain', this routine can map either continents of land,
   -- or lakes.

   procedure Vmap_Cont
     (Cont_Map    : in out Continent_Map;
      Vmap        : in     View_Map;
      Loc         :        Location_T;
      Bad_Terrain :        Terrain_Display_T) is
   begin
      Cont_Map := (others => false);
      Vmap_Mark_Up_Cont(Cont_Map, Vmap, Loc, Bad_Terrain);
   end Vmap_Cont;

   -- Mark all squares of a continent and the squares that are adjacent
   -- to the continent which are on the board.  Our passed location is
   -- known to be either on the continent or adjacent to the continent.

   procedure Vmap_Mark_Up_Cont
     (Cont_Map    :    out Continent_Map;
      Vmap        : in     View_Map;
      Loc         : in     Location_T;
      Bad_Terrain :        Terrain_Display_T)
   is
      New_Loc : Location_T;
      From, To, Tmp : Perimeter_T;
      This_Terrain : Terrain_Display_T;
   begin
      From.Len := 1;
      From.List(0) := Loc;
      Cont_Map(Loc) := True;            --  loc is on continent, by definition

      while (From.Len > 0)
      loop
         To.Len := 0;                   --  nothing in new perimeter yet

         for I in 0 .. From.Len - 1     --  expand perimeter
         loop
            for D in Direction_T'Range
            loop
               New_Loc := From.List(I) + Dir_Offset(D);
               if Map(New_Loc).On_Board
               then
                  if not Cont_Map(New_Loc)
                  then
                     -- mark, but don't expand, unexplored territory
                     if Vmap(New_Loc).Contents = ' '
                     then
                        Cont_Map(New_Loc) := True;
                     else
                        case Vmap(New_Loc).Contents is
                           when '+' =>
                              This_Terrain := '+';
                           when '.' =>
                              This_Terrain := '.';
                           when others =>
                              This_Terrain := Map(New_Loc).Contents;
                        end case;

                        if This_Terrain /= Bad_Terrain
                        then            --  on continent
                           Cont_Map(New_Loc) := True;
                           To.List(To.Len) := New_Loc;
                           To.Len := To.Len + 1;
                        end if;
                     end if;
                  end if;
               end if;
            end loop;
         end loop;
         Tmp := From;
         From := To;
         To := Tmp;
      end loop;
   end Vmap_Mark_Up_Cont;

   -- Scan a continent recording items of interest on the continent.
   -- XXX This could be done as we mark up the continent.

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
     (Pmap      : in out Path_Map;      --  path map to update
      Vmap      : in     View_Map;
      Move_Info : in     Move_Info_T;   --  objectives and weights
      Perim     : in out Perimeter_T;   --  perimeter to expand
      Ttype     :        Terrain_T;     --  type of terrain to expand -- XXX XXX XXX should be acceptable_terrain_array!
      Cur_Cost  :        Integer;
      Inc_Wcost :        Integer;
      Inc_Lcost :        Integer;
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
         when others =>
            raise Program_Error;
      end case;
   end Terrain_Type;

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
         elsif (Loc < MAP_WIDTH or Loc >- MAP_SIZE - MAP_WIDTH) and Pmap(Loc).Inc_Cost > Pmap(Loc).Cost
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

--  static void
--  expand_prune (view_map_t *vmap, path_map_t *pmap, long loc, int type, perimeter_t *to, int *explored)
--  {
--      int i;
--      long new_loc;

--      *explored += 1;

--      if (type == T_LAND) vmap[loc].contents = '+';
--      else vmap[loc].contents = '.';

--      FOR_ADJ (loc, new_loc, i)
--      if (new_loc >= 0 && new_loc < MAP_SIZE && vmap[new_loc].contents == ' ') {
--              if (!pmap[new_loc].cost && !pmap[new_loc].inc_cost) {
--                      to->list[to->len] = new_loc;
--                      to->len += 1;
--              }
--              if (type == T_LAND)
--                      pmap[new_loc].cost += 1;
--              else pmap[new_loc].inc_cost += 1;
--      }
--  }

--  /*
--   * Find the shortest path from the current location to the
--   * destination which passes over valid terrain.  We return
--   * the destination if a path exists.  Otherwise we return the
--   * origin.
--   *
--   * This is similar to 'find_objective' except that we know our destination.
--   */

--  long
--  vmap_find_dest (path_map_t path_map[], view_map_t vmap[], long cur_loc, long dest_loc, int owner, int terrain)
--  {
--      perimeter_t *from;
--      perimeter_t *to;
--      int cur_cost;
--      int start_terrain;
--      move_info_t move_info;
--      char old_contents;

--      old_contents = vmap[dest_loc].contents;
--      vmap[dest_loc].contents = '%'; /* mark objective */
--      move_info.city_owner = owner;
--      move_info.objectives = "%";
--      move_info.weights[0] = 1;

--      from = &p1;
--      to = &p2;

--      if (terrain == T_AIR) start_terrain = T_LAND;
--      else start_terrain = terrain;

--      start_perimeter (path_map, from, cur_loc, start_terrain);
--      cur_cost = 0; /* cost to reach current perimeter */

--      for (;;) {
--              to->len = 0; /* nothing in perim yet */
--              expand_perimeter (path_map, vmap, &move_info, from,
--                                terrain, cur_cost, 1, 1, to, to);
--              cur_cost += 1;
--              if (to->len == 0 || best_cost <= cur_cost) {
--                      vmap[dest_loc].contents = old_contents;
--                      return best_loc;
--              }
--              SWAP (from, to);
--      }
--  }

--  /*
--   * Starting with the destination, we recursively back track toward the source
--   * marking all cells which are on a shortest path between the start and the
--   * destination.  To do this, we know the distance from the destination to
--   * the start.  The destination is on a path.  We then find the cells adjacent
--   * to the destination and nearest to the source and place them on the path.
--   *
--   * If we know square P is on the path, then S is on the path if S is
--   * adjacent to P, the cost to reach S is less than the cost to reach P,
--   * and the cost to move from S to P is the difference in cost between
--   * S and P.
--   *
--   * Someday, this routine should probably use perimeter lists as well.
--   */

--  void
--  vmap_mark_path (path_map_t *path_map, const view_map_t *vmap, long dest)
--  {
--      int n;
--      long new_dest;

--      if (path_map[dest].cost == 0) return; /* reached end of path */
--      if (path_map[dest].terrain == T_PATH) return; /* already marked */

--      path_map[dest].terrain = T_PATH; /* this square is on path */

--      /* loop to mark adjacent squares on shortest path */
--      FOR_ADJ (dest, new_dest, n)
--      if (path_map[new_dest].cost == path_map[dest].cost - path_map[dest].inc_cost)
--                      vmap_mark_path (path_map, vmap, new_dest);

--  }

--  /*
--   * Create a marked path map.  We mark those squares adjacent to the
--   * starting location which are on the board.  'find_dir' must be
--   * invoked to decide which squares are actually valid.
--   */

--  void
--  vmap_mark_adjacent (path_map_t path_map[], long loc)
--  {
--      int i;
--      long new_loc;

--      FOR_ADJ_ON (loc, new_loc, i)
--              path_map[new_loc].terrain = T_PATH;
--  }

--  /*
--   * Modify a marked path map.  We mark those squares adjacent to the
--   * starting location which are on the board and which are adjacent
--   * to a location on the existing shortest path.
--   */

--  void
--  vmap_mark_near_path (path_map_t path_map[], long loc)
--  {
--      int i, j;
--      long new_loc, xloc;
--      int hit_loc[8];

--      memset(hit_loc, 0, 8 * sizeof(int));

--      FOR_ADJ_ON (loc, new_loc, i) {
--              FOR_ADJ_ON (new_loc, xloc, j)
--              if (xloc != loc && path_map[xloc].terrain == T_PATH) {
--                      hit_loc[i] = 1;
--                      break;
--              }
--      }
--      for (i = 0; i < 8; i++)
--      if (hit_loc[i])
--      path_map[loc + dir_offset[i]].terrain = T_PATH;
--  }

--  /*
--   * Look at each neighbor of 'loc'.  Select the first marked cell which
--   * is on a short path to the desired destination, and which holds a valid
--   * terrain.  Note that while this terrain is matched against a 'vmap',
--   * it differs slightly from terrains used above.  This terrain is the
--   * terrain to which we can move immediately, and does not include terrain
--   * for which we would have to wait for another piece to move off of.
--   *
--   * We prefer diagonal moves, and we try to have as many squares
--   * as possible containing something in 'adj_char'.
--   *
--   * For tie-breaking, we prefer moving to cells that are adjacent to
--   * as many other squares on the path.  This should have a few benefits:
--   *
--   * 1)  Fighters are less likely to be blocked from reaching a city
--   * because they stay in the center of the path and increase the number
--   * of options for subsequent moves.
--   *
--   * 2)  Transports will approach a city so that as many armies
--   * as possible can hop off the tt on one turn to take a valid
--   * path toward the city.
--   *
--   * 3)  User pieces will move more intuitively by staying in the
--   * center of the best path.
--   */

--  static direction_t order[] = {NORTHWEST, NORTHEAST, SOUTHWEST, SOUTHEAST,
--                                      WEST, EAST, NORTH, SOUTH};

--  long
--  vmap_find_dir (path_map_t path_map[], const view_map_t *vmap, long loc, const char *terrain, const char *adj_char)
--  {
--      int i, count, bestcount;
--      long bestloc, new_loc;
--      int path_count, bestpath;
--      char *p;

--      if (trace_pmap)
--              print_pzoom ("Before vmap_find_dir:", path_map, vmap);

--      bestcount = -INFINITY; /* no best yet */
--      bestpath = -1;
--      bestloc = loc;

--      for (i = 0; i < 8; i++) { /* for each adjacent square */
--              new_loc = loc + dir_offset[order[i]];
--              if (path_map[new_loc].terrain == T_PATH) { /* which is on path */
--                      p = strchr (terrain, vmap[new_loc].contents);

--                      if (p != NULL) { /* desirable square? */
--                              count = vmap_count_adjacent (vmap, new_loc, adj_char);
--                              path_count = vmap_count_path (path_map, new_loc);

--                              /* remember best location */
--                              if ((count > bestcount) || ((count == bestcount) && (path_count > bestpath)))
--                              {
--                                      bestcount = count;
--                                      bestpath = path_count;
--                                      bestloc = new_loc;
--                              }
--                      }
--              }
--      }
--      return (bestloc);
--  }

--  /*
--   * Count the number of adjacent squares of interest.
--   * Squares are weighted so that the first in the list
--   * is the most interesting.
--   */

--  static int
--  vmap_count_adjacent (const view_map_t *vmap, long loc, const char *adj_char)
--  {
--      int i, count;
--      long new_loc;
--      char *p;
--      int len;

--      len = strlen (adj_char);

--      count = 0;

--      FOR_ADJ_ON (loc, new_loc, i) {
--              p = strchr (adj_char, vmap[new_loc].contents);
--              if (p) count += 8 * (len - (p - adj_char));
--      }
--      return (count);
--  }

--  /* Count the number of adjacent cells that are on the path. */

--  static int
--  vmap_count_path (path_map_t *pmap, long loc)
--  {
--      int i, count;
--      long new_loc;

--      count = 0;

--      FOR_ADJ_ON (loc, new_loc, i)
--      if (pmap[new_loc].terrain == T_PATH)
--              count += 1;

--      return (count);
--  }

--  /*
--   * See if a location is on the shore.  We return true if a surrounding
--   * cell contains water and is on the board.
--   */

--  int
--  rmap_shore (long loc)
--  {
--      long i, j;

--      FOR_ADJ_ON (loc, j, i)
--              if (map[j].contents == '.')
--                      return (TRUE);

--      return (FALSE);
--  }

--  /*
--   * Return true if a location is surrounded by ocean.  Off board locations
--   * which cannot be moved to are treated as ocean.
--   */

--  int
--  vmap_at_sea (const view_map_t *vmap, long loc)
--  {
--      long i, j;

--      FOR_ADJ_ON (loc, j, i)
--      if (vmap[j].contents == ' ' || vmap[j].contents == '+' || map[j].contents != '.')
--                      return (FALSE);

--      return (TRUE);
--  }

end Empire.Mapping;
