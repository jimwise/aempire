package Empire.Mapping is
   function Rmap_Shore (Loc : in Location_T) return Boolean;
   function Vmap_At_Sea (Vmap : in View_Map; Loc : in Location_T) return Boolean;
   function Vmap_Cont_Scan (Cont_Map : in Continent_Map; Vmap : in View_Map) return Scan_Counts_T;

   procedure Vmap_Find_Aircraft_Obj
     (Objective :    out Location_T;
      Pmap      :    out Path_Map;
      Vmap      : in     View_Map;
      Loc       : in     Location_T;
      Move_Info : in     Move_Info_T);
   procedure Vmap_Find_Ground_Obj
     (Objective :    out Location_T;
      Pmap      :    out Path_Map;
      Vmap      : in     View_Map;
      Loc       : in     Location_T;
      Move_Info : in     Move_Info_T);
   procedure Vmap_Find_Ship_Obj
     (Objective :    out Location_T;
      Pmap      :    out Path_Map;
      Vmap      : in     View_Map;
      Loc       : in     Location_T;
      Move_Info : in     Move_Info_T);

   -- complex pathfinding for comp_move
   procedure Vmap_Find_Landsea_Obj
     (Objective :    out Location_T;
      Pmap      :    out Path_Map;
      Vmap      : in     View_Map;
      Loc       : in     Location_T;
      Move_Info : in     Move_Info_T;
      Beat_Cost :        Integer);
   procedure Vmap_Find_Sealand_Obj
     (Objective :    out Location_T;
      Pmap      :    out Path_Map;
      Vmap      : in     View_Map;
      Loc       : in     Location_T;
      Move_Info : in     Move_Info_T);

   procedure Vmap_Find_Dest
     (New_Loc  :    out Location_T;
      Pmap     :    out Path_Map;
      Vmap     : in out View_Map;       --  XXX XXX XXX see note in package body
      Cur_Loc  :        Location_T;
      Dest_Loc :        Location_T;
      Owner    :        Owner_T;
      Terrain  :        Terrain_T);
   procedure Vmap_Find_Dir
     (Found_Loc :    out Location_T;
      Pmap      : in     Path_Map;
      Vmap      : in     View_Map;
      Loc       : in     Location_T;
      Terrain   : in     Acceptable_Content_Array;
      Adj_Char  : in     Content_Value_Array);

   procedure Vmap_Mark_Adjacent
     (Pmap : in out Path_Map;
      Loc  : in     Location_T);
   procedure Vmap_Mark_Near_Path
     (Pmap : in out Path_Map;
      Loc  : in     Location_T);
   procedure Vmap_Mark_Path
     (Pmap : in out Path_Map;
      Vmap : in     View_Map;
      Dest : in     Location_T);
   procedure Vmap_Prune_Explore_Locs
     (Vmap : in out View_Map);

private

   procedure Add_Cell
     (Pmap     : in out Path_Map;
      New_Loc  : in     Location_T;
      Perim    : in out Perimeter_T;
      Terrain  : in     Terrain_T;
      Cur_Cost : in     Integer;
      Inc_Cost : in     Integer);

   procedure Expand_Perimeter
     (Pmap      : in out Path_Map;
      Vmap      : in     View_Map;
      Move_Info : in     Move_Info_T;
      Perim     : in     Perimeter_T;
      Ttype     :        Terrain_T;
      Cur_Cost  :        Integer;
      Inc_Wcost :        Integer;
      Inc_Lcost :        Integer;
      Waterp    : in out Perimeter_T;
      Landp     : in out Perimeter_T);

   procedure Expand_Prune
     (Vmap     : in out View_Map;
      Pmap     : in out Path_Map;
      Loc      : in     Location_T;
      Ttype    : in     Terrain_T;
      To       : in out Perimeter_T;
      Explored : in out Integer);

   function Objective_Cost
     (Vmap      : in View_Map;
      Move_Info : in Move_Info_T;
      Loc       : in Location_T;
      Base_Cost : in Integer)
     return Integer;

   procedure Start_Perimeter
     (Pmap    : in out Path_Map;
      Perim   : in out Perimeter_T;
      Loc     : in     Location_T;
      Terrain : in     Terrain_T);

   function Terrain_Type
     (Pmap      : in Path_Map;
      Vmap      : in View_Map;
      Move_Info : in Move_Info_T;
      From_Loc  : in Location_T;
      To_Loc    : in Location_T)
   return Terrain_T;

   function Vmap_Count_Adjacent
     (Vmap     : in View_Map;
      Loc      : in Location_T;
      Adj_Type : in Content_Value_Array)
     return Integer;

   function Vmap_Count_Path
     (Pmap : in Path_Map;
      Loc  : in Location_T)
     return Integer;

   procedure Vmap_Find_Xobj
     (Objective :    out Location_T;
      Pmap      : in out Path_Map;
      Vmap      : in     View_Map;
      Loc       : in     Location_T;
      Move_Info : in     Move_Info_T;
      Start     : in     Terrain_T;
      Expand    : in     Terrain_T);



   -- XXX XXX XXX these globals are used to cache current best objective
   -- XXX XXX XXX we need a way to pass this information around, as we
   -- XXX XXX XXX currently have a complex non-reentrant sequence of calls

   Best_Loc : Location_T;               --  cost and location of best objective
   Best_Cost : Integer;
end Empire.Mapping;
