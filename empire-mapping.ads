package Empire.Mapping is
   function Rmap_Shore (Loc : in Location_T) return Boolean;
   function Vmap_At_Sea (Vmap : in View_Map; Loc : in Location_T) return Boolean;

   --  ---------------------------------------------------------------------------
   --  Path_Map -- a map with per-cell and incremental costs

   type Path_Map is tagged private;

   function Cost
     (Pmap : in Path_Map;
      Loc  : in Location_T) return Integer;

   function Terrain
     (Pmap : in Path_Map;
      Loc  : in Location_T) return Terrain_T;

   procedure Mark_Adjacent
     (Pmap : in out Path_Map;
      Loc  : in     Location_T);
   procedure Mark_Near_Path
     (Pmap : in out Path_Map;
      Loc  : in     Location_T);
   procedure Mark_Path
     (Pmap : in out Path_Map;
      Vmap : in     View_Map;
      Dest : in     Location_T);

   function Find_Dir
     (Pmap     : in Path_Map;
      Vmap     : in View_Map;
      Loc      : in Location_T;
      Terrain  : in Acceptable_Content_Array;
      Adj_Char : in Content_Value_Array) return Location_T;

   -- clear a loc and its surrounds from a path_map
   -- used in comp_move to keep from oscillating between the first two
   -- locs of a path map
   procedure Clear
     (Pmap : in out Path_Map;
      Loc : in Location_T);

  -- ---------------------------------------------------------------------------

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

   procedure Vmap_Find_Dest
     (New_Loc  :    out Location_T;
      Pmap     :    out Path_Map;
      OVmap    : in     View_Map;
      Cur_Loc  : in     Location_T;
      Dest_Loc : in     Location_T;
      Owner    : in      Owner_T;
      Terrain  : in     Terrain_T);

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

private
   type Path_Map_T is
      record
         Cost : Integer;                -- total cost to get here
         Inc_Cost : Integer;            -- incremental cost to get here
         Terrain : Terrain_T;
      end record;
   type Path_Map_Array is array (Location_T) of Path_Map_T;

   type Path_Map is tagged
      record
         Map : Path_Map_Array;
      end record;


   procedure Add_Cell
     (Pmap     : in out Path_Map;
      New_Loc  : in     Location_T;
      Perim    : in out Location_Vector;
      Terrain  : in     Terrain_T;
      Cur_Cost : in     Integer;
      Inc_Cost : in     Integer);

   procedure Expand_Perimeter
     (Pmap      : in out Path_Map;
      Vmap      : in     View_Map;
      Move_Info : in     Move_Info_T;
      Perim     : in     Location_Vector;
      Ttype     : in     Terrain_T;
      Cur_Cost  : in     Integer;
      Inc_Wcost : in     Integer;
      Inc_Lcost : in     Integer;
      Waterp    : in out Location_Vector;
      Landp     : in out Location_Vector);

   function Objective_Cost
     (Vmap      : in View_Map;
      Move_Info : in Move_Info_T;
      Loc       : in Location_T;
      Base_Cost : in Integer)
     return Integer;

   procedure Start_Perimeter
     (Pmap    : in out Path_Map;
      Perim   : in out Location_Vector;
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
