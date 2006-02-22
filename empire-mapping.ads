package Empire.Mapping is
   function Rmap_Shore (Loc : in Location_T) return Boolean;
   function Vmap_At_Sea (Vmap : in View_Map; Loc : in Location_T) return Boolean;
   procedure Vmap_Cont (Cont_Map : in out Continent_Map; Vmap : in View_Map; Loc : Location_T; Bad_Terrain : Terrain_T);
   function Vmap_Cont_Scan (Cont_Map : in Continent_Map; Vmap : in View_Map) return Scan_Counts_T;
   function Vmap_Find_Aobj (Pmap : in Path_Map; Vmap : in View_Map; Loc : in Location_T; Move_Info : in Move_Info_T) return Location_T;
   function Vmap_Find_Dest (Pmap : in Path_Map; Vmap : in View_Map; Cur_Loc : Location_T; Dest_Loc : Location_T; Owner : Owner_T; Terrain : Terrain_T) return Location_T;
   function Vmap_Find_Dir (Pmap : in Path_Map; Vmap : in View_Map; Loc : in Location_T; Terrain : in Acceptable_Content_Array; Adj_Char : in Content_Value_Array) return Location_T;
   function Vmap_Find_Lobj (Pmap : in Path_Map; Vmap : in View_Map; Loc : in Location_T; Move_Info : in Move_Info_T) return Location_T;
   function Vmap_Find_Lwobj (Pmap : in Path_Map; Vmap : in View_Map; Loc : in Location_T; Move_Info : in Move_Info_T) return Location_T;
   function Vmap_Find_Wobj (Pmap : in Path_Map; Vmap : in View_Map; Loc : in Location_T; Move_Info : in Move_Info_T) return Location_T;
   function Vmap_Find_Wlobj (Pmap : in Path_Map; Vmap : in View_Map; Loc : in Location_T; Move_Info : in Move_Info_T) return Location_T;
   procedure Vmap_Mark_Adjacent (Pmap : in out Path_Map; Loc : in Location_T);
   procedure Vmap_Mark_Near_path (Pmap : in out Path_Map; Loc : in Location_T);
   procedure Vmap_Mark_Path (Pmap : in out Path_Map; Vmap : in View_Map; Loc : in Location_T);
   procedure Vmap_Mark_Up_Cont (Cont_Map : out Continent_Map; Vmap : in View_Map; Loc : in Location_T; Bad_Terrain : Terrain_Display_T);
   procedure Vmap_Prune_Explore_Locs (Vmap : in out View_Map);
end Empire.Mapping;
