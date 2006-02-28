
package Empire.Mapping is
   function Rmap_Shore (Loc : in Location_T) return Boolean;
   function Vmap_At_Sea (Vmap : in View_Map; Loc : in Location_T) return Boolean;
   procedure Vmap_Cont (Cont_Map : in out Continent_Map; Vmap : in View_Map; Loc : Location_T; Bad_Terrain : Terrain_Display_T);
   function Vmap_Cont_Scan (Cont_Map : in Continent_Map; Vmap : in View_Map) return Scan_Counts_T;

   procedure Vmap_Find_Aobj (New_Loc: out Location_T; Pmap : out Path_Map; Vmap : in View_Map; Loc : in Location_T; Move_Info : in Move_Info_T);
   procedure Vmap_Find_Dest (New_Loc: out Location_T; Pmap : out Path_Map; Vmap : in View_Map; Cur_Loc : Location_T; Dest_Loc : Location_T; Owner : Owner_T; Terrain : Terrain_T);
   procedure Vmap_Find_Dir (New_Loc: out Location_T; Pmap : out Path_Map; Vmap : in View_Map; Loc : in Location_T; Terrain : in Acceptable_Terrain_Array; Adj_Char : in Content_Value_Array);
   procedure Vmap_Find_Lobj (New_Loc: out Location_T; Pmap : out Path_Map; Vmap : in View_Map; Loc : in Location_T; Move_Info : in Move_Info_T);
   procedure Vmap_Find_Lwobj (New_Loc: out Location_T; Pmap : out Path_Map; Vmap : in View_Map; Loc : in Location_T; Move_Info : in Move_Info_T; Beat_Cost : Integer);
   procedure Vmap_Find_Wobj (New_Loc: out Location_T; Pmap : out Path_Map; Vmap : in View_Map; Loc : in Location_T; Move_Info : in Move_Info_T);
   procedure Vmap_Find_Wlobj (New_Loc: out Location_T; Pmap : out Path_Map; Vmap : in View_Map; Loc : in Location_T; Move_Info : in Move_Info_T);

   procedure Vmap_Mark_Adjacent (Pmap : in out Path_Map; Loc : in Location_T);
   procedure Vmap_Mark_Near_path (Pmap : in out Path_Map; Loc : in Location_T);
   procedure Vmap_Mark_Path (Pmap : in out Path_Map; Vmap : in View_Map; Loc : in Location_T);
   procedure Vmap_Mark_Up_Cont (Cont_Map : out Continent_Map; Vmap : in View_Map; Loc : in Location_T; Bad_Terrain : Terrain_Display_T);
   procedure Vmap_Prune_Explore_Locs (Vmap : in out View_Map);
end Empire.Mapping;
