with Ada.Containers.Vectors;

package Empire.Comp_Move is

   procedure Comp_Move;

private

   package Location_Vectors is new Ada.Containers.Vectors
     (Element_Type => Location_T,
      Index_Type => Natural);
   use Location_Vectors;

   Emap : View_Map;                     -- pruned explore map

-- Define ratios for numbers of cities we want producing each object.
--
-- Early in the game, we want to produce armies, patrols, and transports for
-- rapid expansion.  After a while, we add fighters and other ships
-- for exploration.  Later, we add ships of all kinds for control of
-- the sea.

   Ratios : array (Integer range <>) of Piece_Value_Array :=
     (1 => (ARMY => 60, TRANSPORT => 20, PATROL => 10, others => 0),
      2 => (ARMY => 90, TRANSPORT => 40, FIGHTER|PATROL|DESTROYER|SUBMARINE => 10, others => 0),
      3 => (ARMY => 120, TRANSPORT => 60, FIGHTER|PATROL => 20, DESTROYER|SUBMARINE|CARRIER|BATTLESHIP => 10, others => 0),
      4 => (ARMY => 150, TRANSPORT => 70, FIGHTER|PATROL => 30, DESTROYER|SUBMARINE => 20, CARRIER|BATTLESHIP => 10, others => 0));
   subtype Ratio_Range is Integer range Ratios'First .. Ratios'Last;

   Ratio : Ratio_Range;

   -- how close an army must be to a transport to be considered 'near' it
   LOAD_RADIUS : constant Integer := 2;
   -- weight we assign to a user city on a continent where he has one and we have none (and unowned cities are one or zero)
   SINGLE_USER_CITY_WEIGHT : constant Integer := 2;

   procedure Do_Cities;
   procedure Do_Pieces;

   procedure Comp_Prod (Cityp : in out City_Info_T; Is_Land_Locked : in Boolean);
   procedure Comp_Set_Needed (Cityp : in out City_Info_T; City_Count : in out Piece_Value_Array; Army_Ok : in Boolean; Is_Land_Locked : in Boolean);
   procedure Comp_Set_Prod (Cityp : in out City_Info_T; Ptype : in Piece_Type_T);
   function Need_More (City_Count : in Piece_Value_Array; Prod1 : in Piece_Type_T; Prod2 : in Piece_Type_T) return Piece_Type_T;
   function Overproduced (Cityp : in City_Info_T; City_Count : in Piece_Value_Array) return Boolean;

   procedure Cpiece_Move (Obj : in out Piece_Info_P);
   procedure Army_Move (Obj : in out Piece_Info_P);
   procedure Fighter_Move (Obj : in out Piece_Info_P);
   procedure Ship_Move (Obj : in out Piece_Info_P);
   procedure Transport_Move (Obj : in out Piece_Info_P);

   procedure Move1 (Obj : in out Piece_Info_P);
   function Move_Away (Vmap : in View_Map; Loc : in Location_T; Terrain : in Acceptable_Content_Array) return Location_T;
   procedure Move_Objective (Obj : in out Piece_Info_P; Pathmap : in out Path_Map; Loc : in Location_T; Adj_List : in Content_Value_Array);
   Could_Not_Load : exception;
   procedure Load_Army (Obj : in Piece_Info_P);
   procedure Find_Best_Tt (Best : in out Piece_Info_P; Loc : in Location_T);

   function Find_Attack (Loc : in Location_T; Obj_List : in Content_Value_Array; Terrain : Acceptable_Terrain_Array) return Location_T;
   function Land_Locked (Loc : in Location_T) return Boolean;
   function Nearby_Count (Loc : in Location_T) return Integer;
   function Nearby_Load (Obj : in Piece_Info_P; Loc : in Location_T) return Boolean;

   procedure Make_Army_Load_Map (Obj : in Piece_Info_P; xmap : in out View_Map; Vmap : in View_Map);
   procedure Make_Tt_Load_Map (Xmap : in out View_Map; Vmap : in View_Map);
   procedure Make_Unload_Map (Xmap : in out View_Map; Vmap : in View_Map);
   procedure Unmark_Explore_Locs (Xmap : in out View_Map);

   -- utility func to turn a numeric weight into a Content_Display_T for Comp_Map
   function Comp_Map_Weight (Weight : in Natural) return Content_Display_T;

   function Vmap_Water (Vmap : in View_Map; Loc : in Location_T) return Continent_Map;
   function Vmap_Land (Vmap : in View_Map; Loc : in Location_T) return Continent_Map;
   function Vmap_Flood_Fill (Vmap : in View_Map; Loc : in Location_T; Good_Terrain : in Acceptable_Terrain_Array) return Continent_Map;

end Empire.Comp_Move;
