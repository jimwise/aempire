package Empire.Comp_Move is

   procedure Comp_Move;

private

   Emap : View_Map;                     -- pruned explore map

-- Define ratios for numbers of cities we want producing each object.
--
-- Early in the game, we want to produce armies, patrols, and transports for
-- rapid expansion.  After a while, we add fighters and other ships
-- for exploration.  Later, we add ships of all kinds for control of
-- the sea.

   Ratio1 : aliased Piece_Value_Array := (ARMY => 60, TRANSPORT => 20, PATROL => 10, others => 0);

   Ratio2 : aliased Piece_Value_Array := (ARMY => 90, TRANSPORT => 40,
                              FIGHTER|PATROL|DESTROYER|SUBMARINE => 10, others => 0);

   Ratio3 : aliased Piece_Value_Array := (ARMY => 120, TRANSPORT => 60, FIGHTER|PATROL => 20,
                              DESTROYER|SUBMARINE|CARRIER|BATTLESHIP => 10, others => 0);

   Ratio4 : aliased Piece_Value_Array := (ARMY => 150, TRANSPORT => 70, FIGHTER|PATROL => 30,
                              DESTROYER|SUBMARINE => 20, CARRIER|BATTLESHIP => 10);

   Ratio : Piece_Value_P := Ratio1'Access;


   procedure Do_Cities;
   procedure Do_Pieces;
   procedure Check_Endgame;

   procedure Comp_Prod (Cityp : in out City_Info_T; Is_Lake : in Boolean);
   -- XXX city_count needs to be in/out?  Or is reset before each call?
   procedure Comp_Set_Needed (Cityp : in out City_Info_T; City_Count : in out Piece_Value_Array; Army_Ok : in Boolean; Is_Lake : in Boolean);
   procedure Comp_Set_Prod (Cityp : in out City_Info_T; Ptype : in Piece_Type_T);
   function Need_More (City_Count : in Piece_Value_Array; Prod1 : in Piece_Type_T; Prod2 : in Piece_Type_T) return Piece_Type_T;
   function Overproduced (Cityp : in City_Info_T; City_Count : in Piece_Value_Array) return Boolean;

   procedure Cpiece_Move (Obj : in out Piece_Info_T);
   procedure Army_Move (Obj : in out Piece_Info_T);
   procedure Fighter_Move (Obj : in out Piece_Info_T);
   procedure Ship_Move (Obj : in out Piece_Info_T);
   procedure Transport_Move (Obj : in out Piece_Info_T);

   procedure Move1 (Obj : in out Piece_Info_T);
   procedure Move_Away (Vmap : in View_Map; Loc : in Location_T; Terrain : Acceptable_Terrain_Array);
   procedure Move_Objective (Obj : Piece_Info_T; Pathmap : in out Path_Map; New_Loc : in Location_T; Adj_List : in Content_Value_Array);

   -- XXX `success' result only checked in one place (Board_Ship).  should go.
   procedure Load_Army (Obj : in out Piece_Info_T; Success : out Boolean);
   procedure Board_Ship (Obj : in out Piece_Info_T; Pmap : in out Path_Map; Loc : Location_T);
   procedure Find_Best_Tt (Best : in out Piece_Info_P; Loc : in Location_T);

   function Find_Attack (Loc : in Location_T; Obj_List : in Content_Value_Array; Terrain : Acceptable_Terrain_Array) return Location_T;
   function Lake (Loc : in Location_T) return Boolean;
   function Nearby_Count (Loc : in Location_T) return Integer;
   function Nearby_Load (Obj : in Piece_Info_T; Loc : in Location_T) return Boolean;

   procedure Make_Army_Load_Map (Obj : in out Piece_Info_T; xmap : in out View_Map; Vmap : in View_Map);
   procedure Make_Tt_Load_Map (Xmap : in out View_Map; Vmap : in View_Map);
   procedure Make_Unload_Map (Xmap : in out View_Map; Vmap : in View_Map);
   procedure Unmark_Explore_Locs (Xmap : in out View_Map);

end Empire.Comp_Move;
