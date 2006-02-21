package Empire.Objects is
   procedure Describe_Obj (Obj : in Piece_Info_T);
   procedure Disembark (Obj : in out Piece_Info_T);
   procedure Embark (Ship : in out Piece_Info_T; Obj : in out Piece_Info_T);
   function Find_City (Loc : in Location_T) return City_Info_P;
   procedure Find_Nearest_City (Loc : in Location_T; Owner : in Owner_T; City_Loc : out Location_T; estimated_Cost : out Integer);
   function Find_Nfull (Ptype : in Piece_Type_T; Loc : in Location_T) return Piece_Info_P;
   function Find_Obj (Ptype : in Piece_Type_T; Loc : in Location_T) return Piece_Info_P;
   function Find_Obj_At_Loc (Loc : in Location_T) return Piece_Info_P;
   function Find_Transport (Owner : in Owner_T; Loc : in Location_T) return Location_T;
   -- this _really_ belongs elsewhere, since it queries the user
   function Get_Piece_Name return Piece_Type_T;
   function Good_Loc (Obj : in Piece_Info_T; Loc: in Location_T) return Boolean;
   procedure Kill_City (City : in out City_Info_T);
   procedure Kill_Obj (Obj : in out Piece_Info_T; Loc : Location_T);
   procedure Move_Obj (Obj : in out Piece_Info_T; New_Loc : Location_T);
   procedure Move_Sat (Obj : in out Piece_Info_T);
   function Obj_Capacity (Obj : in Piece_Info_T) return Integer; -- XXX limit?
   function Obj_Moves (Obj : in Piece_Info_T) return Integer; -- XXX limit?
   procedure Produce (City : in out City_Info_T);
   procedure Scan (Vmap : in out View_Map; Loc : in Location_T);
   procedure Set_Prod (City : in out City_Info_T);
end Empire.Objects;
