package Empire.Utility is
   procedure Check;
   procedure Panic (Why : in String);

private

--   procedure Check_Cargo (List : in Piece_Info_P; Ctype : in Piece_Type_T);
--   procedure Check_Obj_Cargo (List : in Piece_Info_P);
--   procedure Check_Obj (List : in Piece_Info_P; Owner : in Piece_Owner_T);

   In_Free  : array (Object'Range) of Boolean;
   In_Obj   : array (Object'Range) of Boolean;
   In_Loc   : array (Object'Range) of Boolean;
   In_Cargo : array (Object'Range) of Boolean;
end Empire.Utility;
