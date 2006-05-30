package Empire.Lists is
   procedure Link (Head : in out Piece_Info_P; Obj : in out Piece_Info_P; List : in Link_Type_T);
   procedure Unlink (Head : in out Piece_Info_P; Obj : in out Piece_Info_P; List : in Link_Type_T);
end Empire.Lists;
