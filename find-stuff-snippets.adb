
-- find a user object, given cursor location

   function User_Obj_At_Loc (Loc : in Location_T; Types : in Acceptable_Piece_Array) return Piece_Info_P is
      Obj : Piece_Info_P;
   begin
      Obj := Find_Obj_At_Loc(Loc);

      if (Obj /= null) and (Obj.Owner = USER) and Types(Obj)
      then
         return Obj;
      else
         -- XXX for general usage, perhaps caller should do this!
         Ui.Huh;
      end if;
   end E_Set_Func;

  function User_City_At_Loc (Loc : in Location_T; return City_Info_P) is
     Cityp : City_Info_P
  begin
     Cityp := Find_City

