package body Empire.Lists is

   procedure Link
     (Head : in out Piece_Info_P;
      Obj  : in     Piece_Info_P;
      List : in     Link_Type_T)
   is
   begin
      Obj.Links (List).Prev := null;
      Obj.Links (List).Next := Head;
      if Head /= null then
         Head.Links (List).Prev := Obj;
      end if;
      Head := Obj;
   end Link;

   procedure Unlink
     (Head : in out Piece_Info_P;
      Obj  : in     Piece_Info_P;
      List : in     Link_Type_T)
   is
   begin
      if Obj.Links (List).Next /= null then
         Obj.Links (List).Next.Links (List).Prev := Obj.Links (List).Prev;
      end if;

      if Obj.Links (List).Prev /= null then
         Obj.Links (List).Prev.Links (List).Next := Obj.Links (List).Next;
      else
         Head := Obj.Links (List).Next;
      end if;
      --  make it harder for errors to stay hidden
      Obj.Links (List).Next := null;
      Obj.Links (List).Prev := null;
   end Unlink;

end Empire.Lists;
