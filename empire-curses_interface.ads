package Empire.Curses_Interface is
   No_Current_Sector : exception;

   procedure Alert;
   procedure Error (err : in String);
   function Get_Chx return Character;
   function Get_Int (Message : in String; Low : in Integer; High : in Integer) return Integer;
   function Get_Str return String;
   function Get_Yn (Message : in String) return Boolean;
   procedure Huh;
   procedure Info (Info : in String);
   procedure Prompt (Info : in String);
   procedure Redraw;
   procedure Init_Ui;
   procedure Clear;
   procedure End_Ui;


   procedure Init_Map;                  -- XXX merge with Init
   function Cur_Sector return Sector_T;
   procedure Display_Loc (Whose : in Owner_T; Vmap : in View_Map; Loc : in Location_T);
   procedure Display_Locx (Whose : in Owner_T; Vmap : in View_Map; Loc : in Location_T);
   procedure Display_Loc_U (Loc : in Location_T);
   procedure Display_Score;
   procedure Help (Text : in Help_Array);
   procedure Kill_Display;
   procedure Move_Cursor (Cursor : in out Location_T; Offset : in Integer; Same_Screen : out Boolean);
   type Movie_Screen is array (Location_T) of Content_Display_T;
   procedure Print_Movie_Screen (Mapbuf : in Movie_Screen);
   procedure Print_Pzoom (Prompt : in String; Pmap : in Path_Map; Vmap : in View_Map);
   procedure Print_Sector (Whose : in Owner_T; Vmap : in View_Map; Sec : in Sector_T);
   procedure Print_Sector_U (Sec : in Sector_T);
   procedure Print_Sector_C (Sec : in Sector_T);
   procedure Print_Zoom (Vmap : in View_Map);
   procedure Sector_Change;

end Empire.Curses_Interface;
