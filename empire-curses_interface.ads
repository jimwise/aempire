with Terminal_Interface.Curses;

package Empire.Curses_Interface is
   type Movie_Screen is array (Location_T) of Content_Display_T;

   procedure Alert;
   procedure Error (S : in String);
   function Get_Chx return Character;
   function Get_Int (Message : in String; Low : in Integer; High : in Integer) return Integer;
   function Get_Yn (Message : in String) return Boolean;
   function Get_Piece_Name return Piece_Type_T;

   procedure Huh;
   procedure Info (S : in String);
   procedure Prompt (S : in String);
   procedure Redraw;
   procedure Init_Ui;
   procedure Clear;
   procedure End_Ui;

   procedure Init_Map;                  -- XXX merge with Init
   function Cur_Sector return Sector_T;
   procedure Display_Loc (Whose : in Piece_Owner_T; Loc : in Location_T);
   procedure Display_Locx (Whose : in Piece_Owner_T; Loc : in Location_T);
   procedure Display_Score;
   procedure Help (Text : in Help_Array);
   procedure Kill_Display;
   procedure Move_Cursor (Cursor : in out Location_T; Dir : in Direction_T);

   procedure Print_Movie_Screen (Mapbuf : in Movie_Screen);
   procedure Print_Pzoom (S : in String; Pmap : in Path_Map; Vmap : in View_Map);
   procedure Print_Sector (Whose : in Piece_Owner_T; Sector : in Sector_T);
   procedure Print_Zoom (Vmap : in View_Map);
   procedure Sector_Change;

private
   procedure Display_Screen (Which : in Piece_Owner_T);
   function On_Screen (Loc : in Location_T) return Boolean;
   procedure Print_Pzoom_Cell (Pmap    : in Path_Map;
                               Vmap    : in View_Map;
                               Row     : in Row_T;
                               Col     : in Column_T;
                               Row_Inc : in Integer;
                               Col_Inc : in Integer);
   procedure Print_Zoom_Cell (Vmap    : in View_Map;
                              Row     : in Row_T;
                              Col     : in Column_T;
                              Row_Inc : in Integer;
                              Col_Inc : in integer);
   procedure Show_Loc (Which : in Piece_Owner_T;
                       Loc   : in Location_T);
   procedure Print_Map_Frame (First_Row : in Row_T;
                              First_Col : in Column_T;
                              Row_Inc   : in Integer;
                              Col_Inc   : in Integer;
                              Label     : in String);

   Whose_Map : Owner_T := UNOWNED;      --  user or computer (none yet)
   Ref_Row : Row_T;                     --  current top row displayed
   Ref_Col : Column_T;                  --  current left-most column displayed
   Save_Sector : Sector_T;              --  current displayed sector
   Save_Cursor : Location_T;            --  ??? -- current displayed cursor position
   Change_Ok : Boolean := True;         --  may new sector be displayed?

   NUMTOPS : constant := 4;      -- number of lines at top of screen for messages
   NUMINFO : constant := NUMTOPS - 1;
   NUMSIDES : constant := 6;     -- number of lines at side of screen

   TOP_ROWS : constant := 4;
   BOTTOM_ROWS : constant := 1;
   SIDE_COLUMNS : constant := 6;

   Map_Win_Height : Terminal_Interface.Curses.Line_Count;
   Map_Win_Width : Terminal_Interface.Curses.Column_Count;

   Map_Win, Status_Win, Info_Win : Terminal_Interface.Curses.Window;

   Zoom_List : Content_Value_Array :=
     ('X' => 1, 'O' => 2, '*' => 3,
      't' => 4, 'c' => 5, 'b' => 6,
      's' => 7, 'd' => 8, 'p' => 9,
      'f' => 10, 'a' => 11,
      'T' => 12, 'C' => 13, 'B' => 14,
      'S' => 15, 'D' => 16, 'P' => 17,
      'F' => 18, 'A' => 19,
      'z' => 20, 'Z' => 21,
      '+' => 22, '.' => 23, ' ' => 24,
      others => 0);

  type Path_Display_T is
     ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
      'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
     '-', '!', '%', ' ', '&');
end Empire.Curses_Interface;
