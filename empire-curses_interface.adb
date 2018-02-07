with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Terminal_Interface.Curses.Text_IO;
with Empire.Locations;

package body Empire.Curses_Interface is
   use type Terminal_Interface.Curses.Line_Count;
   use type Terminal_Interface.Curses.Column_Count;

--  XXX XXX note that very little of this is actually curses based. this should
--  XXX XXX really be split into a driver-independent and a curses part.

   package Curses renames Terminal_Interface.Curses;
   package Text_Io renames Terminal_Interface.Curses.Text_IO;

   --  This file contains routines for displaying sectors and moving the cursor
   --  about in a sector. We need to remember the following information:
   --
   --  the current map portion displayed on the screen;
   --
   --  whether the displayed portion is from the user's or the computer's point
   --  of view;

   procedure Init_Map is
   begin
      Map_Win_Height := Curses.Lines - TOP_ROWS - BOTTOM_ROWS;
      Map_Win_Width  := Curses.Columns - SIDE_COLUMNS;

      Map_Win :=
        Curses.New_Window (Map_Win_Height, Map_Win_Width, TOP_ROWS, 0);
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Init_Map;

   --  This routine is called when the current display has been trashed and no
   --  sector is shown on the screen.

   procedure Kill_Display is
   begin
      Whose_Map := UNOWNED;
   end Kill_Display;

   --  Return the currently displayed user sector, if any. If a user sector is
   --  not displayed, return -1.

   function Cur_Sector return Sector_T is
   begin
      --  XXX XXX XXX in original, we returned -1 if no current sector. I've
      --  XXX XXX XXX played with an exception in that case, but that's messy
      --  XXX XXX XXX may be worth making this a procedure with an out boolean
      --  XXX XXX XXX but only two callers currently use this function, and
      --  neither XXX XXX XXX takes much overhead from re-displaying sector 0
      --  in the (rare) XXX XXX XXX case that we are coming from showing a comp
      --  view or just starting

      if Whose_Map /= USER then
         return Sector_T'First;
      end if;

      return Save_Sector;
   end Cur_Sector;

   --  Display a location on the screen. We figure out the sector the location
   --  is in and display that sector. The cursor is left at the requested
   --  location.
   --
   --  We redisplay the sector only if we either have been requested to
   --  redisplay the sector, or if the location is not on the screen.

   procedure Display_Loc (Whose : in Piece_Owner_T; Loc : in Location_T) is
   begin
      if Whose /= Whose_Map or not On_Screen (Loc) then
         Print_Sector (Whose, Locations.Loc_Sector (Loc));
      end if;

      Show_Loc (Whose, Loc);
   end Display_Loc;

   --  Display a location iff the location is on the screen
   procedure Display_Locx (Whose : in Piece_Owner_T; Loc : in Location_T) is
   begin
      if Whose = Whose_Map and On_Screen (Loc) then
         Show_Loc (Whose, Loc);
      end if;
   end Display_Locx;

   --  Display a location which exists on the screen

   procedure Show_Loc (Which : in Piece_Owner_T; Loc : in Location_T) is
      R : Row_T;
      C : Column_T;
   begin
      R := Locations.Loc_Row (Loc) - Ref_Row + 1;
      C := Locations.Loc_Col (Loc) - Ref_Col + 1;
      Curses.Add
        (Win    => Map_Win,
         Line   => Curses.Line_Position (R),
         Column => Curses.Column_Position (C),
         Ch     => Ctab (View (Which) (Loc).Contents));

      Save_Cursor := Loc;               --  remember cursor location;
      --  move cursor over location we just showed
      Curses.Move_Cursor
        (Map_Win,
         Curses.Line_Position (R),
         Curses.Column_Position (C));
      Curses.Refresh (Map_Win);
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Show_Loc;

--  Print a sector of the user's on the screen. If it is already displayed, we
--  do nothing. Otherwise we redraw the screen. Someday, some intelligence in
--  doing this might be interesting. We heavily depend on curses to update the
--  screen in a reasonable fashion.
   --
   --  If the desired sector is not displayed, we clear the screen. We then
   --  update the screen to reflect the current map. We heavily depend on
   --  curses to correctly optimize the redrawing of the screen.
   --
   --  When redrawing the screen, we figure out where the center of the sector
   --  is in relation to the map. We then compute the screen coordinates where
   --  we want to display the center of the sector. We will remember the sector
   --  displayed, the map displayed, and the map location that appears in the
   --  upper-left corner of the screen.

   procedure Print_Sector (Whose : in Piece_Owner_T; Sector : in Sector_T) is
      First_Row, Last_Row : Row_T;
      First_Col, Last_Col : Column_T;
      Display_Rows        : Integer;
      Display_Cols        : Integer;
   begin
      Save_Sector := Sector;            --  remember last sector displayed

      Display_Rows := Integer (Map_Win_Height) - 2;
      Display_Cols := Integer (Map_Win_Width) - 2;

      --  XXX XXX although most of this should `just work' on very large and
      --  very small XXX XXX screens, we kind of assume that we can show a
      --  whole sector at a time, and that XXX XXX we can't show the whole
      --  map at a time

      --  compute row and column edges of sector
      First_Row := Locations.Sector_Row (Sector) * ROWS_PER_SECTOR;
      First_Col := Locations.Sector_Col (Sector) * COLS_PER_SECTOR;
      Last_Row  := First_Row + ROWS_PER_SECTOR - 1;
      Last_Col  := First_Col + COLS_PER_SECTOR - 1;

      if Whose /= Whose_Map or
        not On_Screen (Locations.Row_Col_Loc (First_Row, First_Col)) or
        not On_Screen (Locations.Row_Col_Loc (Last_Row, Last_Col))
      then
         Curses.Clear (Map_Win);
      end if;

      --  figure out first row and col to print; subtract half the extra lines
      --  from the first line
      declare
         Extra_Rows, Extra_Cols : Natural;
      begin
         Extra_Rows := Display_Rows - ROWS_PER_SECTOR;
         Extra_Cols := Display_Cols - COLS_PER_SECTOR;

         if First_Row < Extra_Rows / 2 then
            Ref_Row := 1;
         else
            Ref_Row := First_Row - (Extra_Rows) / 2;
         end if;

         if First_Col < Extra_Cols / 2 then
            Ref_Col := 1;
         else
            Ref_Col := First_Col - (Extra_Cols) / 2;
         end if;
      end;

      --  XXX original had some logic to try not to waste space at the
      --  bottom/right of the screen XXX but it didn't work _quite_ right,
      --  and is less useful as we now have more, smaller sectors XXX world...

      if Ref_Row < 1 then
         Ref_Row := 1;
      end if;
      --  XXX check if these should be - 2 instead of removed if Ref_Row >
      --  MAP_HEIGHT - 1 then
      --     Ref_Row := MAP_HEIGHT - 1;
      --  end if;
      if Ref_Col < 1 then
         Ref_Col := 1;
      end if;
      --  if Ref_Col > MAP_WIDTH - 1 then
      --    Ref_Col := MAP_WIDTH - 1;
      --  end if;

      Whose_Map :=
        Whose;               --  remember which map we are displaying
      Ref_Row := 1;
      Ref_Col := 1;
      Display_Screen (Whose);

      Display_Turn;
      Display_Score;
      Curses.Refresh;
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Print_Sector;

   --  Display the portion of the map that appears on the screen

   procedure Display_Screen (Which : in Piece_Owner_T) is
      Display_Rows : constant Integer := Integer (Map_Win_Height) - 2;
      Display_Cols : constant Integer := Integer (Map_Win_Width) - 2;
      R            : Row_T;
      C            : Column_T;
      T            : Location_T;
   begin
      R := Ref_Row;
      while R < Ref_Row + Display_Rows - 1 and R < Row_T'Last loop
         C := Ref_Col;
         while C < Ref_Col + Display_Cols - 1 and C < Column_T'Last loop
            T := Locations.Row_Col_Loc (R, C);
            Curses.Add
              (Win    => Map_Win,
               Line   => Curses.Line_Position (R - Ref_Row + 1),
               Column => Curses.Column_Position (C - Ref_Col + 1),
               Ch     => Ctab (View (Which) (T).Contents));
            C := C + 1;
         end loop;
         R := R + 1;
      end loop;
      Curses.Box (Map_Win);
      Curses.Refresh (Map_Win);
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Display_Screen;

   --  Move the cursor in a specified direction. We return TRUE if the cursor
   --  remains in the currently displayed screen, otherwise FALSE. We display
   --  the cursor on the screen, if possible.

   --  XXX XXX yes, there is a curses func with the same name. but this
   --  is exported (ui driver independent) XXX XXX still, should probably
   --  change...

   procedure Move_Cursor (Cursor : in out Location_T; Dir : in Direction_T) is
      T : Location_T;
      R : Row_T;
      C : Column_T;
   begin
      T := Cursor + Dir_Offset (Dir); --  proposed location

--  XXX XXX should we allow scrolling? or does this, and I'm missing something?
      if not On_Screen (T) then
         Alert;
         return;
      end if;

      Cursor      := T;  --  update cursor position as requested
      Save_Cursor := Cursor;
      R           := Locations.Loc_Row (Save_Cursor);
      C           := Locations.Loc_Col (Save_Cursor);

      --  according to original code:
      --  Under ncurses (only) we need to redraw here, or we get weird errors,
      --  but only when moving the cursor to the right...
      --  let's try without, and see what happens... Curses.Redraw;

      Curses.Move_Cursor
        (Map_Win,
         Line   => Curses.Line_Position (R - Ref_Row + 1),
         Column => Curses.Column_Position (C - Ref_Col + 1));
      Curses.Refresh (Map_Win);
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Move_Cursor;

   --  See if a location is displayed on the screen

   function On_Screen (Loc : in Location_T) return Boolean is
      New_R : Row_T;
      New_C : Column_T;
   begin
      New_R := Locations.Loc_Row (Loc);
      New_C := Locations.Loc_Col (Loc);

      if
        (New_R < Ref_Row
         or else New_R > Ref_Row + Integer (Map_Win_Height) - 3) or
        (New_C < Ref_Col or else New_C > Ref_Col + Integer (Map_Win_Width) - 3)
      then
         return False;
      end if;

      return True;
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end On_Screen;

   --  Print a condensed version of the map

   procedure Print_Zoom (Vmap : in View_Map) is
      R, C, Row_Inc, Col_Inc : Integer;
   begin
      Row_Inc :=
        (MAP_HEIGHT + Integer (Map_Win_Height) - 2) /
        (Integer (Map_Win_Height) - 2);
      Col_Inc :=
        (MAP_WIDTH + Integer (Map_Win_Width) - 2) /
        (Integer (Map_Win_Width) - 2);

      Curses.Clear (Map_Win);

      R := 0;
      while R < MAP_HEIGHT loop
         C := 0;
         while C < MAP_WIDTH loop
            Print_Zoom_Cell (Vmap, R, C, Row_Inc, Col_Inc);
            C := C + Col_Inc;
         end loop;
         R := R + Row_Inc;
      end loop;

      Curses.Box (Map_Win);
      Curses.Refresh (Map_Win);

      Prompt ("Zoomed Map Round " & Integer'Image (Date));
      Print_Map_Frame (0, 0, Row_Inc, Col_Inc);
      Display_Score;
      Display_Turn;

      Curses.Refresh;

      Prompt ("Press any key to continue...");
      Wait_Chx;
      Prompt ("");

      if Whose_Map = USER then
         Print_Sector (USER, Save_Sector);
      end if;
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Print_Zoom;

   --  Print a single cell in condensed format

   procedure Print_Zoom_Cell
     (Vmap    : in View_Map;
      Row     : in Row_T;
      Col     : in Column_T;
      Row_Inc : in Integer;
      Col_Inc : in Integer)
   is
      R    : Row_T;
      C    : Column_T;
      T    : Location_T;
      Cell : Content_Display_T;
   begin
      Cell := ' ';
      R    := Row;
      while R < Row + Row_Inc and R < MAP_HEIGHT - 1
         --  while within area being compr
          loop
         C := Col;
         while C < Col + Col_Inc and C < MAP_WIDTH - 1 loop
            T := Locations.Row_Col_Loc (R, C);
            if Zoom_List (Vmap (T).Contents) < Zoom_List (Cell) then
               Cell := Vmap (T).Contents;
            end if;
            C := C + 1;
         end loop;
         R := R + 1;
      end loop;

      Curses.Add
        (Win    => Map_Win,
         Line   => Curses.Line_Position (Row / Row_Inc + 1),
         Column => Curses.Column_Position (Col / Col_Inc + 1),
         Ch     => Ctab (Cell));

   end Print_Zoom_Cell;

   --  Print a condensed version of a pathmap (for debugging)
   procedure Print_Pzoom
     (S    : in String;
      Pmap : in Mapping.Path_Map;
      Vmap : in View_Map)
   is
      R, C : Natural;        -- must support larger values than Row_T, Col_T
      Row_Inc, Col_Inc : Natural;
   begin
      Row_Inc :=
        (MAP_HEIGHT + Integer (Map_Win_Height) - 2) /
        (Integer (Map_Win_Height) - 2);
      Col_Inc :=
        (MAP_WIDTH + Integer (Map_Win_Width) - 2) /
        (Integer (Map_Win_Width) - 2);

      Curses.Clear (Map_Win);

      R := 0;
      while R < MAP_HEIGHT loop
         C := 0;
         while C < MAP_WIDTH loop
            Print_Pzoom_Cell
              (Pmap,
               Vmap,
               Row_T (R),
               Column_T (C),
               Row_Inc,
               Col_Inc);
            C := C + Col_Inc;
         end loop;
         R := R + Row_Inc;
      end loop;

      Curses.Box (Map_Win);
      Curses.Refresh (Map_Win);

      Prompt ("Path Debug Map Round " & Integer'Image (Date));
      Print_Map_Frame (0, 0, Row_Inc, Col_Inc);
      Display_Score;
      Display_Turn;

      Curses.Refresh;

      Prompt (S);
      Wait_Chx;
      Prompt ("");

      if Whose_Map = USER then
         Print_Sector (USER, Save_Sector);
      end if;
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Print_Pzoom;

   --  Print a single cell of a pathmap in condensed format. We average all
   --  squares in the cell and take the mod 10 value. Squares with a value of
   --  -1 are printed with '-', squares with a value of INFINITY/2 are printed
   --  with 'P', and squares with a value of INFINITY are printed with 'Z'.
   --  Squares with a value between P and Z are printed as U.

   procedure Print_Pzoom_Cell
     (Pmap    : in Mapping.Path_Map;
      Vmap    : in View_Map;
      Row     : in Row_T;
      Col     : in Column_T;
      Row_Inc : in Integer;
      Col_Inc : in Integer)
   is
      Sum  : Integer := 0;
      D    : Integer := 0;                 --  squares per cell
      Cell : Path_Display_T;
   begin
      for R in Row .. Row + Row_Inc - 1 loop
         for C in Col .. Col + Col_Inc - 1 loop
            Sum := Sum + Pmap.Cost (Locations.Row_Col_Loc (R, C));
            Debug_Error
              ("sum is now " &
               Integer'Image (Sum) &
               ", d is now " &
               Integer'Image (D));
            D := D + 1;
         end loop;
      end loop;

      Sum := Sum / D;

      if Pmap.Terrain (Locations.Row_Col_Loc (Row, Col)) = T_PATH then
         Cell := '-';
      elsif Sum < 0 then
         Cell := '!';
      elsif Sum = INFINITY / 2          -- (!)
      then
         Cell := '%';
      elsif Sum = INFINITY then
         Cell := ' ';
      elsif Sum > INFINITY / 2          --  (!)
      then
         Cell := '&';
      else
         Sum  := Sum mod 36;
         Cell := Path_Display_T'Val (Sum);
      end if;

      --  for cells outside of path area, show what we would show in normal
      --  zoom
      if Cell = ' ' then
         Print_Zoom_Cell (Vmap, Row, Col, Row_Inc, Col_Inc);
      else
         Curses.Add
           (Win    => Map_Win,
            Line   => Curses.Line_Position (Row / Row_Inc + 1),
            Column => Curses.Column_Position (Col / Col_Inc + 1),
            Ch     => Ptab (Cell));
      end if;
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Print_Pzoom_Cell;

   --  print map's decorations -- coordinates along the bottom and side, and a
   --  label down the side

   procedure Print_Map_Frame
     (First_Row : in Row_T;
      First_Col : in Column_T;
      Row_Inc   : in Integer;
      Col_Inc   : in Integer)
   is
      R : Integer;
      C : Integer;
   begin
      --  print x-coordinates along bottom of screen
      Curses.Move_Cursor (Line => Curses.Lines - 1, Column => 0);
      Curses.Clear_To_End_Of_Line;
      C := First_Col;

      while C < First_Col + ((Integer (Map_Win_Width) - 1) * Col_Inc) and
        C <= MAP_WIDTH - Col_Inc
      loop
         if (C / Col_Inc) mod 10 = 0 then
            Curses.Add
              (Line   => Curses.Lines - 1,
               Column => Curses.Column_Count (C / Col_Inc + 1),
               Str    => Integer'Image (C));
         end if;
         C := C + Col_Inc;
      end loop;

      --  print y-coordinates along right of screen
      R := First_Row;
      while R < First_Row + ((Integer (Map_Win_Height) - 1) * Row_Inc) and
        R <= MAP_HEIGHT - Row_Inc
      loop
         if (R / Row_Inc) mod 10 = 0 and R < MAP_HEIGHT then
            Curses.Add
              (Line   => Curses.Line_Position (R / Row_Inc + NUMTOPS + 1),
               Column => Curses.Columns - NUMSIDES,
               Str    => Integer'Image (R),
               Len    => NUMSIDES - 1);
         else
            Curses.Move_Cursor
              (Line   => Curses.Line_Position (R / Row_Inc + NUMTOPS + 1),
               Column => Curses.Columns - NUMSIDES);
            Curses.Clear_To_End_Of_Line;
         end if;
         R := R + Row_Inc;
      end loop;
   end Print_Map_Frame;
   --  Display the score off in the corner of the screen

   procedure Display_Score is
      U, C : String (1 .. 5) := (others => ' ');
   begin
      Ada.Strings.Fixed.Move
        (Integer'Image (User_Score),
         U,
         Justify => Ada.Strings.Right);
      Ada.Strings.Fixed.Move
        (Integer'Image (Comp_Score),
         C,
         Justify => Ada.Strings.Right);

      Curses.Add
        (Line   => 0,
         Column => Curses.Columns - 12,
         Str    => " User  Comp");
      Curses.Add (Line => 1, Column => Curses.Columns - 12, Str => U & C);
      Curses.Refresh;
   end Display_Score;

   procedure Display_Turn is
      S : constant String :=
        "Sector " &
        Integer'Image (Cur_Sector) &
        " Round " &
        Integer'Image (Date);
   begin
      for R in 1 .. S'Length loop
         exit when R + NUMTOPS + 1 >= MAP_HEIGHT;
         Curses.Add
           (Line   => Curses.Line_Position (R + NUMTOPS + 1),
            Column => Curses.Columns - NUMSIDES + 4,
            Ch     => S (R));
      end loop;
      Curses.Refresh;
   end Display_Turn;

   --  given a frame of a movie (i.e. the map at one particular point), print a
   --  zoomed map at that point)

   procedure Print_Movie_Screen (Mapbuf : in Movie_Screen) is
   --      int row_inc, col_inc;
   --      int r, c, i, j;
   --      char cell;
   begin
      Info ("XXX XXX XXX Move playback is not yet supported");

      --  XXX XXX XXX this should get some of the same polish as
      --  print_{p,}_zoom
--  row_inc = (MAP_HEIGHT + lines - NUMTOPS - 2) / (lines - NUMTOPS);
--  col_inc = (MAP_WIDTH + cols - 1) / (cols - 1);

--  for (r = 0; r < MAP_HEIGHT; r += row_inc)
--    for (c = 0; c < MAP_WIDTH; c += col_inc) {
--      cell = ' ';
--    for (i = r; i < r + row_inc; i++)
--      for (j = c; j < c + col_inc; j++)
--        if (strchr(zoom_list, mapbuf[row_col_loc(i,j)]) <
--          strchr(zoom_list, cell))
--      cell = mapbuf[row_col_loc(i,j)];
--      mvwaddch(stdscr, r/row_inc + NUMTOPS, c/col_inc, cell);
--    }

--      wrefresh(stdscr);
   end Print_Movie_Screen;

--  Print a screen of help information

   procedure Help (Text : in Help_Array) is
      R, C : Integer;
      Text_Lines,
      Obj_Lines,
      Start_Col,
      Start_Row,
      Help_Height,
      Help_Width : Integer;
      Help_Win   : Curses.Window;
      Num_Objs : constant Integer := Piece_Type_T'Pos (Piece_Type_T'Last) + 1;
   begin
      Text_Lines := (Text'Length + 1) / 2;
      Obj_Lines  := (Num_Objs + 1) / 2;

      Help_Height :=
        Integer'Min (Text_Lines + Obj_Lines + 4, Integer (Map_Win_Height));
      Help_Width := Integer'Min (78, Integer (Curses.Columns));

      Start_Row := (Integer (Map_Win_Height) - Help_Height) / 2 + NUMTOPS + 1;
      Start_Col := (Integer (Map_Win_Width) - Help_Width) / 2 + 2;

      Help_Win :=
        Curses.New_Window
          (Curses.Line_Count (Help_Height),
           Curses.Column_Count (Help_Width),
           Curses.Line_Position (Start_Row),
           Curses.Column_Position (Start_Col));

      Curses.Switch_Character_Attribute
        (Help_Win,
         (Reverse_Video => True, others => False),
         True);
      Curses.Move_Cursor (Help_Win, Line => 1, Column => 1);
      Text_Io.Put (Help_Win, Strings.To_String (Text (0)));
      Curses.Move_Cursor (Help_Win, Line => 1, Column => 40);
      Text_Io.Put (Help_Win, "See empire(6) for more information.");
      Curses.Switch_Character_Attribute
        (Help_Win,
         (Reverse_Video => True, others => False),
         False);

      for I in Text'Range loop
         if I <= Text_Lines then
            Curses.Move_Cursor
              (Help_Win,
               Line   => Curses.Line_Position (I + 1),
               Column => 1);
            Text_Io.Put (Help_Win, Strings.To_String (Text (I)));
         else
            Curses.Move_Cursor
              (Help_Win,
               Line   => Curses.Line_Position (I - Text_Lines + 1),
               Column => 40);
            Text_Io.Put (Help_Win, Strings.To_String (Text (I)));
         end if;
      end loop;

      Curses.Switch_Character_Attribute
        (Help_Win,
         (Reverse_Video => True, others => False),
         True);
      Curses.Move_Cursor
        (Help_Win,
         Line   => Curses.Line_Position (Text_Lines + 2),
         Column => 1);
      Text_Io.Put (Help_Win, "  Piece   Yours Enemy Moves Hits Cost");
      Curses.Move_Cursor
        (Help_Win,
         Line   => Curses.Line_Position (Text_Lines + 2),
         Column => 40);
      Text_Io.Put (Help_Win, "  Piece   Yours Enemy Moves Hits Cost");
      Curses.Switch_Character_Attribute
        (Help_Win,
         (Reverse_Video => True, others => False),
         False);

      for J in Piece_Attr'Range loop
         if Piece_Type_T'Pos (J) < (Num_Objs + 1) / 2 then
            R := Piece_Type_T'Pos (J);
            C := 1;
         else
            R := Piece_Type_T'Pos (J) - (Num_Objs + 1) / 2;
            C := 40;
         end if;
         Curses.Move_Cursor
           (Help_Win,
            Line   => Curses.Line_Position (R + Text_Lines + 3),
            Column => Curses.Column_Position (C));
         declare
            Nickname   : String (1 .. 12);
            Uname      : constant Content_Display_T := Piece_Attr (J).U_Cont;
            Cname      : constant Content_Display_T := Piece_Attr (J).C_Cont;
            Speed      : String (1 .. 6);
            Max_Hits   : String (1 .. 5);
            Build_Time : String (1 .. 6);
         begin
            Ada.Strings.Fixed.Move
              (Strings.To_String (Piece_Attr (J).Nickname),
               Nickname,
               Justify => Ada.Strings.Right);
            Ada.Strings.Fixed.Move
              (Integer'Image (Piece_Attr (J).Speed),
               Speed);
            Ada.Strings.Fixed.Move
              (Integer'Image (Piece_Attr (J).Max_Hits),
               Max_Hits);
            Ada.Strings.Fixed.Move
              (Integer'Image (Piece_Attr (J).Build_Time),
               Build_Time);
            --  XXX XXX XXX in display, this doesn't quite line up right. tweak
            --  later
            Text_Io.Put
              (Help_Win,
               Nickname &
               " " &
               Content_Display_T'Image (Uname) &
               "     " &
               Content_Display_T'Image (Cname) &
               Speed &
               Max_Hits &
               Build_Time);
         end;
      end loop;

      Curses.Box (Help_Win);
      Curses.Refresh (Help_Win);

      Prompt ("Press any key to continue");
      Wait_Chx;
      Prompt ("");

      Curses.Clear (Help_Win);
      Curses.Refresh (Help_Win);

      Curses.Delete (Help_Win);

      if Whose_Map = USER then
         Print_Sector (USER, Save_Sector);
      end if;
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Help;

   function Get_Piece_Name return Piece_Choice_T is
      C : Character;
   begin
      C := Get_Chx;

      for I in Piece_Attr'Range loop
         if C = Piece_Attr (I).Sel_Char then
            return I;
         end if;
      end loop;
      return NOPIECE;
   end Get_Piece_Name;

   --  what follows was in term.c in the original (the above was in
   --  display.c) this file contains various routines used to control the user
   --  communications area of the terminal. This area consists of the top 3
   --  lines of the terminal where messages are displayed to the user and
   --  input is acquired from the user.
   --
   --  There are two types of output in this area. One type is interactive
   --  output. This consists of a prompt line and an error message line. The
   --  other type of output is informational output. The user must be given
   --  time to read informational output.
   --
   --  Whenever input is received, the top three lines are cleared and the
   --  screen refreshed as the user has had time to read these lines.

   --  Here are routines that handle printing to the top few lines of the
   --  screen.

   --  Print a prompt on the status line

   procedure Prompt (S : in String) is
   begin
      Curses.Move_Cursor (Status_Win, 0, 0);
      Curses.Clear_To_End_Of_Line (Status_Win);
      Text_Io.Put (Status_Win, S);
      Curses.Refresh (Status_Win);
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Prompt;

   procedure Info (S : in String) is
   begin
      Curses.Allow_Scrolling (Info_Win, True);
      Curses.Scroll (Info_Win);
      Curses.Move_Cursor (Info_Win, NUMINFO - 1, 0);
      Text_Io.Put (Info_Win, S);
      Curses.Refresh (Info_Win);
      Curses.Allow_Scrolling (Info_Win, False);
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Info;

   procedure Debug_Info (S : in String) is
   begin
      if Debug then
         Info (Integer'Image (Debug_Count) & ": " & S);
      end if;
      Debug_Count := Debug_Count + 1;
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Debug_Info;

   --  Print an error message on the second message line

   procedure Error (S : in String) is
   begin
      Curses.Allow_Scrolling (Status_Win, True);
      Curses.Scroll (Status_Win);
      Curses.Move_Cursor (Status_Win, 0, 0);
      Text_Io.Put (Status_Win, S);
      Curses.Refresh (Status_Win);
      Curses.Beep;
      Curses.Nap_Milli_Seconds (2000);
      Curses.Scroll (Status_Win, -1);
      Curses.Refresh (Status_Win);
      Curses.Allow_Scrolling (Status_Win, False);
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Error;

   procedure Debug_Error (S : in String) is
   begin
      if Debug then
         Error (Integer'Image (Debug_Count) & ": " & S);
      end if;
      Debug_Count := Debug_Count + 1;
   end Debug_Error;

   --  Print out a generic error message

   procedure Huh is
   begin
      Error ("Type ? for Help.");
   end Huh;

   --  Get a character from the user and convert it to uppercase

   function Get_Chx return Character is
      K : Curses.Real_Key_Code;
      C : Character;
   begin
      Curses.Set_Cbreak_Mode (True);
      K := Curses.Get_Keystroke (Status_Win);
      Curses.Set_Cbreak_Mode (False);

      --  XXX XXX XXX is this right?
      C := Character'Val (K);
      C := Ada.Characters.Handling.To_Upper (C);
      return C;
   exception
      when Constraint_Error =>
         return Character'First;
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Get_Chx;

   procedure Wait_Chx is
      K : Curses.Real_Key_Code;
   begin
      Curses.Set_Cbreak_Mode (True);
      K := Curses.Get_Keystroke (Status_Win);
      Curses.Set_Cbreak_Mode (False);
   end Wait_Chx;

   --  Input an integer from the user. low and high set bounds

   function Get_Int
     (Message : in String;
      Low     : in Integer;
      High    : in Integer) return Integer
   is
      L : Integer;
      S : String (1 .. STRING_MAX);
   begin
--      char    buf[STRSIZE], *end;

      loop
         Prompt (Message);
         Curses.Set_Echo_Mode (True);
         Curses.Set_Cbreak_Mode (False);
         --  XXX XXX XXX can this raise constraint_error? XXX XXX XXX IIUC, it
         --  truncates
         Curses.Get (Status_Win, S, S'Length);
         Curses.Set_Cbreak_Mode (True);
         Curses.Set_Echo_Mode (False);

         begin
            L := Integer'Value (S);
            if L >= Low and L <= High then
               return L;
            else
               Error
                 ("Please enter an integer in the range " &
                  Integer'Image (Low) &
                  ".." &
                  Integer'Image (High) &
                  ".");
            end if;
         exception
            when Constraint_Error =>
               Error ("Please enter an integer.");
         end;
      end loop;
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Get_Int;

   --  Input a yes or no response from the user. We loop until we get a valid
   --  response. We return TRUE iff the user replies 'y'.

   function Get_Yn (Message : in String) return Boolean is
      C : Character;
   begin
      loop
         Prompt (Message);
         C := Get_Chx;

         case C is
            --  remember, Get_Chx upcases
            when 'Y' =>
               return True;
            when 'N' =>
               return False;
            when others =>
               Error ("Please answer Y or N.");
         end case;
      end loop;
   end Get_Yn;

   --  Clear the screen, and note that no sector is currently displayed

   procedure Clear is
   begin
      Curses.Clear;
      Curses.Refresh;
      Kill_Display;
   end Clear;

   --  Redraw the screen
   --  XXX as above, this has the same name as a curses function, but this is
   --  ui driver independent
   procedure Redraw is
   begin
      Curses.Refresh;
   end Redraw;

   --  Clean up the display. This routine gets called as we leave the game

   procedure End_Ui is
   begin
      Curses.Move_Cursor (Line => Curses.Lines - 1, Column => 0);
      Curses.Clear_To_End_Of_Line;
      Curses.Refresh;
      Curses.End_Screen;
   end End_Ui;

   --  Initialize the terminal
   procedure Init_Ui is
   begin
      Curses.Init_Screen;
      Curses.Set_Echo_Mode (False);
      Curses.Set_Cbreak_Mode (False);

      --  XXX XXX XXX XXX we call lines at all the right places, and try to
      --  cope. this might be better XXX XXX XXX XXX to revert to...
      --      if (lines > MAP_HEIGHT + NUMTOPS + 1)
      --              lines = MAP_HEIGHT + NUMTOPS + 1;
      --      if (cols > MAP_WIDTH + NUMSIDES)
      --              cols = MAP_WIDTH + NUMSIDES;

      Status_Win := Curses.New_Window (1, Curses.Columns - 12, 0, 0);
      Curses.Switch_Character_Attribute
        (Status_Win,
         (Reverse_Video => True, others => False),
         True);
      Info_Win := Curses.New_Window (NUMINFO, Curses.Columns - 12, 1, 0);
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Init_Ui;

   procedure Alert is
   begin
      Curses.Beep;
   exception
      when Curses.Curses_Exception =>
         raise Curses.Curses_Exception;
   end Alert;

end Empire.Curses_Interface;
