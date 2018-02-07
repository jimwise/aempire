--
--  parse command line for empire
--
--  options:
--
--    -w water: percentage of map that is water.  Must be in the range
--             10..90.  Default is 70.
--
--    -s smooth: amount of smoothing performed to generate map.  Must
--             be a nonnegative integer.  Default is 5.
--
--    -S save_interval: sets turn interval between saves.
--             default is 10
--

with Text_IO;
with Ada.Command_Line;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with GNAT.Command_Line;

with Empire;

procedure Main is
   Flag_Small_W : Integer := 70;
   Flag_Small_S : Integer := 5;
   Flag_Large_S : Integer := 10;
   Land : Integer;
   Invalid_Command_Line : exception;
   OPTFLAGS : constant String :=  "w: s: d: S: t V C";
   USAGE : constant String :=
     "[-w water] [-s smooth] [-d delay] [-S save_interval] [-V]";
begin

   loop
      case GNAT.Command_Line.Getopt (OPTFLAGS) is

         when 'w' =>
            Flag_Small_W := Integer'Value (GNAT.Command_Line.Parameter);
            if Flag_Small_W < 10 or Flag_Small_W > 90 then
               Text_IO.Put_Line (Ada.Command_Line.Command_Name &
                                   ": -W argument must be in range 10..90");
               raise Invalid_Command_Line;
            end if;

         when 's' =>
            Flag_Small_S := Integer'Value (GNAT.Command_Line.Parameter);
            if Flag_Small_S < 0 then
               Text_IO.Put_Line (Ada.Command_Line.Command_Name &
                    ": -s argument must be greater than or equal to zero");
               raise Invalid_Command_Line;
            end if;

         when 'S' =>
            Flag_Large_S := Integer'Value (GNAT.Command_Line.Parameter);
            if Flag_Large_S < 1 then
               Text_IO.Put_Line (Ada.Command_Line.Command_Name &
                      ": -S argument must be greater than or equal to 1");
               raise Invalid_Command_Line;
            end if;

         when 't' =>
            Empire.Traditional := True;

         when 'C' =>
            Empire.Color := False;

         when 'V' =>
            Text_IO.Put_Line (Empire.VERSION_STRING);
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
            return;

         when others =>
            exit;
      end case;
   end loop;

   Empire.Smooth := Flag_Small_S;
   Empire.Water_Ratio := Flag_Small_W;
   Empire.Save_Interval := Flag_Large_S;

   --  compute min distance between cities
   --  available land
   Land := Empire.MAP_SIZE * (100 - Empire.Water_Ratio) / 100;
   Land := Land / Empire.NUM_CITY;      -- land per city
   Empire.Min_City_Dist := Integer (Sqrt (Float (Land)));

   Empire.Run_Empire;

   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);

exception

   --  XXX should be stderr?
   when GNAT.Command_Line.Invalid_Switch =>
      Text_IO.Put_Line (Ada.Command_Line.Command_Name & ": usage: " &
                          GNAT.Command_Line.Full_Switch &
                          ": invalid switch");
      raise Invalid_Command_Line;

   when GNAT.Command_Line.Invalid_Parameter =>
      Text_IO.Put_Line (Ada.Command_Line.Command_Name & ": usage: " &
                          GNAT.Command_Line.Full_Switch &
                          ": argument required");
      raise Invalid_Command_Line;

   when Invalid_Command_Line =>
      Text_IO.Put_Line (Ada.Command_Line.Command_Name & ": usage: " &
                       Ada.Command_Line.Command_Name & " " & USAGE);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end Main;
