-- math.c -- various mathematical routines.
--
-- This file contains routines used to create random integers.  The
-- initialization routine 'rand_init' should be called at program startup.
-- The flavors of random integers that can be generated are:
--
--     rand_long (n) -- returns a random integer in the range 0..n-1
--
-- Other routines include:
--
--     dist (a, b) -- returns the straight-line distance between two locations.

with Empire.Locations;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random;
use Ada.Numerics.Float_Random;

package body Empire.Math is

   function Dist (A : in Location_T; B : in Location_T) return Integer is
      Ax, Ay, Bx, By : Integer;
   begin
      Ax := Locations.Loc_Row(A);
      Ay := Locations.Loc_Col(A);
      Bx := Locations.Loc_Row(B);
      By := Locations.Loc_Col(B);
      
      -- XXX use an integer equivalent, possibly isqrt from cempire.
      return Integer(Sqrt( Float((Ax-Bx) * (abs Ax-Bx)) + Float((Ay-By) * (Ay-By))));
   end Dist;

   procedure Rand_Init is
   begin
      Reset(G);
   end Rand_Init;

   function Rand_Long (High : Integer) return Integer is
   begin
      if High < 2
      then
         return 0;
      end if;

      return Integer(Float(High) * Random(G)) mod High;
   end Rand_Long;

end Empire.Math;
