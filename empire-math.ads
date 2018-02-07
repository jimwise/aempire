with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

package Empire.Math is
   function Dist (A : in Location_T; B : in Location_T) return Integer;
   procedure Rand_Init;
   function Rand_Long (High : in Integer) return Integer;
private
   G : Generator;
end Empire.Math;
