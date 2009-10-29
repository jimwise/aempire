package Empire.Objects is
   procedure Disembark (Obj : in Piece_Info_P);
   procedure Embark (Ship : in Piece_Info_P; Obj : in Piece_Info_P);

   function Find_City_At_Loc (Loc    : in Location_T;
                              Owners : in Acceptable_Owner_Array := (others => TRUE)) return City_Info_P;
   function Find_Obj_At_Loc (Loc         : in Location_T;
                             Types       : in Acceptable_Piece_Array := (SATELLITE => FALSE, others => TRUE);
                             Owners      :    Acceptable_Owner_Array := (others => TRUE);
                             Ignore_Full :    Boolean                := False) return Piece_Info_P;

   -- originals returned Loc as found loc to indicate none found; XXX maybe still should?  check users...
   -- XXX XXX XXX these two should have the same signature, and probably the latter two should be one general Find_Nearest_Obj
   -- XXX XXX XXX with filters like Find_Obj_At_Loc...

   -- XXX XXX XXX IMPORTANT:  these use the map, not a vmap, and as such should only be called with Owner set to the calling
   -- XXX XXX XXX player.  To encourage this, Owner is an Owner_T, unlike the versions in Find_*_At_Loc, and is non-optional

   -- XXX XXX re-order parameters to allow three filter args to come last.  then re-order other Find_* to match
   procedure Find_Nearest_Obj (Loc            : in     Location_T;
                               Owner          : in     Owner_T;
                               Types          : in     Acceptable_Piece_Array := (SATELLITE => FALSE, others => TRUE);
                               Max_Range      : in     Integer                := INFINITY;
                               Ignore_Full    : in     Boolean                := False;
                               Found          :    out Boolean;
                               Obj_Loc        :    out Location_T;
                               Estimated_Cost :    out Integer);
   procedure Find_Nearest_City (Loc            : in     Location_T;
                                Owner          : in     Owner_T;
                                Found          :    out Boolean;
                                City_Loc       :    out Location_T;
                                Estimated_Cost :    out Integer);

   -- this belongs elsewhere, since it queries the user (and doesn't otherwise search)
   function Good_Loc (Obj : in Piece_Info_P; Loc: in Location_T) return Boolean;
   procedure Kill_City (City : in out City_Info_T);
   procedure Kill_Obj (Obj : in Piece_Info_P; Loc : Location_T);
   procedure Move_Obj (Obj : in Piece_Info_P; New_Loc : Location_T);
   procedure Move_Sat (Obj : in Piece_Info_P);
   function Obj_Capacity (Obj : in Piece_Info_P) return Integer; -- XXX limit?
   function Obj_Moves (Obj : in Piece_Info_P) return Integer; -- XXX limit?
   procedure Produce (City : in out City_Info_T);
   procedure Scan (Owner : in Piece_Owner_T;
                   Loc   : in Location_T);
   procedure Ask_Prod (City : in out City_Info_T);

   procedure Describe_City (City : in City_Info_T);
   procedure Describe_Obj (Obj : in Piece_Info_P);

private

   function Bounce (Loc              : in Location_T;
                    Dir1, Dir2, Dir3 : in Direction_T)
                   return Direction_T;

   procedure Kill_One (Obj : in Piece_Info_P);

   procedure Move_Sat1 (Obj : in Piece_Info_P);

   procedure Scan_Sat (Owner : in Piece_Owner_T;
                       Loc   : in Location_T);
   procedure Update (Owner : in Piece_Owner_T;
                     Loc   : in Location_T);
end Empire.Objects;
