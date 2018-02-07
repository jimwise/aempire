package Empire.Locations is
   function Loc_Row (Loc : in Location_T) return Row_T;
   function Loc_Col (Loc : in Location_T) return Column_T;
   function Row_Col_Loc (Row : in Row_T; Col : in Column_T) return Location_T;
   function Sector_Row (Sec : in Sector_T) return Row_T;
   function Sector_Col (Sec : in Sector_T) return Column_T;
   function Row_Col_Sector (Row : in Row_T; Col : in Column_T) return Sector_T;
   function Loc_Sector (Loc : in Location_T) return Sector_T;
   function Sector_Loc (Sec : in Sector_T) return Location_T;
end Empire.Locations;
