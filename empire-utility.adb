with Text_IO;
with Ada.Command_Line;

with Empire.Ui;

package body Empire.Utility is

   -- various utility routines

   --  Here is a little routine to perform consistency checking on the
   --  database.
   --
   --  We perform the following functions:
   --
   --  1) Make sure no list contains loops.
   --
--  2) Make sure every object is in either the free list with 0 hits, or it is
--  in the correct object list and a location list with non-zero hits, and an
--  appropriate owner.
   --
   --  3) Make sure every city is on the map.
   --
   --  4) Make sure every object is in the correct location and that objects on
   --  the map have non-zero hits.
   --
   --  5) Make sure every object in a cargo list has a ship pointer.
   --
   --  6) Make sure every object with a ship pointer is in that ship's cargo
   --  list.

   procedure Check is
   -- P : Piece_Info_P;
   begin
      if not Debug then
         return;
      end if;

      --  XXX XXX XXX XXX this needs major rework for the new world order XXX
      --  XXX XXX XXX (see below -- this is very ptr-arithmetic intensive, XXX
      --  XXX XXX XXX such as assuming that subtracting the array address XXX
      --  XXX XXX XXX from the member address gives you the index

      -- nothing in any list yet
--      In_Free := (others => False);
--      In_Obj := (others => False);
--      In_Loc := (others => False);
--      In_Cargo := (others => False);

      --  Mark all objects in free list. Make sure objects in free list have
      --  zero hits.

--      P := Free_List;
--      while P /= null
--      loop
--              i = p - object;
--              assert (!in_free[i]);
--              in_free[i] = 1;
--              assert (p->hits == 0);
--              if (p->piece_link.prev)
--                      assert (p->piece_link.prev->piece_link.next == p);
--         P := P.Links(Piece_Link).Next;
--      end loop;

--      /* Mark all objects in the map.
--      Check that cities are in corect location.
--      Check that objects are in correct location,
--      have a good owner, and good hits. */

--      for (i = 0; i < MAP_SIZE; i++) {
--              if (map[i].cityp) assert (map[i].cityp->loc == i);

--              for (p = map[i].objp; p != NULL; p = p->loc_link.next) {
--                      assert (p->loc == i);
--                      assert (p->hits > 0);
--                      assert (p->owner == USER || p->owner == COMP);

--                      j = p - object;
--                      assert (!in_loc[j]);
--                      in_loc[j] = 1;

--                      if (p->loc_link.prev)
--                              assert (p->loc_link.prev->loc_link.next == p);
--              }
--      }

--      /* make sure all cities are on map */

--      for (i = 0; i < NUM_CITY; i++)
--              assert (map[city[i].loc].cityp == &(city[i]));

--      /* Scan object lists. */

--      check_obj (comp_obj, COMP);
--      check_obj (user_obj, USER);

--      /* Scan cargo lists. */

--      check_cargo (user_obj[TRANSPORT], ARMY);
--      check_cargo (comp_obj[TRANSPORT], ARMY);
--      check_cargo (user_obj[CARRIER], FIGHTER);
--      check_cargo (comp_obj[CARRIER], FIGHTER);

--      /* Make sure all objects with ship pointers are in cargo. */

--      check_obj_cargo (comp_obj);
--      check_obj_cargo (user_obj);

--      /* Make sure every object is either free or in loc and obj list. */

--      for (i = 0; i < LIST_SIZE; i++)
--              assert (in_free[i] != (in_loc[i] && in_obj[i]));
   end Check;

--  /*
--   * Check object lists.  We look for:
--   *
--   * 1)  Loops and bad prev pointers.
--   *
--   * 2)  Dead objects.
--   *
--   * 3)  Invalid types.
--   *
--   * 4)  Invalid owners.
--   */

--  static void
--  check_obj (piece_info_t **list, const int owner) {
--      long j;
--      piece_type_t i;
--      const piece_info_t *p;

--      for (i = FIRST_OBJECT; i < NUM_OBJECTS; i++)
--              for (p = list[i]; p != NULL; p = p->piece_link.next)
--              {
--                      assert (p->owner == owner);
--                      assert (p->type == i);
--                      assert (p->hits > 0);

--                      j = p - object;
--                      assert (!in_obj[j]);
--                      in_obj[j] = 1;

--                      if (p->piece_link.prev)
--                              assert (p->piece_link.prev->piece_link.next == p);
--              }
--  }

--  /*
--   * Check cargo lists.  We assume object lists are valid.
--   * as we will place bits in the 'in_cargo' array that are used by
--   * 'check_obj'.
--   *
--   * Check for:
--   *
--   * 1)  Number of items in list is same as cargo count.
--   *
--   * 2)  Type of cargo is correct.
--   *
--   * 3)  Location of cargo matches location of ship.
--   *
--   * 4)  Ship pointer of cargo points to correct ship.
--   *
--   * 5)  There are no loops in cargo list and prev ptrs are correct.
--   *
--   * 6)  All cargo is alive.
--   */

--  static void
--  check_cargo (const piece_info_t *list, const piece_type_t cargo_type) {
--      const piece_info_t *p, *q;
--      long j, count;

--      for (p = list; p != NULL; p = p->piece_link.next) {
--              count = 0;
--              for (q = p->cargo; q != NULL; q = q->cargo_link.next) {
--                      count += 1; /* count items in list */
--                      assert (q->type == cargo_type);
--                      assert (q->owner == p->owner);
--                      assert (q->hits > 0);
--                      assert (q->ship == p);
--                      assert (q->loc == p->loc);

--                      j = q - object;
--                      assert (!in_cargo[j]);
--                      in_cargo[j] = 1;

--                      if (p->cargo_link.prev)
--                              assert (p->cargo_link.prev->cargo_link.next == p);
--                  }
--              assert (count == p->count);
--          }
--  }

--  /*
--   * Scan through object lists making sure every object with a ship
--   * pointer appears in a cargo list.  We assume object and cargo
--   * lists are valid.
--   */

--  static void
--  check_obj_cargo (piece_info_t **list) {
--      const piece_info_t *p;
--      piece_type_t i;

--      for (i = FIRST_OBJECT; i < NUM_OBJECTS; i++)
--              for (p = list[i]; p != NULL; p = p->piece_link.next)
--                      if (p->ship)
--                              assert(in_cargo[p-object]);
--  }

   --  Print a message and then exit

   procedure Panic (Why : in String) is
   begin
      Ui.End_Ui;
      -- XXX should be stderr?
      Text_IO.Put_Line (Ada.Command_Line.Command_Name & ": " & Why);
      raise Program_Error;
   end Panic;

--  void
--  emp_panic (const char *file, const int line, const char *why) {
--      term_end();

--      if (why == NULL)
--              fprintf(stderr, "empire panic (file %s, line %d)\n", file, line);
--      else
--              fprintf(stderr, "empire panic (file %s, line %d): %s\n", file, line, why);

--      abort();
--  }

end Empire.Utility;
