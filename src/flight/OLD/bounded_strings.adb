---------------------------------------------------------------------------
-- FILE       : bounded_strings.adb
-- SUBJECT    : Body of a package for manipulating general purpose bounded strings.
-- PROGRAMMER : (C) Copyright 2015 by Vermont Technical College
--
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Bounded_Strings is

   --
   -- NOTE: Append_Outgoing_Line and Append_Payload_Line are essential identical except for the
   -- different output types. They should probably be instantiated from a generic procedure (or
   -- maybe this entire package should be generic).
   --

   procedure Append_Outgoing_Line
     (Line : in out Outgoing_Record; Additional : in String; Size : in Natural) is
      Available_Space       : Outgoing_Size_Type;
      Desired_Outgoing_Size : Natural;
      Characters_To_Copy    : Outgoing_Size_Type;
   begin
      Available_Space := Maximum_Outgoing_Length - Line.Size;

      -- Use the smallest of Additional'Length and Size
      if Size > Additional'Length then
         Desired_Outgoing_Size := Additional'Length;
      else
         Desired_Outgoing_Size := Size;
      end if;

      -- Verify that there is sufficient space. Truncate if necessary.
      if Desired_Outgoing_Size > Available_Space then
         Characters_To_Copy := Available_Space;
      else
         Characters_To_Copy := Desired_Outgoing_Size;
      end if;

      for I in 1 .. Characters_To_Copy loop
         pragma Loop_Invariant(Line.Size + Characters_To_Copy <= Maximum_Outgoing_Length);

         Line.Text(Line.Size + I) := Additional(Additional'First + (I - 1));
      end loop;
      Line.Size := Line.Size + Characters_To_Copy;
   end Append_Outgoing_Line;


   --
   -- NOTE: Prepare_Outgoing_Line, Prepare_Incoming_Line, and Prepare_Payload_Line are essential
   -- identical except for the different return types. They should probably be instantiated from
   -- a generic function (or maybe this entire package should be generic).
   --

   function Prepare_Outgoing_Line(Line : String; Size : Natural) return Outgoing_Record is
      Result                : Outgoing_Record;
      Desired_Outgoing_Size : Natural;
      Characters_To_Copy    : Outgoing_Size_Type;
   begin
      -- Use the smallest of Line'Length and Size.
      Result.Text := (others => Character'Val(0));
      if Size > Line'Length then
         Desired_Outgoing_Size := Line'Length;
      else
         Desired_Outgoing_Size := Size;
      end if;

      -- Verify that there is sufficent space. Truncate if necessary.
      if Desired_Outgoing_Size > Maximum_Outgoing_Length then
         Characters_To_Copy := Maximum_Outgoing_Length;
      else
         Characters_To_Copy := Desired_Outgoing_Size;
      end if;

      for I in 1 .. Characters_To_Copy loop
         Result.Text(I) := Line(Line'First + (I - 1));
      end loop;
      Result.Size := Characters_To_Copy;
      return Result;
   end Prepare_Outgoing_Line;


   function Prepare_Incoming_Line(Line : String; Size : Natural) return Incoming_Record is
      Result                : Incoming_Record;
      Desired_Incoming_Size : Natural;
      Characters_To_Copy    : Incoming_Size_Type;
   begin
      -- Use the smallest of Line'Length and Size.
      Result.Text := (others => Character'Val(0));
      if Size > Line'Length then
         Desired_Incoming_Size := Line'Length;
      else
         Desired_Incoming_Size := Size;
      end if;

      -- Verify that there is sufficent space. Truncate if necessary.
      if Desired_Incoming_Size > Maximum_Incoming_Length then
         Characters_To_Copy := Maximum_Incoming_Length;
      else
         Characters_To_Copy := Desired_Incoming_Size;
      end if;

      for I in 1 .. Characters_To_Copy loop
         Result.Text(I) := Line(Line'First + (I - 1));
      end loop;
      Result.Size := Characters_To_Copy;
      return Result;
   end Prepare_Incoming_Line;


   procedure Append_Payload_Line
     (Line : in out Payload_Record; Additional : in String; Size : in Natural) is
      Available_Space      : Payload_Size_Type;
      Desired_Payload_Size : Natural;
      Characters_To_Copy   : Payload_Size_Type;
   begin
      Available_Space := Maximum_Payload_Length - Line.Size;

      -- Use the smallest of Additional'Length and Size
      if Size > Additional'Length then
         Desired_Payload_Size := Additional'Length;
      else
         Desired_Payload_Size := Size;
      end if;

      -- Verify that there is sufficient space. Truncate if necessary.
      if Desired_Payload_Size > Available_Space then
         Characters_To_Copy := Available_Space;
      else
         Characters_To_Copy := Desired_Payload_Size;
      end if;

      for I in 1 .. Characters_To_Copy loop
         pragma Loop_Invariant(Line.Size + Characters_To_Copy <= Maximum_Payload_Length);

         Line.Text(Line.Size + I) := Additional(Additional'First + (I - 1));
      end loop;
      Line.Size := Line.Size + Characters_To_Copy;
   end Append_Payload_Line;


   function Prepare_Payload_Line(Line : String; Size : Natural) return Payload_Record is
      Result               : Payload_Record;
      Desired_Payload_Size : Natural;
      Characters_To_Copy   : Payload_Size_Type;
   begin
      -- Use the smallest of Line'Length and Size.
      Result.Text := (others => Character'Val(0));
      if Size > Line'Length then
         Desired_Payload_Size := Line'Length;
      else
         Desired_Payload_Size := Size;
      end if;

      -- Verify that there is sufficent space. Truncate if necessary.
      if Desired_Payload_Size > Maximum_Payload_Length then
         Characters_To_Copy := Maximum_Payload_Length;
      else
         Characters_To_Copy := Desired_Payload_Size;
      end if;

      for I in 1 .. Characters_To_Copy loop
         Result.Text(I) := Line(Line'First + (I - 1));
      end loop;
      Result.Size := Characters_To_Copy;
      return Result;
   end Prepare_Payload_Line;

end Bounded_Strings;
