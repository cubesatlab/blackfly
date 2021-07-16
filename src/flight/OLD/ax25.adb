--------------------------------------------------------------------------------
-- FILE   : ax25.adb
-- SUBJECT: Body of a package that does formatting/unformatting of ax25 data
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Ax25 is

   CubeSat_Address : constant AX25_Address_Type := "W1VTC2";

   -- TODO: log source addresses somewhere
   -- extremely stupid ax25 unformatter, this does not check source or destination addresses
   procedure Remove(Payload : in out Bounded_Strings.Payload_Record; Success : out Boolean) is
      New_Packet_Size   : Bounded_Strings.Payload_Size_Type := 0;
      Source_Address    : Bounded_Strings.Payload_Record;
      Temp_Char_Byte    : Utility.Byte_Type;
   begin
      Success := True;
      -- verify to make sure the destination address is correct, source shouldn't matter
      -- ax25 bitsuffs the addresses so they are all shifted up 1 bit

      for I in Bounded_Strings.Incoming_Index_Type range
        AX25_Address_Index'First .. AX25_Address_Index'Last
      loop
         if Character'Pos(Payload.Text(I)) /= (Character'Pos(CubeSat_Address(I)) * 2) then
            Success := False;
         end if;
      end loop;

      if Success then
         for I in Bounded_Strings.Payload_Index_Type range 17 .. Payload.Size loop
            pragma Loop_Invariant(New_Packet_Size < I);
            New_Packet_Size := New_Packet_Size + 1;
            Payload.Text(New_Packet_Size) := Payload.Text(I);
         end loop;
         Payload.Size := New_Packet_Size;

         -- extract the source address
         Source_Address := Bounded_Strings.Prepare_Payload_Line(" ", 0);
         for I in Bounded_Strings.Incoming_Index_Type range
           AX25_Address_Index'First .. AX25_Address_Index'Last loop
            -- the source address bytes start 8 bytes in.
            Temp_Char_Byte := Character'Pos(Payload.Text(I + 7));
            if Temp_Char_Byte /= 0 then
               Temp_Char_Byte := Temp_Char_Byte / 2; -- unstuff the ascii  ;[
            end if;
            Source_Address.Text(I) := Character'Val(Temp_Char_Byte);
         end loop;
         Source_Address.Size := 6;

         -- Append the payload to the source address with a : to look cool
         -- there should be no way for the extracted source and payload to be larger than the
         -- max payload size.
         Bounded_Strings.Append_Payload_Line(Source_Address, ":", 1);
         Bounded_Strings.Append_Payload_Line(Source_Address, Payload.Text, Payload.Size);

         -- log the source address
         File_Handler.Write_To_Log_File(File_Handler.Ax25_Type, Source_Address);
      end if;
   end Remove;

end Ax25;
