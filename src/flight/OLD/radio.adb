--------------------------------------------------------------------------------
-- FILE   : radio.adb
-- SUBJECT: Implementation for control program interface to the radio.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Radio_Port;

package body Radio
  with
    Refined_State => (State => (Scanner_State, Transmitter_State))
is

   ------------------------------------------------------
   -- Constants
   ------------------------------------------------------
   Number_Of_Payload_Checksum_Bytes : constant := 2;
   Number_Of_Header_Checksum_Bytes  : constant := 2;
   Number_Of_Sync_Characters        : constant := 2;
   Size_Zero                        : constant := 0;

   ------------------------------------------------------
   -- Sync Characters which accompany every message
   ------------------------------------------------------
   Sync_1 : constant Character := 'H';
   Sync_2 : constant Character := 'e';

   ------------------------------------------------------
   -- Baud Rates
   ------------------------------------------------------
   Baud_Rate_9600   : constant := 0;
   Baud_Rate_19200  : constant := 1;
   Baud_Rate_38400  : constant := 2;
   Baud_Rate_76800  : constant := 3;
   Baud_Rate_115200 : constant := 4;

   ------------------------------------------------------
   -- Arrray/String types
   ------------------------------------------------------
   subtype Packet_Header_Index is Positive range 1 .. 8;
   subtype Packet_Header is String(Packet_Header_Index);
   subtype Full_Packet_Size is
     Positive range 1 .. Packet_Header_Index'Last + (Bounded_Strings.Payload_Size_Type'Last + 2);
   subtype Full_Packet is String(Full_Packet_Size);
   subtype String_Payload_Range is
     Positive range 1 .. Bounded_Strings.Payload_Size_Type'Last + 1;
   subtype String_Payload is String(String_Payload_Range);
   subtype Header_Without_Sync_Range is Positive range 1 .. 6;
   subtype Header_Without_Sync is String(Header_Without_Sync_Range);

   ------------------------------------------------------
   -- Command types
   ------------------------------------------------------
   Incoming_Command : constant Character := Character'Val(16#10#);
   Outgoing_Command : constant Character := Character'Val(16#20#);

   type Commands_Array_Type is array (Commands) of Character;
   Commands_Array : constant Commands_Array_Type := Commands_Array_Type'
     (No_Op_Command                         => Character'Val(16#01#),
      Reset_Command                         => Character'Val(16#02#),
      Transmit_Command                      => Character'Val(16#03#),
      Received_Data_Command                 => Character'Val(16#04#),
      Get_Transceiver_Configuration_Command => Character'Val(16#05#),
      Set_Transceiver_Configuration_Command => Character'Val(16#06#),
      Telemetry_Command                     => Character'Val(16#07#),
      Write_Flash_Command                   => Character'Val(16#08#),
      Invalid_Command                       => Character'Val(16#FF#));

   ------------------------------------------------------
   -- State machine states for reading in data
   ------------------------------------------------------
   type Scanner_State_Type is
     (Search_For_H,
      Search_For_E,
      Gather_Header,
      Gather_Message,
      Done);

   ------------------------------------------------------
   --- Globals/State
   ------------------------------------------------------
   Scanner_State     : Scanner_State_Type;
   Transmitter_State : Boolean;

   ------------------------------------------------------
   -- Subprograms
   ------------------------------------------------------

   procedure Initialize(Success : out Boolean)
     with
       Refined_Global => (Output => (Radio_Port.State, Scanner_State, Transmitter_State)),
       Refined_Depends => ((Radio_Port.State, Scanner_State, Success, Transmitter_State) =>
                                 null)
   is
   begin
      Radio_Port.Open;
      Scanner_State     := Done;
      Success           := True;
      Transmitter_State := False;
   end Initialize;


   -- takes a character and returns a command associated with it
   function Character_To_Command(Command_Character : in Character) return Commands is
      Command : Commands := Invalid_Command;
   begin
      for I in Commands loop
         if Commands_Array(I) = Command_Character then
            Command := I;
         end if;
      end loop;
      return Command;
   end Character_To_Command;


   -- Removes the sync characters from a given string. Returns the new line and the size as out
   -- params
   procedure Strip_Sync_Characters
     (Line     : in  String;
      Size     : in  Bounded_Strings.Incoming_Size_Type;
      Out_Line : out Bounded_Strings.Incoming_Type;
      Out_Size : out Bounded_Strings.Incoming_Size_Type)
     with
       Depends => (Out_Line => (Line, Size),
                   Out_Size => Size),
     Pre => (Line'Length >= 3 and Line'Length <= 265 and Size >= 2 )
   is
   begin
      Out_Line := Bounded_Strings.Incoming_Type'(others => ' ');

      for I in Bounded_Strings.Incoming_Index_Type range Number_Of_Sync_Characters + 1 ..
        Bounded_Strings.Incoming_Type'Last
      loop

         Out_Line(I - Number_Of_Sync_Characters) := Line(I);
         exit when I = Size + Number_Of_Sync_Characters;
      end loop;

      Out_Size := Size - Number_Of_Sync_Characters;
   end Strip_Sync_Characters;


   -- Get the checksum for a given string.
   procedure Calculate_Checksum
     (Message    : in  Bounded_Strings.Incoming_Type;
      Size       : in  Bounded_Strings.Incoming_Size_Type;
      Checksum_A : out Utility.Byte_Type;
      Checksum_B : out Utility.Byte_Type)
     with
       Depends => ((Checksum_A, Checksum_B) => (Message, Size))
   is
   begin
      Checksum_A := 0;
      Checksum_B := 0;
      for I in Natural range Message'First .. Size loop
         Checksum_A := Checksum_A + Character'Pos(Message(I));
         Checksum_B := Checksum_B + Checksum_A;
      end loop;
   end Calculate_Checksum;


   -- Appends a payload to a packet header and creates a single record.
   procedure Assemble_Packet
     (Header     : in Packet_Header;
      Payload    : in Bounded_Strings.Payload_Record;
      Out_Record : out Bounded_Strings.Incoming_Record)
     with
       Depends => (Out_Record => (Header, Payload))
   is
   begin
      Out_Record := Bounded_Strings.Incoming_Record'(Text => Bounded_Strings.Incoming_Type'(others => ' '), Size => 0);

      for I in Bounded_Strings.Incoming_Index_Type range Packet_Header_Index'First ..
        Packet_Header_Index'Last
      loop
         Out_Record.Text(I) := Header (I);
      end loop;

      for I in Bounded_Strings.Incoming_Index_Type range Bounded_Strings.Payload_Index_Type'First .. Payload.Size loop
         Out_Record.Text(I + Header'Last) := Payload.Text(I);
      end loop;

      Out_Record.Size := Packet_Header_Index'Last + Payload.Size;
   end Assemble_Packet;


   -- Converts a header into an assembled message type. Mostly to make me feel better about
   -- other parts of the code
   procedure Assemble_Packet_No_Payload
     (Header     : in Packet_Header;
      Out_Record : out Bounded_Strings.Incoming_Record)
     with
       Depends => (Out_Record => Header)
   is
   begin
      Out_Record := Bounded_Strings.Incoming_Record'
        (Text => Bounded_Strings.Incoming_Type'(others => ' '), Size => 0);

      for I in Bounded_Strings.Incoming_Index_Type range Packet_Header_Index'First ..
        Packet_Header_Index'Last
      loop
         Out_Record.Text(I) := Header(I);
      end loop;

      Out_Record.Size := Packet_Header_Index'Last;
   end Assemble_Packet_No_Payload;


   -- Creates a packet header. Message size can range from 0 .. 255, but would be 0 if only
   -- sending a command without a message. The header is only constructred with the Incoming
   -- command value, meaning these headers must go out to the radio. The header checksum is also
   -- computed here with a call to the appropriate subprogram.
   --
   procedure Create_Packet_Header
     (Message_Size : in Utility.Byte_Type;
      Command      : in Commands;
      Header       : out Packet_Header)
     with
       Depends => (Header => (Command, Message_Size))
   is
      Most_Significant_Byte   : constant Utility.Byte_Type := 0;
      Least_Significant_Byte  : Utility.Byte_Type;
      Checksum_A              : Utility.Byte_Type;
      Checksum_B              : Utility.Byte_Type;
      Line_Without_Sync       : Bounded_Strings.Incoming_Type;
      Size_Without_Sync       : Bounded_Strings.Incoming_Size_Type;
   begin

      Header := Packet_Header'(others => ' ');

      Least_Significant_Byte    := Message_Size;
      Header(Header'First)     := Sync_1;
      Header(Header'First + 1) := Sync_2;
      Header(Header'First + 2) := Incoming_Command;
      Header(Header'First + 3) := Commands_Array(Command);
      Header(Header'First + 4) := Character'Val(Most_Significant_Byte);
      Header(Header'First + 5) := Character'Val(Least_Significant_Byte);

      Strip_Sync_Characters
        (Header, Header_Without_Sync_Range'Last, Line_Without_Sync, Size_Without_Sync);
      Calculate_Checksum(Line_Without_Sync, Size_Without_Sync, Checksum_A, Checksum_B);

      Header(Header'First + 6) := Character'Val(Checksum_A);
      Header(Header'First + 7) := Character'Val(Checksum_B);
   end Create_Packet_Header;


   -- Creates a full size message packet per the specification of the Helium Radio. Takes an
   -- already created packet header as an in param and a payload. Calls assemble message to put
   -- the header and the payload together. Computes the checksum over that data (without the
   -- sync characters). Appends the checksum on to the end of the record and passes out the
   -- final_packet.
   --
   procedure Create_Full_Size_Message_Packet
     (Header       : in  Packet_Header;
      Message      : in  Bounded_Strings.Payload_Record;
      Final_Packet : out Bounded_Strings.Incoming_Record)
     with
       Depends => (Final_Packet => (Header, Message))
   is
      Checksum_C        : Utility.Byte_Type;
      Checksum_D        : Utility.Byte_Type;
      Line_Without_Sync : Bounded_Strings.Incoming_Type;
      Size_Without_Sync : Bounded_Strings.Incoming_Size_Type;
   begin
      Assemble_Packet(Header, Message, Final_Packet);
      Strip_Sync_Characters
        (Final_Packet.Text, Final_Packet.Size, Line_Without_Sync, Size_Without_Sync);
      Calculate_Checksum(Line_Without_Sync, Size_Without_Sync, Checksum_C, Checksum_D);

      -- Append checksums on to the end of the message
      Final_Packet.Text(Packet_Header'Last + (Message.Size + 1)) := Character'Val(Checksum_C);
      Final_Packet.Text(Packet_Header'Last + (Message.Size + 2)) := Character'Val(Checksum_D);
      Final_Packet.Size := Final_Packet.Size + Number_Of_Payload_Checksum_Bytes;
   end Create_Full_Size_Message_Packet;


   -- Takes two checksum values and the accompanying message. Calculates two new checksum values
   -- with the message and compares the new values to the old values. Returns true of the
   -- checksums match (are valid). Returns false if the checksums do not match.
   --
   function Verify_Checksum
     (Line                : in Bounded_Strings.Incoming_Type;
      Size                : in Bounded_Strings.Incoming_Size_Type;
      Received_Checksum_A : in Utility.Byte_Type;
      Received_Checksum_B : in Utility.Byte_Type) return Boolean
   is
      New_Checksum_A : Utility.Byte_Type;
      New_Checksum_B : Utility.Byte_Type;
   begin
      Calculate_Checksum(Line, Size, New_Checksum_A, New_Checksum_B);
      return New_Checksum_A = Received_Checksum_A and New_Checksum_B = Received_Checksum_B;
   end Verify_Checksum;


   -- Takes a string and a size and passes to Bounded_Strings.Prepare_Outgoing_Line. Sends the
   -- returned value of that function call out to the serial port.
   --
   procedure Data_Out
     (Data         : in  String;
      Message_Size : in  Natural;
      Write_Status : out Boolean)
     with
       Global => (In_Out => Radio_Port.State),
       Depends => ((Radio_Port.State, Write_Status) =>
                     (Data, Message_Size, Radio_Port.State))
   is
      Outgoing_Record  : Bounded_Strings.Outgoing_Record;
   begin

      Outgoing_Record := Bounded_Strings.Prepare_Outgoing_Line (Data, Message_Size);
      Radio_Port.Put_Line_To_Radio(Outgoing_Record, Write_Status);
   end Data_Out;


   -- Validates the integrity of the incoming message record header by comparing the given
   -- checksum with a checksum calculated over the received header. Sync characters are ignored.
   --
   function Is_Valid_Header(In_Header : in Packet_Header) return Boolean is
      Valid_Result      : Boolean;
      Is_Valid_Checksum : Boolean;
      In_Checksum_A_C   : Utility.Byte_Type;
      In_Checksum_B_D   : Utility.Byte_Type;
      Line_Without_Sync : Bounded_Strings.Incoming_Type;
      Size_Without_Sync : Bounded_Strings.Incoming_Size_Type;
   begin

      -- Verify that the incoming message has the correct incoming type command. Although the
      -- command is actually an "outgoing command" because it is "out" from the radio.
      --
      if  In_Header(In_Header'First + 2) /= Outgoing_Command then
         Valid_Result := False;
      else
         Strip_Sync_Characters
           (In_Header, Header_Without_Sync_Range'Last, Line_Without_Sync, Size_Without_Sync);

         In_Checksum_A_C   := Character'Pos(In_Header (Packet_Header_Index'Last - 1));
         In_Checksum_B_D   := Character'Pos(In_Header (Packet_Header_Index'Last));
         Is_Valid_Checksum := Verify_Checksum
           (Line_Without_Sync, Size_Without_Sync, In_Checksum_A_C, In_Checksum_B_D);

         if Is_Valid_Checksum then
            Valid_Result := True;
         else
            Valid_Result := False;
         end if;
      end if;
      return Valid_Result;
   end Is_Valid_Header;


   -- Validates the integrity of the incoming payload by verifying the receieved payload
   -- checksum. Returns True if checksums match.
   --
   function Is_Valid_Payload(In_Record : Bounded_Strings.Incoming_Record) return Boolean is
      Line_Without_Sync      : Bounded_Strings.Incoming_Type;
      Line_Without_Sync_Size : Bounded_Strings.Incoming_Size_Type;
      Checksum_C             : Utility.Byte_Type;
      Checksum_D             : Utility.Byte_Type;
      Is_Valid               : Boolean;
      Is_Valid_Checksum      : Boolean;
   begin
      Strip_Sync_Characters
        (In_Record.Text, In_Record.Size, Line_Without_Sync, Line_Without_Sync_Size);

      Checksum_C  := Character'Pos(Line_Without_Sync (Line_Without_Sync_Size - 1));
      Checksum_D  := Character'Pos(Line_Without_Sync (Line_Without_Sync_Size));

      Is_Valid_Checksum := Verify_Checksum
        (Line_Without_Sync,
         Line_Without_Sync_Size - Number_Of_Payload_Checksum_Bytes,
         Checksum_C, Checksum_D);

      if Is_Valid_Checksum then
         Is_Valid := True;
      else
         Is_Valid := False;
      end if;
     return Is_Valid;
   end Is_Valid_Payload;


   -- Calls Is_Valid_Header. If the header passes the integrity check then the header is checked
   -- for one of the Acknowledement types - Ack, Not_Ack, Payload_Exists. If Is_Valid_Header
   -- returns false then the returned Acknowledgement from this subprogram is Checksum_Fail.
   --
   procedure Process_Header(Header : in Packet_Header; Result : out Acknowledgement)
     with
       Depends => (Result => Header)
   is
      Is_Valid : Boolean;
   begin
      Is_Valid := Is_Valid_Header(Header);
      if not Is_Valid then
         Result := Checksum_Fail;
      else
         if Header(5) = Character'Val(16#0A#) and Header(6) = Character'Val(16#0A#) then
            Result := Ack;
         elsif Header(5) = Character'Val(16#FF#) and Header(6) = Character'Val(16#FF#) then
            Result := Not_Ack;
         else
            Result := Payload_Exists;
         end if;
      end if;
   end Process_Header;


   -- Helper for Receive_Incoming. Part of the state machine to recieve incoming headers
   procedure Scan_For_H
     (Ch              : in     Character;
      Header          : in out Packet_Header;
      Header_Location : in out Packet_Header_Index)
     with
       Global => (In_Out => Scanner_State),
       Depends => ((Header, Header_Location) =>+ (Ch, Header_Location),
                   Scanner_State =>+ Ch)
   is
   begin
      if Ch = Sync_1 then
         Header(Header_Location) := Ch;
         Scanner_State           := Search_For_E;
         Header_Location         := Header_Location + 1;
      end if;
   end Scan_For_H;


   -- Helper for Receive_Incoming. Part of the state machine to recieve incoming headers
   procedure Scan_For_E
     (Ch              : in Character;
      Header          : in out Packet_Header;
      Header_Location : in out Packet_Header_Index)
     with
       Global => (In_Out => Scanner_State),
       Depends => ((Header, Header_Location) =>+ (Ch, Header_Location),
                   Scanner_State =>+ Ch)
   is
   begin
      if Ch = Sync_2 then
         Header(Header_Location) := Ch;
         Scanner_State           := Gather_Header;
         if Header_Location /= Packet_Header_Index'Last then
            Header_Location := Header_Location + 1;
         end if;
      else
         -- If sync_2 isn't found then move pointer back to beginning check if the current
         -- character is sync_1. If it is sync_1 assume that previous sync_1 was corrupt and
         -- this is the correct sync_1 otherwise assume all data is corrupt and start over.
         Header_Location := Packet_Header_Index'First;
         if Ch /= Sync_1 then
            Scanner_State := Search_For_H;
         else
            Header(Header_Location) := Ch;
            Header_Location         := Header_Location + 1;
         end if;
      end if;
   end Scan_For_E;


   -- Helper for Receive_Incoming. Part of the state machine to recieve incoming headers
   procedure Scan_Header
     (Ch               : in     Character;
      Header           : in out Packet_Header;
      Header_Location  : in out Packet_Header_Index)
     with
       Global => (In_Out => Scanner_State),
       Depends => (Header =>+ (Ch, Header_Location),
                   (Header_Location, Scanner_State) =>+ Header_Location)
   is
   begin
      Header(Header_Location) := Ch;
      if Header_Location = Header'Last then
         Scanner_State  := Done;
      else
         Header_Location := Header_Location + 1;
      end if;
   end Scan_Header;


   -- Part of the state machine to receive incoming messages
   procedure Scan_Message
     (Ch               : in Character;
      Expected_Size    : in Bounded_Strings.Payload_Index_Type;
      Message          : in out Bounded_Strings.Payload_Record;
      Message_Location : in out Bounded_Strings.Payload_Index_Type)
     with
       Global => (In_Out => Scanner_State),
       Depends => (Message =>+ (Ch, Message_Location),
                   Message_Location =>+ null,
                   Scanner_State =>+ (Expected_Size, Message_Location))
   is
   begin
      Message.Text(Message_Location) := Ch;
      Message.Size := Message.Size + 1;
      if Message_Location = Message.Text'Last then
         Message_Location := Bounded_Strings.Payload_Index_Type'First;
      else
         Message_Location := Message_Location + 1;
      end if;

      if Message_Location > Expected_Size then
         Scanner_State := Done;
      end if;
   end Scan_Message;


   -- This function returns the size of the incoming message. Message sizes should never be
   -- greater than 255. If one is, the size is truncated to 255.
   --
   function Get_Payload_Size(Header : Packet_Header) return Bounded_Strings.Incoming_Size_Type
     with
       Post => (Get_Payload_Size'Result <= 255)
   is
      Result : Bounded_Strings.Incoming_Size_Type;
   begin
      -- Header size is really 2 bytes, should fix this even if shouldn't be more than 255!
      Result := Bounded_Strings.Incoming_Size_Type'(Character'Pos(Header(6)));
      if Result > 255 then
         Result := 255;
      end if;
      return Result;
   end Get_Payload_Size;


   -- Gets data from the Radio_Port. Looks for and constructs a header if it exists.
   procedure Receive_Incoming_Header
     (Header : out Packet_Header)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, Utility.Hardware),
                  Output => Scanner_State),
       Depends => ((Header, Radio_Port.State, Scanner_State) => Radio_Port.State,
                   Utility.Hardware =>+ Radio_Port.State,
                   null             => Utility.Timer_Done)
   is
      Ch              : Character;
      Status          : Boolean;
      Header_Location : Packet_Header_Index := Packet_Header_Index'First;
   begin
      Header        := Packet_Header'(others => ' ');
      Scanner_State := Search_For_H;

      while Scanner_State /= Done loop
         Radio_Port.Get_From_Radio(Ch, Status);
         if not Status then
            Scanner_State := Done;
         end if;
         case Scanner_State is
            when Search_For_H =>
               Scan_For_H(Ch, Header, Header_Location);
            when Search_For_E =>
               Scan_For_E(Ch, Header, Header_Location);
            when Gather_Header =>
               Scan_Header(Ch, Header, Header_Location);
            when others =>
               Scanner_State := Done;
         end case;
         exit when Scanner_State = Done;
      end loop;
   end Receive_Incoming_Header;


   -- Takes an already received header as in param. If the state machine is in state
   -- Gather_Message then the payload is gathered and creates an incoming message with the
   -- header and payload
   --
   procedure Receive_Incoming_Payload
     (Header : in  Packet_Header; Out_Message : out Bounded_Strings.Payload_Record)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, Utility.Hardware),
                  Output => Scanner_State),
       Depends => ((Out_Message, Radio_Port.State, Scanner_State) =>
                     (Header, Radio_Port.State),
                   Utility.Hardware =>+ (Header,Radio_Port.State),
                   null             => Utility.Timer_Done)
   is
      Ch               : Character;
      Status           : Boolean;
      Message_Location : Bounded_Strings.Payload_Index_Type :=
        Bounded_Strings.Payload_Index_Type'First;
      Expected_Size    : Bounded_Strings.Payload_Index_Type;
   begin
      Scanner_State := Gather_Message;
      Out_Message   := Bounded_Strings.Payload_Record'
        (Text => Bounded_Strings.Payload_Type'(others => ' '), Size => 0);
      Expected_Size := Get_Payload_Size(Header);

      while Scanner_State /= Done loop
         Radio_Port.Get_From_Radio(Ch, Status);
         if not Status then
            Scanner_State := Done;
         end if;
         case Scanner_State is
            when Gather_Message =>
               Scan_Message(Ch, Expected_Size,  Out_Message, Message_Location);
            when others =>
               Scanner_State := Done;
         end case;
         exit when Scanner_State = Done;
      end loop;
   end Receive_Incoming_Payload;


   -- Reads one byte from the buffer and appends it to the packet. This is used to read the last
   -- two payload checksum bytes.
   procedure Append_Incoming_Byte(Packet : in out  Bounded_Strings.Incoming_Record)
     with
       Global => (Input => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, Utility.Hardware)),
       Depends => ((Packet, Radio_Port.State) =>+ Radio_Port.State,
                   Utility.Hardware =>+ null,
                   null => Utility.Timer_Done),
       Pre => (Packet.Size < Bounded_Strings.Incoming_Index_Type'Last)
   is
      Ch      : Character;
      Success : Boolean;
   begin
      Radio_Port.Get_From_Radio(Ch, Success);
      if Success then
         Packet.Size := Packet.Size + 1;
         Packet.Text(Packet.Size) := Ch;
      end if;
   end Append_Incoming_Byte;


   -- Strips away the header and strips away the payload checksum. The extracted message is
   -- returned as a payload record.
   procedure Extract_Payload_From_Packet
     (In_Record   : in  Bounded_Strings.Incoming_Record;
      Out_Message : out Bounded_Strings.Payload_Record)
     with
       Depends => (Out_Message => In_Record)
   is
   begin
      Out_Message := Bounded_Strings.Payload_Record'
        (Text => Bounded_Strings.Payload_Type'(others => ' '), Size => 0);

      for I in Bounded_Strings.Incoming_Index_Type range
        Packet_Header_Index'Last + 1 .. Bounded_Strings.Incoming_Index_Type'Last
      loop
         Out_Message.Text(I - Packet_Header_Index'Last) := In_Record.Text(I);
         exit when I = In_Record.Size - Number_Of_Payload_Checksum_Bytes;
      end loop;
      Out_Message.Size :=
        (In_Record.Size - Packet_Header_Index'Last) - Number_Of_Payload_Checksum_Bytes;
   end Extract_Payload_From_Packet;


   procedure Send_Packet
     (Command       : in  Send_Header_Only_Commands;
      Write_Success : out Boolean)
   is
      Header : Packet_Header;
   begin
      Create_Packet_Header(Size_Zero, Command, Header);
      Data_Out(Header, Packet_Header_Index'Last, Write_Success);
   end Send_Packet;


   -- Sends a packet with a payload.
   procedure Send_Packet_With_Payload
     (Command       : in  Send_Message_Commands_No_Message_Response;
      Payload       : in  Bounded_Strings.Payload_Record;
      Write_Success : out Boolean)
   is
      Header          : Packet_Header;
      Complete_Packet : Bounded_Strings.Incoming_Record;
   begin
      Create_Packet_Header(Utility.Byte_Type(Payload.Size), Command, Header);
      Create_Full_Size_Message_Packet(Header, Payload, Complete_Packet);
      Data_Out(Complete_Packet.Text, Complete_Packet.Size, Write_Success);
   end Send_Packet_With_Payload;


   -- Reads in the next incoming packet header and verifies checksums. If theres a payload, it
   -- reads in the payload and 2 payload checksum bytes and verifies the payload checksums. A
   -- fully assembled packet (with or without a payload) will be returned. Ack_Type will hold
   -- the information about the packet like if its Ack or Nack, if there is a payload or if the
   -- checksums failed.
   procedure Read_Incoming_Packet
     (Packet   : out Bounded_Strings.Incoming_Record;
      Ack_Type : out Acknowledgement)
     with
       Global => (Input => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, Utility.Hardware),
                  Output => Scanner_State),
       Depends => ((Ack_Type, Packet, Radio_Port.State, Scanner_State) => Radio_Port.State,
                   Utility.Hardware =>+ Radio_Port.State,
                   null             => Utility.Timer_Done)
   is
      Header  : Packet_Header;
      Payload : Bounded_Strings.Payload_Record;
   begin
      Receive_Incoming_Header(Header);
      Process_Header(Header,Ack_Type);

      if Ack_Type = Payload_Exists then
         Receive_Incoming_Payload(Header, Payload);
         Assemble_Packet(Header, Payload, Packet);

         -- append last two bytes for payload checksum bytes
         Append_Incoming_Byte(Packet);
         Append_Incoming_Byte(Packet);

         -- check the payload checksums, If they don't fail, set the packets type to Ack
         if not Is_Valid_Payload(Packet) then
            Ack_Type := Checksum_Fail;
         else
            Ack_Type := Ack;
            Packet.Size := Packet.Size - 2;
         end if;
      else
         -- if no payload, make the packet just a header
         Assemble_Packet_No_Payload(Header, Packet);
      end if;
   end Read_Incoming_Packet;


   -- This procedure is used when only the ack/nack response needs to be returned
   procedure Scan_For_Packet(Command : in Commands; Ack_Nack_Response : out Acknowledgement)
     with
       Refined_Global => (Input  => Utility.Timer_Done,
                          In_Out => (Radio_Port.State, Scanner_State, Utility.Hardware)),
       Refined_Depends => ((Ack_Nack_Response, Radio_Port.State) => (Command, Radio_Port.State),
                           (Scanner_State, Utility.Hardware) =>+ (Command, Radio_Port.State),
                           null => Utility.Timer_Done)
   is
      Packet        : Bounded_Strings.Incoming_Record;
      Done_Scanning : Boolean := False;
   begin
      -- initialize Ack_Nack_Response
      Ack_Nack_Response := No_Packet;
      while not Done_Scanning loop
         Read_Incoming_Packet(Packet, Ack_Nack_Response);

         -- if the returned command is blank (32), the incoming buffer is empty
         if Character'Pos(Packet.Text(4)) = 32 then
            Ack_Nack_Response := No_Packet; -- return No_Packet
            Done_Scanning := True;

            -- if its the packet we are looking for then we are done
         elsif Packet.Text(4) = Commands_Array(Command) then
            Done_Scanning := True;

         -- This should never happen but it could be caused if another packet came back in
         -- the response buffer first.
         else
            Ack_Nack_Response := Receive_Fail;
         end if;
         exit when Done_Scanning;
      end loop;
   end Scan_For_Packet;


   procedure Scan_For_Packet_With_Payload
     (Command           : in  Commands;
      Payload           : out Bounded_Strings.Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
     with
       Refined_Global => (Input => Utility.Timer_Done,
                          In_Out => (Radio_Port.State, Scanner_State, Utility.Hardware)),
       Refined_Depends => ((Ack_Nack_Response, Payload, Radio_Port.State) =>
                             (Command, Radio_Port.State),
                           (Scanner_State, Utility.Hardware) =>+ (Command, Radio_Port.State),
                           null => Utility.Timer_Done)
   is
      Packet        : Bounded_Strings.Incoming_Record;
      Done_Scanning : Boolean := False;
   begin
      -- initialize Ack_Nack_Response & payload
      Ack_Nack_Response := No_Packet;
      Payload := Bounded_Strings.Payload_Record'
        (Text => Bounded_Strings.Payload_Type'(others => ' '), Size => 1);
      while not Done_Scanning loop
         Read_Incoming_Packet(Packet,Ack_Nack_Response);

         -- if the returned command is blank (32), the incoming buffer is empty
         if Character'Pos(Packet.Text(4)) = 32 then
            Ack_Nack_Response := No_Packet; -- return No_Packet
            Done_Scanning := True;

         -- if its the packet we are looking for then we are done
         elsif Packet.Text(4) = Commands_Array(Command) then
            Extract_Payload_From_Packet(Packet, Payload);
            Done_Scanning := True;

         -- This should never happen but it could be caused if another packet came back in
         -- the response buffer first. This can happen when scanning for the wrong kind of
         -- packet.
         else
            Ack_Nack_Response := Receive_Fail;
            -- maybe it would be good to have this handle other packets somehow? or throw a
            -- bunch of errors?
         end if;
         exit when Done_Scanning;
      end loop;
   end Scan_For_Packet_With_Payload;


   procedure No_Op(Ack_Nack_Response : out Acknowledgement)
     with
       Refined_Global => (Input => Utility.Timer_Done,
                          In_Out => (Radio_Port.State, Scanner_State, Utility.Hardware)),
       Refined_Depends => ((Ack_Nack_Response, Radio_Port.State) =>
                             Radio_Port.State,
                           (Scanner_State, Utility.Hardware) =>+ Radio_Port.State,
                           null             => Utility.Timer_Done)
   is
      Packet_Sent : Boolean;
   begin
      -- send transmit packet with
      Send_Packet(No_Op_Command,
                  Packet_Sent);
      --Time.Sleep(Utility.Millisecond_Type(10));
      if not Packet_Sent then
         Ack_Nack_Response := Transmit_Fail;
      else
         Scan_For_Packet(No_Op_Command,Ack_Nack_Response);
      end if;
   end No_Op;


   procedure Reset(Ack_Nack_Response : out Acknowledgement)
     with
       Refined_Global => (Input => Utility.Timer_Done,
                          In_Out => (Radio_Port.State, Scanner_State, Utility.Hardware)),
       Refined_Depends => ((Ack_Nack_Response, Radio_Port.State) => Radio_Port.State,
                           (Scanner_State, Utility.Hardware) =>+ Radio_Port.State,
                           null                              => Utility.Timer_Done)
   is
      Packet_Sent : Boolean;
   begin
      -- send transmit packet with
      Send_Packet(Reset_Command,
                  Packet_Sent);
      if not Packet_Sent then
         Ack_Nack_Response := Transmit_Fail;
      else
         Scan_For_Packet(Reset_Command,Ack_Nack_Response);
      end if;
   end Reset;


   procedure Transmit_On
     with
       Refined_Global => (Output => Transmitter_State),
       Refined_Depends => (Transmitter_State => null)
   is
   begin
      Transmitter_State := True;
   end Transmit_On;


   procedure Transmit_Off
     with
       Refined_Global => (Output => Transmitter_State),
       Refined_Depends => (Transmitter_State => null)
   is
   begin
      Transmitter_State := False;
   end Transmit_Off;


   procedure Transmit
     (Message           : in Bounded_Strings.Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
     with
       Refined_Global => (Input => (Transmitter_State, Utility.Timer_Done),
                          In_Out => (Radio_Port.State, Scanner_State, Utility.Hardware)),
       Refined_Depends => ((Ack_Nack_Response, Radio_Port.State) =>
                             (Message, Radio_Port.State, Transmitter_State),
                           (Scanner_State, Utility.Hardware) =>+
                             (Message, Radio_Port.State, Transmitter_State),
                           null => Utility.Timer_Done)
   is
      Packet_Sent : Boolean;
   begin
      Ack_Nack_Response := Transmit_Fail;
      if Transmitter_State then
         -- send transmit packet with
         Send_Packet_With_Payload(Transmit_Command,
                                  Message,
                                  Packet_Sent);
         if not Packet_Sent then
            Ack_Nack_Response := Transmit_Fail;
         else
            Scan_For_Packet(Transmit_Command,Ack_Nack_Response);
         end if;
      end if;
   end Transmit;


   procedure Get_Transceiver_Configuration
     (Transceiver_Configuration : out Bounded_Strings.Payload_Record;
      Ack_Nack_Response         : out Acknowledgement)
     with
       Refined_Global => (Input => (Utility.Timer_Done),
                        In_Out => (Radio_Port.State, Scanner_State, Utility.Hardware)),
       Refined_Depends => ((Ack_Nack_Response, Radio_Port.State, Transceiver_Configuration) =>
                          Radio_Port.State,
                       (Scanner_State, Utility.Hardware) =>+ Radio_Port.State,
                       null => Utility.Timer_Done)
   is
      Packet_Sent              : Boolean;
   begin
      -- initialize Transceiver_Configuration
      Transceiver_Configuration := Bounded_Strings.Prepare_Payload_Line(" ",1);

      -- send transmit packet with
      Send_Packet(Get_Transceiver_Configuration_Command,
                               Packet_Sent);
      if not Packet_Sent then
         Ack_Nack_Response := Transmit_Fail;
      else
         Scan_For_Packet_With_Payload
           (Get_Transceiver_Configuration_Command,
            Transceiver_Configuration,
            Ack_Nack_Response);

         if Ack_Nack_Response = Ack and Transceiver_Configuration.Size /= 32 then
            -- some weird payload was received.. send a receive_fail ack because somethings
            -- probably broken
            Ack_Nack_Response := Receive_Fail;
         end if;

      end if;
   end Get_Transceiver_Configuration;


   -- Set radio configuration.
   procedure Set_Transceiver_Configuration
     (Configuration     : in  Bounded_Strings.Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
     with
       Refined_Global => (Input => Utility.Timer_Done,
                          In_out => (Radio_Port.State, Scanner_State, Utility.Hardware)),
       Refined_Depends => ((Ack_Nack_Response, Radio_Port.State) =>
                             (Configuration, Radio_Port.State),
                           (Scanner_State, Utility.Hardware) =>+
                             (Configuration, Radio_Port.State),
                           null => Utility.Timer_Done)
   is
      Packet_Sent : Boolean;
   begin
      -- send a Set_Transceiver_Configuration packet with payload
      Send_Packet_With_Payload
        (Set_Transceiver_Configuration_Command, Configuration, Packet_Sent);

      if not Packet_Sent then
         Ack_Nack_Response := Transmit_Fail;
      else
         Scan_For_Packet(Set_Transceiver_Configuration_Command, Ack_Nack_Response);
      end if;
   end Set_Transceiver_Configuration;


   procedure Get_Telemetry
     (Telemetry_Data : out Bounded_Strings.Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
     with
       Refined_Global => (Input => Utility.Timer_Done,
                          In_Out => (Radio_Port.State, Scanner_State, Utility.Hardware)),
       Refined_Depends => ((Ack_Nack_Response, Radio_Port.State, Telemetry_Data) =>
                                 Radio_Port.State,
                           (Scanner_State, Utility.Hardware) =>+ Radio_Port.State,
                           null => Utility.Timer_Done)
   is
      Packet_Sent : Boolean;
   begin
      -- Initialize Telemetry Data
      Telemetry_Data := Bounded_Strings.Prepare_Payload_Line(" ",1);

      -- send telemetry packet asking for some data
      Send_Packet(Telemetry_Command,
                  Packet_Sent);
      if not Packet_Sent then
         Ack_Nack_Response := Transmit_Fail;
      else
         Scan_For_Packet_With_Payload(Telemetry_Command, Telemetry_Data, Ack_Nack_Response);

         if Ack_Nack_Response = Ack and Telemetry_Data.Size /= 18 then
               Ack_Nack_Response := Receive_Fail;
         end if;
      end if;
   end Get_Telemetry;


   procedure Write_Configuration_To_Flash
     (Data              : in Bounded_Strings.Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
     with
       Refined_Global => (Input => Utility.Timer_Done,
                          In_Out => (Radio_Port.State, Scanner_State, Utility.Hardware)),
       Refined_Depends => ((Ack_Nack_Response, Radio_Port.State) => (Data, Radio_Port.State),
                           (Scanner_State, Utility.Hardware) =>+ (Data, Radio_Port.State),
                           null => Utility.Timer_Done)
   is
      Packet_Sent : Boolean;
   begin
     Send_Packet_With_Payload(Write_Flash_Command,
                              Data,
                              Packet_Sent);
      if not Packet_Sent then
         Ack_Nack_Response := Transmit_Fail;
      else
         Scan_For_Packet(Write_Flash_Command, Ack_Nack_Response);
      end if;
   end Write_Configuration_To_Flash;

end Radio;
