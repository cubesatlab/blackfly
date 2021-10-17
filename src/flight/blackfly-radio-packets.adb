--------------------------------------------------------------------------------
-- FILE   : blackfly-radio-packets.adb
-- SUBJECT: Implementation of the radio packet management subprograms.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College and the University of Vermont
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.Bounded_Strings;
use  CubedOS.Lib;

package body Blackfly.Radio.Packets is

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
   subtype Packet_Header_Index       is Positive range 1 .. 8;
   subtype Packet_Header             is String(Packet_Header_Index);
   subtype Full_Packet_Size is
     Positive range 1 .. Packet_Header_Index'Last + (Maximum_Payload_Length + 2);
   subtype Full_Packet               is String(Full_Packet_Size);
   subtype String_Payload_Range      is Positive range 1 .. Maximum_Payload_Length + 1;
   subtype String_Payload            is String(String_Payload_Range);
   subtype Header_Without_Sync_Range is Positive range 1 .. 6;
   subtype Header_Without_Sync       is String(Header_Without_Sync_Range);

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

   procedure Initialize(Success : out Boolean) is
   begin
      Open;
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


   -- Removes the sync characters from a given string.
   -- Returns the new line and the size as out parameters.
   procedure Strip_Sync_Characters
     (Line     : in  Bounded_Strings.Bounded_String;
      Out_Line : out Bounded_Strings.Bounded_String)
   is
   begin
      Bounded_Strings.Clear(Out_Line);
      for I in Number_Of_Sync_Characters + 1 .. Bounded_Strings.Length(Line) loop
         Bounded_Strings.Append(Out_Line, Bounded_Strings.Element(Line, I));
      end loop;
   end Strip_Sync_Characters;


   -- Get the checksum for a given string.
   procedure Calculate_Checksum
     (Message    : in  Bounded_Strings.Bounded_String;
      Checksum_A : out Byte;
      Checksum_B : out Byte)
   is
   begin
      Checksum_A := 0;
      Checksum_B := 0;
      for I in 1 .. Bounded_Strings.Length(Message) loop
         Checksum_A := Checksum_A + Character'Pos(Bounded_Strings.Element(Message, I));
         Checksum_B := Checksum_B + Checksum_A;
      end loop;
   end Calculate_Checksum;


   -- Appends a payload to a packet header and creates a single record.
   procedure Assemble_Packet
     (Header     : in  Packet_Header;
      Payload    : in  Payload_Record;
      Out_Record : out Incoming_Record)
   is
   begin
      Bounded_Strings.Clear (Out_Record);
      Bounded_Strings.Append(Out_Record, Header);
      Bounded_Strings.Append(Out_Record, Payload);
   end Assemble_Packet;


   -- Converts a header into an assembled message.
   procedure Assemble_Packet_No_Payload
     (Header     : in  Packet_Header;
      Out_Record : out Incoming_Record)
   is
   begin
      Bounded_Strings.Clear (Out_Record);
      Bounded_Strings.Append(Out_Record, Header);
   end Assemble_Packet_No_Payload;


   -- Creates a packet header. Message size can range from 0 .. 255, but would be 0 if only
   -- sending a command without a message. The header is only constructred with the Incoming
   -- command value, meaning these headers must go out to the radio. The header checksum is also
   -- computed here with a call to the appropriate subprogram.
   procedure Create_Packet_Header
     (Message_Size : in  Byte;
      Command      : in  Commands;
      Header       : out Packet_Header)
   is
      Most_Significant_Byte  : constant Byte := 0;
      Least_Significant_Byte : Byte;
      Checksum_A             : Byte;
      Checksum_B             : Byte;
      Line                   : Incoming_Record;
      Line_Without_Sync      : Incoming_Record;
   begin
      Header := Packet_Header'(others => ' ');

      Least_Significant_Byte   := Message_Size;
      Header(Header'First)     := Sync_1;
      Header(Header'First + 1) := Sync_2;
      Header(Header'First + 2) := Incoming_Command;
      Header(Header'First + 3) := Commands_Array(Command);
      Header(Header'First + 4) := Character'Val(Most_Significant_Byte);
      Header(Header'First + 5) := Character'Val(Least_Significant_Byte);

      Bounded_Strings.Append(Line, Header(Header'First .. Header'First + 5));
      Strip_Sync_Characters(Line, Line_Without_Sync);
      Calculate_Checksum(Line_Without_Sync, Checksum_A, Checksum_B);

      Header(Header'First + 6) := Character'Val(Checksum_A);
      Header(Header'First + 7) := Character'Val(Checksum_B);
   end Create_Packet_Header;


   -- Creates a full size message packet per the specification of the Helium Radio. Takes an
   -- already created packet header as an in param and a payload. Calls assemble message to put
   -- the header and the payload together. Computes the checksum over that data (without the
   -- sync characters). Appends the checksum on to the end of the record and passes out the
   -- final_packet.
   procedure Create_Full_Size_Message_Packet
     (Header       : in  Packet_Header;
      Message      : in  Payload_Record;
      Final_Packet : out Incoming_Record)
   is
      Checksum_C        : Byte;
      Checksum_D        : Byte;
      Line_Without_Sync : Incoming_Record;
   begin
      Assemble_Packet(Header, Message, Final_Packet);
      Strip_Sync_Characters(Final_Packet, Line_Without_Sync);
      Calculate_Checksum(Line_Without_Sync, Checksum_C, Checksum_D);

      -- Append checksums on to the end of the message.
      Bounded_Strings.Append(Final_Packet, Character'Val(Checksum_C));
      Bounded_Strings.Append(Final_Packet, Character'Val(Checksum_D));
   end Create_Full_Size_Message_Packet;


   -- Takes two checksum values and the accompanying message. Calculates two new checksum values
   -- with the message and compares the new values to the old values. Returns true of the
   -- checksums match (are valid). Returns false if the checksums do not match.
   --
   function Verify_Checksum
     (Line                : in Incoming_Record;
      Received_Checksum_A : in Byte;
      Received_Checksum_B : in Byte) return Boolean
   is
      New_Checksum_A : Byte;
      New_Checksum_B : Byte;
   begin
      Calculate_Checksum(Line, New_Checksum_A, New_Checksum_B);
      return New_Checksum_A = Received_Checksum_A and New_Checksum_B = Received_Checksum_B;
   end Verify_Checksum;


   -- Takes a string and a size and passes to Prepare_Outgoing_Line. Sends the returned value
   -- of that function call out to the serial port.
   procedure Data_Out
     (Data         : in  String;
      Message_Size : in  Natural;
      Write_Status : out Boolean)
   is
      Outgoing : Outgoing_Record;
   begin
      Outgoing := Prepare_Outgoing_Line(Data, Message_Size);
      Put_Line_To_Radio(Outgoing, Write_Status);
   end Data_Out;


   -- Validates the integrity of the incoming message record header by comparing the given
   -- checksum with a checksum calculated over the received header. Sync characters are ignored.
   function Is_Valid_Header(In_Header : in Packet_Header) return Boolean is
      Valid_Result      : Boolean;
      Is_Valid_Checksum : Boolean;
      In_Checksum_A_C   : Byte;
      In_Checksum_B_D   : Byte;
      Line_Without_Sync : Bounded_Strings.Incoming_Type;
      Size_Without_Sync : Bounded_Strings.Incoming_Size_Type;
   begin

      -- Verify that the incoming message has the correct incoming type command. Although the
      -- command is actually an "outgoing command" because it is "out" from the radio.
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
   function Is_Valid_Payload(In_Record : Incoming_Record) return Boolean is
      Line_Without_Sync      : Incoming_Record;
      Checksum_C             : Byte;
      Checksum_D             : Byte;
      Is_Valid               : Boolean;
      Is_Valid_Checksum      : Boolean;
   begin
      Strip_Sync_Characters(In_Record, Line_Without_Sync);

      Checksum_C  := Character'Pos(Line_Without_Sync_Size - 1);
      Checksum_D  := Character'Pos(Line_Without_Sync(Line_Without_Sync_Size));

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
   procedure Process_Header(Header : in Packet_Header; Result : out Acknowledgement) is
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


   -- Helper for Receive_Incoming. Part of the state machine to recieve incoming headers.
   procedure Scan_Header
     (Ch               : in     Character;
      Header           : in out Packet_Header;
      Header_Location  : in out Packet_Header_Index)
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
     (Ch               : in     Character;
      Expected_Size    : in     Payload_Index_Type;
      Message          : in out Payload_Record;
      Message_Location : in out Payload_Index_Type)
   is
   begin
      Message.Text(Message_Location) := Ch;
      Message.Size := Message.Size + 1;
      if Message_Location = Message.Text'Last then
         Message_Location := Payload_Index_Type'First;
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
   function Get_Payload_Size(Header : Packet_Header) return Incoming_Size_Type
   is
      Result : Incoming_Size_Type;
   begin
      -- Header size is really 2 bytes, should fix this even if shouldn't be more than 255!
      Result := Incoming_Size_Type'(Character'Pos(Header(6)));
      if Result > 255 then
         Result := 255;
      end if;
      return Result;
   end Get_Payload_Size;


   -- Gets data from the Radio_Port. Looks for and constructs a header if it exists.
   procedure Receive_Incoming_Header(Header : out Packet_Header) is
      Ch              : Character;
      Status          : Boolean;
      Header_Location : Packet_Header_Index := Packet_Header_Index'First;
   begin
      Header        := Packet_Header'(others => ' ');
      Scanner_State := Search_For_H;

      while Scanner_State /= Done loop
         Get_From_Radio(Ch, Status);
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
   procedure Receive_Incoming_Payload
     (Header : in  Packet_Header; Out_Message : out Payload_Record)
   is
      Ch               : Character;
      Status           : Boolean;
      Message_Location : Payload_Index_Type :=
        Bounded_Strings.Payload_Index_Type'First;
      Expected_Size    : Payload_Index_Type;
   begin
      Scanner_State := Gather_Message;
      Out_Message   := Payload_Record'
        (Text => Bounded_Strings.Payload_Type'(others => ' '), Size => 0);
      Expected_Size := Get_Payload_Size(Header);

      while Scanner_State /= Done loop
         Get_From_Radio(Ch, Status);
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
   procedure Append_Incoming_Byte(Packet : in out  Incoming_Record) is
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
     (In_Record   : in  Incoming_Record;
      Out_Message : out Payload_Record)
   is
   begin
      Out_Message := Payload_Record'
        (Text => Payload_Type'(others => ' '), Size => 0);

      for I in Incoming_Index_Type range
        Packet_Header_Index'Last + 1 .. Incoming_Index_Type'Last
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
      Payload       : in  Payload_Record;
      Write_Success : out Boolean)
   is
      Header          : Packet_Header;
      Complete_Packet : Incoming_Record;
   begin
      Create_Packet_Header(Byte(Payload.Size), Command, Header);
      Create_Full_Size_Message_Packet(Header, Payload, Complete_Packet);
      Data_Out(Complete_Packet.Text, Complete_Packet.Size, Write_Success);
   end Send_Packet_With_Payload;


   -- Reads in the next incoming packet header and verifies checksums. If theres a payload, it
   -- reads in the payload and 2 payload checksum bytes and verifies the payload checksums. A
   -- fully assembled packet (with or without a payload) will be returned. Ack_Type will hold
   -- the information about the packet like if its Ack or Nack, if there is a payload or if the
   -- checksums failed.
   procedure Read_Incoming_Packet
     (Packet   : out Incoming_Record;
      Ack_Type : out Acknowledgement)
   is
      Header  : Packet_Header;
      Payload : Payload_Record;
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
   is
      Packet        : Incoming_Record;
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
      Payload           : out Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
   is
      Packet        : Incoming_Record;
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


   procedure No_Op(Ack_Nack_Response : out Acknowledgement) is
      Packet_Sent : Boolean;
   begin
      -- Send transmit packet with
      Send_Packet(No_Op_Command, Packet_Sent);

      --Time.Sleep(Utility.Millisecond_Type(10));
      if not Packet_Sent then
         Ack_Nack_Response := Transmit_Fail;
      else
         Scan_For_Packet(No_Op_Command, Ack_Nack_Response);
      end if;
   end No_Op;


   procedure Reset(Ack_Nack_Response : out Acknowledgement) is
      Packet_Sent : Boolean;
   begin
      -- Send transmit packet with
      Send_Packet(Reset_Command, Packet_Sent);
      if not Packet_Sent then
         Ack_Nack_Response := Transmit_Fail;
      else
         Scan_For_Packet(Reset_Command,Ack_Nack_Response);
      end if;
   end Reset;


   procedure Transmit_On is
   begin
      Transmitter_State := True;
   end Transmit_On;


   procedure Transmit_Off is
   begin
      Transmitter_State := False;
   end Transmit_Off;


   procedure Transmit
     (Message           : in  Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
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
     (Transceiver_Configuration : out Payload_Record;
      Ack_Nack_Response         : out Acknowledgement)
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


   procedure Set_Transceiver_Configuration
     (Configuration     : in  Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
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
     (Telemetry_Data : out Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
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
     (Data              : in  Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
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

end Blackfly.Radio.Packets;
