--------------------------------------------------------------------------------
-- FILE   : radio.ads
-- SUBJECT: Specification for control program interface to the radio.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Bounded_Strings;
with Utility;
with Radio_Port;

use type Utility.Byte_Type;

package Blackfly.Radio.Packets
  with Abstract_State => State
is

   ------------------------------------------------------
   -- Scalar Types
   ------------------------------------------------------
   type Byte          is mod 256;
   type Int2          is range -2**15 .. 2**15 - 1;
   type Int4          is range -2**31 .. 2**31 - 1;
   type Unsigned_Int1 is range 0 .. 2**8 - 1;
   type Unsigned_Int2 is range 0 .. 2**16 - 1;
   type Unsigned_Int4 is range 0 .. 2**32 - 1;

   ------------------------------------------------------
   -- Radio Structures
   ------------------------------------------------------

   subtype Index_6  is Positive range 1 .. 6;
   subtype String_6 is String(Index_6);

   type Telemetry_Structure_Type is
      record
         Op_Counter        : Unsigned_Int2;
         MSP430_Temp       : Int2;
         RSSI              : Float;
         PLL_Lock          : Unsigned_Int2;
         Bytes_Received    : Unsigned_Int4;
         Bytes_Transmitted : Unsigned_Int4;
      end record;

   type Radio_Configuration_Type is
      record
         Interface_Baud_Rate : Unsigned_Int1;  -- Radio Interface Baud Rate (9600 = 0x00)
         TX_Power_Amp_Level : Unsigned_Int1;   -- Tx Power Amp level (min = 0x00 max = 0xFF)
         RX_RF_Baud_Rate : Unsigned_Int1;  -- Radio RX RF Baud Rate (9600 = 0x00)
         TX_RF_Baud_Rate : Unsigned_Int1;  -- Radio TX RF baud Rate (9600 = 0x00)
         RX_Modulation : Unsigned_Int1; -- (0x00 = GFSK)
         TX_Modulation : Unsigned_Int1; -- (0x00 = GFSK)
         RX_Frequency : Unsigned_Int4;  -- (Channel Rx Frequency (ex: 450000 = 450 MHz)
         TX_Frequency : Unsigned_Int4;  -- (Channel Tx Frequency (ex: 450000 = 450 MHz)
         Source : String_6;             -- AX.25 Mode Source Call Sign (default NOCALL)
         Destination : String_6;        -- AX.25 Mode Destination Call Sign (default CQ)
         TX_Preamble : Unsigned_Int2;   -- Ax.25 Mode Tx Preamble Byte Length (0x00 = 20 flags)
         TX_Postamble : Unsigned_Int2;  -- Ax.25 Mode Tx Postamble Byte Length (0x00 = 20 flags)
         Function_Config : Int2;        -- Radio configuration discrete behaviors
      end record;

   -- Ack/NotAck
   type Acknowledgement is
     (Ack,
      Not_Ack,
      Transmit_Fail,  -- if a packet does not transmit properly
      Receive_Fail,   -- If the packet data is read and an anomaly occers to break everything like invalid header comes back
      Payload_Exists, -- if a packet header is read and its size bytes indicate that there is a payload
      Checksum_Fail,  -- if the packets checksums dont add up correctly
      No_Packet);     -- if the packet data is read back and there are no acks f


   -- TODO: make this its own child package maybe? Im not sure how I would do this in Spark yet
   --       but it would make the package look a lot cleaner. for now I guess it will just have
   --       _Command after the name
   -- full command list
   type Commands is
     (Transmit_Command,                       -- Send n number of bytes to radio board
      Set_Transceiver_Configuration_Command,  -- Set radio configuration
      Write_Flash_Command,                    -- Write flash with MD5 Checksum
      No_Op_Command,                          -- Increments command processing counter
      Reset_Command,                          -- Reset radio processors and systems.
      Telemetry_Command,                      -- Query a telemetry frame
      Get_Transceiver_Configuration_Command,  -- Read radio configuration
      Received_Data_Command,                  -- Received n number of bytes AX.25 packet
      Invalid_Command);                       -- all other commands

   subtype Send_Message_Commands_No_Message_Response is
    Commands range Transmit_Command .. Write_Flash_Command;

   subtype Send_Header_Only_Commands is
     Commands range No_Op_Command .. Get_Transceiver_Configuration_Command;

   subtype Message_Response_Header_Commands is
     Send_Header_Only_Commands range Telemetry_Command .. Get_Transceiver_Configuration_Command;

   subtype No_Message_Response_Header_Commands is
     Send_Header_Only_Commands range No_Op_Command .. Reset_Command;

   -- takes a character and returns a command associated with it
   function Character_To_Command(Command_Character : in Character) return Commands;


   procedure Initialize(Success : out Boolean)
     with
       Global => (Output => (Radio_Port.State, State)),
       Depends => ((Radio_Port.State, State, Success) => null);


   -- Can be called from outside packages to transmit messages without headers.
   procedure Send_Packet
     (Command           : in  Send_Header_Only_Commands;
      Write_Success     : out Boolean)
     with
       Global => (In_Out => Radio_Port.State),
       Depends => ((Radio_Port.State, Write_Success) => (Command, Radio_Port.State));


   -- Can be called from outside packages to transmit a message, i.e. picture data, gps, data,
   -- etc. Message is a Payload_Record. The text of the message record cannot exceed
   -- Max_Payload_Size
   --
   procedure Send_Packet_With_Payload
     (Command       : in Send_Message_Commands_No_Message_Response;
      Payload       : in Bounded_Strings.Payload_Record;
      Write_Success : out Boolean)
     with
       Global => (In_Out => Radio_Port.State),
       Depends => ((Radio_Port.State, Write_Success) =>
                     (Command,Payload, Radio_Port.State));


   -- This procedure is used when only the ack/nack response needs to be returned
   procedure Scan_For_Packet(Command : in Commands; Ack_Nack_Response : out Acknowledgement)
     with
       Global => (Input  => (Utility.Timer_Done),
                  In_out => (Radio_Port.State, State, Utility.Hardware)),
       Depends => ((Ack_Nack_Response, Radio_Port.State) => (Command, Radio_Port.State),
                   (State, Utility.Hardware) =>+ (Command, Radio_Port.State),
                   null                      => Utility.Timer_Done);


   procedure Scan_For_Packet_With_Payload
     (Command           : in  Commands;
      Payload           : out Bounded_Strings.Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, State, Utility.Hardware)),
       Depends => ((Ack_Nack_Response, Payload, Radio_Port.State) =>
                     (Command, Radio_Port.State),
                   (State, Utility.Hardware) =>+ (Command, Radio_Port.State),
                   null                      => Utility.Timer_Done);


   procedure No_Op(Ack_Nack_Response : out Acknowledgement)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, State, Utility.Hardware)),
       Depends => ((Ack_Nack_Response, Radio_Port.State) => Radio_Port.State,
                   (State, Utility.Hardware)             =>+ Radio_Port.State,
                   null                                  => Utility.Timer_Done);


   procedure Reset(Ack_Nack_Response : out Acknowledgement)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, State, Utility.Hardware)),
       Depends => ((Ack_Nack_Response, Radio_Port.State) => Radio_Port.State,
                   (State, Utility.Hardware)             =>+ Radio_Port.State,
                   null                                  => Utility.Timer_Done);


   procedure Transmit_On
     with
       Global => (In_Out => State),
       Depends => (State =>+ null);


   procedure Transmit_Off
     with
       Global => (In_Out => State),
       Depends => (State =>+ null);


   procedure Transmit
     (Message           : in Bounded_Strings.Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, State, Utility.Hardware)),
       Depends => ((Ack_Nack_Response, Radio_Port.State, State) =>
                     (Message, Radio_Port.State, State),
                   Utility.Hardware =>+ (Message, Radio_Port.State, State),
                   null             => Utility.Timer_Done);


   procedure Get_Transceiver_Configuration
     (Transceiver_Configuration : out Bounded_Strings.Payload_Record;
      Ack_Nack_Response         : out Acknowledgement)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, State, Utility.Hardware)),
       Depends => ((Ack_Nack_Response, Radio_Port.State, Transceiver_Configuration) =>
                     Radio_Port.State,
                   (State, Utility.Hardware) =>+ Radio_Port.State,
                   null             => Utility.Timer_Done);


   procedure Set_Transceiver_Configuration
     (Configuration     : in  Bounded_Strings.Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, State, Utility.Hardware)),
       Depends => ((Ack_Nack_Response, Radio_Port.State) => (Configuration, Radio_Port.State),
                   (State, Utility.Hardware) =>+ (Configuration, Radio_Port.State),
                   null                      => Utility.Timer_Done);


   procedure Get_Telemetry
     (Telemetry_Data    : out Bounded_Strings.Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, State, Utility.Hardware)),
       Depends => ((Ack_Nack_Response, Radio_Port.State, Telemetry_Data) =>
                         Radio_Port.State,
                   (State, Utility.Hardware) =>+ Radio_Port.State,
                   null                      => Utility.Timer_Done);


   procedure Write_Configuration_To_Flash
     (Data              : in Bounded_Strings.Payload_Record;
      Ack_Nack_Response : out Acknowledgement)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (Radio_Port.State, State, Utility.Hardware)),
       Depends => ((Ack_Nack_Response, Radio_Port.State) => (Data, Radio_Port.State),
                   (State, Utility.Hardware) =>+ (Data, Radio_Port.State),
                   null                      => Utility.Timer_Done);

end Blackfly.Radio.Packets;
