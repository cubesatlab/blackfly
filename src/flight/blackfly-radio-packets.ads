--------------------------------------------------------------------------------
-- FILE   : blackfly-radio-packets.ads
-- SUBJECT: Specification for the interface to radio packet management subprograms.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College and the University of Vermont
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with BlackFly.Radio.Port; use BlackFly.Radio.Port;

package Blackfly.Radio.Packets is

   ------------------------------------------------------
   -- Scalar Types
   ------------------------------------------------------
   type Byte          is mod 256;  -- Consider replacing this with CubedOS.Lib.Octet
   type Int2          is range -2**15 .. 2**15 - 1;
   type Int4          is range -2**31 .. 2**31 - 1;
   type Unsigned_Int1 is range      0 .. 2** 8 - 1;
   type Unsigned_Int2 is range      0 .. 2**16 - 1;
   type Unsigned_Int4 is range      0 .. 2**32 - 1;

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
         Interface_Baud_Rate : Unsigned_Int1; -- Radio Interface Baud Rate (9600 = 0x00)
         TX_Power_Amp_Level  : Unsigned_Int1; -- Tx Power Amp level (min = 0x00 max = 0xFF)
         RX_RF_Baud_Rate     : Unsigned_Int1; -- Radio RX RF Baud Rate (9600 = 0x00)
         TX_RF_Baud_Rate     : Unsigned_Int1; -- Radio TX RF baud Rate (9600 = 0x00)
         RX_Modulation       : Unsigned_Int1; -- (0x00 = GFSK)
         TX_Modulation       : Unsigned_Int1; -- (0x00 = GFSK)
         RX_Frequency        : Unsigned_Int4; -- (Channel Rx Frequency (ex: 450000 = 450 MHz)
         TX_Frequency        : Unsigned_Int4; -- (Channel Tx Frequency (ex: 450000 = 450 MHz)
         Source              : String_6;      -- AX.25 Mode Source Call Sign (default NOCALL)
         Destination         : String_6;      -- AX.25 Mode Destination Call Sign (default CQ)
         TX_Preamble         : Unsigned_Int2; -- Ax.25 Mode Tx Preamble Byte Length (0x00 = 20 flags)
         TX_Postamble        : Unsigned_Int2; -- Ax.25 Mode Tx Postamble Byte Length (0x00 = 20 flags)
         Function_Config     : Int2;          -- Radio configuration discrete behaviors
      end record;

   -- Ack/NotAck
   type Acknowledgement is
     (Ack,
      Not_Ack,
      Transmit_Fail,  -- If a packet does not transmit properly
      Receive_Fail,   -- If the packet data is read and an anomaly occers to break everything like invalid header comes back
      Payload_Exists, -- If a packet header is read and its size bytes indicate that there is a payload
      Checksum_Fail,  -- If the packets checksums dont add up correctly
      No_Packet);     -- If the packet data is read back and there are no acks f


   -- TODO: make this its own child package maybe?
   type Commands is
     (Transmit_Command,                       -- Send n number of bytes to radio board
      Set_Transceiver_Configuration_Command,  -- Set radio configuration
      Write_Flash_Command,                    -- Write flash with MD5 Checksum
      No_Op_Command,                          -- Increments command processing counter
      Reset_Command,                          -- Reset radio processors and systems.
      Telemetry_Command,                      -- Query a telemetry frame
      Get_Transceiver_Configuration_Command,  -- Read radio configuration
      Received_Data_Command,                  -- Received n number of bytes AX.25 packet
      Invalid_Command);                       -- All other commands

   subtype Send_Message_Commands_No_Message_Response is
    Commands range Transmit_Command .. Write_Flash_Command;

   subtype Send_Header_Only_Commands is
     Commands range No_Op_Command .. Get_Transceiver_Configuration_Command;

   subtype Message_Response_Header_Commands is
     Send_Header_Only_Commands range Telemetry_Command .. Get_Transceiver_Configuration_Command;

   subtype No_Message_Response_Header_Commands is
     Send_Header_Only_Commands range No_Op_Command .. Reset_Command;

   -- Returns a command associated with Command_Character.
   function Character_To_Command(Command_Character : in Character) return Commands;


   procedure Initialize(Success : out Boolean);

   -- Can be called from outside packages to transmit messages without headers.
   procedure Send_Packet
     (Command       : in  Send_Header_Only_Commands;
      Write_Success : out Boolean);

   -- Can be called from outside packages to transmit a message, i.e. picture data, gps, data,
   -- etc. Message is a Payload_Record. The text of the message record cannot exceed
   -- Max_Payload_Size
   procedure Send_Packet_With_Payload
     (Command       : in  Send_Message_Commands_No_Message_Response;
      Payload       : in  Payload_Record;
      Write_Success : out Boolean);

   -- This procedure is used when only the ACK/NACK response needs to be returned.
   procedure Scan_For_Packet(Command : in Commands; Ack_Nack_Response : out Acknowledgement);

   procedure Scan_For_Packet_With_Payload
     (Command           : in  Commands;
      Payload           : out Payload_Record;
      Ack_Nack_Response : out Acknowledgement);

   procedure No_Op(Ack_Nack_Response : out Acknowledgement);

   procedure Reset(Ack_Nack_Response : out Acknowledgement);

   procedure Transmit_On;

   procedure Transmit_Off;

   procedure Transmit
     (Message           : in  Payload_Record;
      Ack_Nack_Response : out Acknowledgement);

   procedure Get_Transceiver_Configuration
     (Transceiver_Configuration : out Payload_Record;
      Ack_Nack_Response         : out Acknowledgement);

   procedure Set_Transceiver_Configuration
     (Configuration     : in  Payload_Record;
      Ack_Nack_Response : out Acknowledgement);

   procedure Get_Telemetry
     (Telemetry_Data    : out Payload_Record;
      Ack_Nack_Response : out Acknowledgement);

   procedure Write_Configuration_To_Flash
     (Data              : in  Payload_Record;
      Ack_Nack_Response : out Acknowledgement);

end Blackfly.Radio.Packets;
