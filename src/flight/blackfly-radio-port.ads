---------------------------------------------------------------------------
-- FILE   : blackfly-radio-port.ads
-- SUBJECT: Specification of a package for low level communication with the radio.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College and the University of Vermont
--
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.Bounded_Strings; use CubedOS.Lib.Bounded_Strings;

package BlackFly.Radio.Port is

   -- The names used here are from the original "BasicLEO" (aka VTLunar) codebase.
   Maximum_Incoming_Length : constant := 265;
   subtype Incoming_Index_Type is Positive range 1 .. Maximum_Incoming_Length;
   subtype Incoming_Size_Type  is Natural range 0 .. Maximum_Incoming_Length;
   subtype Incoming_Type       is String(Incoming_Index_Type);
   subtype Incoming_Record     is Bounded_String(Maximum_Incoming_Length);

   Maximum_Outgoing_Length : constant := 265;
   subtype Outgoing_Index_Type is Positive range 1 .. Maximum_Outgoing_Length;
   subtype Outgoing_Size_Type  is Natural range 0 .. Maximum_Outgoing_Length;
   subtype Outgoing_Type       is String(Outgoing_Index_Type);
   subtype Outgoing_Record     is Bounded_String(Maximum_Outgoing_Length);

   Maximum_Payload_Length  : constant := 255;
   subtype Payload_Index_Type  is Positive range 1 .. Maximum_Payload_Length;
   subtype Payload_Size_Type   is Natural range 0 .. Maximum_Payload_Length;
   subtype Payload_Type        is String(Payload_Index_Type);
   subtype Payload_Record      is Bounded_String(Maximum_Payload_Length );

   -- Initializes the port. There is no 'Close' procedure.
   -- The port can be reinitialized by just calling Open again.
   procedure Open;

   -- Reads a character from the port connected to the radio.
   procedure Get_From_Radio
     (Ch         : out Character;
      Rx_Success : out Boolean);

   -- Writes the given character to the serial port connected to the radio.
   procedure Put_To_Radio
     (Ch         : in  Character;
      Tx_Success : out Boolean);

   -- Writes the given line to the serial port connected to the radio.
   procedure Put_Line_To_Radio
     (Line       : in  Outgoing_Record;
      Tx_Success : out Boolean);

end BlackFly.Radio.Port;
