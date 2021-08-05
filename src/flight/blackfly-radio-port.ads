---------------------------------------------------------------------------
-- FILE   : blackfly-radio-port.ads
-- SUBJECT: Specification of a package for low level communication with the radio.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College and the University of Vermont
--
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.Bounded_Strings;

package BlackFly.Radio.Port is

   -- Initializes the port. There is no 'Close' procedure.
   -- The port can be reinitialized by just calling Open again.
   --
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
     (Line       : in Bounded_Strings.Outgoing_Record;
      Tx_Success : out Boolean);

end BlackFly.Radio.Port;
