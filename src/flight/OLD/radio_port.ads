---------------------------------------------------------------------------
-- FILE          : radio_port.ads
-- SUBJECT       : Specification of a package for communicating with the radio.
-- PROGRAMMER    : (C) Copyright 2015 by Vermont Technical College
--
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Bounded_Strings;
with Utility;

use type Utility.Millisecond_Type;

package Radio_Port
  -- TODO: This probably needs to be an external state abstraction.
  with Abstract_State => State
is

   -- Initializes the port. There is no 'Close' procedure. The ports can be reinitialized by
   -- just calling Open again.
   --
   procedure Open
     with
       Global => (Output => State),
       Depends => (State => null);

   -- Reads a character from the serial port connected to the radio.
   procedure Get_From_Radio
     (Ch         : out Character;
      Rx_Success : out Boolean)
     with
       Global => (Input => Utility.Timer_Done, In_Out => (State, Utility.Hardware)),
       Depends => (Ch         => State,
                   Rx_Success => State,
                   State      => State,
                   Utility.Hardware => Utility.Hardware,
                   null             => Utility.Timer_Done);

   -- Writes the given character to the serial port connected to the radio.
   procedure Put_To_Radio
     (Ch         : in  Character;
      Tx_Success : out Boolean)
     with
       Global => (In_Out => State),
       Depends => ((State, Tx_Success) => (State, Ch));

   -- Writes the given line to the serial port connected to the radio.
   procedure Put_Line_To_Radio
     (Line       : in Bounded_Strings.Outgoing_Record;
      Tx_Success : out Boolean)
     with
       Global => (In_Out => State),
       Depends => ((State, Tx_Success) => (State, Line));

end Radio_Port;
