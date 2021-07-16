--------------------------------------------------------------------------------
-- FILE       : i2c.ads
-- SUBJECT    : Specification for I^2C handling package.
-- PROGRAMMER : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Bounded_Strings;
with System;
with Utility;

package I2C
  -- TODO: Almost certainly this needs to be an external state abstraction.
  with Abstract_State => State
is
   -- TODO: I2C's address space is more limited than 16 bits (yes?). The type should be fixed.
   type I2C_Address_Type is range 0 .. 2**16;  -- TODO: 2**16 - 1?
   type I2C_Status is (Success, Error_Timeout, Error_No_Ack);

   procedure Initialize
     with
       Global => (Output => State),
       Depends => (State => null);

   procedure Write
     (Data                : in  Bounded_Strings.Outgoing_Record;
      Destination_Address : in  I2C_Address_Type;
      Tx_Success          : out I2C_Status)
     with
       Global => (Input => Utility.Timer_Done, In_Out => (State, Utility.Hardware)),
       Depends => (Tx_Success       =>  (State, Data, Destination_Address),
                   State            =>  (State, Data, Destination_Address),
                   Utility.Hardware =>+ (State, Data, Destination_Address),
                   null             =>  (Utility.Timer_Done));

   procedure Read
     (Data                : in out Bounded_Strings.Incoming_Record;
      Destination_Address : in  I2C_Address_Type;
      Rx_Success          : out I2C_Status)
     with
       Global => (Input => Utility.Timer_Done, In_Out => (State, Utility.Hardware)),
       Depends => ((State, Data, Rx_Success, Utility.Hardware) =>
                     (State, Destination_Address, Utility.Hardware, Data),
                   null => (Utility.Timer_Done));


end I2C;
