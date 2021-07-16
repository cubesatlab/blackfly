--------------------------------------------------------------------------------
-- FILE   : power.ads
-- SUBJECT: Specification for control program interface to the power board.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Bounded_Strings;
with I2C;
with Utility;
with Time;

use type I2C.I2C_Status;

package Power is

   type Byte is mod 256;

   -- See corresponding descriptions below
   type    Data_Range             is range 0 .. 31;
   subtype ADC_Channel_Range      is Data_Range range 0 .. 31;
   subtype PDM_Off_Range          is Data_Range range 0 .. 7;
   subtype Heater_Force_Off_Range is Data_Range range 0 .. 1;

   type Command_Type is
   (ADC,               -- Read ADC Channel
    PDM_Off,           -- Turns off the selected PDM for a short time
    Heater_Force_Off,  -- Forces Battery Heater off
    Power_Status,      -- Request status bytes
    Version,           -- Request Firmware Version
    Watchdog);         -- Causes soft reset of the micro

   subtype Non_Data_Command_Type is Command_Type range Power_Status .. Watchdog;
   subtype Data_Command_Type     is Command_Type range ADC .. Heater_Force_Off;

   type Data_Command_Lookup_Array is array(Command_Type) of Byte;
   Command_Array : constant Data_Command_Lookup_Array := Data_Command_Lookup_Array'
     (ADC              => 16#00#,
      Power_Status     => 16#01#,
      PDM_Off          => 16#02#,
      Version          => 16#04#,
      Heater_Force_Off => 16#05#,
      Watchdog         => 16#80#);

   procedure Send_ADC_Command
     (Data_Channel : ADC_Channel_Range;
      Response     : out Bounded_Strings.Incoming_Record;
      Success      : out Boolean)
     with
       Global => (In_Out => (I2C.State, Utility.Hardware, Time.Time_Since_Reset),
                  Input => Utility.Timer_Done),
       Depends => ((Response, I2C.State, Success, Utility.Hardware) =>
                       (Data_Channel, I2C.State, Utility.Hardware),
                 Time.Time_Since_Reset =>+ (Data_Channel, I2C.State),
                 null => Utility.Timer_Done);

   --procedure ADC_Conversion

   procedure Send_PDM_Off_Command
     (Data     : PDM_Off_Range;
      Response : out Bounded_Strings.Incoming_Record;
      Success  : out Boolean)
     with
       Global => (In_Out => (I2C.State, Utility.Hardware, Time.Time_Since_Reset),
                  Input => Utility.Timer_Done),
       Depends => ((Response, I2C.State, Success, Utility.Hardware) =>
                       (Data, I2C.State, Utility.Hardware),
                 Time.Time_Since_Reset =>+ (Data, I2C.State),
                 null => Utility.Timer_Done);

   procedure Send_Heater_Force_Off_Command
     (Data     : Heater_Force_Off_Range;
      Response : out Bounded_Strings.Incoming_Record;
      Success  : out Boolean)
     with
       Global => (In_Out => (I2C.State, Utility.Hardware, Time.Time_Since_Reset),
              Input => Utility.Timer_Done),
       Depends => ((Response, I2C.State, Success, Utility.Hardware) =>
                       (Data, I2C.State, Utility.Hardware),
                 Time.Time_Since_Reset =>+ (Data, I2C.State),
               null => Utility.Timer_Done);

   procedure Send_Command_No_Data
     (Command  : Non_Data_Command_Type;
      Response : in out Bounded_Strings.Incoming_Record;
      Success  : out Boolean)
     with
       Global => (In_Out => (I2C.State, Utility.Hardware, Time.Time_Since_Reset),
              Input => Utility.Timer_Done),
       Depends => ((Response, I2C.State, Success, Utility.Hardware) =>
                       (Command, I2C.State, Utility.Hardware, Response),
                   Time.Time_Since_Reset =>+ (Command, I2C.State),
                   null => Utility.Timer_Done);

end Power;
