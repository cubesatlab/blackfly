--------------------------------------------------------------------------------
-- FILE   : antenna_handler.ads
-- SUBJECT: Specification of a package that does high level antenna manipulations.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Antenna;
with Bounded_Strings;
with I2C;
with Command_Handler;
with Time;
with Files;
with Files.File_System;
with File_Handler;
with Protocol;
with Protocol_Handler;
with Radio;
with Radio_Port;
with Utility;

use type Protocol.Command_Type;
use type Files.File_System.FS_Error_Code_Type;
use type Utility.Byte_Type;
use type Utility.Minute_Type;

package Antenna_Handler
  with Abstract_State => State
is

   First_Deployment : Boolean;

   function Get_Deployed return Boolean
     with Global => (Input => State);

   procedure Initialize
     with
       Global => (Input => (Time.Time_Since_Reset, Utility.Timer_Done),
                  In_Out => (Files.File_System.State, I2C.State, Utility.Hardware),
                  Output => (First_Deployment, State)),
       Depends => (Files.File_System.State =>+
                    (I2C.State, Time.Time_Since_Reset, Utility.Hardware),
                   (First_Deployment, I2C.State, State, Utility.Hardware) =>
                     (I2C.State, Utility.Hardware),
                   null => Utility.Timer_Done);

   procedure Work_Unit
     with
       Global => (Input => Utility.Timer_Done,
                  In_Out => (Time.Time_Since_Reset,
                             Command_Handler.State,
                             Files.File_System.State,
                             I2C.State,
                             Radio.State,
                             Radio_Port.State,
                             State,
                             Utility.Hardware)),
     Depends => (Command_Handler.State =>+ State,
                 Radio_Port.State =>+ (Command_Handler.State, Radio.State, State),
                 (Files.File_System.State, I2C.State, Utility.Hardware) =>+
                     (Command_Handler.State, I2C.State, Radio.State, Radio_Port.State,
                      State, Time.Time_Since_Reset, Utility.Hardware),
                 Radio.State =>+ (Command_Handler.State, Radio_Port.State, State),
                 State =>+ (I2C.State, Time.Time_Since_Reset, Utility.Hardware),
                 Time.Time_Since_Reset =>+ (Utility.Hardware, I2C.State, State),
                 null => Utility.Timer_Done);

   -- Checks if the power supply is on by asking for ADC data. If it fails, reset the MSP430.
   -- This is unfortunately the only way to make sure the MSP430 doesn't increment the system
   -- time and prematurely deploy the antennas all over us. The PBF pin and the buttons on the
   -- chubsat body don't adequately reset the MSP430 :/ even though the MSP430 has a reset
   -- pin.....
   procedure Deployment_Check
     with
       Global => (Input => Utility.Timer_Done,
                  In_Out => (I2C.State, Time.Time_Since_Reset, Utility.Hardware)),
       Depends => ((I2C.State, Time.Time_Since_Reset, Utility.Hardware) =>+
                   (I2C.State, Utility.Hardware),
                   null => Utility.Timer_Done);

end Antenna_Handler;
