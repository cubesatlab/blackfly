--------------------------------------------------------------------------------
-- FILE   : radio_handler.ads
-- SUBJECT: Specification of a package that does high level radio manipulations.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with AX25;
with Bounded_Strings;
with Command_Handler;
with Radio_Port;
with Radio;
with Utility;
with Protocol;
with Protocol_Handler;
with I2C;
with Power_Handler;
with Antenna_Handler;
with Files;
with Time;
with Files.File_System;
with File_Handler;

use type Protocol.Command_Type;
use type Files.File_System.FS_Error_Code_Type;
use type Radio.Acknowledgement;
use type Utility.Minute_Type;

package Radio_Handler
  with Abstract_State => State
is

   procedure Initialize
     with
       Global => (Output => (Radio.State, Radio_Port.State, State)),
       Depends => ((Radio.State, Radio_Port.State, State) => null);

   procedure Work_Unit
     with
       Global => (Input => (Antenna_Handler.State, Utility.Timer_Done),
                  In_Out => (Command_Handler.State,
                             Files.File_System.State,
                             I2C.State,
                             Radio.State,
                             Radio_Port.State,
                             State,
                             Time.Time_Since_Reset,
                             Utility.Hardware)),
       Depends => (Command_Handler.State =>+ (Antenna_Handler.State,
                                              Files.File_System.State,
                                              Radio.State,
                                              Radio_Port.State,
                                              State,
                                              Time.Time_Since_Reset),
                   (Files.File_System.State,
                    I2C.State,
                    Radio.State,
                    Radio_Port.State,
                    Time.Time_Since_Reset,
                    Utility.Hardware) =>+ (Antenna_Handler.State,
                                           Command_Handler.State,
                                           Files.File_System.State,
                                           I2C.State,
                                           Radio_Port.State,
                                           Radio.State,
                                           State,
                                           Time.Time_Since_Reset,
                                           Utility.Hardware),
                   State =>+ (Antenna_Handler.State,
                              Command_Handler.State,
                              Files.File_System.State,
                              Radio.State,
                              Radio_Port.State,
                     Time.Time_Since_Reset),
                   null  => Utility.Timer_Done);

end Radio_Handler;
