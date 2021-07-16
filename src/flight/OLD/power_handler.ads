--------------------------------------------------------------------------------
-- FILE   : power_handler.ads
-- SUBJECT: Specification of a package that does high level power board manipulation.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Bounded_Strings;
with Files;
with Files.File_System;
with File_Handler;
with I2C;
with Command_Handler;
with Power;
with Time;
with Utility;
with Protocol;
with Protocol_Handler;
with Radio;
with Radio_Port;

use type Protocol.Command_Type;
use type Files.File_System.FS_Error_Code_Type;
use type Files.File_System.File_Size_Type;
use type Power.Byte;
use type Power.Data_Range;

package Power_Handler is

   procedure Read_All_ADC_Channels(ADC_Readings : out Bounded_Strings.Payload_Record)
     with
       Global => (Input => Utility.Timer_Done,
                  In_Out => (I2C.State, Time.Time_Since_Reset, Utility.Hardware)),
       Depends => ((ADC_Readings, I2C.State, Utility.Hardware) => (I2C.State, Utility.Hardware),
                   Time.Time_Since_Reset =>+ (I2C.STate, Utility.Hardware),
                   null => Utility.Timer_Done);

   procedure Work_Unit
     with
       Global => (Input => Utility.Timer_Done,
               In_Out => (Command_Handler.State,
                          Files.File_System.File_Counter,
                          Files.File_System.State,
                          I2C.State,
                          Radio.State,
                          Radio_Port.State,
                          Time.Time_Since_Reset,
                          Utility.Hardware)),
       Depends => ((Command_Handler.State, Files.File_System.File_Counter) =>+
                       Command_Handler.State,
                   (Files.File_System.State, Time.Time_Since_Reset) =>+
                       (Command_Handler.State,
                        Files.File_System.File_Counter,
                        I2C.State,
                        Radio.State,
                        Radio_Port.State,
                        Time.Time_Since_Reset,
                        Utility.Hardware),
                   (I2C.State, Radio.State, Radio_Port.State, Utility.Hardware) =>+
                       (Command_Handler.State,
                        Files.File_System.File_Counter,
                        I2C.State,
                        Radio.State,
                        Radio_Port.State,
                        Utility.Hardware),
                   null => Utility.Timer_Done);

end Power_Handler;
