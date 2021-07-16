--------------------------------------------------------------------------------
-- FILE   : antenna.ads
-- SUBJECT: Specification for control program interface to the antenna.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
with I2C;
with Bounded_Strings;
with Utility;

use type I2C.I2C_Status;

package Antenna is

   type Byte is mod 256;

   type Address_Type is mod 256;

   Microcontroller_A_Address : constant I2C.I2C_Address_Type := 16#31#;
   Microcontroller_B_Address : constant I2C.I2C_Address_Type := 16#32#;

   type Commands is
      -- Commands with args & no responses
     (Deploy_Antenna_1,
      Deploy_Antenna_2,
      Deploy_Antenna_3,
      Deploy_Antenna_4,
      Start_Auto_Sequential_Antenna_Deployment,
      Deploy_Antenna_1_With_Override,
      Deploy_Antenna_2_With_Override,
      Deploy_Antenna_3_With_Override,
      Deploy_Antenna_4_With_Override,

      -- Commands with no args
       -- no response
      Reset,
      Arm_Antenna,
      Disarm_Antenna,
      Cancel_Deployment_Activation,
     -- 2 byte response
      Measure_Antenna_System_Temperature,
      Report_Deployment_Status,
      Report_Ant_1_Deployment_Activation_Time,
      Report_Ant_2_Deployment_Activation_Time,
      Report_Ant_3_Deployment_Activation_Time,
      Report_Ant_4_Deployment_Activation_Time,
      -- 1 byte response
      Report_Ant_1_Deployment_Activation_Count,
      Report_Ant_2_Deployment_Activation_Count,
      Report_Ant_3_Deployment_Activation_Count,
      Report_Ant_4_Deployment_Activation_Count);

   subtype No_Response_Command is
     Commands range Deploy_Antenna_1 .. Cancel_Deployment_Activation;

   subtype One_Byte_Response_Commands is Commands range
     Report_Ant_1_Deployment_Activation_Count .. Report_Ant_4_Deployment_Activation_Count;

   subtype Two_Byte_Response_Commands is Commands range
     Measure_Antenna_System_Temperature .. Report_Ant_4_Deployment_Activation_Time;

   subtype Parameterized_Commands is Commands range
     Deploy_Antenna_1 .. Deploy_Antenna_4_With_Override;

   subtype Non_Parameterized_Commands is Commands range
     Reset .. Report_Ant_4_Deployment_Activation_Count;


   -- Sends a command that requires a parameter to microcontroller A. If the particular command
   -- expects a response the response will be in the Incoming_Record "Response." Boolean
   -- "Success" is also returned true if I2C is successful with the transmission. To change from
   -- Microcontroller A to Microcontroller B see the body of this subprogram
   --
   procedure Run_Command
     (Address   : in  I2C.I2C_Address_Type;
      Command   : in  Parameterized_Commands;
      Parameter : in  Byte;
      Success   : out Boolean)
   with
       Global  => (Input => Utility.Timer_Done,
                   In_Out => (I2C.State, Utility.Hardware)),
       Depends => (I2C.State        =>+ (Address, Command, Parameter),
                   Utility.Hardware =>+ (Address, Command, I2C.State, Parameter),
                   Success          => (Address, Command, I2C.State, Parameter),
                   null             => Utility.Timer_Done);

   procedure Run_Command_Get_Response
     (Address   : in  I2C.I2C_Address_Type;
      Command   : in  Parameterized_Commands;
      Response  : out Bounded_Strings.Incoming_Record;
      Parameter : in  Byte;
      Success   : out Boolean)
   with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (I2C.State, Utility.Hardware)),
       Depends => (I2C.State          =>+ (Address, Command, Parameter, Utility.Hardware),
                   Utility.Hardware   =>+ (Address, Command, I2C.State, Parameter),
                   (Response, Success) =>
                      (Address, Command, I2C.State, Parameter, Utility.Hardware),
                   null               => Utility.Timer_Done);


   -- Sends a command that does not require a parameter. If the particular command expects a
   -- response the response will be in the Incoming_Record "Response." Boolean "Success" is also
   -- returned true if I2C is successful with the transmission. To change from Microcontroller A
   -- to Microcontroller B see the body of this subprogram
   --
   procedure Run_Command_No_Arg
     (Address : in  I2C.I2C_Address_Type;
      Command : in  Non_Parameterized_Commands;
      Success : out Boolean)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (I2C.State, Utility.Hardware)),
       Depends => (I2C.State        =>+ (Address, Command),
                   Utility.Hardware =>+ (Address, Command, I2C.State),
                   Success          => (Address, Command, I2C.State),
                   null             => Utility.Timer_Done);

   procedure Run_Command_No_Arg_Get_Response
     (Address  : in  I2C.I2C_Address_Type;
      Command  : in  Non_Parameterized_Commands;
      Response : out Bounded_Strings.Incoming_Record;
      Success  : out Boolean)
     with
       Global => (Input  => Utility.Timer_Done,
                  In_Out => (I2C.State, Utility.Hardware)),
       Depends => (I2C.State        =>+ (Address, Command, Utility.Hardware),
                   Response         => (Address, Command, I2C.State, Utility.Hardware),
                   Utility.Hardware =>+ (Address, Command, I2C.State),
                   Success          => (Address, Command, I2C.State, Utility.Hardware),
                   null             => Utility.Timer_Done);

end Antenna;
