--------------------------------------------------------------------------------
-- FILE   : antenna_handler.adb
-- SUBJECT: Body of a package that does high level antenna manipulations.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Antenna_Handler
  with Refined_State => (State => (Armed, Deployed))
is
   Armed    : Boolean;
   Deployed : Boolean;

   function Get_Deployed return Boolean
     with Refined_Global => (Input => Deployed)
   is
   begin
      return Deployed;
   end Get_Deployed;


   procedure Is_Antenna_System_Armed(Antenna_System_Armed : out Boolean)
     with
       Global => (Input => Utility.Timer_Done,
                  In_Out => (I2C.State, Utility.Hardware)),
       Depends => ((Antenna_System_Armed, I2C.State, Utility.Hardware) =>
                   (I2C.State, Utility.Hardware),
                   null => Utility.Timer_Done)
   is
      Response        : Bounded_Strings.Incoming_Record;
      Command_Success : Boolean;

   begin
      Antenna_System_Armed := False;

      -- Send the Report Deployment Status command.
      Antenna.Run_Command_No_Arg_Get_Response
        (16#31#, Antenna.Report_Deployment_Status, Response, Command_Success);
      if Command_Success and Response.Size = 2 then

         -- bit 0 is armed state, bit 4 is a safety "Section 4.5.15 of V1.9 user manual".
         if (Utility.Byte_Type(Character'Pos(Response.Text(1))) and
               Utility.Byte_Type(2#0000_0001#)) > 0
         then
            Antenna_System_Armed := True;
         end if;
      end if;
   end Is_Antenna_System_Armed;


   procedure Are_Antennas_Deploying(Antennas_Deploying : out Boolean)
     with
       Global => (Input => Utility.Timer_Done,
                  In_Out => (I2C.State, Utility.Hardware)),
       Depends => ((Antennas_Deploying, I2C.State, Utility.Hardware) =>
                     (I2C.State, Utility.Hardware),
                   null => Utility.Timer_Done)
   is
      Response        : Bounded_Strings.Incoming_Record;
      Command_Success : Boolean;
   begin
      Antennas_Deploying := False;

      -- Send the Report Deployment Status command.
      Antenna.Run_Command_No_Arg_Get_Response
        (16#31#, Antenna.Report_Deployment_Status, Response, Command_Success);
      if Command_Success and Response.Size = 2 then

         -- bits 1 and 5 of each byte are deploying state (1 = deploying)
         if(Utility.Byte_Type(Character'Pos(Response.Text(1))) and
              Utility.Byte_Type(2#00100010#)) > 0 or
           (Utility.Byte_Type(Character'Pos(Response.Text(2))) and
              Utility.Byte_Type(2#00100010#)) > 0 then
            Antennas_Deploying := True;
         end if;
      end if;
   end Are_Antennas_Deploying;


   procedure Are_All_Antennas_Deployed(All_Antennas_Deployed : out Boolean)
     with
       Global => (Input => Utility.Timer_Done,
                  In_Out => (I2C.State, Utility.Hardware)),
       Depends => ((All_Antennas_Deployed, I2C.State, Utility.Hardware) =>
                     (I2C.State, Utility.Hardware),
                   null => Utility.Timer_Done)
   is
      Response : Bounded_Strings.Incoming_Record;
      Command_Success : Boolean;
   begin
      All_Antennas_Deployed := False;
      -- Send the Report Deployment Status command.
      Antenna.Run_Command_No_Arg_Get_Response
        (16#31#, Antenna.Report_Deployment_Status, Response, Command_Success);
      if Command_Success and Response.Size = 2 then

         -- bits 3 and 7 of each byte are deployed state (0 = deployed)
         -- bit 4 of each byte is a safety "Section 4.5.15 of V1.9 user manual".
         if ((Utility.Byte_Type(Character'Pos(Response.Text(1))) and
                Utility.Byte_Type(2#1000_1000#)) +
             (Utility.Byte_Type(Character'Pos(Response.Text(2))) and
                Utility.Byte_Type(2#1000_1000#)) = 0) then
            All_Antennas_Deployed := True;
         end if;
      end if;
   end Are_All_Antennas_Deployed;


   procedure Arm_Antenna_System(Success : out Boolean)
     with
       Global => (Input => Utility.Timer_Done,
                  In_Out => (I2C.State, Utility.Hardware)),
       Depends => ((Success, I2C.State, Utility.Hardware) =>
                     (I2C.State, Utility.Hardware),
                   null => Utility.Timer_Done)
   is
   begin
      -- Send the Arm Antenna System command.
      Antenna.Run_Command_No_Arg(16#31#, Antenna.Arm_Antenna, Success);
      if Success then
         -- Check if Arm Antenna System command was successful.
         Is_Antenna_System_Armed(Success);
      end if;
   end Arm_Antenna_System;


   procedure Disarm_Antenna_System(Success : out Boolean)
     with
       Global => (Input => Utility.Timer_Done,
                  In_Out => (Armed, I2C.State, Utility.Hardware)),
       Depends => ((Armed, I2C.State, Utility.Hardware) =>+ (I2C.State, Utility.Hardware),
                   Success => (I2C.State, Utility.Hardware),
                   null    => Utility.Timer_Done)
   is
   begin
      -- Send the Disarm Antenna System command.
      Antenna.Run_Command_No_Arg(16#31#, Antenna.Disarm_Antenna, Success);
      if Success then

         -- Check if Disarm Antenna System command was successful.
         Is_Antenna_System_Armed(Armed);
         Success := (Armed = False);
      end if;
   end Disarm_Antenna_System;


   procedure Start_Deploying_All_Antennas(Success : out Boolean)
     with
       Global => (Input => Utility.Timer_Done,
                  In_Out => (I2C.State, Utility.Hardware)),
       Depends => ((I2C.State, Utility.Hardware) =>+ I2C.State,
                   Success => I2C.State,
                   null    => Utility.Timer_Done)
   is
   begin
      -- Send the Start Automated Sequential Antenna Deployment command, use the default (30
      -- second) safety time limit.
      Antenna.Run_Command(16#31#, Antenna.Start_Auto_Sequential_Antenna_Deployment, 30, Success);
   end Start_Deploying_All_Antennas;


   -- returns true if the antenna is powered, the only reason why the antenna shouldn't be
   -- powered is if the RBF pin is in or if the footy buttons are depressed.
   procedure Deployment_Check is
      Response : Bounded_Strings.Incoming_Record;
      Success  : Boolean;
      -- The antenna gets 30 tries to respond before we assume that it is off.
      Antenna_Power_Checks : constant := 30;
   begin
      for I in Natural range 1 .. Antenna_Power_Checks loop
         Antenna.Run_Command_No_Arg_Get_Response
           (16#32#, Antenna.Report_Deployment_Status, Response, Success);
         exit when Success and Response.Size = 2;
         Success := False; -- if we were not successfully exited, set Success to False
      end loop;
      -- if we did not get a response, we can only assume that the chubsat is depressed
      if not Success then
         Time.Sleep_Milliseconds(500);
         Utility.Reset;
      end if;
   end Deployment_Check;


   procedure Initialize
     with
       Refined_Global => (Input => (Time.Time_Since_Reset, Utility.Timer_Done),
                          In_Out => (Files.File_System.State, I2C.State, Utility.Hardware),
                          Output => (Armed, Deployed, First_Deployment)),
       Refined_Depends => ((Armed, Deployed, First_Deployment, I2C.State, Utility.Hardware) =>
                             (I2C.State, Utility.Hardware),
                           Files.File_System.State =>+
                             (I2C.State, Time.Time_Since_Reset, Utility.Hardware),
                           null => Utility.Timer_Done)
   is
      Status : Boolean;
      Status_Message : Bounded_Strings.Payload_Record;
   begin
      Armed := False;
      Deployed := False;
      First_Deployment := True;

      Status_Message := Bounded_Strings.Prepare_Payload_Line
        (Line => "Initializing Antenna Handler with antennas not armed and not deployed.",
         Size => 70);

      Is_Antenna_System_Armed(Status);
      if Status then
         Armed := True;
         Status_Message := Bounded_Strings.Prepare_Payload_Line
           (Line => "Initializing Antenna Handler with antennas already armed, strange.",
            Size => 65);
      end if;

      Are_All_Antennas_Deployed(Status);
      if Status then
         Armed    := True;
         Deployed := True;
         -- if the antenna has been armed and deployed, this is most likely not the first
         -- deployment.
         First_Deployment := False;
         Status_Message := Bounded_Strings.Prepare_Payload_Line
           (Line => "Initializing Antenna Handler with antennas already deployed.",
            Size => 59);
      end if;

      -- Logging is good.
      File_Handler.Write_To_Log_File(File_Handler.Antenna_Type, Status_Message);
   end Initialize;


   procedure Update_Telemetry
     with
       Global => (Input => (Time.Time_Since_Reset, Utility.Timer_Done),
                  In_out => (Files.File_System.State, I2C.State, Utility.Hardware)),
       Depends => (Files.File_System.State =>+
                      (I2C.State, Time.Time_Since_Reset, Utility.Hardware),
                   (I2C.State, Utility.Hardware) => (I2C.State, Utility.Hardware),
                   null => Utility.Timer_Done)
   is
      Success           : Boolean;
      Antenna_Response  : Bounded_Strings.Incoming_Record;
      Antenna_Telemetry : Bounded_Strings.Payload_Record;
      Timestamp_Payload : Bounded_Strings.Payload_Record;
      File_Name         : Files.File_System.File_Name_Type;
   begin
      -- Initialize Antenna_Telemetry
      Antenna_Telemetry := Bounded_Strings.Prepare_Payload_Line
        (Line => Bounded_Strings.Payload_Type'(others => Character'Val(0)),
         Size => 4);

      File_Name := File_Handler.Create_Telemetry_File_Name(File_Handler.Antenna_Type);
      -- gather Telemetry information!
      -- The first 2 bytes represent the temperature and the 2nd 2 bytes represent the
      -- deployment status.
      Antenna.Run_Command_No_Arg_Get_Response
        (Address  => 16#31#,
         Command  => Antenna.Measure_Antenna_System_Temperature,
         Response => Antenna_Response,
         Success  => Success);

      if Success then
         Antenna_Telemetry.Text(1) := Antenna_Response.Text(1);
         Antenna_Telemetry.Text(2) := Antenna_Response.Text(2);
      end if;

      Antenna.Run_Command_No_Arg_Get_Response
        (Address  => 16#31#,
         Command  => Antenna.Report_Deployment_Status,
         Response => Antenna_Response,
         Success  => Success);

      if Success then
         Antenna_Telemetry.Text(3) := Antenna_Response.Text(1);
         Antenna_Telemetry.Text(4) := Antenna_Response.Text(2);
      end if;

      Files.File_System.Reset_Error_Code;
      Files.File_System.Open_File(File_Name, Files.File_System.Write);
      Files.File_System.Write_File(Bounded_Strings.Prepare_Payload_Line("ANT", 3));

      -- add the timestamp
      Time.Get_Timestamp_Payload(Time.Time_Since_Reset, Timestamp_Payload);
      Files.File_System.Write_File(Timestamp_Payload);

      Files.File_System.Write_File(Antenna_Telemetry);
      Files.File_System.Close_File;

      if Files.File_System.Get_Error_Code /= Files.File_System.Success then
         -- Do some error logging stuff here?
         Files.File_System.Reset_Error_Code;
      end if;
   end Update_Telemetry;


   procedure Work_Unit
     with
       Refined_Global => (Input => Utility.Timer_Done,
                          In_Out => (Armed,
                                     Deployed,
                                     Radio.State,
                                     Time.Time_Since_Reset,
                                     Command_Handler.State,
                                     Files.File_System.State,
                                     I2C.State,
                                     Radio_Port.State,
                                     Utility.Hardware)),
       Refined_Depends => ((Armed, Time.Time_Since_Reset) =>+
                    (Deployed, I2C.State, Time.Time_Since_Reset, Utility.Hardware),
                    Command_Handler.State =>+ (Armed, Deployed),
                    Deployed =>+ (Armed, I2C.State, Utility.Hardware),
                    (Files.File_System.State, I2C.State, Utility.Hardware) =>+
                      (Armed, Command_Handler.State, Deployed, I2C.State,
                      Radio.State, Radio_Port.State,
                      Time.Time_Since_Reset, Utility.Hardware),
                    (Radio.State, Radio_Port.State) => (Armed, Command_Handler.State,
                                                        Deployed,
                                                        Radio.State,
                                                        Radio_Port.State),
                    null => Utility.Timer_Done)
   is
      Success        : Boolean;
      System_Time    : Utility.Minute_Type;
      Status_Message : Bounded_Strings.Payload_Record;
      Command_Packet : Protocol.Command_Packet_Type;
      Command_Ack    : Protocol.Command_Packet_Type;
   begin
      -- if not deployed, check to see if the antenna is available. If not, reset the ChubSat.
      if not Deployed then
         Deployment_Check;
      end if;
      -- Always check if the antenna is deployed.
      if not Armed or not Deployed then

         -- Antennas may take multiple work units to fully deploy, keep checking them.
         Are_All_Antennas_Deployed(Success);

         if Success then
            Deployed := True;
         end if;

         -- Wait for 30mins to ARM antenna system.
         Time.Get_Minutes(System_Time, Time.Time_Since_Reset);
         if not Deployed and not Armed and System_Time >= 15 then
            Arm_Antenna_System(Success);
            if Success then
               Armed := True;
               Status_Message := Bounded_Strings.Prepare_Payload_Line
                 (Line => "Antenna system has been armed.", Size => 30);
            else
               Status_Message := Bounded_Strings.Prepare_Payload_Line
                 (Line => "Antenna system failed to arm!", Size => 29);
            end if;
            File_Handler.Write_To_Log_File(File_Handler.Antenna_Type, Status_Message);
         end if;

         -- Wait for 30mins to DEPLOY antenna system.
         if not Deployed and Armed and System_Time >= 15 then
            Start_Deploying_All_Antennas(Success);
            if Success then
               Status_Message := Bounded_Strings.Prepare_Payload_Line
                 (Line => "Sent deploy all antennas command.", Size => 33);
            else
               Status_Message := Bounded_Strings.Prepare_Payload_Line
                 (Line => "Failed to start antenna deployment!", Size => 35);
            end if;
            File_Handler.Write_To_Log_File(File_Handler.Antenna_Type, Status_Message);
         end if;

      -- All antennas have finished deploying, good to go!
      else
         -- Make sure the antennas are deployed!
         Are_All_Antennas_Deployed(Success);
         if not Success then
            Arm_Antenna_System(Success);
            if not Success then
               File_Handler.Write_To_Log_File
                 (File_Handler.Antenna_Type,
                  Bounded_Strings.Prepare_Payload_Line
                    (Line => "The Antenna was tricked into thinking that it was deployed but then it turned out not to be deployed and was unarmable, oh nose!",
                     Size => 114));
            end if;
            Start_Deploying_All_Antennas(Success);
            if not Success then
               File_Handler.Write_To_Log_File
                 (File_Handler.Antenna_Type,
                  Bounded_Strings.Prepare_Payload_Line
                    (Line => "Somehow the antenna was deployed during initialization but in now undeployed and the deployment command has failed",
                     Size => 129));
            end if;
         end if;
         -- Good news! The antennas have been deployed! Inform the commandler of our success!
         Command_Handler.Antennas_Deployed;
         -- Check Message Queue for messages.
         for I in Natural range 1 .. Command_Handler.Queue_Size loop
            Command_Handler.Take(I, Command_Packet);
            exit when Command_Packet.Command = 0;

            case Command_Packet.Command is

               -- "Update telemetry file" command.
               when 16#01# =>
                  Update_Telemetry;
                  Command_Handler.Remove_Message(I);

                  -- Send "Update telemetry file" command ack.
                  Command_Ack := Protocol.Prepare_Command_Packet;
                  Command_Ack.Command := 16#01#;
                  Command_Ack.Arguments.Size := 1;
                  Command_Ack.Arguments.Text(1) := Character'Val(16#AA#);
                  Protocol_Handler.Send_Command(Command_Ack);

               when others =>
                  null;
            end case;
         end loop;
      end if;
   end Work_Unit;

end Antenna_Handler;
