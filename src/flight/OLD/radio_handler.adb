--------------------------------------------------------------------------------
-- FILE   : radio_handler.adb
-- SUBJECT: Body of a package that does high level radio manipulations.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Radio_Handler
  with Refined_State => (State => (Initialized, Radio_Forced_Off))
is
   Initialized 	     : Boolean;
   Radio_Forced_Off  : Boolean;

   procedure Initialize
     with
       Refined_Global => (Output => (Initialized, Radio.State, Radio_Port.State, Radio_Forced_Off)),
       Refined_Depends => ((Initialized, Radio.State, Radio_Forced_Off, Radio_Port.State) => null)
   is
   begin
      Radio_Forced_Off := False;
      Radio.Initialize(Initialized);
   end Initialize;


   -- Reads all the messages from the radio to the commandler.

   -- TODO: the commandler will most likely request command packets from the radio
   -- if the radio hardware doesn't give a priority to command packets, it will break everything
   -- if there are multiple messages waiting to be read by the radio. if this happens it can be
   -- fixed by reading all of the incoming messages into the message queue befor sending them to
   -- the commandler.
   procedure Read_Messages
     with
       Global => (Input => (Time.Time_Since_Reset, Utility.Timer_Done),
                  In_Out => (Command_Handler.State,
                             Files.File_System.State,
                             Radio.State,
                             Radio_Port.State,
                             Utility.Hardware)),
       Depends => ((Command_Handler.State,
                   Files.File_System.State,
                   Radio.State,
                   Radio_Port.State,
                   Utility.Hardware) =>+ (Radio.State,
                                          Files.File_System.State,
                                          Radio_Port.State,
                                          Time.Time_Since_Reset),
                   null  => Utility.Timer_Done)
   is
      Payload    	: Bounded_Strings.Payload_Record;
      Ack_Nack_Response : Radio.Acknowledgement;
      Success           : Boolean;
      --  Response : Radio.Acknowledgement;
   begin
      loop
         Radio.Scan_For_Packet_With_Payload
           (Radio.Received_Data_Command, Payload, Ack_Nack_Response);
         if Ack_Nack_Response = Radio.Ack then
            -- remove the ax25 bits and check if its correct
            Ax25.Remove(Payload, Success);
            if Success then
                -- Send payload to the Commandler.
               Protocol_Handler.Process_Telemetry(Payload);
            end if;
         end if;
         if Ack_Nack_Response = Radio.No_Packet then
            exit;
         end if;
      end loop;
   end Read_Messages;


   procedure Update_Telemetry(Success : out Boolean)
     with
       Global => (Input => (Time.Time_Since_Reset, Utility.Timer_Done),
                  In_Out => (Files.File_System.State,
                             Radio.State,
                             Radio_Port.State,
                             Utility.Hardware)),
       Depends => ((Files.File_System.State, Success) => (Files.File_System.State,
                                                         Radio_Port.State,
                                                         Time.Time_Since_Reset),
                    (Radio.State, Radio_Port.State, Utility.Hardware) =>+ Radio_Port.State,
                    null                    => Utility.Timer_Done)
   is
      Payload           : Bounded_Strings.Payload_Record;
      Timestamp_Payload : Bounded_Strings.Payload_Record;
      Response          : Radio.Acknowledgement;
      File_Name         : Files.File_System.File_Name_Type;
   begin
      File_Name := File_Handler.Create_Telemetry_File_Name(File_Handler.Radio_Type);
      Success := True;
      Radio.Get_Telemetry(Payload, Response);
      if Response = Radio.Ack then
         Files.File_System.Reset_Error_Code;
         Files.File_System.Open_File(File_Name, Files.File_System.Write);

         -- add the radio extension bytes
         Files.File_System.Write_File(Bounded_Strings.Prepare_Payload_Line("RAD", 3));

         -- add the timestamp
         Time.Get_Timestamp_Payload(Time.Time_Since_Reset, Timestamp_Payload);
         Files.File_System.Write_File(Timestamp_Payload);

         Files.File_System.Write_File(Payload);
         Files.File_System.Close_File;
      end if;
      if Files.File_System.Get_Error_Code /= Files.File_System.Success or
        Response /= Radio.Ack then
         File_Handler.Write_To_Log_File
           (File_Handler.Radio_Type, Bounded_Strings.Prepare_Payload_Line
              ("Failed to update radio telemetry!", 33));
         Success := False;
      end if;
   end Update_Telemetry;


   procedure Work_Unit
     with
       Refined_Global => (Input => (Antenna_Handler.State,
                                    Utility.Timer_Done),
                          In_Out => (Radio_Forced_Off,
                                     Command_Handler.State,
                                     Files.File_System.State,
                                     I2C.State,
                                     Initialized,
                                     Radio.State,
                                     Radio_Port.State,
                                     Time.Time_Since_Reset,
                                     Utility.Hardware)),
       Refined_Depends => (Command_Handler.State =>+ (Antenna_Handler.State,
                                                      Files.File_System.State,
                                                      Initialized,
                                                      Radio.State,
                                                      Radio_Port.State,
                                                      Time.Time_Since_Reset),
                             Radio_Forced_Off =>+ (Antenna_Handler.State,
                                                   Command_Handler.State,
                                                   Files.File_System.State,
                                                   Initialized,
                                                   Radio_Port.State,
                                                   Time.Time_Since_Reset,
                                                   Radio.State,
                                                   Radio_Forced_Off),
                             (Files.File_System.State,
                              I2C.State,
                              Radio.State,
                              Radio_Port.State,
                              Time.Time_Since_Reset,
                              Utility.Hardware)        =>+ (Antenna_Handler.State,
                                                            Command_Handler.State,
                                                            Files.File_System.State,
                                                            I2C.State,
                                                            Initialized,
                                                            Radio_Port.State,
                                                            Radio.State,
                                                            Time.Time_Since_Reset,
                                                            Radio_Forced_Off,
                                                            Utility.Hardware),
                             Initialized =>+ null,
                             null         => Utility.Timer_Done)
   is
      Command_Packet    : Protocol.Command_Packet_Type;
      Payload           : Bounded_Strings.Payload_Record;
      Beacon_Temp       : Bounded_Strings.Payload_Record;
      Response          : Radio.Acknowledgement;
      Command_Ack       : Protocol.Command_Packet_Type;
      Success           : Boolean;
   begin
      -- Try reinitializing the radio if necessary.
      if not Initialized then
         Radio.Initialize(Initialized);
      end if;

      -- This checks if the antenna is deployed, this seems dangerous though, the antenna might
      -- not fully deploy which will prevent the radio from ever receiving messages. maybe we
      -- need to add an, OR X amount of time/super loops have passed to be safe?
      if Initialized and Antenna_Handler.Get_Deployed then
         -- Check for incoming messages and send them to the Commandler.
         Read_Messages;
         -- Check Message Queue for messages.
         for I in Natural range 1 .. Command_Handler.Queue_Size loop
            -- If the antennas have deployed, keep the transmitter on unless it is forced off
            if Radio_Forced_Off /= True then
               Radio.Transmit_On;
            end if;

            Command_Handler.Take(I, Command_Packet);
            exit when Command_Packet.Command = 0;
            case Command_Packet.Command is
               -- "Get radio transceiver configuration" command.
               when 16#70# =>
                  Radio.Get_Transceiver_Configuration(Command_Ack.Arguments, Response);
                  Command_Handler.Remove_Message(I);

                  -- Send "Get radio transceiver configuration" command ack.
                  if Response = Radio.Ack then
                     Command_Ack.Command := 16#70#;
                     Protocol_Handler.Send_Command(Command_Ack);
                  else
                     Command_Ack.Command := 16#70#;
                     Command_Ack.Arguments :=
                       Bounded_Strings.Prepare_Payload_Line
                         ("Failed to get configuration from Radio!", 39);
                     Protocol_Handler.Send_Command(Command_Ack);
                  end if;

               -- "Update telemetry file" command.
               when 16#72# =>
                  Update_Telemetry(Success);
                  Command_Handler.Remove_Message(I);
                  Command_Ack := Protocol.Prepare_Command_Packet;
                  Command_Ack.Command := 16#72#;
                  Command_Ack.Arguments.Size := 1;
                  if Success then
                     Command_Ack.Arguments.Text(1) := Character'Val(16#AA#);
                  else
                     Command_Ack.Arguments.Text(1) := Character'Val(16#FF#);
                  end if;
                  Protocol_Handler.Send_Command(Command_Ack);

               -- "Send beacon" command.
               when 16#73# =>
                  Command_Handler.Remove_Message(I);

                  -- initialize beacon
                  Payload := Bounded_Strings.Prepare_Payload_Line("VermontLunar", 12);
                  -- Add system time.
                  Time.Get_Timestamp_Payload(Time.Time_Since_Reset, Beacon_Temp);
                  Bounded_Strings.Append_Payload_Line
                    (Payload, Beacon_Temp.Text, Beacon_Temp.Size);

                  -- Add EPS ADC readings.
                  Power_Handler.Read_All_ADC_Channels(Beacon_Temp);
                  Bounded_Strings.Append_Payload_Line
                    (Payload, Beacon_Temp.Text, Beacon_Temp.Size);

                  for C in Natural range 1 .. 10 loop
                     Radio.Transmit(Payload, Response);
                     exit when Response = Radio.Ack;
                  end loop;

               -- PING command. -- just returns the packet -- this could also be used for repeating
               when 16#74# =>
                  Command_Handler.Remove_Message(I);
                  Protocol_Handler.Send_Command(Command_Packet);

               -- Transmit on
               when 16#75# =>
                  Command_Handler.Remove_Message(I);
                  Command_Ack := Protocol.Prepare_Command_Packet;
                  Command_Ack.Command := 16#75#;
                  Command_Ack.Arguments.Size := 1;
                  Command_Ack.Arguments.Text(I) := Character'Val(16#AA#);
                  Protocol_Handler.Send_Command(Command_Ack);
                  Radio.Transmit_On;
                  Radio_Forced_Off := False;

               -- Transmit off
               when 16#76# =>
                  Command_Handler.Remove_Message(I);
                  Command_Ack := Protocol.Prepare_Command_Packet;
                  Command_Ack.Command := 16#76#;
                  Command_Ack.Arguments.Size := 1;
                  Command_Ack.Arguments.Text(I) := Character'Val(16#AA#);
                  Protocol_Handler.Send_Command(Command_Ack);
                  Radio.Transmit_Off;
                  Radio_Forced_Off := True;

               when others =>
                  null;
            end case;
         end loop;
      end if;
   end Work_Unit;

end Radio_Handler;
