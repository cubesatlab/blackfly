--------------------------------------------------------------------------------
-- FILE   : power_handler.adb
-- SUBJECT: Body of a package that does high level power board manipulations.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Power_Handler is

   subtype Bit_Index_Type is Natural range 0 .. 7;

   -- This function appears to be unused. It is being retained in case it is useful later.
   --
   --function Get_Bit_Value(Data : Power.Byte; Bit : Bit_Index_Type) return Power.Byte is
   --   type Powers_Of_2_Type is array(Bit_Index_Type) of Power.Byte;
   --   Powers_Of_2 : constant Powers_Of_2_Type := Powers_Of_2_Type'
   --     (0 =>   1,
   --      1 =>   2,
   --      2 =>   4,
   --      3 =>   8,
   --      4 =>  16,
   --      5 =>  32,
   --      6 =>  64,
   --      7 => 128);
   --
   --   Result : Power.Byte;
   --begin
   --   Result := Data / Powers_of_2(Bit);
   --   if Result rem 2 = 0 then
   --      Result := 0;
   --   else
   --      Result := 1;
   --   end if;
   --   return Result;
   --end Get_Bit_Value;


   -- Reads all the important ADC channels and stores the results into ADC_Readings.
   procedure Read_All_ADC_Channels(ADC_Readings : out Bounded_Strings.Payload_Record) is
      Response : Bounded_Strings.Incoming_Record;
      Success  : Boolean;
   begin
      ADC_Readings := Bounded_Strings.Prepare_Payload_Line(" ", 0);
      for Channel in Power.ADC_Channel_Range range 1 .. Power.ADC_Channel_Range'Last loop
         if Channel = 23 or
            Channel = 18 or
            Channel = 24 or
            Channel = 19 or
            Channel = 28 or
            Channel = 21 or
            Channel = 17 or
            Channel = 26 or
            Channel = 27 or
            Channel = 7  or
            Channel = 4  or
            Channel = 1  or
            Channel = 13 or
            Channel = 10 or
            Channel = 31 or
            Channel = 3  or
            Channel = 6  or
            Channel = 9
         then
            Power.Send_ADC_Command(Channel, Response, Success);

            if Success = True and Response.Size = 2 then
               ADC_Readings.Size := ADC_Readings.Size + 1;
               ADC_Readings.Text(ADC_Readings.Size) := Response.Text(1);
               ADC_Readings.Size := ADC_Readings.Size + 1;
               ADC_Readings.Text(ADC_Readings.Size) := Response.Text(2);
            else
               ADC_Readings.Size := ADC_Readings.Size + 2;
            end if;
         end if;
      end loop;
   end Read_All_ADC_Channels;


   procedure Update_Telemetry(File_Name : in Files.File_System.File_Name_Type)
     with
       Global => (In_Out => (Files.File_System.State,
                             I2C.State,
                             Utility.Hardware,
                             Time.Time_Since_Reset),
                  Input => Utility.Timer_Done),
       Depends => (Files.File_System.State => (I2C.State,
                                         Utility.Hardware,
                                         Files.File_System.State,
                                         File_Name,
                                         Time.Time_Since_Reset),
                   I2C.State =>+ Utility.Hardware,
                   Utility.Hardware =>+ I2C.State,
                   Time.Time_Since_Reset =>+ (I2C.State, Utility.Hardware),
                   null => Utility.Timer_Done)
   is
      Power_Telemetry   : Bounded_Strings.Incoming_Record;
      Power_Response	: Bounded_Strings.Payload_Record;
      Timestamp_Payload : Bounded_Strings.Payload_Record;
      Success 		: Boolean;
   begin
      -- Request the EPS "Status bytes".
      Power_Telemetry := Bounded_Strings.Prepare_Incoming_Line(Line => "  ", Size => 2);
      Power.Send_Command_No_Data(Power.Power_Status, Power_Telemetry, Success);

      if Success then
         Power_Response := Bounded_Strings.Prepare_Payload_Line(" ", 2);
         Power_Response.Text(1) := Power_Telemetry.Text(1);
         Power_Response.Text(2) := Power_Telemetry.Text(2);

         Files.File_System.Reset_Error_Code;
         Files.File_System.Delete_File(File_Name);
         Files.File_System.Open_File(File_Name, Files.File_System.Append);

         -- First three bytes are helper ASCII, next two are the EPS status bytes.
         Files.File_System.Write_File(Bounded_Strings.Prepare_Payload_Line("POW", 3));

         -- add the timestamp
         Time.Get_Timestamp_Payload(Time.Time_Since_Reset, Timestamp_Payload);
         Files.File_System.Write_File(Timestamp_Payload);


         Files.File_System.Write_File(Power_Response);

         -- Last 64 bytes are readings from the ADC, two bytes each.
         Read_All_ADC_Channels(Power_Response);
         Files.File_System.Write_File(Power_Response);

         Files.File_System.Close_File;
         if Files.File_System.Get_Error_Code /= Files.File_System.Success then
            -- TODO: log this error somewhere
            Files.File_System.Reset_Error_Code;
         end if;
      end if;
   end Update_Telemetry;


   procedure Work_Unit
   is
      Command_Packet      : Protocol.Command_Packet_Type;
      Response            : Bounded_Strings.Incoming_Record;
      Success             : Boolean := False;
      Command_Ack         : Protocol.Command_Packet_Type;
      Channel             : Power.ADC_Channel_Range;
      File_System_is_Full : Boolean;
      File_Name           : Files.File_System.File_Name_Type;
   begin
      -- Check Message Queue for messages.
      for I in Natural range 1 .. Command_Handler.Queue_Size loop
         Command_Handler.Take(I, Command_Packet);
         exit when Command_Packet.Command = 0;

         case Command_Packet.Command is
            -- "Read ADC Channel" command.
            when 16#50# =>
               Command_Handler.Remove_Message(I);

               Command_Ack := Protocol.Prepare_Command_Packet;
               Command_Ack.Command := 16#50#;
               if Command_Packet.Arguments.Size >= 1 then
                  for J in Bounded_Strings.Payload_Index_Type range 1 ..
                    Command_Packet.Arguments.Size loop
                     if Character'Pos(Command_Packet.Arguments.Text(J)) <=
                       Power.ADC_Channel_Range'Last then
                        Channel := Character'Pos(Command_Packet.Arguments.Text(J));
                        Power.Send_ADC_Command
                          (Data_Channel => Channel, Response => Response, Success => Success);
                        if Success = True and Response.Size > 0 then
                           Command_Ack.Arguments.Text(1) := Character'Val(16#AA#);
                           Command_Ack.Arguments.Text(2) := Character'Val(Channel);
                           Command_Ack.Arguments.Text(3) := Response.Text(1);
                           Command_Ack.Arguments.Text(4) := Response.Text(2);
                           Command_Ack.Arguments.Size := 4;
                        else
                           Command_Ack.Arguments.Text(1) := Character'Val(16#FF#);
                           Command_Ack.Arguments.Text(2) := Command_Packet.Arguments.Text(J);
                           Command_Ack.Arguments.Size := 2;
                        end if;
                        Protocol_Handler.Send_Command(Command_Ack);
                     end if;
                  end loop;
               else
                  Channel := 19;

                  Power.Send_ADC_Command
                    (Data_Channel => Channel, Response => Response, Success => Success);
                  if Success = True and Response.Size > 0 then
                     Command_Ack.Arguments.Text(1) := Character'Val(16#AA#);
                     Command_Ack.Arguments.Text(2) := Character'Val(Channel);
                     Command_Ack.Arguments.Text(3) := Response.Text(1);
                     Command_Ack.Arguments.Text(4) := Response.Text(2);
                     Command_Ack.Arguments.Size := 4;
                  else
                     Command_Ack.Arguments.Text(1) := Character'Val(16#FF#);
                     Command_Ack.Arguments.Text(2) := Character'Val(Channel);
                     Command_Ack.Arguments.Size := 2;
                  end if;
                  Protocol_Handler.Send_Command(Command_Ack);
               end if;

            -- "Update telemetry file" command.
            when 16#53# =>
               Command_Handler.Remove_Message(I);

               if Command_Packet.Arguments.Text(1) = Character'Val(16#01#) and
                 Command_Packet.Arguments.Size = 1
               then
                  -- Weve been Commandled! Silently dump the power supply data to the file pit
                  -- Update our EPS telemetry.
                  Files.File_System.Next_File_Name(File_Name, File_System_is_Full);
                  if not File_System_is_Full then
                     Update_Telemetry(File_Name);
                  end if;
               else
                  Update_Telemetry
                    (File_Handler.Create_Telemetry_File_Name(File_Handler.Power_Type));
                  -- Send the "Update telemetry file" command ack.
                  Command_Ack := Protocol.Prepare_Command_Packet;
                  Command_Ack.Command := 16#53#;
                  Command_Ack.Arguments.Size := 1;
                  Command_Ack.Arguments.Text(1) := Character'Val(16#AA#);
                  Protocol_Handler.Send_Command(Command_Ack);
               end if;

            -- "Force battery heater off" command.
            when 16#51# =>
               Command_Handler.Remove_Message(I);
               Power.Send_Heater_Force_Off_Command(1, Response, Success);

               -- Send "Force battery heater off" command ack.
               Command_Ack := Protocol.Prepare_Command_Packet;
               Command_Ack.Command := 16#51#;
               Command_Ack.Arguments.Size := 1;
               if Success = True and Response.Size = 2 then
                  Command_Ack.Arguments.Text(1) := Character'Val(16#AA#);
               else
                  Command_Ack.Arguments :=
                    Bounded_Strings.Prepare_Payload_Line
                      (Character'Val(16#FF#) & "Failed to force battery heater off.", 36);
                  -- TODO: log with file handler.
               end if;
               Protocol_Handler.Send_Command(Command_Ack);

            -- "Reset Battery Heater" command.
            when 16#54# =>
               Command_Handler.Remove_Message(I);
               Power.Send_Heater_Force_Off_Command(0, Response, Success);

               -- Send "Reset Battery Heater" command ack.
               Command_Ack := Protocol.Prepare_Command_Packet;
               Command_Ack.Command := 16#54#;
               Command_Ack.Arguments.Size := 1;
               if Success = True and Response.Size = 2 then
                  Command_Ack.Arguments.Text(1) := Character'Val(16#AA#);
               else
                  Command_Ack.Arguments :=
                    Bounded_Strings.Prepare_Payload_Line
                      (Character'Val(16#FF#) & "Failed to reset battery heater.", 33);
                  -- TODO: log with file handler.
               end if;
               Protocol_Handler.Send_Command(Command_Ack);

            -- "Cycle PDM" command.
            when 16#52# =>
               Command_Handler.Remove_Message(I);

               -- Check that PDM value is within the correct range.
               Response := Bounded_Strings.Prepare_Incoming_Line(" ", 0);
               if Character'Pos(Command_Packet.Arguments.Text(1)) >=
                 Power.PDM_Off_Range'First and
                 Character'Pos(Command_Packet.Arguments.Text(1)) <= Power.PDM_Off_Range'Last
               then
                  Power.Send_PDM_Off_Command
                    (Power.PDM_Off_Range
                       (Character'Pos(Command_Packet.Arguments.Text(1))), Response, Success);
               end if;

               -- Send "Cycle PDM" command ack.
               Command_Ack := Protocol.Prepare_Command_Packet;
               Command_Ack.Command := 16#52#;
               Command_Ack.Arguments.Size := 1;
               if Success = True and Response.Size > 0 then
                  Command_Ack.Arguments.Text(1) := Character'Val(16#AA#);
               else
                  Command_Ack.Arguments :=
                    Bounded_Strings.Prepare_Payload_Line
                      (Character'Val(16#FF#) & "Failed to turn PDM off.", 36);
               end if;
               Protocol_Handler.Send_Command(Command_Ack);

            when others =>
               null;
         end case;
      end loop;
   end Work_Unit;

end Power_Handler;
