--------------------------------------------------------------------------------
-- FILE   : power.adb
-- SUBJECT: Body for control program interface to the power board.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
with Time;

package body Power is

   Power_Address : constant := 16#2D#;

   function I2C_Status_To_Boolean(Status : I2C.I2C_Status) return Boolean is
      Boolean_Status : Boolean;
   begin
      case Status is
         when I2C.Success =>
            Boolean_Status := True;
         when I2C.Error_Timeout =>
            Boolean_Status := False;
         when I2C.Error_No_Ack =>
            Boolean_Status := False;
      end case;
      return Boolean_Status;
   end I2C_Status_To_Boolean;


   function Get_Command_Value(Command : Command_Type) return Byte is
   begin
      return Command_Array (Command);
   end Get_Command_Value;


   procedure Send(First_Byte : Byte; Second_Byte : Data_Range; Success : out I2C.I2C_Status)
     with
       Global => (In_Out => (I2C.State, Utility.Hardware),
                  Input => Utility.Timer_Done),
       Depends => ((Success, I2C.State) => (First_Byte, Second_Byte, I2C.State),
                   Utility.Hardware =>+ (First_Byte, Second_Byte, I2C.State),
                   null => Utility.Timer_Done)
   is
      Outgoing_Data : Bounded_Strings.Outgoing_Record := Bounded_Strings.Outgoing_Record'
        (Text => Bounded_Strings.Outgoing_Type'(others => ' '), Size => 0);
   begin
      Outgoing_Data.Text(Outgoing_Data.Text'First) := Character'Val (First_Byte);

      -- if the first byte is a data command then size the Outgoing_Data for data else size it
      -- for a command only.
      if First_Byte = Command_Array(ADC)     or
         First_Byte = Command_Array(PDM_Off) or
         First_Byte = Command_Array(Heater_Force_Off)
      then
         Outgoing_Data.Size := 2;
         Outgoing_Data.Text(Outgoing_Data.Text'First + 1) := Character'Val(Second_Byte);
      else
         Outgoing_Data.Size := 1;
      end if;

      I2C.Write(Outgoing_Data, Power_Address, Success);
   end Send;


   procedure Read_Response
     (Response : in out Bounded_Strings.Incoming_Record; Success  : out I2C.I2C_Status)
     with
       Global => (In_Out => (I2C.State, Utility.Hardware, Time.Time_Since_Reset),
                  Input => Utility.Timer_Done),
       Depends => ((Response, Success, I2C.State, Utility.Hardware) =>
                       (I2C.State, Utility.Hardware, Response),
                   Time.Time_Since_Reset =>+ null,
                   null => Utility.Timer_Done)
   is
   begin
       Time.Sleep_Milliseconds(30);
      for I in Natural range 1 .. 10 loop
         I2C.Read(Response, Power_Address, Success);
         exit when Success /= I2C.Error_No_Ack;
      end loop;
   end Read_Response;


   procedure Send_ADC_Command
     (Data_Channel : ADC_Channel_Range;
      Response     : out Bounded_Strings.Incoming_Record;
      Success      : out Boolean)
   is
      Read_Success : I2C.I2C_Status;
   begin
      Response := Bounded_Strings.Incoming_Record'
        (Text => Bounded_Strings.Incoming_Type'(others => ' '), Size => 2);

      Send(Get_Command_Value(ADC), Data_Channel, Read_Success);

      if Read_Success = I2C.Success then
         Read_Response(Response, Read_Success);
      end if;
      Success := I2C_Status_To_Boolean(Read_Success);
   end Send_ADC_Command;


   procedure Send_PDM_Off_Command
     (Data     : PDM_Off_Range;
      Response : out Bounded_Strings.Incoming_Record;
      Success  : out Boolean)
   is
      Read_Success : I2C.I2C_Status;
   begin
      Response := Bounded_Strings.Incoming_Record'
        (Text => Bounded_Strings.Incoming_Type'(others => ' '), Size => 2);
      Send(Get_Command_Value(PDM_Off), Data, Read_Success);
      if Read_Success = I2C.Success then
         Read_Response(Response, Read_Success);
      end if;
      Success := I2C_Status_To_Boolean(Read_Success);
   end Send_PDM_Off_Command;


   procedure Send_Heater_Force_Off_Command
     (Data     : Heater_Force_Off_Range;
      Response : out Bounded_Strings.Incoming_Record;
      Success  : out Boolean)
   is
      Read_Success : I2C.I2C_Status;
   begin
      Response := Bounded_Strings.Incoming_Record'
        (Text => Bounded_Strings.Incoming_Type'(others => ' '), Size => 2);
      Send(Get_Command_Value(Heater_Force_Off), Data, Read_Success);
      if Read_Success = I2C.Success then
         Read_Response(Response, Read_Success);
      end if;
      Success := I2C_Status_To_Boolean(Read_Success);
   end Send_Heater_Force_Off_Command;


   procedure Send_Command_No_Data
     (Command  : Non_Data_Command_Type;
      Response : in out Bounded_Strings.Incoming_Record;
      Success  : out Boolean)
   is
      Read_Success : I2C.I2C_Status;
   begin
      Send(Get_Command_Value (Command), 0, Read_Success);

      if Read_Success = I2C.Success then
         Read_Response(Response, Read_Success);
      end if;
      Success := I2C_Status_To_Boolean(Read_Success);
   end Send_Command_No_Data;

end Power;
