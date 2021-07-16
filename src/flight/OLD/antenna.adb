--------------------------------------------------------------------------------
-- FILE   : antenna.adb
-- SUBJECT: Body for control program interface to the antenna.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Antenna is

   type Command_Array_Type is array(Commands) of Byte;
   Command_Array : constant Command_Array_Type := Command_Array_Type'
     (Reset                                    => 2#10101010#,
      Arm_Antenna                              => 2#10101101#,
      Disarm_Antenna                           => 2#10101100#,
      Deploy_Antenna_1                         => 2#10100001#,
      Deploy_Antenna_2                         => 2#10100010#,
      Deploy_Antenna_3                         => 2#10100011#,
      Deploy_Antenna_4                         => 2#10100100#,
      Start_Auto_Sequential_Antenna_Deployment => 2#10100101#,
      Deploy_Antenna_1_With_Override           => 2#10111010#,
      Deploy_Antenna_2_With_Override           => 2#10111011#,
      Deploy_Antenna_3_With_Override           => 2#10111100#,
      Deploy_Antenna_4_With_Override           => 2#10111101#,
      Cancel_Deployment_Activation             => 2#10101001#,
      Measure_Antenna_System_Temperature       => 2#11000000#,
      Report_Deployment_Status                 => 2#11000011#,
      Report_Ant_1_Deployment_Activation_Count => 2#10110000#,
      Report_Ant_2_Deployment_Activation_Count => 2#10110001#,
      Report_Ant_3_Deployment_Activation_Count => 2#10110010#,
      Report_Ant_4_Deployment_Activation_Count => 2#10110011#,
      Report_Ant_1_Deployment_Activation_Time  => 2#10110100#,
      Report_Ant_2_Deployment_Activation_Time  => 2#10110101#,
      Report_Ant_3_Deployment_Activation_Time  => 2#10110110#,
      Report_Ant_4_Deployment_Activation_Time  => 2#10110111#);

   -- Converts an I2C Status to a boolean and returns the boolean
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


   -- Looks up the value of a command from a defined array of commands mapped with values
   function Get_Command_Value(Command : Commands) return Byte is
   begin
      return Command_Array(Command);
   end Get_Command_Value;


   -- Calls I2C.Read to read in a response from a provided slave address. Makes an arbitrary 10
   -- attempts to read. Returns an I2C Status and an Incoming_Record "Response."
   --
    procedure Read_Response
     (Response : out Bounded_Strings.Incoming_Record;
      Address  : in  I2C.I2C_Address_Type;
      Success  : out I2C.I2C_Status)
     with
       Global => (In_Out => (I2C.State, Utility.Hardware),
                  Input  => Utility.Timer_Done),
       Depends => ((Response, Success) => (I2C.State, Address, Utility.Hardware),
                 I2C.State           =>+ (Address, Utility.Hardware),
                 Utility.Hardware    =>+ (I2C.State, Address),
                 null                => Utility.Timer_Done)
   is
   begin
      Response := Bounded_Strings.Prepare_Incoming_Line("__", 2);
      for I in Natural range 1 .. 10 loop
         I2C.Read(Response, Address, Success);
         exit when Success /= I2C.Error_No_Ack;
      end loop;
   end Read_Response;


   -- Takes a Commands type and checks which subtype range the command matches. There are three
   -- possible subtypes and they correspond to a expected response size (I2C read length) - 1
   -- byte, 2 bytes, or no bytes. The number of bytes to read is returned.
   --
   function Get_Response_Size(Command : Commands) return Bounded_Strings.Outgoing_Size_Type
     with Global => null
   is
      Size : Bounded_Strings.Outgoing_Size_Type;
   begin
      case Command is
         when One_Byte_Response_Commands =>
            Size := 1;
         when Two_Byte_Response_Commands =>
            Size := 2;
         when others =>
            Size := 0;
      end case;
      return Size;
   end Get_Response_Size;


   procedure Run_Command
     (Address   : in  I2C.I2C_Address_Type;
      Command   : in  Parameterized_Commands;
      Parameter : in  Byte;
      Success   : out Boolean)
   is
      Outgoing_Data : Bounded_Strings.Outgoing_Record := Bounded_Strings.Outgoing_Record'
        (Text => Bounded_Strings.Outgoing_Type'(others => ' '), Size => 0);
      Read_Success : I2C.I2C_Status;
   begin
      Outgoing_Data.Text(Outgoing_Data.Text'First + 1) :=
        Character'Val(Parameter);
      Outgoing_Data.Text(Outgoing_Data.Text'First) :=
        Character'Val(Get_Command_Value(Command));
      Outgoing_Data.Size := 2;

      I2C.Write(Outgoing_Data, Address, Read_Success);
      Success := I2C_Status_To_Boolean(Read_Success);
   end Run_Command;

   procedure Run_Command_Get_Response
     (Address   : in  I2C.I2C_Address_Type;
      Command   : in  Parameterized_Commands;
      Response  : out Bounded_Strings.Incoming_Record;
      Parameter : in  Byte;
      Success   : out Boolean)
   is
      Read_Success : I2C.I2C_Status;
   begin
      Response := Bounded_Strings.Incoming_Record'
        (Text => Bounded_Strings.Incoming_Type'(others => ' '), Size => 0);
      Response.Size := Get_Response_Size(Command);

      Run_Command(Address, Command, Parameter, Success);

      if Success then
         Read_Response(Response, Address, Read_Success);
         Success := I2C_Status_To_Boolean(Read_Success);
      else
         Success := False;
      end if;
   end Run_Command_Get_Response;


   procedure Run_Command_No_Arg
     (Address : in  I2C.I2C_Address_Type;
      Command : in  Non_Parameterized_Commands;
      Success : out Boolean)
   is
      Outgoing_Data : Bounded_Strings.Outgoing_Record := Bounded_Strings.Outgoing_Record'
        (Text => Bounded_Strings.Outgoing_Type'(others => ' '), Size => 0);
      Read_Success : I2C.I2C_Status;
   begin
      Outgoing_Data.Text(Outgoing_Data.Text'First) := Character'Val(Get_Command_Value(Command));
      Outgoing_Data.Size := 1;

      I2C.Write(Outgoing_Data, Address, Read_Success);
      Success := I2C_Status_To_Boolean(Read_Success);
   end Run_Command_No_Arg;

   procedure Run_Command_No_Arg_Get_Response
     (Address  : in  I2C.I2C_Address_Type;
      Command  : in  Non_Parameterized_Commands;
      Response : out Bounded_Strings.Incoming_Record;
      Success  : out Boolean)
   is
      Read_Success : I2C.I2C_Status;
   begin
      Response := Bounded_Strings.Incoming_Record'
        (Text => Bounded_Strings.Incoming_Type'(others => ' '), Size => 0);
      Response.Size := Get_Response_Size(Command);

      Run_Command_No_Arg(Address, Command, Success);
      if Success then
         Read_Response(Response, Address, Read_Success);
         Success := I2C_Status_To_Boolean(Read_Success);
      else
         Success := False;
      end if;

   end Run_Command_No_Arg_Get_Response;

end Antenna;
