---------------------------------------------------------------------------
-- FILE       : i2c.adb
-- SUBJECT    : Body for I^2C handling package.
-- PROGRAMMER : (C) Copyright 2012 by Vermont Technical College
--
---------------------------------------------------------------------------
-- TODO: Turn SPARK_Mode back on.
pragma SPARK_Mode(Off);

package body I2C
--# own State is USCI_B1.State;
is

   --# inherit System, Utility, Bounded_Strings;
   package USCI_B1
   --# own State;
   is

      procedure Initialize;
      --# global out State;
      --# derives State from ;
      pragma Import(C, Initialize);

      -- TODO: Lift this procedure into SPARK. It's much too complicated for C. The annotations
      -- below are guesses.
      procedure Transmit
        (Destination_Address : in  Integer;
         Message             : in  Bounded_Strings.Outgoing_Type;
         Message_Length      : in  Integer;
         Result              : out Integer);
      --# global in out State, Utility.Hardware; in Utility.Timer_Done;
      --# derives State            from State, Destination_Address, Message, Message_Length &
      --#         Utility.Hardware from State, Utility.Hardware, Destination_Address, Message, Message_Length &
      --#         Result           from State, Destination_Address, Message, Message_Length &
      --#         null             from Utility.Timer_Done;
      pragma Import(C, Transmit);

      -- TODO: Lift this procedure into SPARK. It's much too complicated for C. The annotations
      -- below are guesses. Note that this procedure writes into the memory pointed at by
      -- Message, but SPARK does not know this.
      procedure Receive
        (Destination_Address : in  Integer;
         Message             : out Bounded_Strings.Incoming_Type;
         Read_Length         : in Integer;
         Result              : out Integer);
      --# global in out State, Utility.Hardware; in Utility.Timer_Done;
      --# derives State            from State, Utility.Hardware, Destination_Address, Read_Length &
      --#         Utility.Hardware from State, Utility.Hardware, Destination_Address, Read_Length &
      --#         Result           from State, Utility.Hardware, Destination_Address, Read_Length &
      --#         Message          from State, Utility.Hardware, Destination_Address, Read_Length &
      --#         null             from Utility.Timer_Done;
      pragma Import(C, Receive);

   end USCI_B1;


   procedure Send_Message_With_Pointer
     (Message             : in  Bounded_Strings.Outgoing_Record;
      Destination_Address : in  Integer;
      Tx_Success          : out Integer)
   --# global in out USCI_B1.State, Utility.Hardware; in Utility.Timer_Done;
   --# derives Tx_Success       from USCI_B1.State, Message, Destination_Address &
   --#         USCI_B1.State    from USCI_B1.State, Message, Destination_Address &
   --#         Utility.Hardware from USCI_B1.State, Message, Destination_Address, Utility.Hardware &
   --#         null             from Utility.Timer_Done;
   is
   begin
      USCI_B1.Transmit(Destination_Address, Message.Text, Message.Size, Tx_Success);
   end Send_Message_With_Pointer;


   procedure Write
     (Data                : in  Bounded_Strings.Outgoing_Record;
      Destination_Address : in  I2C_Address_Type;
      Tx_Success          : out I2C_Status)
   --# global in out USCI_B1.State, Utility.Hardware; in Utility.Timer_Done;
   --# derives       Tx_Success       from USCI_B1.State, Data, Destination_Address &
   --#               USCI_B1.State    from USCI_B1.State, Data, Destination_Address &
   --#               Utility.Hardware from USCI_B1.State, Data, Destination_Address, Utility.Hardware &
   --#               null             from Utility.Timer_Done;
   is
      Tx_Success_Int : Integer;
   begin
      Send_Message_With_Pointer(Data, Integer(Destination_Address), Tx_Success_Int);
      if Tx_Success_Int = 0 then
         Tx_Success := Success;
      elsif Tx_Success_Int = 1 then
         Tx_Success := Error_Timeout;
      else
         Tx_Success := Error_No_Ack;
      end if;
   end Write;


   procedure Read_Message_With_Pointer
     (Message             : in out Bounded_Strings.Incoming_Record;
      Destination_Address : in  Integer;
      Rx_Success          : out Integer)
   --# global in out USCI_B1.State, Utility.Hardware; in Utility.Timer_Done;
   --# derives       Rx_Success       from USCI_B1.State, Destination_Address, Utility.Hardware, Message &
   --#               USCI_B1.State    from USCI_B1.State, Destination_Address, Utility.Hardware, Message &
   --#               Message          from USCI_B1.State, Destination_Address, Utility.Hardware, Message &
   --#               Utility.Hardware from USCI_B1.State, Destination_Address, Utility.Hardware, Message &
   --#               null             from Utility.Timer_Done;
   is
   begin
      USCI_B1.Receive(Destination_Address, Message.Text, Message.Size, Rx_Success);
   end Read_Message_With_Pointer;


   procedure Read
     (Data                : in out Bounded_Strings.Incoming_Record;
      Destination_Address : in  I2C_Address_Type;
      Rx_Success          : out I2C_Status)
   --# global in out USCI_B1.State, Utility.Hardware; in Utility.Timer_Done;
   --# derives       USCI_B1.State    from USCI_B1.State, Destination_Address, Utility.Hardware, Data &
   --#               Data             from USCI_B1.State, Destination_Address, Utility.Hardware, Data &
   --#               Rx_Success       from USCI_B1.State, Destination_Address, Utility.Hardware, Data &
   --#               Utility.Hardware from USCI_B1.State, Destination_Address, Utility.Hardware, Data &
   --#               null             from Utility.Timer_Done;
   is
      Rx_Success_Int : Integer;
   begin
      Read_Message_With_Pointer(Data, Integer (Destination_Address), Rx_Success_Int);
      if Rx_Success_Int = 0 then
         Rx_Success := Success;
      elsif Rx_Success_Int = 1 then
         Rx_Success := Error_Timeout;
      else
         Rx_Success := Error_No_Ack;
      end if;
   end Read;


   procedure Initialize
   --# global  out USCI_B1.State;
   --# derives     USCI_B1.State from ;
   is
   begin
      USCI_B1.Initialize;
   end Initialize;

end I2C;
