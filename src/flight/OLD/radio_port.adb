---------------------------------------------------------------------------
-- FILE          : radio_port.adb
-- SUBJECT       : CubeSat implementation of radio communication package
-- PROGRAMMER    : (C) Copyright 2012 by Vermont Technical College
--
---------------------------------------------------------------------------
-- TODO: Turn SPARK_Mode back on.
pragma SPARK_Mode(Off);

package body Radio_Port
--# own State is USCI_A1.State;
is

   -- This serial port is connected to the radio.
   package USCI_A1
   --# own State;
   is
      procedure Initialize;
      --# global out State;
      --# derives State from ;
      pragma Import(C, Initialize);

      procedure Receive_Enable;
      --# global in out State;
      --# derives State from State;
      pragma Import(C, Receive_Enable);

      procedure Receive_Disable;
      --# global in out State;
      --# derives State from State;
      pragma Import(C, Receive_Disable);

      procedure Tx_Byte
        (Ch         : in Character;
         Tx_Success : out Integer);
      --# global in out State;
      --# derives       State from State, Ch &
      --#               Tx_Success from State, Ch;
      pragma Import(C, Tx_Byte);

      function Get_Rx_Buffer_Used return Integer;
      --# global in State;
      pragma Import(C, Get_Rx_Buffer_Used);

      function Get_Rx_Buffer_Size return Integer;
      --# global in State;
      pragma Import(C, Get_Rx_Buffer_Size);

      procedure Eat_Char(Ch : out Character);
      --# global in out State;
      --# derives State from State &
      --#         Ch    from State;
      pragma Import(C, Eat_Char);

   end USCI_A1;


   procedure Open
   --# global out USCI_A1.State;
   --# derives    USCI_A1.State from ;
   is
   begin
      USCI_A1.Initialize;
      USCI_A1.Receive_Enable;
   end Open;


   procedure Get_From_Radio(Ch : out Character; Rx_Success : out Boolean)
   --# global in out USCI_A1.State, Utility.Hardware; in Utility.Timer_Done;
   --# derives       USCI_A1.State    from USCI_A1.State &
   --#               Ch               from USCI_A1.State &
   --#               Rx_Success       from USCI_A1.State &
   --#               Utility.Hardware from Utility.Hardware &
   --#               null             from Utility.Timer_Done;
   is
      Finished : Boolean;
   begin
      Utility.Start_B(Utility.Millisecond_Type(20));

      loop
         Finished := Utility.Get_Timer_Finished;
         exit when Finished;
      end loop;

      if USCI_A1.Get_Rx_Buffer_Used > 0 then
         USCI_A1.Eat_Char(Ch);
         Rx_Success := True;
      else
         Ch := ' ';
         Rx_Success := False;
      end if;
   end Get_From_Radio;


   procedure Put_To_Radio(Ch : in  Character; Tx_Success : out Boolean)
   --# global in out USCI_A1.State;
   --# derives       USCI_A1.State from USCI_A1.State, Ch &
   --#               Tx_Success    from USCI_A1.State, Ch;
   is
      C_Bool : Integer;
   begin
      USCI_A1.Tx_Byte(Ch, C_Bool);
      if C_Bool = 1 then
         Tx_Success := True;
      else
         Tx_Success := False;
      end if;
   end Put_To_Radio;


   procedure Put_Line_To_Radio
     (Line : in  Bounded_Strings.Outgoing_Record; Tx_Success : out Boolean)
   --# global in out USCI_A1.State;
   --# derives USCI_A1.State from USCI_A1.State, Line &
   --#         Tx_Success    from USCI_A1.State, Line;
   is
      Counter : Bounded_Strings.Outgoing_Index_Type := 1;
   begin
      for I in Bounded_Strings.Outgoing_Index_Type range 1 .. Line.Size loop
         Put_To_Radio(Line.Text(I), Tx_Success);
         Counter := I;
         exit when Tx_Success = False;
      end loop;

      if Counter = Line.Size then
         Tx_Success := True;
      else
         Tx_Success := False;
      end if;
   end Put_Line_To_Radio;


end Radio_Port;
