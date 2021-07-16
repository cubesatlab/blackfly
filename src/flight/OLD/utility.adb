---------------------------------------------------------------------------
-- FILE          : utility.adb
-- SUBJECT       : Implementation of various utility subprograms.
-- PROGRAMMER    : (C) Copyright 2012 by Vermont Technical College
--
---------------------------------------------------------------------------
-- TODO: Turn SPARK_Mode back on.
pragma SPARK_Mode(Off);

package body Utility
--# own Hardware   is ADC.Hardware, LED.Hardware, Timer_A.Hardware, Timer_B.Hardware, System.Hardware &
--#     Timer_Done is in Timer_B.Finishing_Flag;
is

   --# inherit Utility;
   package LED
   --# own Hardware;
   is
      procedure Initialize;
      --# global out Hardware;
      --# derives Hardware from ;
      pragma Import(C, Initialize);

      ----------------------
      -- main led procedures
      procedure On;
      --# global in out Hardware;
      --# derives Hardware from Hardware;
      pragma Import(C, On);

      procedure Off;
      --# global in out Hardware;
      --# derives Hardware from Hardware;
      pragma Import(C, Off);

      procedure Toggle;
      --# global in out Hardware;
      --# derives Hardware from Hardware;
      pragma Import(C, Toggle);

      ----------------------
      -- PV led procedures
      procedure PV_On(PV_Index : in Utility.PV_Index_Type);
      --# global in out Hardware;
      --# derives Hardware from Hardware, PV_Index;
      pragma Import(C, PV_On);

      procedure PV_Off(PV_Index : in Utility.PV_Index_Type);
      --# global in out Hardware;
      --# derives Hardware from Hardware, PV_Index;
      pragma Import(C, PV_Off);

      procedure PV_Toggle(PV_Index : in Utility.PV_Index_Type);
      --# global in out Hardware;
      --# derives Hardware from Hardware, PV_Index;
      pragma Import(C, PV_Toggle);
   end LED;

   --# inherit Utility;
   package ADC
   --# own Hardware;
   is
      procedure Initialize;
      --# global out Hardware;
      --# derives Hardware from ;
      pragma Import(C, Initialize);

      procedure Read(A6 : out Utility.ADCValue; A7 : out Utility.ADCValue);
      --# global in out Hardware;
      --# derives Hardware from Hardware &
      --#         A6       from Hardware &
      --#         A7       from Hardware;
      pragma Import(C, Read);
   end ADC;

   --# inherit Utility;
   package Timer_A
   --# own Hardware;
   is
      procedure Initialize;
      --# global out Hardware;
      --# derives Hardware from ;
      pragma Import(C, Initialize);

      procedure Sleep(Millisecond_Count : Utility.Millisecond_Type);
      --# global in out Hardware;
      --# derives Hardware from Hardware, Millisecond_Count;
      pragma Import(C, Sleep);
   end Timer_A;

   --# inherit Utility;
   package Timer_B
   --# own Hardware, in Finishing_Flag;
   is
      procedure Initialize;
      --# global out Hardware;
      --# derives Hardware from ;
      pragma Import(C, Initialize);

      procedure Start_B (Millisecond_Count : Utility.Millisecond_Type);
      --# global in out Hardware;
      --# derives Hardware from Hardware, Millisecond_Count;
      pragma Import(C, Start_B);

      procedure Stop_B;
      --# global in out Hardware;
      --# derives Hardware from Hardware;
      pragma Import(C, Stop_B);

      function Get_Finished return Integer;
      --# global in Finishing_Flag;
      pragma Import (C, Get_Finished);
   end Timer_B;

   --# inherit Utility;
   package System
   --# own Hardware;
   is
      procedure Initialize;
      --# global out Hardware;
      --# derives Hardware from ;
      pragma Import(C, Initialize);

      procedure Reset;
      --# global out Hardware;
      --# derives Hardware from ;
      pragma Import(C, Reset);
   end System;


   procedure Initialize
   --# global out ADC.Hardware, LED.Hardware, Timer_A.Hardware, Timer_B.Hardware, System.Hardware;
   --# derives ADC.Hardware     from &
   --#         LED.Hardware     from &
   --#         Timer_A.Hardware from &
   --#         Timer_B.Hardware from &
   --#         System.Hardware  from ;
   is
   begin
      ADC.Initialize;
      LED.Initialize;
      Timer_A.Initialize;
      Timer_B.Initialize;
      System.Initialize;
   end Initialize;

   procedure Reset
   --# global out System.Hardware;
   --# derives System.Hardware from ;
   is
   begin
      System.Reset;
   end Reset;


   procedure ADC_Read(A6_Value : out ADCValue; A7_Value : out ADCValue)
   --# global in out ADC.Hardware;
   --# derives ADC.Hardware from ADC.Hardware &
   --#         A6_Value         from ADC.Hardware &
   --#         A7_Value         from ADC.Hardware;
   is
   begin
      ADC.Read (A6_Value, A7_Value);
   end ADC_Read;


   procedure Sleep(Millisecond_Count : in Millisecond_Type)
   --# global in out Timer_A.Hardware;
   --# derives Timer_A.Hardware from Timer_A.Hardware, Millisecond_Count;
   is
   begin
      Timer_A.Sleep(Millisecond_Count);
   end Sleep;


   procedure Start_B(Millisecond_Count : in Millisecond_Type)
   --# global in out Timer_B.Hardware;
   --# derives Timer_B.Hardware from Timer_B.Hardware, Millisecond_Count;
   is
   begin
      Timer_B.Start_B(Millisecond_Count);
   end Start_B;


   procedure Stop_B
   --# global in out Timer_B.Hardware;
   --# derives Timer_B.Hardware from Timer_B.Hardware;
   is
   begin
      Timer_B.Stop_B;
   end Stop_B;


   function Get_Timer_Finished return Boolean
   --# global in Timer_B.Finishing_Flag;
   is
      Finished : Integer;
      Result   : Boolean;
   begin
      Finished := Timer_B.Get_Finished;
      if Finished = 1 then
         Result := True;
      else
         Result := False;
      end if;
      return Result;
   end Get_Timer_Finished;


   procedure LED_Toggle
   --# global in out LED.Hardware;
   --# derives LED.Hardware from LED.Hardware;
   is
   begin
      LED.Toggle;
   end LED_Toggle;


   procedure LED_On
   --# global in out LED.Hardware;
   --# derives LED.Hardware from LED.Hardware;
   is
   begin
      LED.On;
   end LED_On;


   procedure LED_Off
   --# global in out LED.Hardware;
   --# derives LED.Hardware from LED.Hardware;
   is
   begin
      LED.Off;
   end LED_Off;

   procedure Byte_To_Chars(Byte : in Byte_Type; Char_1 : out Character; Char_2 : out Character)
   is
   begin
      -- first char
      if (Byte - (Byte rem 16#0F#)) / 16#10# > 16#10# then
         Char_1 := Character'Val(((Byte - (Byte rem 16#10#)) / 16#10#) + 55);
      else
         Char_1 := Character'Val(((Byte - (Byte rem 16#10#)) / 16#10#) + 48);
      end if;

      -- second half
      if (Byte rem 16#10#) > 16#09# then
         Char_2 := Character'Val((Byte rem 16) + 55);
      else
         Char_2 := Character'Val((Byte rem 16) + 48);
      end if;
   end Byte_To_Chars;

end Utility;
