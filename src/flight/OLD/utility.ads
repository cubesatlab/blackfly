--------------------------------------------------------------------------------
-- FILE          : utility.ads
-- SUBJECT       : Specification of various utility subprograms.
-- PROGRAMMER    : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package Utility
  -- TODO: It might be nice to distinguish between the ADC and LED hardware.
  -- TODO: Parts (but not all) of Hardware need to be modeled as external state abstractions.
  with Abstract_State => (Hardware, (Timer_Done with External => (Async_Writers)))
is
   type Byte_Type is mod 256;

   ------------------------
   -- Basic math operations
   ------------------------
   type ULong is mod 2**32;

   function Shift_Right(Value : in ULong; Distance : in Natural) return ULong
     with
       Global => null,
       Import,
       Convention => C;

   function Shift_Left(Value : in ULong; Distance : in Natural) return ULong
     with
       Global => null,
       Import,
       Convention => C;

   -- System wide types for time.
   type Millisecond_Type is range 0 .. 1000;
   type Second_Type is range 0 .. 60;
   type Minute_Type is range 0 .. 60;
   type Hour_Type is range 0 .. 24;
   type Day_Type is range 0 .. 365;
   type Year_Type is range 0 .. 99;

   type Timestamp_Type is
      record
         Milliseconds : Millisecond_Type;
         Seconds : Second_Type;
         Minutes : Minute_Type;
         Hours : Hour_Type;
         Days : Day_Type;
         Years : Year_Type;
      end record;

   -- Our native C functions expect longs so size must be 32!
   -- TODO: This is a hackish way of handling interfacing to C. Fix me!
   for Millisecond_Type'Size use 32;
   for Second_Type'Size use 32;
   for Minute_Type'Size use 32;
   for Hour_Type'Size use 32;
   for Day_Type'Size use 32;
   for Year_Type'Size use 32;

   -- Our MSP430 has ADCs!
   type ADCValue is range 0 .. 4095;

   -- There will be 6 PV LEDs, choosing a 0 selects all of them
   type PV_Index_Type is range 0 .. 6;

   -- Initializes various packages that interact directly with the hardware.
   procedure Initialize
     with
       Global => (Output => Hardware),
       Depends => (Hardware => null);

   -- Puts the platform asleep for a specificed number of milliseconds.
   -- The maximum sleep interval is currently 30 minutes.
   procedure Sleep(Millisecond_Count : in Millisecond_Type)
     with
       Global => (In_Out => Hardware),
       Depends => (Hardware =>+ Millisecond_Count);

   procedure Reset
     with
       Global => (Output => Hardware),
       Depends => (Hardware => null);

   procedure Start_B(Millisecond_Count : in Millisecond_Type)
     with
       Global => (In_Out => Hardware),
       Depends => (Hardware =>+ Millisecond_Count);

   procedure Stop_B
     with
       Global => (In_Out => Hardware),
       Depends => (Hardware =>+ null);

   function Get_Timer_Finished return Boolean
     with Global => (Input => Timer_Done);

   -- Reads from the designated Analog to Digital ports.
   procedure ADC_Read(A6_Value : out ADCValue; A7_Value : out ADCValue)
     with
       Global => (In_Out => Hardware),
       Depends => ((Hardware, A6_Value, A7_Value) => Hardware);

   -- Toggles the indicator LED on the Development Board. Useful for debugging.
   procedure LED_Toggle
     with
       Global => (In_Out => Hardware),
       Depends => (Hardware =>+ null);

   -- Turns on the indicator LED on the Development Board. Useful for debugging.
   procedure LED_On
     with
       Global => (In_Out => Hardware),
       Depends => (Hardware =>+ null);

   -- Turns off the indicator LED on the Development Board. Useful for debugging.
   procedure LED_Off
     with
       Global => (In_Out => Hardware),
       Depends => (Hardware =>+ null);

   procedure Byte_To_Chars(Byte : in Byte_Type; Char_1 : out Character; Char_2 : out Character)
     with
       Global => null,
       Depends => ((Char_1, Char_2) => Byte);


end Utility;
