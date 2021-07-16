--------------------------------------------------------------------------------
-- FILE   : ax25.ads
-- SUBJECT: Specification of a package that does formatting/unformatting of ax25 data
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Utility;
with Time;
with Bounded_Strings;
with Files.File_System;
with File_Handler;

use type Utility.Byte_Type;

package Ax25 is

   subtype AX25_Address_Index is Positive range 1..6;
   subtype AX25_Address_Type is String(AX25_Address_Index);

   procedure Remove(Payload : in out Bounded_Strings.Payload_Record; Success : out Boolean)
     with
       Global => (Input => (Time.Time_Since_Reset),
                  In_Out => Files.File_System.State),
       Depends => (Payload =>+ null,
                   Success => (Payload),
                   Files.File_System.State =>+ (Payload, Time.Time_Since_Reset));
end Ax25;
