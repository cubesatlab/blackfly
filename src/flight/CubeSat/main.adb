--------------------------------------------------------------------------------
-- FILE   : main.adb
-- SUBJECT: Main program of the BlackFly flight control software.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College and the University of Vermont
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Real_Time;
with System;

-- Bring in the necessary modules, both from CubedOS and from this application.
with BlackFly.Radio.Messages;

-- The modules are unreferenced here, but need to be "with-ed" to make them part of the executable.
pragma Unreferenced(BlackFly.Radio.Messages);

procedure Main is

   pragma Priority(System.Priority'First);
   use type Ada.Real_Time.Time;
   Next_Release : Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds(1000);
begin
   -- This loop does nothing at the lowest priority. It spends most of its time sleeping.
   loop
      delay until Next_Release;
      Next_Release := Next_Release + Ada.Real_Time.Milliseconds(1000);
      return;
   end loop;
end Main;
