--------------------------------------------------------------------------------
-- FILE   : blackfly-radio.ads
-- SUBJECT: Top level package of the BlackFly radio module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College and the University of Vermont
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package BlackFly.Radio is

   ID : constant Message_Manager.Module_ID_Type := 14;

end BlackFly.Radio;
