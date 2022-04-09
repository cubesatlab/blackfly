--------------------------------------------------------------------------------
-- FILE   : message_manager.adb
-- SUBJECT: Package holding the mailboxes used by CubedOS message passing.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College and the University of Vermont
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Generic_Message_Manager;
pragma Elaborate_All(CubedOS.Generic_Message_Manager);

package Message_Manager is
  new CubedOS.Generic_Message_Manager
    (Domain_Number =>  1,
     Module_Count  => 16,
     Mailbox_Size  =>  8,
     Maximum_Message_Size => 1024);
