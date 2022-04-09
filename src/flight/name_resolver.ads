--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification of a package holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2022 by Vermont Technical College
--
-- Each CubedOS application must provide a version of this file that maps "well-
-- known" names to (Domain_ID, Module_ID) pairs. The precise ID asssigments are
-- arbitrary. CubedOS applications should use the names to get the application-
-- specific ID assignments.
--
-- Each domain must instantiate the generic message manager to include enough
-- mailboxes to hold the highest numbered Module_ID in that domain. That instantiation
-- *must* be called Message_Manager (this name is hard-coded in the modules). One
-- consequence of this is that each domain must be in a separate executable.
--
-- Copy this file to your CubedOS application code base and edit it to mention
-- only the modules you need, including your application-specific modules. Assign
-- IDs "compactly" (with no gaps in the numbering) so that you can create a
-- Message_Manager instantiation with the minimum number of mailboxes.
--------------------------------------------------------------------------------
with Message_Manager; use Message_Manager;

package Name_Resolver is

   -- Core Modules.
   -- Declarations that are commented out are for currently unused core modules.

   -- Name_Resolver            : constant Message_Address := (0, 1);
   -- Log_Server               : constant Message_Address := (0, 2);
   -- Publish_Subscribe_Server : constant Message_Address := (0, 3);
   -- Time_Server              : constant Message_Address := (0, 4);
   -- Event_Server             : constant Message_Address := (0, 5);
   -- File_Server              : constant Message_Address := (0, 6);
   -- Table_Server             : constant Message_Address := (0, 7);
   -- Interpreter              : constant Message_Address := (0, 8);

   -- Application-Specific Modules.
   -- Make up names as you see fit (typically the same as your module's top level package).
   -- Be sure there are no duplicate (Domain_ID, Module_ID) pairs.

   Radio : constant Message_Address := (0, 1);

end Name_Resolver;
