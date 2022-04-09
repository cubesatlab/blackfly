--------------------------------------------------------------------------------
-- FILE   : blackfly-radio-messages.adb
-- SUBJECT: Body of a package that implements the main part of the BlackFly radio module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College and the University of Vermont
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

-- CubedOS core packages.
with Message_Manager;
with Name_Resolver;

with BlackFly.Radio.API;  -- Needed so that the types in the API can be used here.

package body BlackFly.Radio.Messages is
   use Message_Manager;

   -- The package initializer, if needed. This procedure might be called as the message loop
   -- (see below) is starting, or perhaps during package elaboration. If this procedure is not
   -- needed, it should be removed to avoid SPARK flow issues.
   --
   procedure Initialize is
   begin
      null;
   end Initialize;

   -------------------
   -- Message Handling
   -------------------

   -- Here is where the details of handing the messages is done. Probably there will be a
   -- separate subprogram for each message, but the exact organization is, obviously, dependent
   -- on the needs of the module. It might be useful to put these message handling subprograms
   -- into a private sibling package. That would move the complex details of message handling to
   -- a different file and reduce clutter in this file. This file is really just about decoding
   -- and dispatching the messages. We recommend that if a single internal package is used that
   -- it sould be called Sample_Module.Core (for example).

   procedure Handle_A_Request(Message : in Message_Record)
     with Pre => BlackFly.Radio.API.Is_A_Request(Message)
   is
      Status : Message_Status_Type;
   begin
      BlackFly.Radio.API.A_Request_Decode(Message, Status);
      -- Act on the request message.
   end Handle_A_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record) is
   begin
      if BlackFly.Radio.API.Is_A_Request(Message) then
         Handle_A_Request(Message);
      else
         -- An unknown message type has been received. What should be done about that?
         null;
      end if;
      -- When this procedure returns the message loop will immediately try to receive the next
      -- message. Note that all CubedOS send operations are non-blocking so sending an outgoing
      -- message will not delay execution.
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      -- Initialize the internal workings of the module (if required) before processing any of
      -- its messages. It may instead be appropriate for the package to initialize itself at
      -- elaboration time. Note that SPARK requires that applications use the configuration
      -- pragma of Partition_Elaboration_Policy set to Sequential. This ensures that all
      -- packages are elaborated before any library level tasks start.
      --
      Initialize;

      -- Process messages as they arrive. This simple loop may be all that is needed. It may
      -- also be appropriate to do some prechecks or preprocessing of messages before calling
      -- Process_Message.
      --
      loop
         Message_Manager.Fetch_Message(Name_Resolver.Radio.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end BlackFly.Radio.Messages;
