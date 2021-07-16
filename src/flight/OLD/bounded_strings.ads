---------------------------------------------------------------------------
-- FILE       : bounded_strings.ads
-- SUBJECT    : Specification of a package for manipulating general purpose bounded strings.
-- PROGRAMMER : (C) Copyright 2015 by Vermont Technical College
--
-- This package contains some facilities of particular interest to CubeSat Laboratory projects.
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

-- This package contains the facilities for manipulating bounded length strings. The types
-- Incoming_Record and Outgoing_Record are used during I/O operations. By using separate types
-- for input and output, information flowing into and out of the program can't be accidentally
-- mixed. To conserve memory the maximum lengths should be a small as feasible.
--
-- TODO: Perhaps these types should be discriminated based on length?
-- TODO: A number of the subprograms here are nearly identical except for types. Use generics?
--
package Bounded_Strings is

   Maximum_Incoming_Length : constant := 265;
   subtype Incoming_Index_Type is Positive range 1 .. Maximum_Incoming_Length;
   subtype Incoming_Size_Type  is Natural  range 0 .. Maximum_Incoming_Length;
   subtype Incoming_Type       is String(Incoming_Index_Type);
   type    Incoming_Record     is
      record
         Text : Incoming_Type;
         Size : Incoming_Size_Type;
      end record;

   Maximum_Outgoing_Length : constant := 265;
   subtype Outgoing_Index_Type is Positive range 1 .. Maximum_Outgoing_Length;
   subtype Outgoing_Size_Type  is Natural  range 0 .. Maximum_Outgoing_Length;
   subtype Outgoing_Type       is String(Outgoing_Index_Type);
   type    Outgoing_Record     is
      record
         Text : Outgoing_Type;
         Size : Outgoing_Size_Type;
      end record;

   Maximum_Payload_Length : constant := 255;
   subtype Payload_Index_Type is Positive range 1 .. Maximum_Payload_Length;
   subtype Payload_Size_Type  is Natural  range 0 .. Maximum_Payload_Length;
   subtype Payload_Type       is String(Payload_Index_Type);
   type    Payload_Record     is
      record
         Text : Payload_Type;
         Size : Payload_Size_Type;
      end record;

   -- A useful function for simplifying certain assertions.
   function Min(X, Y : Natural) return Natural is
     (if X < Y then X else Y)
   with Ghost;

   -- This procedure appends a String onto the end of an Outgoing_Record. Only Size characters
   -- of the string are copied (starting with the first character). If the entire string is
   -- needed Additional'Length should be used to specify an appropriate size.
   --
   procedure Append_Outgoing_Line
     (Line : in out Outgoing_Record; Additional : in String; Size : in Natural);


   -- This function copies a String into an Outgoing_Record. Only Size characters of the string
   -- are copied (starting with the first character). If the entire string is needed Line'Length
   -- should be used to specify an appropriate size.
   --
   function Prepare_Outgoing_Line(Line : String; Size : Natural) return Outgoing_Record
     with Post =>
       Prepare_Outgoing_Line'Result.Size = Min(Maximum_Outgoing_Length, Min(Line'Length, Size));

   -- This function copies a String into an Incoming_Record. Only Size characters of the string
   -- are copied (starting with the first character). If the entire string is needed Line'Length
   -- should be used to specify an appropriate size.
   --
   function Prepare_Incoming_Line(Line : String; Size : Natural) return Incoming_Record
     with Post =>
       Prepare_Incoming_Line'Result.Size = Min(Maximum_Incoming_Length, Min(Line'Length, Size));

   -- This procedure appends a String onto the end of an Payload_Record. Only Size characters of
   -- the string are copied (starting with the first character). If the entire string is needed
   -- Additional'Length should be used to specify an appropriate size.
   --
   procedure Append_Payload_Line
     (Line : in out Payload_Record; Additional : in String; Size : in Natural);

   -- This function copies a String into an Payload_Record. Only Size characters of the string
   -- are copied (starting with the first character). If the entire string is needed Line'Length
   -- should be used to specify an appropriate size.
   --
   function Prepare_Payload_Line(Line : String; Size : Natural) return Payload_Record
     with Post =>
       Prepare_Payload_Line'Result.Size = Min(Maximum_Payload_Length, Min(Line'Length, Size));

end Bounded_Strings;

