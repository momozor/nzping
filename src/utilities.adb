with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;

package body Utilities is
   package Text_IO renames Ada.Text_IO;
   package Calendar renames Ada.Calendar;
   package Calendar_Formatting renames Ada.Calendar.Formatting;

   procedure Print_Border_Lines (Lines_Length : Integer) is
   begin
      for I in 1 .. Lines_Length loop
         Text_IO.Put ("-");
      end loop;
   end Print_Border_Lines;

   procedure Print_Current_Time is
      Now : constant Calendar.Time := Calendar.Clock;
   begin
      Text_IO.Put_Line ("[ " & Calendar_Formatting.Image (Now) & " ]");
   end Print_Current_Time;

end Utilities;
