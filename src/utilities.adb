with Ada.Text_IO;

package body Utilities is
   package Text_IO renames Ada.Text_IO;

   procedure Print_Border_Lines (Lines_Length : Integer) is
   begin
      for I in 1 .. Lines_Length loop
         Text_IO.Put ("-");
      end loop;
   end Print_Border_Lines;

end Utilities;
