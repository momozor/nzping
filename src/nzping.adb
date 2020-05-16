with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Util.Http.Clients;
with Util.Http.Clients.Curl;

procedure Nzping is
   package Text_IO renames Ada.Text_IO;
   package Unbounded renames Ada.Strings.Unbounded;
   
   procedure Print_Border_Lines (Lines_Length: in Integer) is
   begin
      for I in 1 .. Lines_Length loop
         Text_IO.Put ("-");
      end loop;
   end Print_Border_Lines;
   
   DEFAULT_TOTAL_SCREEN_WIDTH : constant Integer := 80;
   Total_Arguments_Count : constant Natural := 
     Ada.Command_Line.Argument_Count;
begin
   if Total_Arguments_Count = 0 then
      Text_IO.Put_Line ("Usage: nzping url ...");
      Text_IO.Put_Line ("Example: ./nzping https://github.com");
      return;
   end if;
   
   Util.Http.Clients.Curl.Register;
   
   Print_Border_Lines (DEFAULT_TOTAL_SCREEN_WIDTH);
   Text_IO.Put_Line ("");
   
   loop
      for I in 1 .. Total_Arguments_Count loop
         declare
            Http     : Util.Http.Clients.Client;
            URI      : constant String := Ada.Command_Line.Argument (I);
            Response : Util.Http.Clients.Response;
            Status_Code : Natural;
            Status_Type : Unbounded.Unbounded_String;
         begin
            begin
               Http.Get (URI, Response);
               Status_Code := Response.Get_Status;
               
               case Status_Code is
                  when 200 .. 399 =>
                     Status_Type := Unbounded.To_Unbounded_String ("SUCCESS");
                  when others =>
                     Status_Type := Unbounded.To_Unbounded_String ("ERROR");
               end case;
               
               Text_IO.Put_Line ("URI: " 
                                   & URI
                                   & " | Status Type: " 
                                   & Unbounded.To_String (Status_Type)
                                   & " | Status Code: "
                                   & Natural'Image (Status_Code)
                                );
            exception
               when Error : UTIL.HTTP.CLIENTS.CONNECTION_ERROR  =>
                  Text_IO.Put_Line ("Host " & URI & " not found!");
                  
                  Print_Border_Lines (DEFAULT_TOTAL_SCREEN_WIDTH);
                  
                  Text_IO.Put ("");
                  return;
                  
            end;
         end;
      end loop;
      
      Print_Border_Lines (DEFAULT_TOTAL_SCREEN_WIDTH);
      Text_IO.Put_Line ("");
      
      Text_IO.Put_Line ("Checking for another 60 seconds..");
      delay Duration (60.0);
   end loop;
end Nzping;
