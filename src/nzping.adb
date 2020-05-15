with Ada.Text_IO;
with Ada.Command_Line;
with Util.Http.Clients;
with Util.Http.Clients.Curl;

procedure Nzping is
   package Text_IO renames Ada.Text_IO;
   TOTAL_SCREEN_WIDTH : constant Integer := 80;
   Total_Arguments_Count : constant Natural := Ada.Command_Line.Argument_Count;
begin
   if Total_Arguments_Count = 0 then
      Text_IO.Put_Line ("Usage: nzping url ...");
      Text_IO.Put_Line ("Example: nzping https://twitter.com");
      return;
   end if;
   
   Util.Http.Clients.Curl.Register;
   
   for I in 1 .. TOTAL_SCREEN_WIDTH loop
      Text_IO.Put ("-");
   end loop;
   
   while True loop
      for I in 1 .. Total_Arguments_Count loop
         declare
            Http     : Util.Http.Clients.Client;
            URI      : constant String := Ada.Command_Line.Argument (I);
            Response : Util.Http.Clients.Response;
         begin
            begin
               Http.Get (URI, Response);
               Text_IO.Put_Line ("URI: " & URI & " | Status Code: " & Natural'Image (Response.Get_Status));
            exception
               when Error : UTIL.HTTP.CLIENTS.CONNECTION_ERROR  =>
                  Text_IO.Put_Line ("Host " & URI & " not found!");
                  return;
                  
            end;
         end;
      end loop;
      for I in 1 .. TOTAL_SCREEN_WIDTH loop
         Text_IO.Put ("-");
      end loop;
      Text_IO.Put_Line ("");
      Text_IO.Put_Line ("Checking for another 60 seconds..");
      delay Duration(60.0);
   end loop;
end Nzping;
