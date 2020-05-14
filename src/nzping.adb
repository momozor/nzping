with Ada.Text_IO;
with Ada.Command_Line;
with Util.Http.Clients;
with Util.Http.Clients.Curl;

procedure Nzping is
   TOTAL_SCREEN_WIDTH : constant Integer := 80;
   Total_Arguments_Count : constant Natural := Ada.Command_Line.Argument_Count;
begin
   if Total_Arguments_Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: nzping url ...");
      Ada.Text_IO.Put_Line ("Example: nzping https://twitter.com");
      return;
   end if;
   
   Util.Http.Clients.Curl.Register;
   
   while True loop
      Ada.Text_IO.Put_Line ("-----------------------------------------------");
      for I in 1 .. Total_Arguments_Count loop
         declare
            Http     : Util.Http.Clients.Client;
            URI      : constant String := Ada.Command_Line.Argument (I);
            Response : Util.Http.Clients.Response;
         begin
            begin
               Http.Get (URI, Response);
               Ada.Text_IO.Put_Line ("|URI: " & URI & " | Status Code: " & Natural'Image (Response.Get_Status));
            exception
               when Error : UTIL.HTTP.CLIENTS.CONNECTION_ERROR  =>
                  Ada.Text_IO.Put_Line ("Host " & URI & " not found!");
            end;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("------------------------------------------------");
      Ada.Text_IO.Put_Line ("");
      Ada.Text_IO.Put_Line ("Checking for another 60 seconds..");
      delay Duration(60.0);
   end loop;
end Nzping;
