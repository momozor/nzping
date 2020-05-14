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
   
   Ada.Text_IO.Put_Line ("-----------------------------------------------");
   for I in 1 .. Total_Arguments_Count loop
      declare
         Http     : Util.Http.Clients.Client;
         URI      : constant String := Ada.Command_Line.Argument (I);
         Response : Util.Http.Clients.Response;
      begin
         Http.Get (URI, Response);
         Ada.Text_IO.Put_Line ("|Status Code: " & Natural'Image (Response.Get_Status) & "|" & URI & "                            |");
      end;
   end loop;
   
   Ada.Text_IO.Put_Line ("------------------------------------------------");
end Nzping;
