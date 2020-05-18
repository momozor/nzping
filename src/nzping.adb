with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Util.Http.Clients;
with Util.Http.Clients.Curl;

with Utilities;

procedure Nzping is
   package Text_IO renames Ada.Text_IO;
   package Float_Text_IO renames Ada.Float_Text_IO;
   package Unbounded renames Ada.Strings.Unbounded;
   package Fixed renames Ada.Strings.Fixed;
   package NZUtils renames Utilities;

   Default_Total_Screen_Width : constant Integer := 80;
   Total_Arguments_Count : constant Natural := Ada.Command_Line.Argument_Count;
   Recheck_Interval_Seconds   : Float;

begin
   if Total_Arguments_Count = 0 or Total_Arguments_Count = 1 then
      Text_IO.Put_Line ("Usage: nzping interval_in_seconds url ...");
      Text_IO.Put_Line ("Example: ./nzping 60.0 https://github.com");
      return;
   end if;

   begin
      Recheck_Interval_Seconds := Float'Value (Ada.Command_Line.Argument (1));

   exception
      when Error : Constraint_Error =>
         Text_IO.Put_Line
           ("[ERROR] interval time must be in floating-point or integer!");
         return;
   end;

   Util.Http.Clients.Curl.Register;

   NZUtils.Print_Border_Lines (Default_Total_Screen_Width);
   Text_IO.New_Line;

   loop
      for I in 2 .. Total_Arguments_Count loop
         declare
            Http        : Util.Http.Clients.Client;
            URI         : constant String := Ada.Command_Line.Argument (I);
            Response    : Util.Http.Clients.Response;
            Status_Code : Natural;
            Status_Type : Unbounded.Unbounded_String;
         begin
            begin

               if Fixed.Index (URI, "http") > 0 or
                 Fixed.Index (URI, "https") > 0
               then

                  Http.Get (URI, Response);

               else
                  Text_IO.Put_Line
                    ("[ERROR] Link must start with http or https!!!");
                  return;
               end if;

               Status_Code := Response.Get_Status;

               case Status_Code is
                  when 200 .. 399 =>
                     Status_Type := Unbounded.To_Unbounded_String ("SUCCESS");
                  when others =>
                     Status_Type := Unbounded.To_Unbounded_String ("ERROR");
               end case;

               Text_IO.Put_Line
                 ("URI: " & URI & " | Status Type: " &
                  Unbounded.To_String (Status_Type) & " | Status Code: " &
                  Natural'Image (Status_Code));
            exception
               when Error : Util.Http.Clients.Connection_Error =>
                  Text_IO.Put_Line ("Host " & URI & " not found!");

                  NZUtils.Print_Border_Lines (Default_Total_Screen_Width);

                  Text_IO.New_Line;
                  return;

            end;
         end;
      end loop;

      NZUtils.Print_Border_Lines (Default_Total_Screen_Width);
      Text_IO.New_Line;

      Text_IO.Put ("Checking for another");
      Float_Text_IO.Put
        (Recheck_Interval_Seconds, Fore => 5, Aft => 2, Exp => 0);
      Text_IO.Put (" second(s)..");
      Text_IO.New_Line;

      delay Duration (Recheck_Interval_Seconds);
   end loop;
end Nzping;
