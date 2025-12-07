-------------------------------------------------------------------------------
--  Polyglot_TUI.App
--
--  Main application controller implementation.
--  Manages the TUI lifecycle, views, and user interaction.
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Polyglot_TUI.Terminal;  use Polyglot_TUI.Terminal;

package body Polyglot_TUI.App is

   --  View dimensions
   Header_Height : constant := 1;
   Footer_Height : constant := 2;
   Status_Row    : constant := 1;

   procedure Draw_Header (App : Application) is
      Title : constant String := "Polyglot I18n TUI";
      Header_Style : constant Style :=
         (Foreground => Color_White,
          Background => Color_Blue,
          Attributes => (Bold => True, others => False));
   begin
      Set_Style (Header_Style);
      Clear_Line (1);
      Put_At (1, 1, Title);

      --  Show current locale on right side
      if App.State.Active_Locale /= Empty_Locale then
         declare
            Loc : constant String :=
               "[" & To_String (App.State.Active_Locale) & "]";
         begin
            Put_At (1, App.Term_Size.Width - Loc'Length + 1, Loc);
         end;
      end if;

      Reset_Style;
   end Draw_Header;

   procedure Draw_Footer (App : Application) is
      Help_Line : constant String :=
         "q:Quit  j/k:Navigate  Enter:Select  /:Search  ?:Help";
      Footer_Style : constant Style :=
         (Foreground => Color_Black,
          Background => Color_White,
          Attributes => No_Attributes);
   begin
      --  Status line
      if App.State.Status_Length > 0 then
         Put_At (App.Term_Size.Height - 1, 1,
                 App.State.Status_Message (1 .. App.State.Status_Length));
      else
         Clear_Line (App.Term_Size.Height - 1);
      end if;

      --  Help line
      Set_Style (Footer_Style);
      Clear_Line (App.Term_Size.Height);
      Put_At (App.Term_Size.Height, 1, Help_Line);
      Reset_Style;
   end Draw_Footer;

   procedure Draw_Catalog_Browser (App : Application) is
      Content_Start : constant Positive := Header_Height + 1;
      Content_End   : constant Positive := App.Term_Size.Height - Footer_Height;
      Row           : Positive := Content_Start;
      Idx           : Natural := App.State.Scroll_Offset;
   begin
      --  Clear content area
      for I in Content_Start .. Content_End loop
         Clear_Line (I);
      end loop;

      if App.Locale_Count = 0 then
         Put_At (Content_Start + 2, 3, "No catalogs loaded.");
         Put_At (Content_Start + 3, 3, "Use :load <locale> <path> to load a catalog.");
         return;
      end if;

      --  Draw catalog entries
      for I in 1 .. Natural (App.Locale_Count) loop
         exit when Row > Content_End;

         declare
            Cat   : constant Catalog := App.Catalogs (I);
            Loc   : constant String := To_String (Get_Locale (Cat));
            Count : constant String := Natural'Image (Entry_Count (Cat));
            Line  : String (1 .. App.Term_Size.Width) := (others => ' ');
            Is_Selected : constant Boolean := (I - 1 = Idx);
         begin
            --  Format: [locale] - N entries
            Move (Loc, Line, 3);
            Move (" - " & Trim (Count, Ada.Strings.Left) & " entries",
                  Line, Loc'Length + 4);

            if Is_Selected then
               Set_Style ((Foreground => Color_Black,
                           Background => Color_Cyan,
                           Attributes => No_Attributes));
            end if;

            Put_At (Row, 1, Line);

            if Is_Selected then
               Reset_Style;
            end if;
         end;

         Row := Row + 1;
      end loop;
   end Draw_Catalog_Browser;

   procedure Draw_Key_Browser (App : Application) is
      Content_Start : constant Positive := Header_Height + 2;
      Content_End   : constant Positive := App.Term_Size.Height - Footer_Height;
   begin
      --  Draw box for key list
      Draw_Box (Header_Height + 1, 1,
                App.Term_Size.Width, Content_End - Header_Height,
                Single, "Translation Keys");

      --  Content would go here based on selected catalog
      Put_At (Content_Start, 3, "Select a catalog first...");
   end Draw_Key_Browser;

   procedure Draw_Editor (App : Application) is
      Content_Start : constant Positive := Header_Height + 2;
   begin
      Draw_Box (Header_Height + 1, 1,
                App.Term_Size.Width, App.Term_Size.Height - Header_Height - Footer_Height,
                Double, "Edit Translation");

      if App.State.Selected_Key /= Empty_Key then
         Put_At (Content_Start, 3,
                 "Key: " & To_String (App.State.Selected_Key));
      end if;
   end Draw_Editor;

   procedure Draw_Help (App : Application) is
      pragma Unreferenced (App);
      Content_Start : constant Positive := 4;
   begin
      Clear;
      Put_At (2, 3, "Polyglot I18n TUI - Help");
      Put_At (Content_Start, 3,     "Navigation:");
      Put_At (Content_Start + 1, 5, "j/Down    - Move down");
      Put_At (Content_Start + 2, 5, "k/Up      - Move up");
      Put_At (Content_Start + 3, 5, "g         - Go to top");
      Put_At (Content_Start + 4, 5, "G         - Go to bottom");
      Put_At (Content_Start + 5, 5, "PgUp/PgDn - Page up/down");
      Put_At (Content_Start + 7, 3, "Actions:");
      Put_At (Content_Start + 8, 5, "Enter     - Select/Edit");
      Put_At (Content_Start + 9, 5, "/         - Search");
      Put_At (Content_Start + 10, 5, "d         - Delete");
      Put_At (Content_Start + 11, 5, "s         - Save");
      Put_At (Content_Start + 12, 5, "Esc       - Go back");
      Put_At (Content_Start + 13, 5, "q         - Quit");
      Put_At (Content_Start + 15, 3, "Press any key to return...");
   end Draw_Help;

   procedure Draw (App : Application) is
   begin
      case App.State.Current_View is
         when Catalog_Browser =>
            Draw_Header (App);
            Draw_Catalog_Browser (App);
            Draw_Footer (App);

         when Key_Browser =>
            Draw_Header (App);
            Draw_Key_Browser (App);
            Draw_Footer (App);

         when Key_Editor =>
            Draw_Header (App);
            Draw_Editor (App);
            Draw_Footer (App);

         when Search_View =>
            Draw_Header (App);
            Draw_Catalog_Browser (App);
            Draw_Footer (App);

         when Help_View =>
            Draw_Help (App);

         when Settings_View =>
            Draw_Header (App);
            Put_At (3, 3, "Settings (not implemented)");
            Draw_Footer (App);
      end case;

      Refresh;
   end Draw;

   procedure Handle_Input (App : in out Application; Key : Key_Event) is
   begin
      case Key.Code is
         when Key_Escape =>
            Go_Back (App);

         when Key_Enter =>
            Select_Item (App);

         when Key_Up =>
            Move_Up (App);

         when Key_Down =>
            Move_Down (App);

         when Key_Page_Up =>
            Page_Up (App);

         when Key_Page_Down =>
            Page_Down (App);

         when Key_Char =>
            case Key.Character is
               when 'q' | 'Q' =>
                  App.State.Is_Running := False;

               when 'j' =>
                  Move_Down (App);

               when 'k' =>
                  Move_Up (App);

               when 'g' =>
                  Go_To_Top (App);

               when 'G' =>
                  Go_To_Bottom (App);

               when '/' =>
                  Switch_View (App, Search_View);

               when '?' =>
                  Switch_View (App, Help_View);

               when 'd' =>
                  Delete_Item (App);

               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
   end Handle_Input;

   procedure Initialize (App : out Application) is
   begin
      App.State := (Current_View   => Catalog_Browser,
                    Previous_View  => Catalog_Browser,
                    Active_Locale  => Empty_Locale,
                    Selected_Key   => Empty_Key,
                    Cursor         => (Row => 1, Column => 1),
                    Scroll_Offset  => 0,
                    Search_Query   => Empty_Key,
                    Is_Running     => True,
                    Has_Unsaved    => False,
                    Status_Message => (others => ' '),
                    Status_Length  => 0);
      App.Locale_Count := 0;
      App.Term_Size := Get_Dimensions;

      Terminal.Initialize;
   end Initialize;

   procedure Run (App : in Out Application) is
      Key : Key_Event;
   begin
      while App.State.Is_Running loop
         Draw (App);
         Key := Wait_Key;
         Handle_Input (App, Key);
      end loop;
   end Run;

   procedure Shutdown (App : in Out Application) is
   begin
      Terminal.Shutdown;
      App.State.Is_Running := False;
   end Shutdown;

   function Get_State (App : Application) return App_State is
   begin
      return App.State;
   end Get_State;

   function Is_Running (App : Application) return Boolean is
   begin
      return App.State.Is_Running;
   end Is_Running;

   procedure Switch_View (App : in Out Application; View : View_Mode) is
   begin
      App.State.Previous_View := App.State.Current_View;
      App.State.Current_View := View;
   end Switch_View;

   procedure Go_Back (App : in Out Application) is
   begin
      App.State.Current_View := App.State.Previous_View;
   end Go_Back;

   procedure Load_Catalog
      (App    : in Out Application;
       Locale : Locale_String;
       Path   : String)
   is
      pragma Unreferenced (Path);
   begin
      if App.Locale_Count < Max_Locales then
         App.Locale_Count := App.Locale_Count + 1;
         App.Catalogs (Natural (App.Locale_Count)) := Empty (Locale);
         Set_Status (App, "Loaded catalog for " & To_String (Locale));
      else
         Set_Status (App, "Error: Maximum number of locales reached");
      end if;
   end Load_Catalog;

   procedure Save_Catalog
      (App    : in out Application;
       Locale : Locale_String;
       Path   : String)
   is
      pragma Unreferenced (Locale, Path);
   begin
      App.State.Has_Unsaved := False;
      Set_Status (App, "Catalog saved");
   end Save_Catalog;

   procedure Set_Active_Locale
      (App    : in out Application;
       Locale : Locale_String)
   is
   begin
      App.State.Active_Locale := Locale;
   end Set_Active_Locale;

   procedure Move_Up (App : in out Application) is
   begin
      if App.State.Cursor.Row > 1 then
         App.State.Cursor.Row := App.State.Cursor.Row - 1;
         if App.State.Scroll_Offset > 0 and
            App.State.Cursor.Row <= App.State.Scroll_Offset
         then
            App.State.Scroll_Offset := App.State.Scroll_Offset - 1;
         end if;
      end if;
   end Move_Up;

   procedure Move_Down (App : in Out Application) is
      Max_Items : constant Natural := Natural (App.Locale_Count);
   begin
      if App.State.Cursor.Row < Max_Items then
         App.State.Cursor.Row := App.State.Cursor.Row + 1;
         --  Adjust scroll if needed
         if App.State.Cursor.Row > App.Term_Size.Height - 4 + App.State.Scroll_Offset then
            App.State.Scroll_Offset := App.State.Scroll_Offset + 1;
         end if;
      end if;
   end Move_Down;

   procedure Move_Left (App : in Out Application) is
   begin
      if App.State.Cursor.Column > 1 then
         App.State.Cursor.Column := App.State.Cursor.Column - 1;
      end if;
   end Move_Left;

   procedure Move_Right (App : in Out Application) is
   begin
      App.State.Cursor.Column := App.State.Cursor.Column + 1;
   end Move_Right;

   procedure Page_Up (App : in Out Application) is
      Page_Size : constant Natural := App.Term_Size.Height - 4;
   begin
      if App.State.Scroll_Offset >= Page_Size then
         App.State.Scroll_Offset := App.State.Scroll_Offset - Page_Size;
      else
         App.State.Scroll_Offset := 0;
      end if;
      Go_To_Top (App);
   end Page_Up;

   procedure Page_Down (App : in Out Application) is
      Page_Size : constant Natural := App.Term_Size.Height - 4;
   begin
      App.State.Scroll_Offset := App.State.Scroll_Offset + Page_Size;
   end Page_Down;

   procedure Go_To_Top (App : in Out Application) is
   begin
      App.State.Cursor.Row := 1;
      App.State.Scroll_Offset := 0;
   end Go_To_Top;

   procedure Go_To_Bottom (App : in Out Application) is
   begin
      App.State.Cursor.Row := Natural (App.Locale_Count);
   end Go_To_Bottom;

   procedure Select_Item (App : in Out Application) is
   begin
      case App.State.Current_View is
         when Catalog_Browser =>
            if App.Locale_Count > 0 then
               Switch_View (App, Key_Browser);
            end if;

         when Key_Browser =>
            Switch_View (App, Key_Editor);

         when Help_View =>
            Go_Back (App);

         when others =>
            null;
      end case;
   end Select_Item;

   procedure Delete_Item (App : in Out Application) is
   begin
      Set_Status (App, "Delete not implemented yet");
   end Delete_Item;

   procedure Search (App : in Out Application; Query : String) is
   begin
      App.State.Search_Query := To_Key (Query);
      Switch_View (App, Search_View);
   end Search;

   procedure Clear_Search (App : in Out Application) is
   begin
      App.State.Search_Query := Empty_Key;
      Go_Back (App);
   end Clear_Search;

   procedure Set_Status (App : in Out Application; Message : String) is
      Len : constant Natural := Natural'Min (Message'Length, 80);
   begin
      App.State.Status_Message := (others => ' ');
      App.State.Status_Message (1 .. Len) := Message (Message'First .. Message'First + Len - 1);
      App.State.Status_Length := Len;
   end Set_Status;

   procedure Clear_Status (App : in Out Application) is
   begin
      App.State.Status_Message := (others => ' ');
      App.State.Status_Length := 0;
   end Clear_Status;

end Polyglot_TUI.App;
