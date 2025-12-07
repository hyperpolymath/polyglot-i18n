-------------------------------------------------------------------------------
--  Polyglot_TUI.Terminal
--
--  Terminal abstraction implementation using ANSI escape sequences.
--  Platform-independent terminal control for Unix-like systems.
-------------------------------------------------------------------------------

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;

package body Polyglot_TUI.Terminal is

   --  ANSI escape code constants
   ESC   : constant Character := Ada.Characters.Latin_1.ESC;
   CSI   : constant String := ESC & "[";

   --  State tracking
   Current_Style : Style := Default_Style;
   Term_Init     : Boolean := False;

   --  Box drawing characters (UTF-8)
   type Box_Chars is record
      Top_Left     : String (1 .. 3);
      Top_Right    : String (1 .. 3);
      Bottom_Left  : String (1 .. 3);
      Bottom_Right : String (1 .. 3);
      Horizontal   : String (1 .. 3);
      Vertical     : String (1 .. 3);
   end record;

   Box_Single : constant Box_Chars :=
      (Top_Left     => "┌  ",
       Top_Right    => "┐  ",
       Bottom_Left  => "└  ",
       Bottom_Right => "┘  ",
       Horizontal   => "─  ",
       Vertical     => "│  ");

   Box_Double : constant Box_Chars :=
      (Top_Left     => "╔  ",
       Top_Right    => "╗  ",
       Bottom_Left  => "╚  ",
       Bottom_Right => "╝  ",
       Horizontal   => "═  ",
       Vertical     => "║  ");

   Box_Rounded : constant Box_Chars :=
      (Top_Left     => "╭  ",
       Top_Right    => "╮  ",
       Bottom_Left  => "╰  ",
       Bottom_Right => "╯  ",
       Horizontal   => "─  ",
       Vertical     => "│  ");

   Box_Heavy : constant Box_Chars :=
      (Top_Left     => "┏  ",
       Top_Right    => "┓  ",
       Bottom_Left  => "┗  ",
       Bottom_Right => "┛  ",
       Horizontal   => "━  ",
       Vertical     => "┃  ");

   Box_ASCII : constant Box_Chars :=
      (Top_Left     => "+  ",
       Top_Right    => "+  ",
       Bottom_Left  => "+  ",
       Bottom_Right => "+  ",
       Horizontal   => "-  ",
       Vertical     => "|  ");

   function Get_Box_Chars (Style : Box_Style) return Box_Chars is
   begin
      case Style is
         when Single     => return Box_Single;
         when Double     => return Box_Double;
         when Rounded    => return Box_Rounded;
         when Heavy      => return Box_Heavy;
         when ASCII_Only => return Box_ASCII;
      end case;
   end Get_Box_Chars;

   --  Send raw ANSI sequence
   procedure Send (Sequence : String) is
   begin
      Put (Sequence);
   end Send;

   --  Convert integer to string without leading space
   function Image (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Image;

   procedure Initialize is
   begin
      if not Term_Init then
         --  Enter alternate screen buffer
         Send (CSI & "?1049h");
         --  Enable mouse tracking (optional)
         --  Send (CSI & "?1000h");
         --  Hide cursor initially
         Hide_Cursor;
         --  Clear screen
         Clear;
         Term_Init := True;
      end if;
   end Initialize;

   procedure Shutdown is
   begin
      if Term_Init then
         --  Reset all attributes
         Reset_Style;
         --  Show cursor
         Show_Cursor;
         --  Clear screen
         Clear;
         --  Leave alternate screen buffer
         Send (CSI & "?1049l");
         Term_Init := False;
      end if;
   end Shutdown;

   function Get_Dimensions return Dimensions is
      --  Default dimensions if detection fails
      Result : Dimensions := (Width => 80, Height => 24);
   begin
      --  In a full implementation, this would query terminal size
      --  using TIOCGWINSZ ioctl or similar
      --  For now, return reasonable defaults
      return Result;
   end Get_Dimensions;

   procedure Clear is
   begin
      --  Clear entire screen
      Send (CSI & "2J");
      --  Move cursor to home position
      Send (CSI & "H");
   end Clear;

   procedure Clear_Line (Row : Positive) is
   begin
      Move_Cursor (Row, 1);
      Send (CSI & "2K");
   end Clear_Line;

   procedure Move_Cursor (Row, Column : Positive) is
   begin
      Send (CSI & Image (Row) & ";" & Image (Column) & "H");
   end Move_Cursor;

   procedure Hide_Cursor is
   begin
      Send (CSI & "?25l");
   end Hide_Cursor;

   procedure Show_Cursor is
   begin
      Send (CSI & "?25h");
   end Show_Cursor;

   procedure Set_Style (S : Style) is
      Seq : String (1 .. 64) := (others => ' ');
      Len : Natural := 0;

      procedure Append (Part : String) is
      begin
         if Len > 0 then
            Len := Len + 1;
            Seq (Len) := ';';
         end if;
         for I in Part'Range loop
            Len := Len + 1;
            Seq (Len) := Part (I);
         end loop;
      end Append;

   begin
      Current_Style := S;

      --  Reset first
      Send (CSI & "0m");

      --  Foreground color (30-37 for standard, 38;5;N for 256)
      if S.Foreground > 7 then
         Append ("38;5;" & Image (Natural (S.Foreground)));
      elsif S.Foreground > 0 then
         Append (Image (30 + Natural (S.Foreground)));
      end if;

      --  Background color (40-47 for standard, 48;5;N for 256)
      if S.Background > 7 then
         Append ("48;5;" & Image (Natural (S.Background)));
      elsif S.Background > 0 then
         Append (Image (40 + Natural (S.Background)));
      end if;

      --  Attributes
      if S.Attributes (Bold) then
         Append ("1");
      end if;
      if S.Attributes (Dim) then
         Append ("2");
      end if;
      if S.Attributes (Italic) then
         Append ("3");
      end if;
      if S.Attributes (Underline) then
         Append ("4");
      end if;
      if S.Attributes (Blink) then
         Append ("5");
      end if;
      if S.Attributes (Reverse) then
         Append ("7");
      end if;
      if S.Attributes (Hidden) then
         Append ("8");
      end if;

      if Len > 0 then
         Send (CSI & Seq (1 .. Len) & "m");
      end if;
   end Set_Style;

   procedure Reset_Style is
   begin
      Send (CSI & "0m");
      Current_Style := Default_Style;
   end Reset_Style;

   procedure Put (C : Character) is
   begin
      Ada.Text_IO.Put (C);
   end Put;

   procedure Put (S : String) is
   begin
      Ada.Text_IO.Put (S);
   end Put;

   procedure Put_Line (S : String) is
   begin
      Ada.Text_IO.Put_Line (S);
   end Put_Line;

   procedure Put_At (Row, Column : Positive; S : String) is
   begin
      Move_Cursor (Row, Column);
      Put (S);
   end Put_At;

   procedure Put_At (Row, Column : Positive; S : String; St : Style) is
      Old_Style : constant Style := Current_Style;
   begin
      Set_Style (St);
      Move_Cursor (Row, Column);
      Put (S);
      Set_Style (Old_Style);
   end Put_At;

   procedure Refresh is
   begin
      --  Flush output buffer
      Flush;
   end Refresh;

   function Poll_Key (Timeout_Ms : Natural := 0) return Key_Event is
      pragma Unreferenced (Timeout_Ms);
      Result : Key_Event := (Code => Key_None, others => <>);
   begin
      --  Non-blocking key poll would require system-specific code
      --  (termios on Unix, Windows Console API on Windows)
      --  For now, return no key
      return Result;
   end Poll_Key;

   function Wait_Key return Key_Event is
      Result : Key_Event := (Code => Key_None, others => <>);
      C      : Character;
   begin
      --  Simple blocking read
      Get_Immediate (C);

      case C is
         when ESC =>
            --  Could be escape sequence, check for more
            Result.Code := Key_Escape;

         when LF | CR =>
            Result.Code := Key_Enter;

         when HT =>
            Result.Code := Key_Tab;

         when BS | DEL =>
            Result.Code := Key_Backspace;

         when others =>
            if C >= ' ' and C <= '~' then
               Result.Code := Key_Char;
               Result.Character := C;
            end if;
      end case;

      return Result;
   end Wait_Key;

   procedure Draw_Box
      (Row, Column : Positive;
       Width, Height : Positive;
       Style : Box_Style := Single;
       Title : String := "")
   is
      BC : constant Box_Chars := Get_Box_Chars (Style);
      Title_Start : Positive;
      Title_Len   : Natural;
   begin
      --  Top line
      Move_Cursor (Row, Column);
      Put (BC.Top_Left (1 .. 1));
      for I in 2 .. Width - 1 loop
         Put (BC.Horizontal (1 .. 1));
      end loop;
      Put (BC.Top_Right (1 .. 1));

      --  Title if provided
      if Title'Length > 0 then
         Title_Len := Natural'Min (Title'Length, Width - 4);
         Title_Start := Column + (Width - Title_Len) / 2;
         Put_At (Row, Title_Start, " " & Title (Title'First .. Title'First + Title_Len - 1) & " ");
      end if;

      --  Side lines
      for I in 1 .. Height - 2 loop
         Move_Cursor (Row + I, Column);
         Put (BC.Vertical (1 .. 1));
         Move_Cursor (Row + I, Column + Width - 1);
         Put (BC.Vertical (1 .. 1));
      end loop;

      --  Bottom line
      Move_Cursor (Row + Height - 1, Column);
      Put (BC.Bottom_Left (1 .. 1));
      for I in 2 .. Width - 1 loop
         Put (BC.Horizontal (1 .. 1));
      end loop;
      Put (BC.Bottom_Right (1 .. 1));
   end Draw_Box;

   procedure Draw_Horizontal_Line
      (Row, Column : Positive;
       Width : Positive;
       Style : Box_Style := Single)
   is
      BC : constant Box_Chars := Get_Box_Chars (Style);
   begin
      Move_Cursor (Row, Column);
      for I in 1 .. Width loop
         Put (BC.Horizontal (1 .. 1));
      end loop;
   end Draw_Horizontal_Line;

   procedure Draw_Vertical_Line
      (Row, Column : Positive;
       Height : Positive;
       Style : Box_Style := Single)
   is
      BC : constant Box_Chars := Get_Box_Chars (Style);
   begin
      for I in 0 .. Height - 1 loop
         Move_Cursor (Row + I, Column);
         Put (BC.Vertical (1 .. 1));
      end loop;
   end Draw_Vertical_Line;

end Polyglot_TUI.Terminal;
