--------------------------------------------------------------------------------
-- File: adaMain.adb
--
-- Created on Apr 6, 2011
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with AdaScanner;


use Ada.Text_IO;
use Ada.Integer_Text_IO;
use Ada.Command_Line;
use Ada.Characters.Handling;
use Ada.Strings.Unbounded;
use AdaScanner;

--
-- Description of adaMain
--
-- @author ktoso
--
procedure AdaMain is

    FileIn   :  File_Type;
    FileOut  :  File_Type;

    MyToken  : Token_Type;

    tab : constant Character := Character'Val(9);

    LineNo : Integer := 1;
    odd    : boolean := true;

    procedure Print_Usage is
    begin
        Put("Usage: ");
        Put(Ada.Command_Line.Command_Name);
        Put_Line(" IN_FILE_NAME [OUT_FILE_NAME]");
    end Print_Usage;

begin
    if Argument_Count = 1 then
        Open(FileIn, In_File, Argument(1));
        Create(FileOut, Out_File, Argument(1)&".html");
    elsif Argument_Count = 2 then
        Open(FileIn, In_File, Argument(1));
        Create(FileOut, Out_File, Argument(2));
    else
        Print_Usage;
        return;
    end if;

    Put_Line(FileOut, "<html><head>");
    Put_Line(FileOut, "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>");
    Put_Line(FileOut, "<title>"&Argument(1)&".html</title>");
    Put_Line(FileOut, "<style type='text/css'>");
    Put_Line(FileOut, "pre { font: 1em Courier New, sans-serif; }");

    Put_Line(FileOut, ".oddLine { margin: 0; padding: 0; background: #f0f0f0 }");
    Put_Line(FileOut, ".evenLine { margin: 0; padding: 0; background: #fafafa }");

    Put_Line(FileOut, ".keyword { font-weight: normal; color: blue; }");

    Put_Line(FileOut, ".semicolon , .comma , .and , .or , .assign , .lparent , .rparent , ");
    Put_Line(FileOut, ".lbracket  , .rbracket , .lsqbracket , .rsqbracket , .add , .or  , ");
    Put_Line(FileOut, ".multiply  , .divide   , .colon { color: black }");

    Put_Line(FileOut, ".stringdata , .characterdata { color: #E1AE62; }");

    Put_Line(FileOut, ".unknown { color: red; font-decoration: underline; }");
    Put_Line(FileOut, ".comment { color: gray; font-style: italic; }");
    Put_Line(FileOut, "</style>");
    Put_Line(FileOut, "</head>");
    Put_Line(FileOut, "<body>");
    Put_Line(FileOut, "<pre>");

    Put(FileOut, "<div class='oddLine'>");
    Put(FileOut, LineNo);
    Put(FileOut, ": ");

    loop
        MyToken := Scan(FileIn);
        exit when MyToken = EndToken;

        if MyToken = EolToken then
            Put(FileOut, "</div>");
            LineNo := LineNo + 1;
            if odd then
                odd := false;
                Put(FileOut, "<div class='evenLine'>");
            else
                odd := true;
                Put(FileOut, "<div class='oddLine'>");
            end if;
            Put(FileOut, LineNo);
            Put(FileOut, ": ");
        else
            case MyToken.code is
                when TOKEN_ADD =>
                    Put(FileOut, "<span class='add'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_AND =>
                    Put(FileOut, "<span class='and'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_ASSIGN =>
                    Put(FileOut, "<span class='assign'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_ARROW =>
                    Put(FileOut, "<span class='arrow'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_CHARACTERDATA =>
                    Put(FileOut, "<span class='characterdata'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_COLON =>
                    Put(FileOut, "<span class='colon'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_COMMA =>
                    Put(FileOut, "<span class='comma'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_COMMENT =>
                    Put(FileOut, "<span class='comment'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_DIVIDE =>
                    Put(FileOut, "<span class='divide'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_DOT =>
                    Put(FileOut, "<span class='dot'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_EQUAL =>
                    Put(FileOut, "<span class='equal'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_IDENT =>
                    Put(FileOut, "<span class='ident'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_KEYWORD =>
                    Put(FileOut, "<span class='keyword'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_LBRACKET =>
                    Put(FileOut, "<span class='lbracket'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_RBRACKET =>
                    Put(FileOut, "<span class='rbracket'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_LPARENT =>
                    Put(FileOut, "<span class='lparent'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_RPARENT =>
                    Put(FileOut, "<span class='rparent'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_LSQBRACKET =>
                    Put(FileOut, "<span class='lsqbracket'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_RSQBRACKET =>
                    Put(FileOut, "<span class='rsqbracket'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_MULTIPLY =>
                    Put(FileOut, "<span class='multiply'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_NUMBER =>
                    Put(FileOut, "<span class='number'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_OR =>
                    Put(FileOut, "<span class='or'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_SEMICOLON =>
                    Put(FileOut, "<span class='semicolon'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_SPACE =>
                    --Put(FileOut, "<span class='space'>");
                    Put_Token(FileOut, MyToken);
                    --Put(FileOut, "</span>");
                when TOKEN_STRINGDATA =>
                    Put(FileOut, "<span class='stringdata'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_SUBTRACT =>
                    Put(FileOut, "<span class='subtract'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when TOKEN_UNKNOWN =>
                    Put(FileOut, "<span class='unknown'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
                when others =>
                    Put(FileOut, "<span class='keyword'>");
                    Put_Token(FileOut, MyToken);
                    Put(FileOut, "</span>");
            end case;
        end if;

    end loop;

    Close(FileIn);

    Put_Line(FileOut, "</pre>");
    Put_Line(FileOut, "</body>");
    Put_Line(FileOut, "</html>");

    Close(FileOut);

end AdaMain;