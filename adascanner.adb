--------------------------------------------------------------------------------
-- File: adaScanner.adb
--
-- Created on Apr 6, 2011
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;

use Ada.Text_IO;
use Ada.Command_Line;
use Ada.Characters.Handling;
use Ada.Strings.Maps.Constants;
use Ada.Strings.Unbounded;

--
-- Description of adaScanner
--
-- @author ktoso
--
package body AdaScanner is

    function Scan(F: in File_Type) return Token_Type is
        token : Token_Type := (TOKEN_UNKNOWN, Null_Unbounded_String);
        tmp   : Unbounded_String;
        chr   : Character;
        eol   : boolean;
    begin
        if End_Of_File(F) then
            return EndToken;
        end if;

        if End_Of_Line(F) then
            Get_Immediate(F,Chr);
            return EolToken;
        end if;

        Get_Immediate(F,Chr);

        if chr = ' ' then
            token.code  := TOKEN_SPACE;
            token.value := token.value & chr;
            loop
                Look_Ahead(F, chr, eol);
                if chr = ' ' then
                    Get(F,chr);
                    token.value := token.value & chr;
                else
                    exit;
                end if;
            end loop;
        elsif Is_Digit(chr) then -- looks like a number
            token.code  := TOKEN_NUMBER;
            token.value := token.value & chr;
            loop
                Look_Ahead(F, chr, eol);
                -- number ends on not-number or end-of-line
                if not Is_Digit(chr) or eol then
                    exit;
                else
                    Get(F, chr);
                    token.value := token.value & chr;
                end if;
            end loop;
        elsif chr = '"' then -- looks like a string
            token.code := TOKEN_STRINGDATA;
            token.value := token.value & chr;
            loop
                Get(F,chr);
                if chr = '"' then
                    token.value := token.value & chr;
                    exit;
                else
                    token.value := token.value & chr;
                end if;
            end loop;
        elsif chr = ''' then
            token.code  := TOKEN_CHARACTERDATA;
            token.value := token.value & chr;
            Get(F,chr);
            token.value := token.value & chr;
            Get(F,chr);
            loop
                if chr = ''' then
                    token.value := token.value & chr;
                    exit;
                else
                    token.value := token.value & chr;
                end if;
            end loop;
        elsif Is_Letter(chr) then -- starts with a letter
            token.code  := TOKEN_UNKNOWN;
            token.value := token.value & chr;
            loop
                Look_Ahead(F, chr, eol);
                if eol or (not is_Alphanumeric(chr) and chr /= '_' and chr /= ''') then
                    exit;
                else
                    Get(F, chr);
                    token.value := token.value & chr;
                end if;
            end loop;
            tmp := Translate(token.value, Lower_Case_Map);
            if tmp = "and" then
                token.code := TOKEN_AND;
            elsif tmp = "array" then
                token.code := TOKEN_ARRAY;
            elsif tmp = "begin" then
                token.code := TOKEN_BEGIN;
            elsif tmp =  "body" then
                token.code := TOKEN_BODY;
            elsif tmp = "case" then
                token.code := TOKEN_CASE;
            elsif tmp = "constant" then
                token.code := TOKEN_CONST;
            elsif tmp =  "div" then
                token.code := TOKEN_DIV;
            elsif tmp = "do" then
                token.code := TOKEN_DO;
            elsif tmp = "downto" then
                token.code := TOKEN_DOWNTO;
            elsif tmp = "else" then
                token.code := TOKEN_ELSE;
            elsif tmp = "elsif" then
                token.code := TOKEN_ELSIF;
            elsif tmp = "end" then
                token.code := TOKEN_END;
            elsif tmp = "file" then
                token.code := TOKEN_FILE;
            elsif tmp = "for" then
                token.code := TOKEN_FOR;
            elsif tmp = "function" then
                token.code := TOKEN_FUNCTION;
            elsif tmp = "goto" then
                token.code := TOKEN_GOTO;
            elsif tmp = "if" then
                token.code := TOKEN_IF;
            elsif tmp = "in" then
                token.code := TOKEN_IN;
            elsif tmp = "is" then
                token.code := TOKEN_IS;
            elsif tmp = "label" then
                token.code := TOKEN_LABEL;
            elsif tmp = "loop" then
                token.code := TOKEN_LOOP;
            elsif tmp = "mod" then
                token.code := TOKEN_MOD;
            elsif tmp = "nil" then
                token.code := TOKEN_NIL;
            elsif tmp = "null" then
                token.code := TOKEN_NULL;
            elsif tmp = "not" then
                token.code := TOKEN_NOT;
            elsif tmp = "of" then
                token.code := TOKEN_OF;
            elsif tmp = "or" then
                token.code := TOKEN_OR;
            elsif tmp = "others" then
                token.code := TOKEN_OTHERS;
            elsif tmp = "packed" then
                token.code := TOKEN_PACKED;
            elsif tmp = "package" then
                token.code := TOKEN_PACKAGE;
            elsif tmp = "procedure" then
                token.code := TOKEN_PROCEDURE;
            elsif tmp = "program" then
                token.code := TOKEN_PROGRAM;
            elsif tmp = "record" then
                token.code := TOKEN_RECORD;
            elsif tmp = "return" then
                token.code := TOKEN_RETURN;
            elsif tmp = "repeat" then
                token.code := TOKEN_REPEAT;
            elsif tmp =  "set" then
                token.code := TOKEN_SET;
            elsif tmp = "then" then
                token.code := TOKEN_THEN;
            elsif tmp = "to" then
                token.code := TOKEN_TO;
            elsif tmp = "type" then
                token.code := TOKEN_SYM_TYPE;
            elsif tmp = "until" then
                token.code := TOKEN_UNTIL;
            elsif tmp = "use" then
                token.code := TOKEN_USE;
            elsif tmp = "var" then
                token.code := TOKEN_VAR;
            elsif tmp = "when" then
                token.code := TOKEN_WHEN;
            elsif tmp = "while" then
                token.code := TOKEN_WHILE;
            elsif tmp = "integer" then
                token.code := TOKEN_INTEGER;
            elsif tmp = "string" then
                token.code := TOKEN_STRING;
            elsif tmp = "real" then
                token.code := TOKEN_REAL;
            elsif tmp = "with" then
                token.code := TOKEN_WITH;
            else
                token.code := TOKEN_IDENT;
            end if;
        elsif chr = ';' then -- semicolon
            token.code  := TOKEN_SEMICOLON;
            token.value := token.value & chr;
        elsif chr = ':' then 
            token.value := token.value & chr;
            Look_Ahead(F,chr,eol);
            if chr = '=' then
                Get(F,chr);
                token.code  := TOKEN_ASSIGN;
                token.value := token.value & chr;
            else
                token.code := TOKEN_COLON;
            end if;
        elsif chr = '=' then 
            token.value := token.value & chr;
            Look_Ahead(F,Chr,eol);
            if chr = '>' then
                Get(F,chr);
                token.code  := TOKEN_ARROW;
                token.value := token.value & chr;
            else
                token.code  := TOKEN_EQUAL;
            end if;
        elsif chr = '+' then 
            token.code  := TOKEN_ADD;
            token.value := token.value & chr;
        elsif chr = '-' then 
            token.value := token.value & chr;
            Look_Ahead(F,Chr,eol);
            if chr = '-' then
                token.code := TOKEN_COMMENT;
                loop
                    Look_Ahead(F,Chr,eol);
                    if eol then exit; end if;
                    Get(F,Chr);
                    token.value := token.value & chr;
                end loop;
            else
                token.code  := TOKEN_SUBTRACT;
            end if;
        elsif chr = '/' then
            token.code  := TOKEN_DIVIDE;
            token.value := token.value & chr;
        elsif chr = '*' then
            token.code  := TOKEN_MULTIPLY;
            token.value := token.value & chr;
        elsif chr = '(' then
            token.code  := TOKEN_LPARENT;
            token.value := token.value & chr;
        elsif chr = ')' then
            token.code  := TOKEN_RPARENT;
            token.value := token.value & chr;
        elsif chr = '{' then
            token.code  := TOKEN_LBRACKET;
            token.value := token.value & chr;
        elsif chr = '}' then
            token.code  := TOKEN_RBRACKET;
            token.value := token.value & chr;
        elsif chr = '[' then
            token.code  := TOKEN_LSQBRACKET;
            token.value := token.value & chr;
        elsif chr = ']' then
            token.code  := TOKEN_RSQBRACKET;
            token.value := token.value & chr;
        elsif chr = ',' then
            token.code  := TOKEN_COMMA;
            token.value := token.value & chr;
        elsif chr = '.' then
            token.code  := TOKEN_DOT;
            token.value := token.value & chr;
        elsif chr = '&' then
            token.code  := TOKEN_AND;
            token.value := token.value & chr;
        elsif chr = '@' then
            token.code  := TOKEN_AT;
            token.value := token.value & chr;
        elsif chr = '|' then
            token.code  := TOKEN_OR;
            token.value := token.value & chr;
        else
--            token.code  := TOKEN_UNKNOWN;
--            token.value := token.value & chr;
            raise Scanner_Exception;
        end if;

        return token;
    end Scan;

    procedure Put_Token(F: File_Type ; token : in Token_Type) is
    begin
        for I in 1 .. Length(token.value) loop
            case Element(token.value,I) is
                when '<' =>
                Put(F, "&lt;");
                when '>' =>
                Put(F, "&gt;");
                when '"' =>
                Put(F, "&quot;");
                when '&' =>
                Put(F, "&amp;");
                when others =>
                Put(F, Element(token.value,I));
            end case;
        end loop;
    end Put_Token;

end AdaScanner;
