--------------------------------------------------------------------------------
-- File: adaScanner.ads
--
-- Created on Apr 6, 2011
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

use Ada.Text_IO;
use Ada.Command_Line;
use Ada.Characters.Handling;
use Ada.Strings.Unbounded;

--
-- Description of adaScanner
--
-- @author ktoso
--
package AdaScanner is

    type TokenCode_Type is (
        TOKEN_UNKNOWN,       -- unknown token
        TOKEN_EOF,           -- end of file
        TOKEN_SPACE,         -- whitespace
        TOKEN_NUMBER,        -- number data
        TOKEN_STRINGDATA,    -- string data
        TOKEN_IDENT,         -- identifier
        TOKEN_KEYWORD,       -- keywords
        TOKEN_SEMICOLON,     -- ;
        TOKEN_COLON,         -- :
        TOKEN_ASSIGN,        -- :=
        TOKEN_EQUAL,         -- =
        TOKEN_ARROW,         -- =>
        TOKEN_ADD,           -- +
        TOKEN_SUBTRACT,      -- -
        TOKEN_MULTIPLY,      -- *
        TOKEN_DIVIDE,        -- /
        TOKEN_AND,           -- &
        TOKEN_AT,            -- @
        TOKEN_OR,            -- |
        TOKEN_LPARENT,       -- '('
        TOKEN_RPARENT,       -- ')'
        TOKEN_LBRACKET,      -- '{'
        TOKEN_RBRACKET,      -- '}'
        TOKEN_LSQBRACKET,    -- '['
        TOKEN_RSQBRACKET,    -- ']'
        TOKEN_COMMA,         -- ,
        TOKEN_DOT,           -- .
        TOKEN_CHARACTERDATA, -- character, eg 'a'
        TOKEN_COMMENT,       -- comment
        TOKEN_EOL,

        TOKEN_ARRAY,
        TOKEN_BEGIN,
        TOKEN_BODY,
        TOKEN_CASE,
        TOKEN_CONST,
        TOKEN_CHARACTER,
        TOKEN_DIV,
        TOKEN_DO,
        TOKEN_DOWNTO,
        TOKEN_ELSE,
        TOKEN_ELSIF,
        TOKEN_END,
        TOKEN_FILE,
        TOKEN_FOR,
        TOKEN_FUNCTION,
        TOKEN_GOTO,
        TOKEN_IF,
        TOKEN_IN,
        TOKEN_IS,
        TOKEN_LABEL,
        TOKEN_LOOP,
        TOKEN_MOD,
        TOKEN_NIL,
        TOKEN_NULL,
        TOKEN_NOT,
        TOKEN_OF,
        TOKEN_OTHERS,
        TOKEN_PACKED,
        TOKEN_PACKAGE,
        TOKEN_PROCEDURE,
        TOKEN_PROGRAM,
        TOKEN_RECORD,
        TOKEN_RETURN,
        TOKEN_REPEAT,
        TOKEN_SET,
        TOKEN_THEN,
        TOKEN_TO,
        TOKEN_SYM_TYPE,
        TOKEN_UNTIL,
        TOKEN_USE,
        TOKEN_VAR,
        TOKEN_WHILE,
        TOKEN_WHEN,
        TOKEN_INTEGER,
        TOKEN_STRING,
        TOKEN_REAL,
        TOKEN_WITH
    );

    type Token_Type is record
        code   : TokenCode_Type;
        value  : Unbounded_String;
    end record;

    EndToken : constant Token_Type := (TOKEN_EOF, Null_Unbounded_String);
    EolToken : constant Token_Type := (TOKEN_EOL, Null_Unbounded_String);

    Scanner_Exception : exception;

    function  Scan(F: in File_Type) return Token_Type;
    procedure Put_Token(F: File_Type; token : in Token_Type);

end AdaScanner;
