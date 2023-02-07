% unit
% module TAScanGlobals

    type tokenRecord :
        record
            no : num
            lineNo : int
            fileNum : num
        end record

    var taEqual : num % := nameNum ('=')
    var taLeftBrace : num %  := nameNum ('{')
    var taRightBrace : num %  := nameNum ('}')
    var taLeftParen : num %  := nameNum ('(')
    var taRightParen : num % := nameNum (')')
    var taColon : num %  := nameNum (':')
    var taEOF : num % := nameNum ("### EOF ###")
    var taEOL : num %  := nameNum ("\n")
    var taINCLUDE : num % := nameNum ("$INCLUDE")
    var taNoAttribute : num % := nameNum (noAttribute)

    proc InitGlobals
        taEqual := nameNum ('=')
        taLeftBrace := nameNum ('{')
        taRightBrace := nameNum ('}')
        taLeftParen := nameNum ('(')
        taRightParen := nameNum (')')
        taColon := nameNum (':')
        taEOF := nameNum ("### EOF ###")
        taEOL := nameNum ("\n")
        taINCLUDE := nameNum ("INCLUDE")
        taNoAttribute := nameNum (noAttribute)
    end InitGlobals

% end TAScanGlobals

