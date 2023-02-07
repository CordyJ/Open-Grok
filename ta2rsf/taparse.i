
% Grammar for TA:

%-   tupleLanguage ::= { stringToken stringToken stringToken }
%-
%-   attributeLanguage ::=
%-             itemId "{" {attributeSetting} "}"
%-
%-   attributeSetting ::=
%-             attributeId
%-           | attributeId "=" attributeValue
%-           | attributeId "{" {attributeSetting} "}"  // Nested attributes
%-
%-   attributeValue ::=
%-             stringToken
%-           | "(" {attributeValue} ")"       // Nested lists
%-
%-   itemId ::=  stringToken        // Fact entity or, in scheme, entity class
%-           | "(" stringToken ")"  // Relation (edge class)
%-           | "(" stringToken stringToken stringToken ")"    // Actual edge
%-
%-   TALanguage ::= {section}
%-
%-   section ::=
%-             SCHEME TUPLE : tupleLanguage
%-           | SCHEME ATTRIBUTE : attributeLanguage
%-           | FACT TUPLE : tupleLanguage
%-           | FACT ATTRIBUTE : attributeLanguage


module TAParse
    
    import nameNum, numName

	export Parse,
        ParseFACTTUPLE, % This should be parsed same was as "getdb"
        ParseFACTATTRIBUTE,
        ParseSCHEMETUPLE,
        ParseSCHEMEATTRIBUTE

	include "prefixes.i"
	include "tascang.i"
	include "tascan.i"

    var taSCHEME : num %nameNum ("SCHEME")
    var taFACT : num %nameNum ("FACT")
    var taTUPLE : num %nameNum ("TUPLE")
    var taATTRIBUTE : num %nameNum ("ATTRIBUTE")
    var taINSTANCE : num
    var taINHERIT : num
    var taTUPLEColon : num
    var taATTRIBUTEColon : num
    

    var installTuple : proc p (fileNo : int, src, rel, trg : num)

    fcn addSchemePrefix (n : num) : num
        % Should check for overflow here
        result nameNum (schemePrefix + numName (n))
    end addSchemePrefix

    proc tripleScan
        TAScan.Scan
        TAScan.Scan
        TAScan.Scan
    end tripleScan

    proc doubleScan
        TAScan.Scan
        TAScan.Scan
    end doubleScan

    proc syntaxErrorMsg (msg : string)
        put "***TA Syntax error[", numName (TAScan.Token.fileNum), ":",
            TAScan.Token.lineNo, "]: ", msg,
            "'", numName (TAScan.Token.no), " ",
            numName (TAScan.NextToken.no), " ... '"
    end syntaxErrorMsg

    forward proc ParseSCHEMEorFACT (fileNo : int)
    forward proc ParseTUPLE (fileNo : int, schemeOrFact : num)
    forward proc ParseATTRIBUTE (fileNo : int, schemeOrFact : num)
    forward proc ParseFreshStart (fileNo : int)

    proc ParseFACTTUPLE (fileNo : int)
        % This should be parsed same was as "getdb"
        ParseTUPLE (fileNo, taFACT)
    end ParseFACTTUPLE

    proc ParseFACTATTRIBUTE (fileNo : int)
        ParseATTRIBUTE (fileNo, taFACT)
    end ParseFACTATTRIBUTE

    proc ParseSCHEMETUPLE (fileNo : int)
        ParseTUPLE (fileNo, taSCHEME)
    end ParseSCHEMETUPLE

    proc ParseSCHEMEATTRIBUTE (fileNo : int)
        ParseATTRIBUTE (fileNo, taSCHEME)
    end ParseSCHEMEATTRIBUTE


    body proc ParseSCHEMEorFACT % (fileNo : int)
        pre TAScan.Token.no = taSCHEME or TAScan.Token.no = taFACT
        const schemeOrFact := TAScan.Token.no
        TAScan.Scan % Skip SCHEME or FACT
        if TAScan.Token.no = taTUPLE then
            TAScan.Scan % Skip TUPLE
            if TAScan.Token.no = taColon then
                TAScan.Scan % Skip colon
            else
                syntaxErrorMsg ("Missing colon ':' in ")
            end if
            ParseTUPLE (fileNo, schemeOrFact)

        elsif TAScan.Token.no = taATTRIBUTE then
            TAScan.Scan
            if TAScan.Token.no = taColon then
                TAScan.Scan
            else
                syntaxErrorMsg ("Missing colon ':' in ")
            end if
            ParseATTRIBUTE (fileNo, schemeOrFact)
            
        elsif TAScan.Token.no = taTUPLEColon then
            syntaxErrorMsg ("Missing space before ':' in ")
            TAScan.Scan % Skip 'TUPLE:'
            ParseTUPLE (fileNo, schemeOrFact)
            
        elsif TAScan.Token.no = taATTRIBUTEColon then
            syntaxErrorMsg ("Missing space before ':' in ")
            TAScan.Scan % Skip 'ATTRIBUTE:'
            ParseATTRIBUTE (fileNo, schemeOrFact)
            
        else
            syntaxErrorMsg ("Bad token follows SCHEME in: ")
            tripleScan
        end if
    end ParseSCHEMEorFACT

    body proc ParseTUPLE % (fileNo : int, schemeOrFact : num)
        pre (schemeOrFact = taSCHEME or schemeOrFact = taFACT)
        loop
            var rel := TAScan.Token.no
            exit when rel = taEOF
            if TAScan.NextToken.no = taLeftBrace or
                    TAScan.Token.no = taLeftParen then
                syntaxErrorMsg ("Order error in: ")
                ParseATTRIBUTE (fileNo, schemeOrFact)
                exit
            end if
            if rel = taSCHEME or rel = taFACT then
                ParseFreshStart (fileNo)
                exit
            end if
            TAScan.Scan
            var src := TAScan.Token.no
            if src = taEOF then
                syntaxErrorMsg ("Unexpected EOF in: ")
                exit
            end if
            if src = taSCHEME or src = taFACT then
                syntaxErrorMsg ("Order error in: ")
                ParseFreshStart (fileNo)
                exit
            end if
            TAScan.Scan
            var trg := TAScan.Token.no
            if trg = taEOF then
                syntaxErrorMsg ("Unexpected EOF in: ")
                exit
            end if
            if trg = taSCHEME or trg = taFACT then
                syntaxErrorMsg ("Order error in: ")
                ParseFreshStart (fileNo)
                exit
            end if
            TAScan.Scan
            if schemeOrFact = taSCHEME then
                if rel ~= taINHERIT then
                    rel := addSchemePrefix (rel)
                end if
                src := addSchemePrefix (src)
                trg := addSchemePrefix (trg)
            else
                assert schemeOrFact = taFACT
                if rel = taINSTANCE then
                    trg := addSchemePrefix (trg)
                end if
            end if
            installTuple (fileNo, src, rel, trg)
        end loop
    end ParseTUPLE

    proc ParseParenEntity (fileNo : int, var item : num)
        pre TAScan.Token.no = taLeftParen
        TAScan.Scan % Skip '('
        const rel := TAScan.Token.no
        if TAScan.NextToken.no = taRightParen then
            % Form is : ( rel )
            item := nameNum (prefixRelEntity +
                numName (rel) + suffixRelEntity)
            doubleScan
        else
            % Form is: ( rel src trg )
            TAScan.Scan % Skip rel
            const src := TAScan.Token.no
            const trg := TAScan.NextToken.no
            doubleScan % skip src & trg
            const rightParen := TAScan.Token.no
            if rightParen ~= taRightParen then
                syntaxErrorMsg ("Expected ')' in: ")
                % Not clear how to try to re-synch with input here
                TAScan.Scan
                return
            end if
            assert TAScan.Token.no = taRightParen
            TAScan.Scan % Skip ')'
            const totalLength := lenEdgePrefix + length (numName (rel)) +
                lenEdgeSeparator + length (numName (src)) + lenEdgeSeparator
                +
                length (numName (trg)) + lenEdgeSuffix
            if totalLength > 255 then
                syntaxErrorMsg ("Edge takes > 255 char's: ")
                item := nameNum ("###DummyEDGE###")
                return
            end if
            item := nameNum (edgePrefix + numName (rel) + edgeSeparator +
                numName (src) + edgeSeparator +
                numName (trg) + edgeSuffix)
            % KLUDGE (or change to TA!) Implicitly emit the edge
            installTuple (fileNo, src, rel, trg)
        end if
    end ParseParenEntity

    const upperCharStr := upper (charstr)

    proc checkedCatenate (var s : string, var lenS : int, t : string)
        pre lenS = length (s)
        const lenT := length (t)
        if lenS + lenT > upperCharStr then
            syntaxErrorMsg ("***Error: Can't handle long list: " +
                s (1 .. 20) + " ...")
            % Attempt a patch up
            s := s (1 .. lenS - lenT) + t
            % No change to length of S
            assert lenS = length (s)
        else
            s += t
            lenS += lenT
        end if
    end checkedCatenate

    proc ParseList (var listNum : num)
        pre listNum = taLeftParen
        TAScan.Scan % Skip '('
        var list := edgePrefix
        var lenList := lenEdgePrefix
        var firstTimeThruLoop := true
        loop
            var item := TAScan.Token.no
            var itemName := numName (item)
            if item = taEOF then
                checkedCatenate (list, lenList, itemName)
                exit
            end if
            if item = taRightParen then
                TAScan.Scan % Skip ')'
                exit
            end if
            if not firstTimeThruLoop then
                checkedCatenate (list, lenList, edgeSeparator)
            end if
            if item = taLeftParen then % Nested list
                ParseList (item)
                itemName := numName (item)
            else
                TAScan.Scan % Skip this list item
            end if
            checkedCatenate (list, lenList, itemName)
            firstTimeThruLoop := false
        end loop
        checkedCatenate (list, lenList, edgeSuffix)
        listNum := nameNum (list)
        const listLabel := listPrefix + intstr (listNum)
        listNum := nameNum (listLabel)
    end ParseList

    body proc ParseATTRIBUTE % (fileNo : int, schemeOrFact : num)
        loop
            var src := TAScan.Token.no
            exit when src = taEOF
            if src = taSCHEME or src = taFACT then
                ParseFreshStart (fileNo)
                exit
            elsif src = taLeftParen then
                ParseParenEntity (fileNo, src)
            elsif TAScan.NextToken.no ~= taLeftBrace then
                % We are looking at: tok1 tok2 ... where tok2 isn't '{'
                ParseFreshStart (fileNo)
                exit
            else
                TAScan.Scan % Skip src (which is just a string)
            end if
            if schemeOrFact = taSCHEME then
                src := nameNum (schemePrefix + numName (src))
            end if
            const brace := TAScan.Token.no
            if brace ~= taLeftBrace then
                ParseFreshStart (fileNo)
                exit
            end if
            if TAScan.NextToken.no = taRightBrace then % Empty attribute list
                %  Just allow this: syntaxErrorMsg ("Empty attribute list in: ")
                doubleScan % Skip '{' and '}'
            else
                TAScan.Scan % Skip '{'
                loop
                    var rel := TAScan.Token.no
                    if rel = taEOF then
                        syntaxErrorMsg ("Unexpected EOF in: ")
                        return
                    elsif rel = taSCHEME or rel = taFACT then
                        syntaxErrorMsg ("Order error in: ")
                        ParseFreshStart (fileNo)
                        return
                    end if
                    if schemeOrFact = taSCHEME then
                        rel := nameNum (schemePrefix + attributePrefix +
                            numName (rel))
                    else
                        rel := nameNum (attributePrefix + numName (rel))
                    end if
                    TAScan.Scan % Skip rel
                    var trg : num
                    if TAScan.Token.no = taEqual then
                        TAScan.Scan % Skip the "="
                        trg := TAScan.Token.no
                        if trg = taLeftParen then
                            ParseList (trg)
                        else
                            TAScan.Scan % Skip the (single token) trg
                        end if
                    else
                        % No "=" after attribute
                        trg := taNoAttribute
                    end if
                    if trg = taEOF then
                        syntaxErrorMsg ("Unexpected EOF in: ")
                        return
                    end if
                    if rel = taSCHEME or rel = taFACT then
                        syntaxErrorMsg ("Order error in: ")
                        ParseFreshStart (fileNo)
                        return
                    end if
                    if schemeOrFact = taSCHEME then
                        trg := nameNum (schemePrefix + numName (trg))
                    end if
                    installTuple (fileNo, src, rel, trg)

                    if TAScan.Token.no = taLeftBrace then
                        syntaxErrorMsg ("Nested attributes not supported: ")
                        tripleScan
                        exit
                    end if

                    if TAScan.Token.no = taRightBrace then
                        TAScan.Scan
                        exit
                    end if
                end loop
            end if
        end loop
    end ParseATTRIBUTE

    body proc ParseFreshStart
        const tok := TAScan.Token.no
        const nextTok := TAScan.NextToken.no
        if tok = taSCHEME or tok = taFACT then
            ParseSCHEMEorFACT (fileNo)
        else
            if nextTok = taLeftBrace then
                put "***Warning: Assuming '", numName (tok),
                    " { ... ' is a fact attribute"
                ParseATTRIBUTE (fileNo, taFACT)
            else
                put "***Warning: Assuming '", numName (tok),
                    " ", numName (nextTok), " ... ' is a fact tuple"
                ParseTUPLE (fileNo, taFACT)
            end if

        end if

    end ParseFreshStart

    proc Parse (fileNo : int, fileName : string,
            paramInstallTuple : proc p (fileNo : int, rel, src, trg : num),
            var success : boolean)

        taSCHEME := nameNum ("SCHEME")
        taFACT := nameNum ("FACT")
        taTUPLE := nameNum ("TUPLE")
        taATTRIBUTE := nameNum ("ATTRIBUTE")
        taTUPLEColon := nameNum ("TUPLE:")
        taATTRIBUTEColon := nameNum ("ATTRIBUTE:")
        taINSTANCE := nameNum ("$INSTANCE")
        taINHERIT := nameNum ("$INHERIT")

        installTuple := paramInstallTuple
        TAScan.Open (fileName, success)
        % Assert: TAScan.Token & TAScan.NextToken now contain 1st 2 tokens

        if not success then
            return
        end if

        const tok := TAScan.Token.no
        if tok ~= taSCHEME & tok ~= taFACT then
            syntaxErrorMsg ("Missing SCHEME or FACT: ")
        end if

        ParseFreshStart (fileNo)

        TAScan.Close
    end Parse
end TAParse

