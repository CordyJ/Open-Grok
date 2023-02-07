module TAOutput

    import nameNum, numName

    export outputTATuple

    var printTuple : boolean := false
    % const stdOutput := - 1

    const quote := "\""
    const backslash := "\\"
    const backslashQuote := backslash + quote

    fcn ta_isPrefix (prefix, name : string) : boolean
        const lenPrefix := length (prefix)
        result length (name) >= lenPrefix and name (1 .. lenPrefix) = prefix
    end ta_isPrefix

    fcn ta_isSuffix (suffix, name : string) : boolean
        const lenSuffix := length (suffix)
        result length (name) >= lenSuffix and
            name (* - lenSuffix + 1 .. *) = suffix
    end ta_isSuffix

    proc stripPrefix (prefixLen : int, var s : string )
        s := s (prefixLen + 1 .. *)
    end stripPrefix

    proc stripSuffix (suffixLen : int, var s : string )
        s := s (1 .. * - suffixLen)
    end stripSuffix

    proc quoteIfNeeded (var s : string)
        const quoteNeeded := s = "" or
            index (s, " ") ~= 0 or
            index (s, "\t") ~= 0 or
            index (s, "\n") ~= 0 or
            index (s, "\"") ~= 0 or
            s = ""
        if quoteNeeded then
            var whereQuote := index (s, quote)
            var whereBackslash := index (s, backslash)
            if whereQuote ~= 0 then
                var leftPart := ""
                var rightPart := s
                loop
                    whereQuote := index (rightPart, quote)
                    whereBackslash := index (rightPart, backslash)
                    exit when whereQuote = 0 & whereBackslash = 0

                    if whereBackslash = 0 or whereQuote < whereBackslash then
                        leftPart += rightPart (1 .. whereQuote - 1) +
                            backslashQuote
                        rightPart := rightPart (whereQuote + 1 .. *)
                    else
                        leftPart += rightPart (1 .. whereBackslash - 1) +
                            backslash + backslash
                        rightPart := rightPart (whereQuote + 1 .. *)
                    end if
                end loop
                leftPart += rightPart
                s := leftPart
            end if
            s := quote + s + quote
        end if
    end quoteIfNeeded

    forward proc outputItem (outFileNo : int, item : int, lastOnLine, needSeparator : boolean)

    proc outputList (outFileNo, listNo : int, lastOnLine : boolean)
        var s := numName (listNo)
        s := s (lenEdgePrefix + 1 .. *) % Chop off prefix
        s := s (1 .. * - lenEdgeSuffix) + edgeSeparator
        put : outFileNo, edgePrefix ..
        if printTuple then
            put edgePrefix ..
        end if
        var firstTimeThruLoop := true
        loop
            const whereSep := index (s, edgeSeparator)
            exit when whereSep = 0
            const nextItem := s (1 .. whereSep - 1)
            s := s (whereSep + lenEdgeSeparator .. *)
            if not firstTimeThruLoop then
                put : outFileNo, edgeSeparator ..
                if printTuple then
                    put edgeSeparator
                end if
            end if
            % Possibly output a nested list here
            outputItem (outFileNo, nameNum (nextItem), false, false)
            % Not on a separate line
            firstTimeThruLoop := false
        end loop
        put : outFileNo, edgeSuffix ..
        if printTuple then
            put edgeSuffix ..
        end if
        if lastOnLine then
            put : outFileNo, ""
        end if
        if printTuple and lastOnLine then
            put ""
        end if
    end outputList

    proc outputTAList (outFileNo, listNo : int)
        var s := numName (listNo)
        assert ta_isPrefix (edgePrefix, s)
        s := s (lenEdgePrefix + 1 .. *) % Chop off prefix
        s := s (1 .. * - lenEdgeSuffix) + edgeSeparator
        put : outFileNo, '(' ..
        var firstTimeThruLoop := true
        loop
            const whereSep := index (s, edgeSeparator)
            exit when whereSep = 0
            var nextItem := s (1 .. whereSep - 1)
            s := s (whereSep + lenEdgeSeparator .. *)
            if not firstTimeThruLoop then
                put : outFileNo, ' ' ..
            end if
            % Possibly output a nested list here
            if ta_isPrefix (listPrefix, nextItem) then
                stripPrefix (lenListPrefix, nextItem)
                % if strintok (nextItem) then
                    outputTAList (outFileNo, strint (nextItem))
                % else
                    % put "***Error: Bad list token:", nextItem ..
                    % put : outFileNo, " BadList:", nextItem ..
                % end if
            else
                quoteIfNeeded (nextItem)
                put : outFileNo, nextItem ..
            end if
            firstTimeThruLoop := false
        end loop
        put : outFileNo, ')' ..
    end outputTAList

    body proc outputItem % (outFileNo: int, item : int, lastOnLine, needSeparator : boolean)
        var Item := numName (item)
        const lenItem := length (Item)
        const isEncodedStr := lenItem >= lenStrPrefix and
            Item (1 .. lenStrPrefix) = strPrefix
        const isEncodedSchemeStr := lenItem >= lenSchemeStrPrefix and
            Item (1 .. lenSchemeStrPrefix) = schemeStrPrefix
        const isEncodedList := lenItem >= lenListPrefix and
            Item (1 .. lenListPrefix) = listPrefix
        const isEncodedSchemeList := lenItem >= lenSchemeListPrefix and
            Item (1 .. lenSchemeListPrefix) = schemeListPrefix
        if isEncodedStr or
                isEncodedSchemeStr or
                isEncodedList or
                isEncodedSchemeList then
            var numChars : string
            if isEncodedStr then
                numChars := Item (lenStrPrefix + 1 .. *)
            elsif isEncodedList then
                numChars := Item (lenListPrefix + 1 .. *)
            elsif isEncodedSchemeStr then
                numChars := Item (lenSchemeStrPrefix + 1 .. *)
            elsif isEncodedSchemeList then
                numChars := Item (lenSchemeListPrefix + 1 .. *)
            end if
            % if strintok (numChars) then
                const strNo := strint (numChars)

				assert not (isEncodedStr or isEncodedSchemeStr)
                % if isEncodedStr or isEncodedSchemeStr then
                    % outputRope (outFileNo, strNo, lastOnLine)
                % else
                    assert isEncodedList or isEncodedSchemeList
                    outputList (outFileNo, strNo, lastOnLine)
                % end if
            % else
                % put "***Error: Bad encoded item: '", Item, "'"
                % outputItem (outFileNo, nameNum ("BAD: " + Item), lastOnLine, 
                    % needSeparator)
            % end if
        else
            quoteIfNeeded (Item)
            if printTuple then
                if lastOnLine then
                    put item
                else
                    if needSeparator then
                        put item : 12, " " ..
                    else
                        put item ..
                    end if
                end if
            end if
            if lastOnLine then
                put : outFileNo, Item
            else
                if needSeparator then
                    put : outFileNo, Item, "\t" ..
                else
                    put : outFileNo, Item ..
                end if
            end if
        end if
    end outputItem

    % This proc will be passed as a parameter to the TA parser
    proc outputTuple (outFileNo : int, src, rel, trg : int)
        outputItem (outFileNo, rel, false, true)
        outputItem (outFileNo, src, false, true)
        outputItem (outFileNo, trg, true, true)
    end outputTuple

    proc ta_putList (outFileNo : int, var s : string)
        pre ta_isPrefix (listPrefix, s)
        stripPrefix (lenListPrefix, s)
        % if strintok (s) then
            const listNo := strint (s)
            outputTAList (outFileNo, listNo)
        % else
            % put "***Error: Bad encoded list: ", s
            % put : outFileNo, " BadList:", s ..
        % end if
    end ta_putList

    % This proc will be passed as a parameter to the TA parser
    proc outputTATuple (outFileNo : int, src, rel, trg : int)
        var Src := numName (src)
        var Rel := numName (rel)
        var Trg := numName (trg)
        if ta_isPrefix (schemePrefix, Rel) then
            stripPrefix (lenSchemePrefix, Src)
            stripPrefix (lenSchemePrefix, Rel)
            stripPrefix (lenSchemePrefix, Trg)
        end if
        if ta_isPrefix (attributePrefix, Rel) then
            stripPrefix (lenAttributePrefix, Rel)
            if ta_isPrefix (edgePrefix, Src) then
                put : outFileNo, "(" ..
                stripPrefix (lenEdgePrefix, Src)
                stripSuffix (lenEdgeSuffix, Src)
                Src := Src + edgeSeparator
                var firstTimeThruLoop := true
                loop
                    const whereSeparator := index (Src, edgeSeparator)
                    exit when whereSeparator = 0
                    var Item := Src (1 .. whereSeparator - 1)
                    if firstTimeThruLoop then
                        put : outFileNo, Item ..
                    else
                        put : outFileNo, ' ', Item ..
                    end if
                    Src := Src (whereSeparator + lenEdgeSeparator .. *)
                    firstTimeThruLoop := false
                end loop
                put : outFileNo, ")" ..
            elsif ta_isPrefix (prefixRelEntity, Src) then
                put : outFileNo, "(" ..
                stripPrefix (lenPrefixRelEntity, Src)
                stripSuffix (lenSuffixRelEntity, Src)
                put : outFileNo, Src ..
                put : outFileNo, ")" ..
            else
                put : outFileNo, Src ..
            end if
            put : outFileNo, "\t{ ", Rel ..
            if Trg = noAttribute then
                % Do nothing, leave in form:  x { attr }
            else
                put : outFileNo, " = " ..
                if ta_isPrefix (listPrefix, Trg) then
                    ta_putList (outFileNo, Trg)
                % elsif ta_isPrefix (strPrefix, Trg) then
                    % put : outFileNo, ' ' ..
                    % putRope (outFileNo,Trg)
                else
                    quoteIfNeeded (Trg)
                    put : outFileNo, Trg ..
                end if
            end if
            put : outFileNo, " }"
        else
            quoteIfNeeded (Rel)
            quoteIfNeeded (Src)
            quoteIfNeeded (Trg)
            put : outFileNo, Rel, '\t', Src, '\t', Trg
        end if
    end outputTATuple
end TAOutput
