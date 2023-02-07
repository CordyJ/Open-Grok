% Scanner for inputting TA to Grok

module TAScanClass

    export Instance, Initialize, Open, Close, Scan

    const dummyToken := - 13
    const dummyFile := "### dummy ###"

    const maxTALineChars := 3 * 255
    const maxTATokenChars := 255
    const TASpecialChars : array 1 .. 5 of char :=
        init ('=', '{', '}', '(', ')')  % Get rid of : as spec char.

    const blank := ' '
    const eolChar := '\n'
    const eofChar : char := chr (127) % Not sure if this is the best value

	type Instance :
		record
    		token, nextToken : tokenRecord
    		fileName : string 
			lineNo, physLineNo : int
    		line, tokenBuffer : array 1 .. maxTALineChars + 1 of char
			fileNo : int 
		end record

	proc Initialize (var me : Instance)
		bind var token to me.token, 
			var nextToken to me.nextToken,
			var fileName to me.fileName,
			var lineNo to me.lineNo,
			var physLineNo to me.physLineNo,
			var line to me.line,
			var tokenBuffer to me.tokenBuffer,
			var fileNo to me.fileNo

		token.no := dummyToken
		token.lineNo := - 67
		token.fileNum := - 93
		fileName := dummyFile

		nextToken := token

		lineNo := - 13 % Starting line of current logical line (not used!)
		physLineNo := - 13 % No of lines input so far

		fileNo := - 25 % Bad value
	end Initialize

	proc getOnePhysicalLine (var me : Instance, var lineLen : int, var success : boolean)
		bind var token to me.token, 
			var nextToken to me.nextToken,
			var fileName to me.fileName,
			var lineNo to me.lineNo,
			var physLineNo to me.physLineNo,
			var line to me.line,
			var tokenBuffer to me.tokenBuffer,
			var fileNo to me.fileNo

		success := true

        if eof (fileNo) then
            success := lineLen + 1 <= maxTALineChars
            if not success then
                put "***Error: Attempt to read line too long"
                return
            end if
            lineLen += 1
            line (lineLen) := eofChar
            return
        end if
        physLineNo += 1

        var ch : char
        loop % To avoid crashing on long input lines, use char by char input
            get : fileNo, ch
            exit when ch = eolChar or eof (fileNo)
            if lineLen = maxTALineChars - 1 then % Leave 1 for eolChar
                put "***Error: Line too long: " ..
                for i : 1 .. 20
                    put line (i) ..
                end for
                put " ..."
                loop
                    get : fileNo, ch
                    exit when eof (fileNo)
                end loop
            end if

            lineLen += 1
            line (lineLen) := ch
        end loop
        lineLen += 1
        line (lineLen) := eolChar
    end getOnePhysicalLine

    % Collect physical lines that end with '\' into one logical line
    proc collectLine (var me : Instance)
		bind var token to me.token, 
			var nextToken to me.nextToken,
			var fileName to me.fileName,
			var lineNo to me.lineNo,
			var physLineNo to me.physLineNo,
			var line to me.line,
			var tokenBuffer to me.tokenBuffer,
			var fileNo to me.fileNo

        lineNo := physLineNo + 1
        var lineLen := 0
        loop
            var success : boolean
            getOnePhysicalLine (me, lineLen, success)
            if not success then
                return
            end if
            const backSlash := '\\'
            exit when lineLen = 1 or line (lineLen-1) ~= backSlash
            lineLen -= 2  % Chop off both backslash and eolChar
        end loop
        const eol := '\n'
        line (lineLen + 1) := eol
    end collectLine

    var nextCharToScan := 1

    proc skipWhiteSpace (var me : Instance)
		bind var token to me.token, 
			var nextToken to me.nextToken,
			var fileName to me.fileName,
			var lineNo to me.lineNo,
			var physLineNo to me.physLineNo,
			var line to me.line,
			var tokenBuffer to me.tokenBuffer,
			var fileNo to me.fileNo

        var nextChar := line (nextCharToScan)
        loop
            exit when nextChar = eofChar
            if nextChar = eolChar or
                    (nextChar = "/" &
                    line (nextCharToScan + 1) = "/") then
                collectLine (me)
                nextCharToScan := 1
                nextChar := line (1)
            elsif nextChar = blank or
                    nextChar = '\t' then
                % Skip the white space
                nextCharToScan += 1
                nextChar := line (nextCharToScan)
            else
                exit
            end if
        end loop
    end skipWhiteSpace

    var char2NumToken : array 0 .. 255 of num
    const ordinaryChar := - 23
    for i : 0 .. 255
        char2NumToken (i) := ordinaryChar
    end for
    for i : 1 .. upper (TASpecialChars)
        const specChar := TASpecialChars (i)
        const specCharOrd := ord (specChar)

        char2NumToken (specCharOrd) := nameNum (specChar)
    end for

    fcn chars2Num (var buffer : array 1 .. * of char,
            startOfString, endOfString : int) : num
        var endStr := endOfString
        const extraSpace := length (schemePrefix) + length (attributePrefix)
        var str_ : string
        if endOfString - startOfString + 1 > upper (charstr) - extraSpace
                then
			assert false
        else
            str_ := ""
            % Slow but sure approach.  Should be optimized
            for i : startOfString .. endStr
                str_ += buffer (i)
            end for
        end if
        result nameNum (str_)
    end chars2Num

    proc Scan (var me : Instance, var paramToken, paramNextToken : tokenRecord,
            var success : boolean)
		bind var token to me.token, 
			var nextToken to me.nextToken,
			var fileName to me.fileName,
			var lineNo to me.lineNo,
			var physLineNo to me.physLineNo,
			var line to me.line,
			var tokenBuffer to me.tokenBuffer,
			var fileNo to me.fileNo

        success := true % Useless parameter
        token := nextToken
        % This will skip line boundaries and find non-white space
        skipWhiteSpace (me)
        token.lineNo := nextToken.lineNo
        nextToken.lineNo := lineNo
        var nextChar := line (nextCharToScan)
        nextToken.no := char2NumToken (ord (nextChar))
        if nextToken.no not= ordinaryChar then % Handle special char
            nextCharToScan += 1
        elsif nextChar = eofChar then
            nextToken.no := taEOF

        elsif nextChar = '\"' or nextChar = '\'' then % Scan quoted string
            const quoteChar := nextChar
            nextCharToScan += 1
            nextChar := line (nextCharToScan)
            var strLen := 0
            loop
                % Invariant: nextCharToScan locates next (unscanned) character
                if nextChar = eolChar then
                    put "***Error: [", fileName, ":", lineNo,
                        "]: Missing final quote on string"
                    exit
                end if
                % Invariant: nextCharToScan locates nextChar
                const nextNextChar := line (nextCharToScan + 1)
                if nextChar = '\\' &
                        nextNextChar ~= eolChar &
                        nextNextChar ~= eofChar &
                        (nextNextChar = "\"" or
                        nextNextChar = "\'" or
                        nextNextChar = '\\') then % What '\' cases missed?
                    % Found an embedded quote or a '\'
                    strLen += 1
                    tokenBuffer (strLen) := nextNextChar
                    nextCharToScan += 2
                    nextChar := line (nextCharToScan)
                elsif nextChar = quoteChar then
                    nextCharToScan += 1
                    exit
                else
                    strLen += 1
                    tokenBuffer (strLen) := nextChar
                    nextCharToScan += 1
                    nextChar := line (nextCharToScan)
                end if
            end loop
            nextToken.no := chars2Num (tokenBuffer, 1, strLen)

        else % Scan string token
            const startOfString := nextCharToScan
            loop
                nextCharToScan += 1
                nextChar := line (nextCharToScan)
                exit when nextChar = blank or nextChar = '\t' or
                    char2NumToken (ord (nextChar)) ~= ordinaryChar or
                    nextChar = eolChar or
                    nextChar = eofChar
            end loop
            nextToken.no := chars2Num (line, startOfString, nextCharToScan -
                1)
        end if
        paramToken := token
        paramNextToken := nextToken
    end Scan

    proc Open (var me : Instance, fileNm : string,
            var success : boolean)
		bind var token to me.token, 
			var nextToken to me.nextToken,
			var fileName to me.fileName,
			var lineNo to me.lineNo,
			var physLineNo to me.physLineNo,
			var line to me.line,
			var tokenBuffer to me.tokenBuffer,
			var fileNo to me.fileNo

        assert fileName = "### dummy ###"

        success := true
        lineNo := 0
        physLineNo := 0
        fileName := fileNm
        token.fileNum := nameNum (fileName)
        nextToken.fileNum := token.fileNum
        open : fileNo, fileName, get
        if fileNo <= 0 then
            success := false
            put "***Error: Can't open TA file '", fileName, "'"
            return
        end if
        collectLine (me)
    end Open

    proc Close (var me : Instance)
		bind var token to me.token, 
			var nextToken to me.nextToken,
			var fileName to me.fileName,
			var lineNo to me.lineNo,
			var physLineNo to me.physLineNo,
			var line to me.line,
			var tokenBuffer to me.tokenBuffer,
			var fileNo to me.fileNo

        if fileNo > 0 then
            close : fileNo
        end if
    end Close

end TAScanClass

