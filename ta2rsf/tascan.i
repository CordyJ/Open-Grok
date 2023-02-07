
module TAScan
	
    export Open, Close, Scan, 
		Token, NextToken

	include "tascanc.i"

    var token, nextToken : tokenRecord

	function Token : tokenRecord
		result token
	end Token

	function NextToken : tokenRecord
		result nextToken
	end NextToken

    const maxOpenIncludes := 40
    var fileStack : array 1 .. maxOpenIncludes of TAScanClass.Instance

    var top : 0 .. maxOpenIncludes := 0

    proc Scan
        var success : boolean
        TAScanClass.Scan (fileStack (top), token, nextToken, success)
        if token.no = taINCLUDE then
            TAScanClass.Scan (fileStack (top), token, nextToken, success) % Skip INCLUDE
            const fileNm := token.no
            var fileName := numName (fileNm)
            const colon := nextToken.no
            if colon ~= taColon then
                put "***Error: Missing colon ':' after: INCLUDE ", fileName
            end if
            if top = maxOpenIncludes then
                put "***Error: Too many INCLUDE files nested"
                return
            end if
            top += 1
            TAScanClass.Initialize (fileStack (top))

			%
			external proc getcwd (var wd : string, nchars : int) % char *getcwd(char *buf, size_t size); 
			var pwdPath : string
			getcwd (pwdPath, 256)
            % var pwdPath := Dir.Current
			%

            fileName := pwdPath + fileName
            TAScanClass.Open (fileStack (top), fileName, success)
            if ~success then
                TAScanClass.Close (fileStack (top))
                top -= 1
                return
            else % Opened INCLUDE file successfully
                Scan % Yep, recursive, see if new file starts with INCLUDE
                Scan
            end if
        elsif token.no = taEOF & top > 1 then
            TAScanClass.Close (fileStack (top))
            top -= 1
            Scan % Prime old INCLUDE file
            Scan
        end if

    end Scan

    proc Open (fileNm : string, var success : boolean)
        pre top = 0
    
        InitGlobals

        top += 1
        TAScanClass.Open (fileStack (top), fileNm, success)
        if not success then
            top := 0
            return
        end if
        Scan
        Scan
    end Open

    proc Close
        for i : 1 .. top
            TAScanClass.Close (fileStack (i))
        end for
        top := 0
    end Close

end TAScan

