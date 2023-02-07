% unit
% module List

    type vote_record :
        record
            name : string
            number : int
        end record

    type shortList : array 1 .. shortListMax of str
    const INDENT := repeat (" ", 4)

    % Kludge cause "star" won't take SIL names that start with % or $

    const dummyFileName := "$$$$"
    var lastFileName : string := dummyFileName

    const tracing := true
    const notTracing := not tracing

    const flagDirectories := "-p" % Parameter for dirToList

    proc openFailMessage (fileNo : int, fileName : string, mode : string)
        put "*** Open '", mode, "' fails for '", fileName, "'"
    end openFailMessage

    proc openForGet (var fileNo : int, fileName : string)
        if lastFileName not= dummyFileName then
            put "***Double open: ", lastFileName, " then ", fileName
        end if
        open : fileNo, fileName, get
        if fileNo > 0 then
            lastFileName := fileName
        else
            openFailMessage (fileNo, fileName, "get")
        end if
    end openForGet

    proc openForRead (var fileNo : int, fileName : string)
        if lastFileName not= dummyFileName then
            put "***Double open: ", lastFileName, " then ", fileName
        end if
        open : fileNo, fileName, read
        if fileNo > 0 then
            lastFileName := fileName
        else
            openFailMessage (fileNo, fileName, "read")
        end if
    end openForRead

    proc openForPut (var fileNo : int, fileName : string)
        if lastFileName not= dummyFileName then
            put "***Double open: ", lastFileName, " then ", fileName
        end if
        open : fileNo, fileName, put
        if fileNo > 0 then
            lastFileName := fileName
        else
            openFailMessage (fileNo, fileName, "put")
        end if
    end openForPut

    proc openForWrite (var fileNo : int, fileName : string)
        if lastFileName not= dummyFileName then
            put "***Double open: ", lastFileName, " then ", fileName
        end if
        open : fileNo, fileName, write
        if fileNo > 0 then
            lastFileName := fileName
        else
            openFailMessage (fileNo, fileName, "write")
        end if
    end openForWrite

    proc openForAppend (var fileNo : int, fileName : string)
        if lastFileName not= dummyFileName then
            put "***Double open: ", lastFileName, " then ", fileName
        end if
        open : fileNo, fileName, seek, mod, put
        if fileNo > 0 then
            lastFileName := fileName
            seek : fileNo, *
        else
            openFailMessage (fileNo, fileName, "append")
        end if
    end openForAppend

    proc closeFile (fileNo : int, fileName : string)
        if fileName not= lastFileName then
            put "*** Warning: last opened ", lastFileName, " but closing ",
                fileName
        end if
        close : fileNo
        % if Error.Last not= 0 then
            % put "***Close fails: ", Error.Last
        % end if
        lastFileName := dummyFileName
    end closeFile

    % Use LSFlags = flagDirectories to get "/" to mark directories
    proc dirToList (dirName : string, LSFlags : string,
            var fileList : array 1 .. * of string (*), var n : int,
            trace : boolean)

        var tempFileName : string
        var fileNo : int
        loop
            var i : int
            randint (i, 0, 999999)
            tempFileName := "/tmp/xxx.HOLT.xxx" + intstr (i)
            open : fileNo, tempFileName, get
            exit when fileNo <= 0 % See if file exists
            close : fileNo
        end loop

        var success : int
        const LSCommand := "ls " + LSFlags + " " + dirName + " > " +
            tempFileName
        system (LSCommand, success)
        if success not= 0 then
            put "### ls was not successful: ", success
            put "   command was: ", LSCommand
            n := 0
            return
        end if

        var fileName : string := tempFileName
        open : fileNo, fileName, get
        if fileNo <= 0 then
            put "### Sorry, can't open: ", tempFileName, " fileNo: ", fileNo
            n := 0
            return
        end if
        n := 0
        loop
            exit when eof (fileNo)
            n += 1
            var entryName : string
            get : fileNo, entryName : *
            fileList (n) := entryName
            if trace then
                put n : 3, " ", fileList (n)
            end if
        end loop

        close : fileNo
        system ("rm " + tempFileName, success)
        if success not= 0 then
            put "Removal of temp file failed"
        end if
    end dirToList

    proc fileToList (fileName : string, var words : array 1 .. * of str,
            var wordsSize : int)
        wordsSize := 0
        var fileNo : int
        openForGet (fileNo, fileName)
        if fileNo <= 0 then
            return
        end if

        loop
            exit when eof (fileNo)
            wordsSize += 1
            var name : charstr
            get : fileNo, name : *   
            words (wordsSize) := nameNum (name)
        end loop
        closeFile (fileNo, fileName)
    end fileToList

    proc putList (names : array 1 .. * of string (*), n : int)
        for i : 1 .. n
            put i : 3, " ", names (i)
        end for
    end putList

    proc putNamList (names : array 1 .. * of str, n : int)
        for i : 1 .. n
            put i : 3, " ", numName (names (i))
        end for
    end putNamList

    proc testDirToList (dirName : string)
        put "Test listing directory: ", dirName
        var names : array 1 .. 1500 of string (25)
        var n : int
        dirToList (dirName, flagDirectories, names, n, false)
        put "                       ", n, " names"
        putList (names, n)
    end testDirToList

    proc testFileToList (fileName : string)
        put "Test reading file: ", fileName
        var names : array 1 .. 500 of str
        var n : int
        fileToList (fileName, names, n)
        put "                       ", n, " names"
        putNamList (names, n)
    end testFileToList

    proc putDirRecursive (dirName : string, indent : string,
            showContents : boolean)
        var names : array 1 .. 1500 of string (25)
        var n : int
        dirToList (dirName, flagDirectories, names, n, false)
        for i : 1 .. n
            if showContents & names (i) = "contents" then
                var contents : array 1 .. 500 of str
                var m : int
                fileToList (dirName + "/contents", contents, m)
                for j : 1 .. m
                    const contentsJ := numName (contents (j))
                    put indent, contentsJ
                    if contentsJ = "" or index (contentsJ, " ") not= 0
                            then
                        put "#####", "Funny file name: '", dirName + "/" +
                            contentsJ, "'"
                    end if
                end for
            else
                put indent, names (i)
                if names (i) (*) = "/" then
                    putDirRecursive (dirName + "/" + names (i) (1 .. * - 1),
                        indent + "    ", showContents)
                end if
            end if
        end for
    end putDirRecursive

    fcn findCurrentDir (dirName : string) : string
        var currentDir := ""
        var i := length (dirName)
        loop
            exit when i = 0 or dirName (i) = "/"
            currentDir := dirName (i) + currentDir
            i -= 1
        end loop
        result currentDir
    end findCurrentDir

    proc listToFile (words : array 1 .. * of str, wordsSize : int,
            fileName : string)
        var fileNo : int
        openForPut (fileNo, fileName)
        if fileNo <= 0 then
            return
        end if

        for i : 1 .. wordsSize
            put : fileNo, numName(words (i))
        end for
        closeFile (fileNo, fileName)
    end listToFile

    fcn fixName (charName : string) : string
        if length (charName) > 0 then
            if charName (1) = "%" then
                result "P_" + charName (2 .. *)
            elsif charName (1) = "$" then
                result "D_" + charName (2 .. *)
            else
                result charName
            end if
        end if
        result charName
    end fixName
    
    fcn fixNameNumber (nameNumber : str) : string
        const name := numName (nameNumber)
        result fixName (name)
    end fixNameNumber

    fcn memberOfList (words : array 1 .. * of str, noWords : int,
            word : str) : boolean
        for i : 1 .. noWords
            if words (i) = word then
                result true
            end if
        end for
        result false
    end memberOfList

    fcn isSubset (leftSet : shortList, leftSetSize : int,
            rightSet : shortList, rightSetSize : int) : boolean
        for i : 1 .. leftSetSize
            if not memberOfList (rightSet, rightSetSize, leftSet (i)) then
                result false
            end if
        end for
        result true
    end isSubset

    proc insertIntoList (var words : array 1 .. * of str, var noWords : int,
            word : str)
        % Does not maintain alphabetic order
        noWords += 1
        if noWords > upper (words) then
            put "#####Short list not long enough for insertion of ", 
                    numName(word)
        end if
        words (noWords) := word
    end insertIntoList

    proc insertIntoListUnique (var words : array 1 .. * of str, 
        var noWords : int, word : str)
        % Does not maintain alphabetic order
        if not memberOfList (words, noWords, word) then
            noWords += 1
            if noWords > upper (words) then
                put "***Sorry, list max size (", upper(words), " limits set. " +
                    " Can't add word '", word, "'"
                quit
            end if
            words (noWords) := word
        end if
    end insertIntoListUnique

    proc listAdd (var list1 : array 1 .. * of str, var list1Size : int,
            list2 : array 1 .. * of str, list2Size : int)
        for i : 1 .. list2Size
            insertIntoListUnique (list1, list1Size, list2 (i))
        end for
    end listAdd

    proc deleteFromList (var list : array 1 .. * of str,
            var listSize : int, item : str)
        var noDeleted := 0
        for i : 1 .. listSize
            if list (i) = item then
                noDeleted += 1
            else
                if noDeleted > 0 then
                    list (i - noDeleted) := list (i)
                end if
            end if
        end for
        listSize -= noDeleted
    end deleteFromList

    proc listSubtract (var list1 : array 1 .. * of str, var list1Size : int,
            list2 : array 1 .. * of str, list2Size : int)
        for i : 1 .. list2Size
            deleteFromList (list1, list1Size, list2 (i))
        end for
    end listSubtract

    proc listIntersect (var list1 : shortList, %array 1 .. * of str,
            var list1Size : int,
            list2 : array 1 .. * of str, list2Size : int)
        var tempList : shortList
        var tempListSize := 0
        for i : 1 .. list2Size
            const member2 := list2 (i)
            for j : 1 .. list1Size
                if memberOfList (list1, list1Size, member2) then
                    insertIntoListUnique (tempList, tempListSize, member2)
                end if
            end for
        end for
        list1 := tempList
        list1Size := tempListSize
    end listIntersect

    proc putListOnLine (list : array 1 .. * of string (*), listSize : int,
            fixNames : boolean)
        for i : 1 .. listSize
            var name := list (i)
            if fixNames then
                name := fixNameNumber (nameNum(name))
            end if
            put name, " " ..
        end for
        put ""
    end putListOnLine

    proc putListWithCommas (fileNo : int,
            list : array 1 .. * of str, listSize : int,
            indent : string, fixNames : boolean)
        for i : 1 .. listSize
            var nameNumber : str := list (i)
            var name := numName (nameNumber)
            if fixNames then
                name := fixNameNumber (nameNumber)
            end if
            if i < listSize then
                put : fileNo, indent, name, ","
            else
                put : fileNo, indent, name
            end if
        end for
    end putListWithCommas

    proc putLabelledList (fileNo : int,
            list : array 1 .. * of str, listSize : int,
            listLabel : string, fixNames : boolean)
        if listSize > 0 then
            put : fileNo, INDENT, listLabel
            putListWithCommas (fileNo, list, listSize, INDENT + INDENT,
                fixNames)
        end if
    end putLabelledList

    proc putLabelledListNoCommas (fileNo : int,
            list : array 1 .. * of str, listSize : int,
            listLabel : string, fixNames : boolean)
        put : fileNo, listLabel
        if listSize > 0 then
            for i : 1 .. listSize
                var nameNumber : str := list (i)
                var name := numName (nameNumber)
                if fixNames then
                    name := fixNameNumber (nameNumber)
                end if
                put : fileNo, INDENT, INDENT, name
            end for
        else
            put INDENT, INDENT, "empty"
        end if
    end putLabelledListNoCommas

    proc putCommentList (fileNo : int,
            list : array 1 .. * of str, listSize : int,
            fixNames : boolean)
        for i : 1 .. listSize
            var nameNumber := list (i)
            var name := numName (nameNumber)
            if fixNames then
                name := fixName (name)
            end if
            put : fileNo, INDENT, "% ", name
        end for
    end putCommentList

    fcn isPrefix (prefix, name : string) : boolean
        const lenPrefix := length (prefix)
        result length (name) >= lenPrefix and name (1 .. lenPrefix) = prefix
    end isPrefix

    fcn isSuffix (suffix, name : string) : boolean
        const lenSuffix := length (suffix)
        result length (name) >= lenSuffix and
            name (* - lenSuffix + 1 .. *) = suffix
    end isSuffix

% end List

