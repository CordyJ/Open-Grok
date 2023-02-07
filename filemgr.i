% This file manager module was built to allow Grok to support
% loops.  Each command file is "cached" into this module.  A loop
% returns to its beginning by "seeking" back there... which is
% accomplished by resetting a "nextLineNo" pointer back to the
% beginning.

% When a file is opened, it is cached here.  In case of a future
% open of the same file, the same cached lines are used.  The
% cache is never flushed.

%       Ric Holt Jan 99

module fileMgr

    import var commandLinesMax

    export Open, Close, Get, Seek, Tell, EOF

    const EOFString := "### EOF ###"

    const commandLineSize := 200
    type commandLineType : string (commandLineSize)
    const nullFileLocator := - 1
    const maxCommandFiles := 50

    var commandLines : 
		% flexible 
		array 1 .. commandLinesMax of commandLineType
    var noCommandLines := 0

    % File record statuses:
    const freeRecord := 0
    const openFile := 1
    const closedFile := 2

    var fileRecord : array 1 .. maxCommandFiles of
        record
            fileName : string
            status : int
            firstLineNo : int
            lastLineNo : int
            nextLineNo : int
        end record

    for i : 1 .. maxCommandFiles
        fileRecord (i).status := freeRecord
    end for

    procedure increaseCommandLines
        put "Grok increases max command lines from ", commandLinesMax,
            " to " ..
        const linesAddOn := 2000
        commandLinesMax += linesAddOn
        put commandLinesMax
        % new commandLines, commandLinesMax
    end increaseCommandLines

    fcn findFileRecord (fileName : string) : int
        for i : 1 .. maxCommandFiles
            if fileRecord (i).status = closedFile &
                    fileRecord (i).fileName = fileName then
                result i
            end if
        end for
        result nullFileLocator
    end findFileRecord

    fcn findFreeFileRecord : int
        for i : 1 .. maxCommandFiles
            if fileRecord (i).status = freeRecord then
                result i
            end if
        end for
        result nullFileLocator
    end findFreeFileRecord

    procedure Open (var commandFileNo : int, commandFileName : string)

        var oldFileIndex := findFileRecord (commandFileName)
        var fileIndex : int

        if oldFileIndex = nullFileLocator then
            var fileNo : int
            open : fileNo, commandFileName, get
            if fileNo <= 0 then
                commandFileNo := - 1
                return
            end if

            fileIndex := findFreeFileRecord
            if fileIndex < 0 then
                put "***Sorry, no room for command file: ", commandFileName
                commandFileNo := - 1
                return
            end if

            fileRecord (fileIndex).fileName := commandFileName
            fileRecord (fileIndex).status := openFile
            fileRecord (fileIndex).firstLineNo := noCommandLines + 1
            fileRecord (fileIndex).nextLineNo := noCommandLines + 1
            loop
                exit when eof (fileNo)
                if noCommandLines = commandLinesMax then
                    increaseCommandLines
                end if
                noCommandLines += 1
                get : fileNo, commandLines (noCommandLines) : *
            end loop
            close : fileNo
            fileRecord (fileIndex).lastLineNo := noCommandLines
        else
            fileIndex := oldFileIndex
            if fileRecord (fileIndex).status = openFile then
                put "***Error: Reopened file: ",
                    fileRecord (fileIndex).fileName
                return
            end if
            assert fileRecord (fileIndex).status = closedFile
            fileRecord (fileIndex).status := openFile
            % Rewind file to beginning
            fileRecord (fileIndex).nextLineNo :=
                fileRecord (fileIndex).firstLineNo
        end if
        commandFileNo := fileIndex

    end Open

    procedure Close (commandFileLocator : int)
        pre commandFileLocator >= 1 & commandFileLocator <= maxCommandFiles

        assert fileRecord (commandFileLocator).status = openFile
        % Closed during "Open"  --- close : commandFileLocator
        fileRecord (commandFileLocator).status := closedFile
    end Close

    procedure Get (commandFileLocator : int, var line : string)
        pre commandFileLocator >= 1 & commandFileLocator <= maxCommandFiles

        const nextLineNo := fileRecord (commandFileLocator).nextLineNo
        if nextLineNo <= fileRecord (commandFileLocator).lastLineNo then
            line := commandLines (nextLineNo)
            fileRecord (commandFileLocator).nextLineNo := nextLineNo + 1
        else
            line := EOFString
        end if
    end Get

    procedure Tell (commandFileLocator : int, var lineNo : int)
        pre commandFileLocator >= 1 & commandFileLocator <= maxCommandFiles

        lineNo := fileRecord (commandFileLocator).nextLineNo
    end Tell

    procedure Seek (commandFileLocator : int, lineNo : int)
        pre commandFileLocator >= 1 & commandFileLocator <= maxCommandFiles

        fileRecord (commandFileLocator).nextLineNo := lineNo
    end Seek

    fcn EOF (commandFileLocator : int) : boolean
        pre commandFileLocator >= 1 & commandFileLocator <= maxCommandFiles

        result fileRecord (commandFileLocator).nextLineNo >
            fileRecord (commandFileLocator).lastLineNo
    end EOF

end fileMgr

