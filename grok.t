% File "grok.t".  The Grok Relational Calculator


% Converted (sort of) to Turing Plus by Jim Cordy, 31 Oct 2000
% See the file "README.T+" for details.


% Updates 29 Dec 98 by Holt:
% Allowed multi-line statements, via backslash as last line of a string.


% Updates 10-31 Dec 97 by Holt:
% Extensive rewrite.  Uses pigeon hole & radix sorts.  New fast
% transitive closure.  Fixed known bugs.  Added many little features.
% Auto expansion of data base size and name table size.
% Still has a static max on set sizes.  New help menu documents
% supported features.


% Updates 7 Dec 97 by Holt:
%       Made Tzerpos sequence to sort db using Unix facility optional
%           This should be reactived for Unix version.
%       Allowed tab in statements (ignored it).
%       Rewrote scanner to allow the := operator.
%       Allowed the // comment.
%       Implemented statements of the form   x := expn
%           where operators are any of +, -, ^, *, inv, dom, rng, ent
%           and x can be either a relation or a set.
%           This is true assignment (and not += as is the case for
%           = with relations).
%       Left in the = operator for backward compatibility ---/"-"
%           It still means += with relations.
%       Need to test transitive closure with external sort [did this]
%       Implemented fast rel composition & intersection
%       Separated out options into "maxsizes.tu"
%       Fixed up menu to current state of Grok.
%       Added check of each "new" for explicit out-of-memory messages.

% Fixed all the following:
%   7 Dec 97 Known bugs or needed fixes:

% Other needed fixes:

%=================================================
% The Grok program, which supports the Grok language.
%               Written by R.C. Holt  Nov 1995

% This program manipulates sets and binary relations.
% It inputs relations from a file "data base", where each
% tuple has this form:
%       relationName sourceName targetName
% For example:
%       declare fileName procName
% This form is called RSF (Rigi Standard Form) because it is the standard
% external format for Univ of Victoria Rigi files describing relations.
% Each item in a set (or part of a tuple) is a string.

% This program was originally written to draw pictures (landscapes)
% for programs (Tobey in particular) written in PLIX.  There are some
% specialized procedures that are useful only for handling RSF
% files prepared by Mike Whitney from XREF Plix output. [Not externall
% documented.]

% Since inception, this program has steadily evolved in the general
% direction of a prototype for manipulating sets and binary
% relations, in a way that is useful for software re-engineering
% and for creating landscapes.

% See the command menu for a list of facilities of the Grok language.

% Short comings:
%       The data structures used here are extremely simple (and rather slow).
%       The relational composition algorithm does
%       not eliminate duplicate tuples.  These can be gotten rid of
%       by writing the tuples out as RSF, and usings Unix's "sort -u"
%       utility.  [Done by Tzerpos.] [Use of heap sort also fixes this]
%       The relation assignment operations here generally add to the array
%       of tuples without checking for duplication. [Fixed by Holt].

include "%system"
include "globals.i"
include "nametab.i"
include "list.i"
include "pigeon.i"
include "filemgr.i"

include "ta2rsf/taparse.i"
include "ta2rsf/prefixes.i"
include "ta2rsf/taoutput.i"
include "ta2rsf/graxout.i"

var relNames : shortList
var relNamesSize : int

type listNode :
    record
        list : shortList
        size : int
    end record

var listNodes : collection of listNode
type setPtr : pointer to listNodes

type domNode :
    record
        domSet : setPtr
        predSet : setPtr
    end record

type domNodeSet :
    record
        list : array 1 .. shortListMax of domNode
        size : int
    end record

var domNodeSets : collection of domNodeSet
type domSetPtr : pointer to domNodeSets

fcn newSet : setPtr
    var p : setPtr
    new listNodes, p
    if p = nil (listNodes) then
        put "***Sorry, out of memory.  No room for new set."
        quit
    end if
    result p
end newSet

fcn newEmptySet : setPtr
    var p : setPtr
    p := newSet
    listNodes (p).size := 0
    result p
end newEmptySet

fcn sameSet (s : setPtr) : setPtr
    var p : setPtr
    p := newSet
    listNodes (p).size := listNodes (s).size
    listNodes (p).list := listNodes (s).list
    result p
end sameSet

fcn newSingletonSet (s : str) : setPtr
    var p : setPtr
    p := newSet
    listNodes (p).list (1) := s
    listNodes (p).size := 1
    result p
end newSingletonSet

proc freeSet (var p : setPtr)
    % Later could optimize to store old set records and reuse them
    free listNodes, p
end freeSet

proc clearSet (p : setPtr)
    listNodes (p).size := 0
end clearSet

proc setInsert (s : str, p : setPtr)
    insertIntoListUnique (listNodes (p).list, listNodes (p).size, s)
end setInsert

proc setRemove (s : str, p : setPtr)
    deleteFromList (listNodes (p).list, listNodes (p).size, s)
end setRemove

fcn subset (p, q : setPtr) : boolean
    result isSubset (listNodes (p).list, listNodes (p).size, listNodes (q).list, listNodes (q).size)
end subset

fcn isMember (s : str, p : setPtr) : boolean
    result memberOfList (listNodes (p).list, listNodes (p).size, s)
end isMember

var setVars : array 1 .. maxSets of
    record
        setName : string
        p : setPtr
    end record

var setVarsSize := 0

var STRINGVars : array 1 .. maxSTRINGs of
    record
        STRINGName : string
        String : string
    end record

var STRINGVarsSize := 0

fcn findSetVar (setName : string) : int
    for i : 1 .. setVarsSize
        if setVars (i).setName = setName then
            result i
        end if
    end for
    result 0
end findSetVar

fcn findSTRINGVar (STRINGName : string) : int
    for i : 1 .. STRINGVarsSize
        if STRINGVars (i).STRINGName = STRINGName then
            result i
        end if
    end for
    result 0
end findSTRINGVar

const tempPrefix := "tmp#" % Prefix for temporary sets in expressions
const lenTempPrefix := length (tempPrefix)

proc clearSetVars
    for i : 1 .. setVarsSize
        free listNodes, setVars (i).p
    end for
    setVarsSize := 0
end clearSetVars

proc clearSTRINGVars
    STRINGVarsSize := 0
end clearSTRINGVars

proc deleteSTRINGVar (varName : string)
    var foundVar := false
    for i : 1 .. STRINGVarsSize
        if foundVar then
            STRINGVars (i - 1) := STRINGVars (i)
        elsif varName = STRINGVars (i).STRINGName then
            foundVar := true
        end if
    end for
    if not foundVar then
        put "***Error in Grok in deleteSTRINGVar: ", varName
    end if
    STRINGVarsSize -= 1
end deleteSTRINGVar

proc deleteNamedSetTemps
    var noDeleted := 0
    for i : 1 .. setVarsSize
        const setVarName := setVars (i).setName
        if length (setVarName) >= lenTempPrefix and
                setVarName (1 .. lenTempPrefix) = tempPrefix then
            noDeleted += 1
            free listNodes, setVars (i).p
        elsif noDeleted > 0 then
            setVars (i - noDeleted) := setVars (i)
        end if
    end for
    setVarsSize -= noDeleted
end deleteNamedSetTemps

proc deleteNamedSTRINGTemps
    var noDeleted := 0
    for i : 1 .. STRINGVarsSize
        const STRINGVarName := STRINGVars (i).STRINGName
        if length (STRINGVarName) >= lenTempPrefix and
                STRINGVarName (1 .. lenTempPrefix) = tempPrefix then
            noDeleted += 1
        elsif noDeleted > 0 then
            STRINGVars (i - noDeleted) := STRINGVars (i)
        end if
    end for
    STRINGVarsSize -= noDeleted
end deleteNamedSTRINGTemps

proc deleteNamedTemps
    deleteNamedSetTemps
    deleteNamedSTRINGTemps
end deleteNamedTemps

fcn isSetVar (setName : string) : boolean
    result findSetVar (setName) not= 0
end isSetVar

fcn isSTRINGVar (STRINGName : string) : boolean
    result findSTRINGVar (STRINGName) not= 0
end isSTRINGVar

fcn findSetValue (setName : string) : setPtr
    pre isSetVar (setName)
    result setVars (findSetVar (setName)).p
end findSetValue

proc findSingletonValue (fileName : string, var value : string,
        var isSingleton : boolean)
    if isSetVar (fileName) then
        const setPtr := findSetValue (fileName)
        if listNodes (setPtr).size = 1 then
            value := numName (listNodes (setPtr).list (1))
            isSingleton := true
        else
            isSingleton := false
        end if
    else
        value := fileName
        isSingleton := true
    end if
end findSingletonValue

proc makeSetVar (setName : string, p : setPtr)
    setVarsSize += 1
    if setVarsSize > upper (setVars) then
        put "Sorry, out of room for new set ", setName
        return
    end if
    setVars (setVarsSize).setName := setName
    setVars (setVarsSize).p := p
end makeSetVar

proc makeSTRINGVar (STRINGName : string, s : STRING)
    STRINGVarsSize += 1
    if STRINGVarsSize > upper (STRINGVars) then
        put "Sorry, out of room for new string variable ", STRINGName
        return
    end if
    STRINGVars (STRINGVarsSize).STRINGName := STRINGName
    STRINGVars (STRINGVarsSize).String := s
end makeSTRINGVar

proc assignExistingSetVar (setName : string, p : setPtr)
    pre isSetVar (setName)
    const loc := findSetVar (setName)
    freeSet (setVars (loc).p)
    setVars (loc).p := p
end assignExistingSetVar

proc assignExistingSTRINGVar (STRINGName : string, s : STRING)
    pre isSTRINGVar (STRINGName)
    const loc := findSTRINGVar (STRINGName)
    STRINGVars (loc).String := s
end assignExistingSTRINGVar

proc assignSetVar (setName : string, p : setPtr)
    if isSetVar (setName) then
        assignExistingSetVar (setName, p)
    else
        makeSetVar (setName, p)
    end if
end assignSetVar

proc assignSTRINGVar (STRINGName : string, s : STRING)
    if isSTRINGVar (STRINGName) then
        assignExistingSTRINGVar (STRINGName, s)
    else
        makeSTRINGVar (STRINGName, s)
    end if
end assignSTRINGVar

fcn strrealok (s : string) : boolean
	const numberchars := "0123456789."
	for i : 1 .. length (s)
		if index (numberchars, s (i)) = 0 then
			result false
		end if
	end for
	result true
end strrealok

fcn isNUMBER (s : string) : boolean
    result strrealok (s) or
        (isSTRINGVar (s) and
        strrealok (STRINGVars (findSTRINGVar (s)).String))
end isNUMBER

fcn isBOOL (s : string) : boolean
    if s = "true" or s = "false" then
        result true
    elsif isSTRINGVar (s) then
        const v := STRINGVars (findSTRINGVar (s)).String
        result v = "true" or v = "false"
    else
        result false
    end if
end isBOOL

fcn isNUMBEROp (op : string) : boolean
    const nOp : array 1 .. 6 of string (3) := init ("+", "-", "*", "/", "div", "mod")
    for i : 1 .. upper (nOp)
        if op = nOp (i) then
            result true
        end if
    end for
    result false
end isNUMBEROp

fcn isCOMPAREOp (op : string) : boolean
    const cOp : array 1 .. 7 of string (3) := init ("==", "~=", "=", "<", ">", "<=", ">=")
    for i : 1 .. upper (cOp)
        if op = cOp (i) then
            result true
        end if
    end for
    result false
end isCOMPAREOp

forward fcn isRel (relName : string) : boolean

proc evalStringAsSingleton (token : string, var thisSet : setPtr,
        var isTemp : boolean)
    if isSetVar (token) or isRel (token) then
        put "***Error: '", token, " should be a string value"
        thisSet := findSetValue (token)
        isTemp := false
    else
        var strValue := token
        if isSTRINGVar (token) then
            strValue := STRINGVars (findSTRINGVar (token)).String
        end if
        thisSet := newSingletonSet (nameNum (strValue))
        isTemp := true
    end if
end evalStringAsSingleton

fcn evalStringAsSingletonTemp (token : string) : setPtr
    if isSetVar (token) or isRel (token) then
        put "***Error: '", token, " should be a string value"
        const p := findSetValue (token)
        const thisSet := newSet
        listNodes (thisSet) := listNodes (p) % Copy value
        result thisSet
    else
        var strValue := token
        if isSTRINGVar (token) then
            strValue := STRINGVars (findSTRINGVar (token)).String
        end if
        result newSingletonSet (nameNum (strValue))
    end if
end evalStringAsSingletonTemp

proc evalSet (token : string, var thisSet : setPtr,
        var isTemp : boolean)
    if isSetVar (token) then
        thisSet := findSetValue (token)
        isTemp := false
    else
        var strValue := token
        if isSTRINGVar (token) then
            strValue := STRINGVars (findSTRINGVar (token)).String
        end if
        thisSet := newSingletonSet (nameNum (strValue))
        isTemp := true
        put "***Warning: '", strValue, "' is assumed to be singleton set."
    end if
end evalSet

fcn evalSetAsTemp (token : string) : setPtr
    if isSetVar (token) then
        const p := findSetValue (token)
        const thisSet := newSet
        listNodes (thisSet) := listNodes (p) % Copy value
        result thisSet
    else
        var strValue := token
        if isSTRINGVar (token) then
            strValue := STRINGVars (findSTRINGVar (token)).String
        end if
        put "***Warning: '", strValue, "' is assumed to be singleton set"
        result newSingletonSet (nameNum (strValue))
    end if
end evalSetAsTemp

fcn isSTRINGLiteral (S : string) : boolean
    result length (S) >= 2 &
        (S (1) = "\"" & S (*) = "\"" or
        S (1) = "'" & S (*) = "'")
end isSTRINGLiteral

fcn unquote (X : string) : string
    pre isSTRINGLiteral (X)
    var S := X (2 .. * - 1)
    loop
        const loc := index (S, "\\\"")
        exit when loc = 0
        S := S (1 .. loc - 1) + S (loc + 1 .. *)
    end loop
    loop
        const loc := index (S, "\\'")
        exit when loc = 0
        S := S (1 .. loc - 1) + S (loc + 1 .. *)
    end loop
    result S
end unquote

fcn evalSTRING (S : string) : string
    if isSTRINGVar (S) then
        result STRINGVars (findSTRINGVar (S)).String
    elsif isSTRINGLiteral (S) then
        result unquote (S)
    elsif isSetVar (S) then
        const setVarName := S
        var thisSetPtr : setPtr
        var thisSetIsTemp : boolean
        evalSet (setVarName, thisSetPtr, thisSetIsTemp)
        const resultStr := numName (listNodes (thisSetPtr).list (1))
        if thisSetIsTemp then
            freeSet (thisSetPtr)
        end if
        put "***Warning: Using member '", resultStr,
            "' of set '", S, "' as string"
        result resultStr
    else
        put "***Warning: Treating '", S, "' as a string literal"
        result S
    end if
end evalSTRING

fcn evalNUMBER (NUM : string) : real
    pre isNUMBER (NUM)
    if strrealok (NUM) then
        result strreal (NUM)
    else
        result strreal (STRINGVars (findSTRINGVar (NUM)).String)
    end if
end evalNUMBER

fcn evalBOOL (B : string) : boolean
    pre isBOOL (B)
    if B = "true" or B = "false" then
        result B = "true"
    else
        result STRINGVars (findSTRINGVar (B)).String = "true"
    end if
end evalBOOL

fcn bool2BOOL (b : boolean) : string
    if b then
        result "true"
    else
        result "false"
    end if
end bool2BOOL

proc putLiteral (literalName : string)
    put "Literal value: " + literalName
end putLiteral

proc putSetVar (setName : string)
    if not isSetVar (setName) then
        put "** ", setName, " is not a set"
        return
    end if
    const loc := findSetVar (setName)
    const p := setVars (loc).p
    if listNodes (p).size = 0 then
        put "Set is empty"
    else
        putLabelledListNoCommas (stdOutput, listNodes (p).list, listNodes (p).size,
            "Set contains " + intstr (listNodes (p).size) +
            " items:", false)
    end if
end putSetVar

proc putSTRINGVar (STRINGName : string)
    if not isSTRINGVar (STRINGName) then
        put "** ", STRINGName, " is not a STRING"
        return
    end if
    const loc := findSTRINGVar (STRINGName)
    put "    ", STRINGVars (loc).String
end putSTRINGVar

proc putSetVarSample (setName : string, startPos, sampleSize : int)
    if not isSetVar (setName) then
        put "** ", setName, " is not a set"
        return
    end if
    const loc := findSetVar (setName)
    const p := setVars (loc).p
    if listNodes (p).size = 0 then
        put "Set ", setName, " is empty"
    else
        var n := 0
        var sampled := 0
        loop
            exit when n = listNodes (p).size
            n += 1
            if n >= startPos and sampled < sampleSize then
                put INDENT, numName (listNodes (p).list (n))
                sampled += 1
            end if
        end loop
        put INDENT, INDENT, "Sampled ", sampled, " of ", listNodes (p).size,
            " elements"
    end if
end putSetVarSample

proc findRelationNames (whereStart : int)
    var clock1, clock2 : int
    if timing then
        clock (clock1)
    end if
    if optimize then
        const seen := 1
        const notSeen := 0
        for i : 1 .. nameCount
            Index1 (i) := notSeen
        end for
        for i : whereStart + 1 .. DBSize
            Index1 (DBRel (i)) := seen
        end for
        for i : 1 .. nameCount
            if Index1 (i) = seen then
                insertIntoListUnique (relNames, relNamesSize, i)
            end if
        end for
    else
        for i : whereStart + 1 .. DBSize
            insertIntoListUnique (relNames, relNamesSize, DBRel (i))
        end for
    end if
    putLabelledList (stdOutput, relNames, relNamesSize, "Relations are",
        false)
    if timing then
        clock (clock2)
        put " Time to find relation names: ", (clock2 - clock1) / 1000 : 0 :
            1
    end if
end findRelationNames

body fcn isRel % (relName : string) : boolean
    const relNumber := nameNum (relName)
    result memberOfList (relNames, relNamesSize, relNumber)
end isRel

proc shouldBeSet (s : string)
    if ~isSetVar (s) then
        put "***Assuming '", s, "' is a set"
    end if
end shouldBeSet

proc shouldBeRel (s : string)
    if ~isRel (s) then
        put "***Assuming '", s, "' is a relation"
    end if
end shouldBeRel

proc insertDB (src, relName, trg : str)
    assert DBSize <= DBMax
    if DBSize = DBMax then
        increaseDB
    end if
    DBSize += 1
    DBSrc (DBSize) := src
    DBRel (DBSize) := relName
    DBTrg (DBSize) := trg
end insertDB

proc getdb (DBFileName : string, gettingDB : boolean)
    % This implements both 'getdb' and 'adddb';
    %       the flag gettingdb determines which of these.
    var fileNo : int
    var fileName := DBFileName
    var whereStart := DBSize
    if gettingDB then
        DBSize := 0
        whereStart := 0
    end if
    var addCount := 0

    put "Inputting: ", fileName
    openForGet (fileNo, fileName)
    if fileNo <= 0 then
        return
    end if
    loop
        get : fileNo, skip
        exit when eof (fileNo)
        var relName, srcName, trgName : string
        get : fileNo, relName, skip
        if eof (fileNo) then
            put "***Unexpected EOF"
            exit
        end if
        get : fileNo, srcName, skip
        if eof (fileNo) then
            put "***Unexpected EOF"
            exit
        end if
        get : fileNo, trgName

        if length (relName) > strSize or
                length (srcName) > strSize or
                length (trgName) > strSize then
            put "*** ERROR: Name longer than ", strSize, " in this tuple: ",
                skip, relName, " ", srcName, " ", trgName
            put "    Ignoring this tuple"

        else
            addCount += 1
            const relNumber := nameNum (relName)
            const srcNumber := nameNum (srcName)
            const trgNumber := nameNum (trgName)
            insertDB (srcNumber, relNumber, trgNumber)
            if progress & addCount mod 1000 = 0 then
                put addCount : 5, " ", relName : 8, " ", srcName : 14, " ",
                    trgName
            end if
        end if
    end loop
    closeFile (fileNo, fileName)

    put "Read ", addCount, " tuples into DB from file ", fileName
    put "   DB size is ", DBSize
    findRelationNames (whereStart)
end getdb

proc insertDBWithFile (fileNo : int, src, rel, trg : num)
    insertDB (src, rel, trg)
end insertDBWithFile

proc getta (fileName : string, gettingTA : boolean)
    % This implements both 'getta' and 'addta';
    %       the flag gettingta determines which of these.


    put "Inputting TA: ", fileName

    if gettingTA then
        DBSize := 0
    end if

    const whereStart := DBSize

    const oldDBSize := DBSize

    var dummyFileNo : int := - 19
    var success : boolean
    TAParse.Parse (dummyFileNo, fileName, insertDBWithFile, success)

    put "Read ", DBSize - oldDBSize, " tuples into DB from TA file ",
        fileName
    put "   DB size is ", DBSize

    findRelationNames (whereStart)
end getta

proc cleardb
    DBSize := 0
    relNamesSize := 0
end cleardb

proc quoteIfNeeded (var s : string)
    const quoteNeeded := index (s, " ") ~= 0 or
        index (s, "\t") ~= 0 or
        index (s, "\n") ~= 0 or
        s = ""
    if quoteNeeded then
        s := "\"" + s + "\""
    end if
end quoteIfNeeded

proc putdb (DBFileName : string)
    var fileNo : int
    var fileName := DBFileName

    put "Outputting: ", fileName
    openForPut (fileNo, fileName)
    if fileNo <= 0 then
        put "Sorry, can't output file ", fileName
        return
    end if
    for n : 1 .. DBSize
        var Rel := numName (DBRel (n))
        var Src := numName (DBSrc (n))
        var Trg := numName (DBTrg (n))
        quoteIfNeeded (Rel)
        quoteIfNeeded (Src)
        quoteIfNeeded (Trg)

        put : fileNo, Rel : 10, "  ", Src : 15, "  ", Trg
        if progress & n mod 1000 = 0 then
            put n : 5, " ", Rel : 8, " ", Src : 14, " ", Trg
        end if
    end for
    closeFile (fileNo, fileName)
    put "Wrote ", DBSize, " tuples into file ", fileName

end putdb

const SCHEMETUPLE := 1
const SCHEMEATTR := 2
const FACTTUPLE := 3
const FACTATTR := 4
const NONE := 0
var section := NONE

const sectionHeader : array 1 .. 4 of string (30) :=
    init ("SCHEME TUPLE :", "SCHEME ATTRIBUTE :",
    "FACT TUPLE :", "FACT ATTRIBUTE :")

fcn whatSection (section, src, rel, trg : str) : int
    const Rel := numName (rel)
    if isPrefix (schemePrefix, Rel) then
        if isPrefix (schemeAttributePrefix, Rel) then
            result SCHEMEATTR
        else
            result SCHEMETUPLE
        end if
    else
        if isPrefix (attributePrefix, Rel) then
            result FACTATTR
        else
            result FACTTUPLE
        end if
    end if
end whatSection

proc putta (fileName : string)
    var fileNo : int

    put "Outputting TA: ", fileName
    openForPut (fileNo, fileName)
    if fileNo <= 0 then
        put "Sorry, can't output file ", fileName
        return
    end if
    for n : 1 .. DBSize

        const src := DBSrc (n)
        const rel := DBRel (n)
        const trg := DBTrg (n)

        const newSection := whatSection (section, src, rel, trg)
        if newSection ~= section then
            put : fileNo, sectionHeader (newSection)
            section := newSection
        end if

        TAOutput.outputTATuple (fileNo, src, rel, trg)

        if progress & n mod 1000 = 0 then
            var Rel := numName (DBRel (n))
            var Src := numName (DBSrc (n))
            var Trg := numName (DBTrg (n))
            put n : 5, " ", Rel : 8, " ", Src : 14, " ", Trg
        end if
    end for
    closeFile (fileNo, fileName)
    put "Wrote ", DBSize, " TA lines into file ", fileName

end putta

proc putgrax (fileName : string, facts : boolean)
    var fileNo : int

    put "Outputting Grax: ", fileName
    openForPut (fileNo, fileName)
    if fileNo <= 0 then
        put "Sorry, can't output file ", fileName
        return
    end if
    % Maybe sort here
    % Output heading of file here
    put : fileNo, "<grax>"
    for n : 1 .. DBSize

        const src := DBSrc (n)
        const rel := DBRel (n)
        const trg := DBTrg (n)

        const newSection := whatSection (section, src, rel, trg)
        const isFact := newSection = FACTTUPLE or
                newSection = FACTATTR
        % Output fact
        GraxOutput.outputGraxTuple (isFact, fileNo, src, rel, trg)

        if progress & n mod 1000 = 0 then
            var Rel := numName (DBRel (n))
            var Src := numName (DBSrc (n))
            var Trg := numName (DBTrg (n))
            put n : 5, " ", Rel : 8, " ", Src : 14, " ", Trg
        end if
    end for
    % Output trailer of file hear
    put : fileNo, "</grax>"
    closeFile (fileNo, fileName)
    put "Wrote ", DBSize, " Grax facts into file ", fileName

end putgrax

% Separator/header is '#TA#' --- added to double check file formats
var hash := '#'
var T : char := 'T'
var A : char := 'A'

var compressedCode := 373737

proc writedb (DBFileName : string)
    var fileNo : int
    var fileName := DBFileName

    put "Outputting (compressed): ", fileName
    openForWrite (fileNo, fileName)
    if fileNo <= 0 then
        return
    end if

    write : fileNo, hash, T, A, hash % Header
    write : fileNo, compressedCode, DBSize, nameCount

    const columnSize := DBSize * 4

    % Write tuples, a column at a time, as binary 4 byte numbers
    write : fileNo, DBRel : columnSize
    write : fileNo, DBSrc : columnSize
    write : fileNo, DBTrg : columnSize

    write : fileNo, hash, T, A, hash

    writeNames (fileNo)

    write : fileNo, hash, T, A, hash

    closeFile (fileNo, fileName)
    put "Wrote ", DBSize, " tuples and ", nameCount,
        " names into file ", fileName
end writedb

proc readdb (DBFileName : string)
    clearSetVars
    clearSTRINGVars
    cleardb
    resetNameTab

    var fileNo : int
    var fileName := DBFileName

    put "Inputting (compressed): ", fileName
    openForRead (fileNo, fileName)
    if fileNo <= 0 then
        return
    end if

    var hash1, T, A, hash2 : char

    read : fileNo, hash1, T, A, hash2
    if hash1 ~= hash or T ~= 'T' or A ~= 'A' or hash2 ~= hash then
        put "***In readdb, wrong header (1): '", hash1, T, A, hash2, "'"
        return
    end if
    var code, newDBSize, noOfNames : int
    read : fileNo, code, newDBSize, noOfNames
    if not (newDBSize >= 0 and noOfNames >= 0) then
        put "***In readdb, bad sizes"
        return
    end if
    if code not= compressedCode then
        put "***In readdb, wrong code: ", code
        return
    end if
    loop
        exit when newDBSize <= DBMax
        increaseDB
    end loop

    DBSize := newDBSize

    const columnSize := DBSize * 4

    var clock1, clock2 : int
    if timing then
        clock (clock1)
    end if

    % Read tuples, a column at a time, as binary 4 byte numbers
    read : fileNo, DBRel : columnSize
    read : fileNo, DBSrc : columnSize
    read : fileNo, DBTrg : columnSize

    if timing then
        clock (clock2)
        put " Time to read tuples: ", (clock2 - clock1) / 1000 : 0 : 1
    end if

    read : fileNo, hash1, T, A, hash2
    if hash1 ~= hash or T ~= 'T' or A ~= 'A' or hash2 ~= hash then
        put "***In readdb, wrong header (2): '", hash1, T, A, hash2, "'"
        return
    end if

    readNames (fileNo, noOfNames)
    findRelationNames (0)

    read : fileNo, hash1, T, A, hash2
    if hash1 ~= hash or T ~= 'T' or A ~= 'A' or hash2 ~= hash then
        put "***In readdb, wrong header (3): '", hash1, T, A, hash2, "'"
        return
    end if

    closeFile (fileNo, fileName)
    put "Read ", DBSize, " tuples and ", nameCount,
        " names from file ", fileName
end readdb

fcn isTempId (id : string) : boolean
    result length (id) >= lenTempPrefix & id (1 .. lenTempPrefix) =
        tempPrefix
end isTempId

proc putRel (rel : string)
    const relNumber := nameNum (rel)
    if isTempId (rel) then
        put "Relation:"
    else
        put "Relation ", rel, ":"
    end if
    var n := 0
    for i : 1 .. DBSize
        if relNumber = DBRel (i) then
            n += 1
            put INDENT, numName (DBSrc (i)) : 14, " ", numName (DBTrg (i))
        end if
    end for
    if n = 0 then
        put INDENT, "Relation is empty"
    else
        put INDENT, INDENT, n, " tuples"
    end if
end putRel

proc putRelSample (rel : string, startPos, sampleSize : int)
    const relNumber := nameNum (rel)
    put "Relation ", rel, ":"
    var n := 0
    var sampled := 0
    for i : 1 .. DBSize
        if relNumber = DBRel (i) then
            n += 1
            if n >= startPos & sampled < sampleSize then
                sampled += 1
                put INDENT, numName (DBSrc (i)) : 14, " ",
                    numName (DBTrg (i))
            end if
        end if
    end for
    if n = 0 then
        put INDENT, "Relation is empty"
    else
        put INDENT, INDENT, "Sampled ", sampled, " of ", n, " tuples"
    end if
end putRelSample

fcn srcDotRel (src : setPtr, rel : string) : setPtr
    const relNumber := nameNum (rel)
    var trg := newEmptySet
    if isRel (rel) then
        if optimize then
            const inSet := 0
            const notSeen := 1
            for i : 1 .. nameCount
                Index1 (i) := notSeen
                Index2 (i) := notSeen
            end for

            for i : 1 .. listNodes (src).size
                Index1 (listNodes (src).list (i)) := inSet
            end for
            % Assert Index1 (i) = inSet iff i is in the src set

            for i : 1 .. DBSize
                if DBRel (i) = relNumber & Index1 (DBSrc (i)) = inSet then
                    Index2 (DBTrg (i)) := inSet
                end if
            end for

            var trgSize := 0
            for i : 1 .. nameCount
                if Index2 (i) = inSet then
                    if trgSize >= shortListMax then
                        put "***Error: Grok set overflow. ",
                            "Increase 'shortListMax' beyond ", shortListMax
                        exit
                    end if
                    trgSize += 1
                    listNodes (trg).list (trgSize) := i
                end if
            end for
            listNodes (trg).size := trgSize

        else
            for i : 1 .. listNodes (src).size
                const srcItem := listNodes (src).list (i)
                for j : 1 .. DBSize
                    if DBRel (j) = relNumber and DBSrc (j) = srcItem then
                        setInsert (DBTrg (j), trg)
                    end if
                end for
            end for
        end if
    else
        put "***Sorry, '", rel, "' is not a relation"
    end if
    result trg
end srcDotRel

% Find sources of this object for the relation
fcn relDotTrg (rel : string, trg : setPtr) : setPtr
    const relNumber := nameNum (rel)
    var src := newEmptySet
    if isRel (rel) then
        if optimize then
            const inSet := 0
            const notSeen := 1
            for i : 1 .. nameCount
                Index1 (i) := notSeen
                Index2 (i) := notSeen
            end for

            for i : 1 .. listNodes (trg).size
                Index1 (listNodes (trg).list (i)) := inSet
            end for
            % Assert Index1 (i) = inSet iff i is in the trg set

            for i : 1 .. DBSize
                if DBRel (i) = relNumber & Index1 (DBTrg (i)) = inSet then
                    Index2 (DBSrc (i)) := inSet
                end if
            end for

            var srcSize := 0
            for i : 1 .. nameCount
                if Index2 (i) = inSet then
                    if srcSize >= shortListMax then
                        put "***Error: Grok set overflow. ",
                            "Increase 'shortListMax' beyond ", shortListMax
                        exit
                    end if
                    srcSize += 1
                    listNodes (src).list (srcSize) := i
                end if
            end for
            listNodes (src).size := srcSize

        else
            for i : 1 .. listNodes (trg).size
                const trgItem := listNodes (trg).list (i)
                for j : 1 .. DBSize
                    if DBRel (j) = relNumber and DBTrg (j) = trgItem then
                        setInsert (DBSrc (j), src)
                    end if
                end for
            end for
        end if
    else
        put "***Sorry, '", rel, "' is not a relation"
    end if
    result src
end relDotTrg

proc putSrcDotRel (src : setPtr, rel : str)
    const relNumber := numName (rel)
    var trg := srcDotRel (src, relNumber)
    putLabelledListNoCommas (stdOutput, listNodes (trg).list, listNodes (trg).size,
        "There are " + intstr (listNodes (trg).size) + " items:", false)
    freeSet (trg)
end putSrcDotRel

% List targets of this object for the relation
proc putSrcItemDotRel (srcItem, rel : str)
    var src := newSingletonSet (srcItem)
    putSrcDotRel (src, rel)
    freeSet (src)
end putSrcItemDotRel

proc putRelDotTrg (rel : string, trg : setPtr)
    var src := relDotTrg (rel, trg)
    putLabelledListNoCommas (stdOutput, listNodes (src).list, listNodes (src).size,
        "There are " + intstr (listNodes (src).size) + " items:", false)
    free listNodes, src
end putRelDotTrg

% List sources of this object for the relation
proc putRelDotTrgItem (rel, trgName : string)
    const trgNumber := nameNum (trgName)
    var trg := newSingletonSet (trgNumber)
    putRelDotTrg (rel, trg)
    freeSet (trg)
end putRelDotTrgItem

% Find set union, returning left as result
fcn setAdd (left, right : setPtr) : setPtr
    const seen := 0
    const notSeen := 1
    for i : 1 .. nameCount
        Index1 (i) := notSeen
    end for

    for i : 1 .. listNodes (left).size
        Index1 (listNodes (left).list (i)) := seen
    end for
    % Assert Index1 (i) = seen iff i is member of left set

    for i : 1 .. listNodes (right).size
        Index1 (listNodes (right).list (i)) := seen
    end for
    % Assert Index1 (i) = seen iff i is member of set (left + right)

    var leftSize := 0
    for i : 1 .. nameCount
        if Index1 (i) = seen then
            if leftSize >= shortListMax then
                put "***Error: Grok name table overflow. ",
                    "Increase shortListMax beyond ", shortListMax
                exit
            end if
            leftSize += 1
            listNodes (left).list (leftSize) := i
        end if
    end for
    listNodes (left).size := leftSize

    result left
end setAdd

% Intersect two sets, returning left as result
fcn setIntersect (left, right : setPtr) : setPtr
    const seen := 0
    const notSeen := 1
    for i : 1 .. nameCount
        Index1 (i) := notSeen
        Index2 (i) := notSeen
    end for

    for i : 1 .. listNodes (left).size
        Index1 (listNodes (left).list (i)) := seen
    end for
    % Assert Index1 (i) = seen iff i is member of left set

    for i : 1 .. listNodes (right).size
        Index2 (listNodes (right).list (i)) := seen
    end for
    % Assert Index2 (i) = seen iff i is member of right set

    var leftSize := 0
    for i : 1 .. nameCount
        if Index1 (i) = seen and Index2 (i) = seen then
            leftSize += 1
            listNodes (left).list (leftSize) := i
        end if
    end for
    listNodes (left).size := leftSize

    result left
end setIntersect

% Take right away from left, returning left as result
fcn setSubtract (left, right : setPtr) : setPtr
    const seen := 0
    const notSeen := 1
    const subtracted := 2
    for i : 1 .. nameCount
        Index1 (i) := notSeen
    end for

    for i : 1 .. listNodes (left).size
        Index1 (listNodes (left).list (i)) := seen
    end for
    % Assert Index1 (i) = seen iff i is member of left set

    for i : 1 .. listNodes (right).size
        Index1 (listNodes (right).list (i)) := subtracted
    end for
    % Assert Index1 (i) = seen iff i is member of set (left - right)

    var leftSize := 0
    for i : 1 .. nameCount
        if Index1 (i) = seen then
            leftSize += 1
            listNodes (left).list (leftSize) := i
        end if
    end for
    listNodes (left).size := leftSize

    result left
end setSubtract

% Comparison of sets
fcn setComparison (COMPAREOp : string (2), left, right : setPtr) : boolean
    const inLeft := 0
    const notSeen := 1

    var excessOnLeft : boolean
    var excessOnRight := false

    var leftCount := 0
    var commonCount := 0

    for i : 1 .. nameCount
        Index1 (i) := notSeen
    end for

    for i : 1 .. listNodes (left).size
        Index1 (listNodes (left).list (i)) := inLeft
        leftCount += 1
    end for
    % Assert Index1 (i) = inLeft iff i is member of left set
    %    and leftCount gives cardinality of "left"

    for i : 1 .. listNodes (right).size
        if Index1 (listNodes (right).list (i)) = notSeen then
            excessOnRight := true
        else
            assert Index1 (listNodes (right).list (i)) = inLeft
            commonCount += 1
        end if
    end for
    % Assert commonCount is cardinality of (left ^ right) and
    %   excessRight tells if elements exist in right but not in left

    excessOnLeft := leftCount > commonCount

    if COMPAREOp = "==" or COMPAREOp = "=" then
        result (not excessOnLeft) & (not excessOnRight)
    elsif COMPAREOp = "~=" then
        result excessOnLeft or excessOnRight
    elsif COMPAREOp = "<" then
        result (not excessOnLeft) & excessOnRight
    elsif COMPAREOp = ">" then
        result (not excessOnRight) & excessOnLeft
    elsif COMPAREOp = "<=" then
        result not excessOnLeft
    elsif COMPAREOp = ">=" then
        result not excessOnRight
    else
        put "***Error in Grok: Bad COMPARE op: ", COMPAREOp
        result false
    end if

end setComparison

fcn equalSets (left, right : setPtr) : boolean
    var a, b, l, r, ll, rr : setPtr
    var eq : boolean

    l := sameSet (left)
    r := sameSet (right)
    a := setSubtract (l, r)

    ll := sameSet (left)
    rr := sameSet (right)
    b := setSubtract (rr, ll)

    if listNodes (a).size = 0 and listNodes (b).size = 0 then
        eq := true
    else
        eq := false
    end if
    freeSet (l)
    freeSet (r)
    freeSet (ll)
    freeSet (rr)
    result eq
end equalSets

% Find subset having a given prefix
fcn subsetWithPrefix (prefix : string, thisSet : setPtr) : setPtr
    var prefixSet := newEmptySet
    for i : 1 .. listNodes (thisSet).size
        const setNumber := listNodes (thisSet).list (i)
        const setName := numName (setNumber)
        if isPrefix (prefix, setName) then
            setInsert (setNumber, prefixSet)
        end if
    end for
    result prefixSet
end subsetWithPrefix

% Find subset having a given suffix
fcn subsetWithSuffix (suffix : string, thisSet : setPtr) : setPtr
    var suffixSet := newEmptySet
    for i : 1 .. listNodes (thisSet).size
        const setNumber := listNodes (thisSet).list (i)
        const setName := numName (setNumber)
        if isSuffix (suffix, setName) then
            setInsert (setNumber, suffixSet)
        end if
    end for
    result suffixSet
end subsetWithSuffix

proc deleteRelationTuples (relNumber : str, startScan : int)
    var noDeleted := 0
    for i : startScan .. DBSize
        if DBRel (i) = relNumber then
            noDeleted += 1
        else
            if noDeleted > 0 then
                DBRel (i - noDeleted) := DBRel (i)
                DBSrc (i - noDeleted) := DBSrc (i)
                DBTrg (i - noDeleted) := DBTrg (i)
            end if
        end if
    end for
    DBSize -= noDeleted
end deleteRelationTuples

proc deleteRelation (rel : string)
    const relNumber := nameNum (rel)
    if isRel (rel) then
        deleteFromList (relNames, relNamesSize, relNumber)
        deleteRelationTuples (relNumber, 1)
    end if
end deleteRelation

include "relcomp.i"
include "relmerge.i"
include "closur2.i"

% Remove from data base all tuples that reference elements of set s
proc removeSetFromDB (s : setPtr)
    var noDeleted := 0
    for i : 1 .. DBSize
        if progress & i mod 1000 = 0 then
            put "    ", i : 4, " ", DBSrc (i), " ", DBRel (i), " ", DBTrg (i)
        end if
        if isMember (DBSrc (i), s) or isMember (DBTrg (i), s) then
            noDeleted += 1
        else
            if noDeleted > 0 then
                DBRel (i - noDeleted) := DBRel (i)
                DBSrc (i - noDeleted) := DBSrc (i)
                DBTrg (i - noDeleted) := DBTrg (i)
            end if
        end if
    end for
    DBSize -= noDeleted
    if verbal then
        put "Deleted ", noDeleted, " tuples. DBSize is ", DBSize
    end if
end removeSetFromDB

% Remove from data base all tuples that have src in s1, trg in s2
proc removeSetsFromDB (s1, s2 : setPtr, rel : string)
    const relNumber := nameNum (rel)
    var noDeleted := 0
    for i : 1 .. DBSize
        if progress & i mod 1000 = 0 then
            put "    ", i : 4, " ", DBSrc (i), " ", DBRel (i), " ",
                DBTrg (i)
        end if
        if isMember (DBSrc (i), s1) and isMember (DBTrg (i), s2) and
                DBRel (i) not= relNumber then
            noDeleted += 1
        else
            if noDeleted > 0 then
                DBRel (i - noDeleted) := DBRel (i)
                DBSrc (i - noDeleted) := DBSrc (i)
                DBTrg (i - noDeleted) := DBTrg (i)
            end if
        end if
    end for
    DBSize -= noDeleted
    if verbal then
        put "Deleted ", noDeleted, " tuples. DBSize is ", DBSize
    end if
end removeSetsFromDB

% Remove from data base all tuples that reference elements of outside of set s
proc sliceSetFromDB (s : setPtr)
    var noDeleted := 0
    for i : 1 .. DBSize
        if progress & i mod 1000 = 0 then
            put "    ", i : 4, " ", DBSrc (i), " ", DBRel (i), " ", DBTrg (i)
        end if
        if not isMember (DBSrc (i), s) or not isMember (DBTrg (i), s) then
            noDeleted += 1
        else
            if noDeleted > 0 then
                DBRel (i - noDeleted) := DBRel (i)
                DBSrc (i - noDeleted) := DBSrc (i)
                DBTrg (i - noDeleted) := DBTrg (i)
            end if
        end if
    end for
    DBSize -= noDeleted
    if verbal then
        put "Deleted ", noDeleted, " tuples. DBSize is ", DBSize
    end if
end sliceSetFromDB

% Remove from data base all tuples that do not reference s at all
proc wsliceSetFromDB (s : setPtr)
    var noDeleted := 0
    for i : 1 .. DBSize
        if progress & i mod 1000 = 0 then
            put "    ", i : 4, " ", DBSrc (i), " ", DBRel (i), " ", DBTrg (i)
        end if
        if not isMember (DBSrc (i), s) and not isMember (DBTrg (i), s) then
            noDeleted += 1
        else
            if noDeleted > 0 then
                DBRel (i - noDeleted) := DBRel (i)
                DBSrc (i - noDeleted) := DBSrc (i)
                DBTrg (i - noDeleted) := DBTrg (i)
            end if
        end if
    end for
    DBSize -= noDeleted
    if verbal then
        put "Deleted ", noDeleted, " tuples. DBSize is ", DBSize
    end if
end wsliceSetFromDB


% For a giving covering set s, compute the set t, such that by
% following the given relation, starting in s, you can only get
% to items in t (and such items in t cannot be gotten from outside of s.
% This is a bit kludgy in that if a potential item in t is in a cycle,
% this algorithm will not add it to t.
fcn coveredSet (covering : setPtr, relName : string) : setPtr
    putLabelledList (stdOutput, listNodes (covering).list, listNodes (covering).size,
        "Make covered set for", true)
    var res := newSet
    listNodes (res) := listNodes (covering)

    % The covered set is to consist of all items that can be accessed from
    % the root (reflexively) transitively, not counting items
    % than can be accessed from outside of the covering and covered files.

    % Loop, adding to list, as long as list grows
    %var c := 0 % Scans items for further expansion
    loop
        %exit when c >= listNodes (covered).size % Expand by all covered items
        %c += 1
        %const thisCoveredItem := listNodes (covered).list (c)
        % put thisCoveredItem, " ", c, " expansion"

        % Find all items related to this covered item
        % To do so, find all items that this item relates to.
        %var thisCovered := newSingletonSet (thisCoveredItem)
        %var relatedSet := srcDotRel (thisCovered, relName)
        %freeSet (thisCovered)


        var covered := srcDotRel (res, relName)
        covered := setSubtract (covered, res)
        loop
            var both := sameSet (covered)
            both := setAdd (both, res)
            var falseOnes := newEmptySet
            for i : 1 .. listNodes (covered).size
                var thisObj := newSingletonSet (listNodes (covered).list (i))
                var fathers := relDotTrg (relName, thisObj)
                if not subset (fathers, both) then
                    falseOnes := setAdd (falseOnes, thisObj)
                end if
            end for
            exit when listNodes (falseOnes).size = 0
            covered := setSubtract (covered, falseOnes)
        end loop
        exit when listNodes (covered).size = 0
        res := setAdd (res, covered)
    end loop
    result res
end coveredSet

% Implement R0 := R1 - ID (ID is the identity relation)
proc assignRelSubtractID (R0, R1 : string)
    const R0Number := nameNum (R0)
    const R1Number := nameNum (R1)
    var addCount := 0
    for i : 1 .. DBSize
        if DBRel (i) = R1Number and DBSrc (i) not= DBTrg (i) then
            addCount += 1
            insertDB (DBSrc (i), R0Number, DBTrg (i))
            if progress & addCount mod 1000 = 0 then
                put INDENT, addCount : 4, " ", numName (DBTrg (i)), " ",
                    numName (DBSrc (i))
            end if
        end if
    end for
    if verbal then
        put "Added ", addCount, " tuples for ", R0, ". DBSize is ", DBSize
    end if
    insertIntoListUnique (relNames, relNamesSize, R0Number)
end assignRelSubtractID

% Add cross product (Kludge: may result in duplicates).
proc addCrossProduct (left : setPtr, relName : str, right : setPtr)
    for i : 1 .. listNodes (left).size
        for j : 1 .. listNodes (right).size
            insertDB (listNodes (left).list (i), relName, listNodes (right).list (j))
        end for
    end for
    if verbal then
        put "Added ", listNodes (left).size * listNodes (right).size,
            " tuples.  DBSize is ", DBSize
    end if
    insertIntoListUnique (relNames, relNamesSize, relName)
end addCrossProduct







% Make a phase file for all files called from a set of driver files

% Make a phase file for all files called from a given driver



% Emit a subsystem and its descendents

proc relTree (rel : string)
    const relNumber := nameNum (rel)

    if isRel (rel) then


        var noDeleted : int
        for i : 1 .. DBSize
            if DBRel (i) = relNumber then
                noDeleted := 0
                for j : i + 1 .. DBSize
                    if DBRel (j) = relNumber and DBTrg (j) = DBTrg (i) then
                        noDeleted += 1
                        put "Deleting ", rel, " ", DBSrc (j), " ", DBTrg (j)
                    else
                        if noDeleted > 0 then
                            DBRel (j - noDeleted) := DBRel (j)
                            DBSrc (j - noDeleted) := DBSrc (j)
                            DBTrg (j - noDeleted) := DBTrg (j)
                        end if
                    end if
                end for
                DBSize -= noDeleted
            end if
        end for


    else
        put "Sorry, ", rel, " is not a relation"
    end if
end relTree

proc domination (rel : string)
    const relNumber := nameNum (rel)
    var starterSet := newEmptySet
    var newChildrenSet : setPtr
    const artificialNodeN0 := nameNum ("artificial_node_n0")
    var allNodeSet := newSingletonSet (artificialNodeN0)
    if isRel (rel) then
        put "point  1"
        loop
            newChildrenSet := newEmptySet

            for i : 1 .. DBSize
                if DBRel (i) = relNumber and
                        isMember (DBSrc (i), allNodeSet) and
                        not isMember (DBTrg (i), allNodeSet) then
                    setInsert (DBTrg (i), newChildrenSet)
                end if
            end for
            if listNodes (newChildrenSet).size = 0 then
                freeSet (newChildrenSet)
                exit
            end if
            listAdd (listNodes (allNodeSet).list, listNodes (allNodeSet).size, 
				listNodes (newChildrenSet).list, listNodes (newChildrenSet).size)
            freeSet (newChildrenSet)
        end loop
        for i : 1 .. listNodes (allNodeSet).size
            setInsert (i, starterSet)
        end for
        put "point 2"
        var bigArray : domSetPtr
        new domNodeSets, bigArray
        if bigArray = nil (domNodeSets) then
            put "***Sorry, out of memory [in domination]"
            quit
        end if
        domNodeSets (bigArray).size := listNodes (starterSet).size
        for i : 1 .. domNodeSets (bigArray).size
            domNodeSets (bigArray).list (i).domSet := sameSet (starterSet)
            domNodeSets (bigArray).list (i).predSet := newEmptySet
            if (i mod 100) = 0 then
                put i
            end if
        end for
        put "point 3"
        for i : 1 .. listNodes (starterSet).size
            for j : 1 .. DBSize
                if DBRel (j) = relNumber and DBTrg (j) = listNodes (allNodeSet).list (i) then
                    for k : 1 .. listNodes (allNodeSet).size
                        if listNodes (allNodeSet).list (k) = DBSrc (j) then
                            setInsert (k, domNodeSets (bigArray).list (i).predSet)
                        end if
                    end for
                end if
            end for
        end for
        put "point 4"
        for i : 1 .. domNodeSets (bigArray).size
            if listNodes (allNodeSet).list (i) = artificialNodeN0 then
                freeSet (domNodeSets (bigArray).list (i).domSet)
                domNodeSets (bigArray).list (i).domSet :=
                    newSingletonSet (i)
                exit
            end if
        end for

        %   INITIALIZATION COMPLETE
        put "point 5"
        var CHANGE := true
        var newDomSet, tempSet : setPtr
        loop
            CHANGE := false
            for i : 1 .. domNodeSets (bigArray).size
                if listNodes (allNodeSet).list (i) not= artificialNodeN0 then
                    put "Dealing with node ", listNodes (allNodeSet).list (i),
                        " -> ", i
                    newDomSet := sameSet (domNodeSets (bigArray).list (
                        listNodes (domNodeSets (bigArray).list (i).predSet).list (1)).
                        domSet)
                    for j : 2 .. listNodes (domNodeSets (bigArray).list (i).predSet).size
                        newDomSet := setIntersect (newDomSet, domNodeSets (bigArray).list (
                            listNodes (domNodeSets (bigArray).list (i).predSet).list (j))
                            .domSet)
                    end for
                    setInsert (i, newDomSet)
                    if not equalSets (newDomSet, domNodeSets (bigArray).list (i).domSet)
                            then
                        CHANGE := true
                    end if
                    freeSet (domNodeSets (bigArray).list (i).domSet)
                    domNodeSets (bigArray).list (i).domSet := sameSet (newDomSet)
                    freeSet (newDomSet)
                end if
            end for
            exit when not CHANGE
        end loop

        const dominateNumber := nameNum ("dominate")
        for i : 1 .. domNodeSets (bigArray).size
            for j : 1 .. listNodes (domNodeSets (bigArray).list (i).domSet).size
                insertDB (listNodes (allNodeSet).list (
                    listNodes (domNodeSets (bigArray).list (i).domSet).list (j)),
                    dominateNumber, listNodes (allNodeSet).list (i))
            end for
        end for
        insertIntoListUnique (relNames, relNamesSize, dominateNumber)
    else
        put "Sorry, ", rel, " is not a relation"
    end if
    freeSet (allNodeSet)
    freeSet (starterSet)
end domination

proc fixfix % I'm sure curious about what this is supposed to do!
    var a, noDeleted : int
    var change : boolean
    noDeleted := 0

    const callNumber := nameNum ("call")
    const fetchNumber := nameNum ("fetch")
    const storeNumber := nameNum ("store")
    const filedclNumber := nameNum ("filedcl")
    const filedefNumber := nameNum ("filedef")

    for i : 1 .. DBSize
        change := false % Strange --- this is always false from now on!
        if DBRel (i) = callNumber or
                DBRel (i) = fetchNumber or
                DBRel (i) = storeNumber
                then
            begin
                var DBSrcIName := numName (DBSrc (i))
                if DBSrcIName (1 .. 2) = "*\^" then
                    DBSrcIName := DBSrcIName (3 .. *)
                end if

                if DBSrcIName (1) = "\^" then
                    DBSrcIName := DBSrcIName (2 .. *)
                end if

                a := index (DBSrcIName, "\^")
                if a > 1 then
                    DBSrcIName := DBSrcIName (1 .. a - 1)
                end if
                DBSrc (i) := nameNum (DBSrcIName)
            end
            begin
                var DBTrgIName := numName (DBTrg (i))
                if DBTrgIName (1 .. 2) = "*\^" then
                    DBTrgIName := DBTrgIName (3 .. *)
                end if

                if DBTrgIName (1) = "\^" then
                    DBTrgIName := DBTrgIName (2 .. *)
                end if

                a := index (DBTrgIName, "\^")
                if a > 0 then
                    noDeleted += 1
                    change := true
                end if
                DBTrg (i) := nameNum (DBTrgIName)
            end
        elsif DBRel (i) = filedclNumber or
                DBRel (i) = filedefNumber then
            begin
                var DBTrgIName := numName (DBTrg (i))
                if DBTrgIName (1 .. 2) = "*\^" then
                    DBTrgIName := DBTrgIName (3 .. *)
                end if
                if DBTrgIName (1) = "\^" then
                    DBTrgIName := DBTrgIName (2 .. *)
                end if
                a := index (DBTrgIName, "\^")
                if a > 0 then
                    noDeleted += 1
                    change := true
                end if
                DBTrg (i) := nameNum (DBTrgIName)
            end
        end if

        if not change then
            DBRel (i - noDeleted) := DBRel (i)
            DBSrc (i - noDeleted) := DBSrc (i)
            DBTrg (i - noDeleted) := DBTrg (i)
        end if
    end for
    DBSize -= noDeleted
end fixfix

proc emitta (ssSet : setPtr, ffSet : setPtr, dirName : string)
    var fileNo1, fileNo2 : int
    var fileToOpen1, fileToOpen2 : string
    if dirName = "" then
        fileToOpen1 := "tmp1"
        fileToOpen2 := "tmp2"
    else
        fileToOpen1 := dirName + "/tmp1"
        fileToOpen2 := dirName + "/tmp2"
    end if
    openForPut (fileNo1, fileToOpen1)
    if fileNo1 <= 0 then
        return
    end if
    %put : fileNo1, "SCHEME TUPLE :"
    %put : fileNo1, ""
    %put : fileNo1, "useproc module module"
    %put : fileNo1, "useproc module subsystem"
    %put : fileNo1, "useproc subsystem module"
    %put : fileNo1, "useproc subsystem subsystem"
    %put : fileNo1, "usevar  module module"
    %put : fileNo1, "usevar  module subsystem"
    %put : fileNo1, "usevar  subsystem module"
    %put : fileNo1, "usevar  subsystem subsystem"
    %put : fileNo1, "contain subsystem module"
    %put : fileNo1, "contain subsystem subsystem"
    %put : fileNo1, ""
    %put : fileNo1, "FACT TUPLE :"
    %put : fileNo1, ""

    for i : 1 .. listNodes (ssSet).size
        put : fileNo1, "$INSTANCE ", listNodes (ssSet).list (i), " subsystem"
    end for

    for i : 1 .. listNodes (ffSet).size
        put : fileNo1, "$INSTANCE ", listNodes (ffSet).list (i), " module"
    end for

    put : fileNo1, ""
    closeFile (fileNo1, fileToOpen1)

    openForPut (fileNo2, fileToOpen2)

    if fileNo2 <= 0 then
        return
    end if
    for i : 1 .. DBSize
        %    if DBRel (i) = thisRel then
        put : fileNo2, DBRel (i), " ", DBSrc (i), " ", DBTrg (i)
        %    end if
    end for

    closeFile (fileNo2, fileToOpen2)

end emitta





% Emit a subsystem that imports its clients


function interface_count (s : setPtr) : int
    var intf := newEmptySet
    const useprocNumber := nameNum ("useproc")
    const usevarNumber := nameNum ("usevar")
    for i : 1 .. DBSize
        if DBRel (i) = useprocNumber or
                DBRel (i) = usevarNumber then
            if (not isMember (DBSrc (i), s)) and isMember (DBTrg (i), s) then
                setInsert (DBTrg (i), intf)
            end if
        end if
    end for
    var a := listNodes (intf).size
    freeSet (intf)
    result a
end interface_count

function interface_grows (ss, file : string) : boolean
    var src := evalSetAsTemp (ss)
    var contents := srcDotRel (src, "contain")
    %put "Subsystem : ", ss
    %put "File : ", file
    freeSet (src)
    var filePtr := evalSetAsTemp (file)
    var reduced_contents := sameSet (contents)
    reduced_contents := setSubtract (reduced_contents, filePtr)
    %put "Contents.size = ", listNodes (contents).size
    %put "Reduced contents.size = ", listNodes (reduced_contents).size
    freeSet (filePtr)
    var a := interface_count (contents)
    %put "Previous interface : ", a
    var b := interface_count (reduced_contents)
    %put "New interface :", b
    %var dfg : string
    %get dfg
    freeSet (contents)
    freeSet (reduced_contents)
    result b >= a
end interface_grows

include "asgnop.i"

forward proc otherCommands (words : array 1 .. * of charstr,
    var noWords : int,
    var handledCommand : boolean)

proc evalCommand (words : array 1 .. * of charstr, var noWords : int,
        var handledCommand : boolean)
    assignOpCommands (words, noWords, handledCommand)
    if not handledCommand then
        put "***Bad command: " ..
        for i : 1 .. noWords
            put " ", words (i) ..
        end for
        put ""
    end if
end evalCommand

const tracingCommands := false

procedure traceEvalCommandNTokens (N : int, words : array 1 .. * of charstr,
        handledCommand : boolean)
    if tracingCommands then
        put "###TRACE evalCommand", N, "Tokens: " ..
        for i : 1 .. N
            put words (i), " " ..
        end for
        put "[" ..
		if handledCommand then
			put "true" ..
		else
			put "false" ..
		end if
		put "]" ..
        put ""
    end if
end traceEvalCommandNTokens

% This procedure is never called:

% This procedure is never called:

procedure evalCommand3Tokens (X1, X2, X3 : charstr,
        var handledCommand : boolean)
    var noWords := 3
    var words : array 1 .. maxTokens of charstr
    words (1) := X1
    words (2) := X2
    words (3) := X3
    traceEvalCommandNTokens (3, words, handledCommand)
    evalCommand (words, noWords, handledCommand)
end evalCommand3Tokens

procedure evalCommand4Tokens (X1, X2, X3, X4 : charstr,
        var handledCommand : boolean)
    var noWords := 4
    var words : array 1 .. maxTokens of charstr
    words (1) := X1
    words (2) := X2
    words (3) := X3
    words (4) := X4
    traceEvalCommandNTokens (4, words, handledCommand)
    evalCommand (words, noWords, handledCommand)
end evalCommand4Tokens

procedure evalCommand5Tokens (X1, X2, X3, X4, X5 : charstr,
        var handledCommand : boolean)
    var noWords := 5
    var words : array 1 .. maxTokens of charstr
    words (1) := X1
    words (2) := X2
    words (3) := X3
    words (4) := X4
    words (5) := X5
    traceEvalCommandNTokens (5, words, handledCommand)
    evalCommand (words, noWords, handledCommand)
end evalCommand5Tokens

procedure evalCommandNTokens (noTokens : int,
        tokens : array 1 .. * of charstr,
        var handledCommand : boolean)
    var noWords := noTokens
    traceEvalCommandNTokens (noTokens, tokens, handledCommand)
    evalCommand (tokens, noWords, handledCommand)
end evalCommandNTokens

procedure delRelOrSet (id : string)
    if isRel (id) then
        deleteRelation (id)
    else
        % Do not delete set (delset is wrong command)
        % Just leave temp sets around
    end if
end delRelOrSet

var tempCount := 0

procedure genTempId (var temp : string)
    tempCount += 1
    temp := tempPrefix + intstr (tempCount)
end genTempId

procedure freeIfTemp (id : string)
    if isTempId (id) then
        delRelOrSet (id)
    end if
end freeIfTemp

procedure evalInfix (var trg : string, left, right : string,
        operator : string, var handledCommand : boolean)
    genTempId (trg)
    evalCommand5Tokens (trg, "=", left, operator, right, handledCommand)
    freeIfTemp (left)
    freeIfTemp (right)
end evalInfix

proc evalPredefSymbol (var trg : string, predefName : string,
        var handledCommand : boolean)
    genTempId (trg)
    evalCommand3Tokens (trg, "=", predefName, handledCommand)
end evalPredefSymbol

proc evalTokenSymbol (var trg : string, predefName : string,
        var handledCommand : boolean)
    % Case of string or identifier that is not predefined
    genTempId (trg)
    put "##evalToken: " + trg + " = " + predefName
    evalCommand3Tokens (trg, "=", predefName, handledCommand)
end evalTokenSymbol

procedure evalPrefix (var trg : string, src : string, operator : string,
        var handledCommand : boolean)
    genTempId (trg)
    evalCommand4Tokens (trg, "=", operator, src, handledCommand)
    freeIfTemp (src)
end evalPrefix

procedure evalDoublePrefix (var trg : string, XFixOperator : string,
        operand1, operand2 : string, var handledCommand : boolean)
    genTempId (trg)
    evalCommand5Tokens (trg, "=", XFixOperator,
        operand1, operand2, handledCommand)
    freeIfTemp (operand1)
    freeIfTemp (operand2)
end evalDoublePrefix

procedure evalSetConstructor (var trg : string, noOperands : int,
        operand : array 1 .. * of string, var handledCommand : boolean)
    genTempId (trg)
    const noWords := 2 * noOperands + 3
    var words : array 1 .. maxTokens of charstr
    words (1) := trg
    words (2) := "="
    words (3) := "{"
    for i : 1 .. noOperands
        words (2 * i + 2) := operand (i)
        if i < noOperands then
            words (2 * i + 3) := ","
        else
            words (2 * i + 3) := "}"
        end if
    end for

    evalCommandNTokens (noWords, words, handledCommand)
    for i : 1 .. noOperands
        freeIfTemp (operand (i))
    end for
end evalSetConstructor

% The only postfix operator is transitive closure
procedure evalPostfix (var trg : string, src : string, operator : string,
        var handledCommand : boolean)
    genTempId (trg)
    evalCommand4Tokens (trg, "=", src, operator, handledCommand)
    freeIfTemp (src)
end evalPostfix

const EOF := "### EOF ###"

type lineType :
    record
        noWordsToParse : int
        wordsToParse : array 1 .. maxTokens of charstr
        nextWordToParse : int
        token, nextToken : charstr
    end record


procedure getToken (var LINE : lineType)
    LINE.token := LINE.nextToken
    if LINE.nextWordToParse > LINE.noWordsToParse then 
        LINE.nextToken := EOF
    else
        LINE.nextToken := LINE.wordsToParse (LINE.nextWordToParse)
        LINE.nextWordToParse += 1
    end if
    
end getToken

% Look-ahead function needed to parse transitive closure (+ and *)
function nextTokenIsIdent (var LINE : lineType) : boolean
    if LINE.nextToken = EOF then
        result false
    end if
    const operator := "+-*\^)"
    if length (LINE.nextToken) = 0 then
        % Highly unlikely case
        put "Warning: Token of length zero"
        result false
    end if
    const startChar := LINE.nextToken (1)
    if index (operator, startChar) ~= 0 then
        result false
    end if
    if LINE.nextToken = "X" then
        result false
    end if
    result true % All other cases are identifiers
end nextTokenIsIdent

proc dumpWords (words : array 1 .. * of charstr,
        var noWords : int)
    for i : 1 .. noWords
        put " ", words (i) ..
    end for
    put ""
end dumpWords


% Forward declarations for "recursive descent" parser
% for Grok expressions.

% A Grok expression has this form:

%    expn            := booleans

%    booleans        := comparisons { boolOp comparisons }
%    comparisons     := additions { comparOp additions }
%    additions       := multiplications { addOp multiplications }
%    multiplications := primary { multOp primary }
%    primary         := "(" booleans ")"
%                     | { prefixOp } primary { suffixOp }

%    boolOp          := "and" | "or"  | "&"
%    comparOp        := "=="  | "="   | "~="  | "<"   | ">" | <=" | ">="
%    addOp           := "+"   | "-"   | "cat"
%    multOp          := "*"   | "X"   | "^"   | "o"   | "."
%                             | "/"   | "mod" | "div"
%    prefixOp        := "inv" | "dom" | "rng" | "ent"
%                             | "-"   | "not" | "~"   | "#" | "pick" | "$" |
%                             | "head"| "tail"
%                             | "rel2tripleset" | "triple2relname"
%    suffixOp        := "+"   | "*"   (transitive closure)

% ??What's a 'primary'?

% The parser has the same structure as this grammar.

forward procedure expn (var eValue : string,
    var handledCommand : boolean, var LINE : lineType)

forward procedure ands (var bValue : string,
    var handledCommand : boolean, var LINE : lineType)

forward procedure ors (var bValue : string,
    var handledCommand : boolean, var LINE : lineType)

forward procedure comparisons (var cValue : string,
    var handledCommand : boolean, var LINE : lineType)

forward procedure additions (var aValue : string,
    var handledCommand : boolean, var LINE : lineType)

forward procedure multiplications (var mValue : string,
    var handledCommand : boolean, var LINE : lineType)

forward procedure primary (var pValue : string,
    var handledCommand : boolean, var LINE : lineType)

% Bodies for procedures for "recursive descent" parser

body procedure expn
    ands (eValue, handledCommand, LINE)
end expn

% Handle "ands": and
body procedure ands
    var nextValue : string
    ors (bValue, handledCommand, LINE)
    loop
        var boolValue : string
        const operator := LINE.token
        if operator = "and" or operator = "&" then
			getToken (LINE)
			ors (nextValue, handledCommand, LINE)
			evalInfix (boolValue, bValue, nextValue, operator,
				handledCommand)
			bValue := boolValue
		else
			exit
        end if
    end loop
end ands

% Handle "ors": or
body procedure ors
    var nextValue : string
    comparisons (bValue, handledCommand, LINE)
    loop
        var boolValue : string
        const operator := LINE.token
        if operator = "or" then
			getToken (LINE)
			comparisons (nextValue, handledCommand, LINE)
			evalInfix (boolValue, bValue, nextValue, operator,
				handledCommand)
			bValue := boolValue
		else
			exit
        end if
    end loop
end ors

% Handle "comparisons": == ~= < > <= >=
body procedure comparisons
    var nextValue : string
    additions (cValue, handledCommand, LINE)
    loop
        var compValue : string
        const operator := LINE.token
        if operator = "==" or operator = "~=" or operator = "=" 
				or operator = "<" or operator = ">" or operator = "<=" 
				or operator = ">=" then
			getToken (LINE)
			additions (nextValue, handledCommand, LINE)
			evalInfix (compValue, cValue, nextValue, operator,
				handledCommand)
			cValue := compValue
		else
			exit
        end if
    end loop
end comparisons

% Handle "additions": +, -, cat
body procedure additions
    var nextValue : string
    multiplications (aValue, handledCommand, LINE)
    loop
        var sumValue : string
        const operator := LINE.token
        if operator = "+" or operator = "-" or operator = "cat" then
			getToken (LINE)
			multiplications (nextValue, handledCommand, LINE)
			evalInfix (sumValue, aValue, nextValue, operator,
				handledCommand)
			aValue := sumValue
		else
			exit
        end if
    end loop
end additions


% Handle "multiplications": *, ".", "o", X, ^ (intersection), /, mod, div
body procedure multiplications
    var nextValue : string
    primary (mValue, handledCommand, LINE)
    loop
        var productValue : string
        const operator := LINE.token
        if operator = "*" or operator = "." or operator = "o" or operator = "X" 
				or operator = "\^" or operator = "/" or operator = "mod" 
				or operator = "div" then
			getToken (LINE)
			primary (nextValue, handledCommand, LINE)
			evalInfix (productValue, mValue, nextValue, operator,
				handledCommand)
			mValue := productValue
		else
			exit
        end if
    end loop
end multiplications

% Handle names of relations and sets, string (and boolean and number) values,
% as well as unary operators, namely, prefix: -, not, inv, dom, rng, ent
%                    postfix: +, * (transitive closure)
body procedure primary % (var pValue : string, var handledCommand : boolean, var LINE : lineType)
    var nextValue : string
    const operator := LINE.token
    if operator = "-" or operator = "not" or operator = "~" or operator = "#" 
			or operator = "inv" or operator = "id" or operator = "dom" 
			or operator = "rng" or operator = "ent" or operator = "duplicate" 
			or operator = "pick" or operator = "$" or operator = "head" 
			or operator = "tail" or operator = "rel2tripleset" 
			or operator = "triple2relname" then
		% Handle prefix operators
		getToken (LINE)
		if operator = "head" or operator = "tail"  or
			operator = "rel2tripleset" or operator = "triple2relname" then
			nextValue := LINE.token  
			getToken (LINE)
		else
			primary (nextValue, handledCommand, LINE)
		end if
		evalPrefix (pValue, nextValue, operator, handledCommand)

	elsif operator = "(" then % Handle parenthesized subexpressions
		getToken (LINE)
		expn (pValue, handledCommand, LINE)
		if LINE.token ~= ")" then
			put "***Syntax error: Missing ')' in: " ..
			for i : 1 .. LINE.noWordsToParse
				put LINE.wordsToParse (i), " " ..
			end for
			put ""
		end if
		getToken (LINE)

	elsif operator = "{" then % Handle set constructor: { a , b, c, ... }
		var noOperands := 0
		var operands : array 1 .. maxTokens of string
		getToken (LINE)
		if LINE.token = "}" then
			const predefName := "EMPTYSET"
			var trgValue : string
			evalPredefSymbol (trgValue, predefName, handledCommand)
			pValue := trgValue
			getToken (LINE)
		else
			loop
				if noOperands = upper (operands) then
					put "***Error: Too many operands in set constructor"
					exit
				end if
				expn (pValue, handledCommand, LINE)
				noOperands += 1
				operands (noOperands) := pValue
				if LINE.token = "}" then
					evalSetConstructor (pValue, noOperands, operands,
						handledCommand)
					getToken (LINE)
					exit
				end if

				if LINE.token = "," then
					getToken (LINE)
				else
					put "Syntax error in set constructor at: '",
						LINE.token, "'"
					exit
				end if

			end loop
		end if

	else % Handle actual primary (there is no prefix operator)
		pValue := LINE.token
		if pValue = "ID" or pValue = "EMPTYREL" or pValue = "DOM" or
				pValue = "RNG" or pValue = "ENT" or pValue = "EMPTYSET"
				or pValue = "relnames"
				then
			const predefName := pValue
			var trgValue : string
			evalPredefSymbol (trgValue, predefName, handledCommand)
			pValue := trgValue
			getToken (LINE)
		elsif pValue = "prefix" or pValue = "suffix"
				or pValue = "cover" then
			const XFixOperator := pValue
			getToken (LINE)
			const operand1 := LINE.token
			getToken (LINE)
			const operand2 := LINE.token
			var trgValue : string
			evalDoublePrefix (trgValue, XFixOperator, operand1, operand2,
				handledCommand)
			pValue := trgValue
			getToken (LINE)
		else % Input is a string (possibly quoted), which may be a var.
			const predefName := pValue
			var trgValue : string
			evalPredefSymbol (trgValue, predefName, handledCommand)
			pValue := trgValue
			getToken (LINE)
		end if
	end if

	loop % Handle suffix operators
		exit when LINE.token ~= "+" & LINE.token ~= "*"
		exit when nextTokenIsIdent (LINE)
		const postFixOperator := LINE.token
		var trgValue : string
		evalPostfix (trgValue, pValue, postFixOperator, handledCommand)
		pValue := trgValue
		getToken (LINE)
	end loop
end primary

procedure parseAssignment (words : array 1 .. * of charstr,
		var noWords : int,
		var handledCommand : boolean)
	% Form: X := expn
	handledCommand := true
	const X := words (1)
	if isTempId (X) or strrealok (X) or isSTRINGLiteral (X) then
		put "***Error: Assignment to: ", X
		return
	end if
	assert words (2) = ":="


	var LINE : lineType
	LINE.token := "# nothing #"
	LINE.nextToken := "# nothing #"
	LINE.noWordsToParse := noWords
	for i : 1 .. noWords
		LINE.wordsToParse (i) := words (i)
	end for
	LINE.nextWordToParse := 1

	% Prime the token reading logic
	getToken (LINE) % Skip X in X := expn
	getToken (LINE) % Skip ":="
	getToken (LINE) % Read "token"
	getToken (LINE) % Read "nextToken"

	var valueId : string
	expn (valueId, handledCommand, LINE)

	if X not= valueId then
		if isRel (X) then
			deleteRelation (X)
		end if
		evalCommand3Tokens (X, "=", valueId, handledCommand)
	else
		put "***Ignoring: ", X, " := ", X
	end if

	freeIfTemp (valueId)

	if LINE.token not= EOF then
		put "***Syntax error in Grok command: " ..
		for i : 1 .. LINE.noWordsToParse
			put LINE.wordsToParse (i), " " ..
		end for
		put ""
	end if
	deleteNamedTemps
end parseAssignment

procedure parseDollarAssignment (words : array 1 .. * of charstr,
		var noWords : int,
		var handledCommand : boolean)
	pre words (1) = "$" and words (3) = ":="
	% Form: $ Y := expn
	handledCommand := true
	const Y := words (2)
	if not isSTRINGVar (Y) then
		put "***Syntax error: in $ ", Y, " := ..."
		return
	end if
	const stringLit := evalSTRING (Y)
	var tempWords : array 1 .. maxTokens of charstr
	tempWords (1) := stringLit
	tempWords (2) := ":="
	for i : 4 .. noWords
		tempWords (i - 1) := words (i)
	end for
	var tempNoWords := noWords - 1
	parseAssignment (tempWords, tempNoWords, handledCommand)
end parseDollarAssignment

procedure parseExpression (words : array 1 .. * of charstr,
		var noWords : int,
		var handledCommand : boolean)
	% Form: expn
	handledCommand := true

	var LINE : lineType
	LINE.token := "# nothing #"
	LINE.nextToken := "# nothing #"
	LINE.noWordsToParse := noWords
	for i : 1 .. noWords
		LINE.wordsToParse (i) := words (i)
	end for
	LINE.nextWordToParse := 1

	% Prime the token reading logic
	getToken (LINE) % Read "token"
	getToken (LINE) % Read "nextToken"

	var valueId : string
	expn (valueId, handledCommand, LINE) % Execute expression

	var noWords2 := 1
	var words2 : array 1 .. 1 of charstr
	words2 (1) := valueId
	otherCommands (words2, noWords2, handledCommand)

	freeIfTemp (valueId)

	if LINE.token not= EOF then
		put "***Syntax Error in Grok command: " ..
		for i : 1 .. LINE.noWordsToParse
			put LINE.wordsToParse (i), " " ..
		end for
		put ""
	end if
	deleteNamedTemps
end parseExpression

function children (set1 : setPtr, Rnumber : str) : setPtr
	var res : setPtr
	res := newEmptySet
	for i : 1 .. listNodes (set1).size
		for j : 1 .. DBSize
			if DBRel (j) = Rnumber and DBSrc (j) = listNodes (set1).list (i) then
				res := setAdd (res, newSingletonSet (DBTrg (j)))
			end if
		end for
	end for
	result res
end children

proc makeTree (objs : setPtr, rel : str, tmpRel : str, rootName : string)
	type ss_record :
		record
			ssNum : str
			ssSize : int
		end record
	type father_record :
		record
			num : str
			size : int
		end record
	var sss : array 1 .. 100 of ss_record
	var fathers : array 1 .. 100 of father_record
	var fatherSize, tableSize : int := 0
	var sorted, found : boolean
	var tmp : father_record
	var oldDBSize : int

	for i : 1 .. DBSize
		if DBRel (i) = tmpRel then
			found := false
			for j : 1 .. tableSize
				if DBSrc (i) = sss (j).ssNum then
					sss (j).ssSize += 1
					found := true
					exit
				end if
			end for
			if not found then
				tableSize += 1
				sss (tableSize).ssNum := DBSrc (i)
				sss (tableSize).ssSize := 1
			end if
		end if
	end for
	oldDBSize := DBSize
	for i : 1 .. listNodes (objs).size
		fatherSize := 0
		for j : 1 .. DBSize
			if DBTrg (j) = listNodes (objs).list (i) and DBRel (j) = tmpRel then
				fatherSize += 1
				fathers (fatherSize).num := DBSrc (j)
				for k : 1 .. tableSize
					if fathers (fatherSize).num = sss (k).ssNum then
						fathers (fatherSize).size := sss (k).ssSize
						exit
					end if
				end for
			end if
		end for
		sorted := false
		loop
			exit when sorted
			sorted := true
			for j : 1 .. fatherSize - 1
				if fathers (j).size > fathers (j + 1).size then
					tmp.num := fathers (j).num
					tmp.size := fathers (j).size
					fathers (j).num := fathers (j + 1).num
					fathers (j).size := fathers (j + 1).size
					fathers (j + 1).num := tmp.num
					fathers (j + 1).size := tmp.size
					sorted := false
				end if
			end for
		end loop
		if fatherSize = 0 then
			insertDB (nameNum (rootName), rel, listNodes (objs).list (i))
		else
			insertDB (fathers (1).num, rel, listNodes (objs).list (i))
			for j : 2 .. fatherSize
				insertDB (fathers (j).num, rel, fathers (j - 1).num)
			end for
			insertDB (nameNum (rootName), rel, fathers (fatherSize).num)
		end if
	end for
	deleteRelation (numName (tmpRel))
	%deleteDuplicateTuples (1)
end makeTree

body proc otherCommands %(words : array 1 .. * of charstr, var noWords : int, var handledCommand : boolean)
	handledCommand := true

	if noWords = 1 and isRel (words (1)) then
		putRel (words (1))

	elsif noWords = 1 and isSetVar (words (1)) then
		putSetVar (words (1))

	elsif noWords = 1 and isSTRINGVar (words (1)) then
		putSTRINGVar (words (1))

	elsif noWords = 2 & words (1) = "relname" then
		const relId := nameNum (evalSTRING (words (2)))
		insertIntoListUnique (relNames, relNamesSize, relId)

		% Special purpose use of symbols ":=" and "="

	elsif noWords = 4 and
			(words (2) = "=" or words (2) = ":=")
			and words (3) = "duplicate" then

		if words (2) = "=" then
			put "***Please replace = by :="
		end if

		const setVarName := words (1)
		const orgSetName := words (4)
		var thisSet := newEmptySet
		var orgSetPtr : setPtr
		var orgSetIsTemp : boolean
		evalSet (orgSetName, orgSetPtr, orgSetIsTemp)
		for i : 1 .. listNodes (orgSetPtr).size
			setInsert (nameNum (numName (listNodes (orgSetPtr).list (i)) +
				".dup"),
				thisSet)
			for j : 1 .. DBSize
				if DBSrc (j) = listNodes (orgSetPtr).list (i) and DBTrg (j) not=
						listNodes (orgSetPtr).list (i) then
					insertDB (nameNum (numName (DBSrc (j)) + ".dup"),
						DBRel (j), DBTrg (j))
				end if
				if DBTrg (j) = listNodes (orgSetPtr).list (i) and DBSrc (j) not=
						listNodes (orgSetPtr).list (i) then
					insertDB (DBSrc (j), DBRel (j),
						nameNum (numName (DBTrg (j)) + ".dup"))
				end if
				if DBTrg (j) = listNodes (orgSetPtr).list (i) and DBSrc (j) =
						listNodes (orgSetPtr).list (i) then
					insertDB (nameNum (numName (DBSrc (j)) + ".dup"),
						DBRel (j), nameNum (numName (DBTrg (j)) +
						".dup"))
				end if
			end for
		end for
		assignSetVar (setVarName, thisSet)
		if orgSetIsTemp then
			freeSet (orgSetPtr)
		end if

	elsif noWords = 5 and
			(words (2) = "=" or words (2) = ":=")
			and words (3) = "cover" and
			isRel (words (5)) then % Form: t = cover s R

		if words (2) = "=" then
			put "***Please replace = by :="
		end if

		const setVarName := words (1)
		const coveringSetName := words (4)
		const relName := words (5)

		var isTempCoveringSet : boolean
		var covering : setPtr
		evalSet (coveringSetName, covering, isTempCoveringSet)
		const covered := coveredSet (covering, relName)
		assignSetVar (setVarName, covered)
		if isTempCoveringSet then
			freeSet (covering)
		end if

		% End of: Special purpose use of symbols ":=" and "="

	elsif noWords >= 3 and words (2) = "=" then
		put "***Please replace = by := (or by ==)"
		assignOpCommands (words, noWords, handledCommand)

	elsif noWords >= 2 & noWords <= 4 & words (1) = "sample" then
		% Form: sample relOrSet [startpos [samplesize] ]
		var startPos := 1
		var sampleSize := 10
		if noWords >= 3 then
			% Should use 'strintok' here but it's busted on winoot
			startPos := strint (words (3))
		end if
		if noWords = 4 then
			% Should use 'strintok' here but it's busted on winoot
			sampleSize := strint (words (4))
		end if
		if isRel (words (2)) then
			const relName := words (2)
			putRelSample (relName, startPos, sampleSize)
		elsif isSetVar (words (2)) then
			const setVarName := words (2)
			putSetVarSample (setVarName, startPos, sampleSize)
		else
			put "***Syntax error"
		end if

	elsif noWords = 1 and
			words (1) = "ID" or
			words (1) = "EMPTYREL" or words (1) = "EMPTYSET" or
			words (1) = "DOM" or words (1) = "RNG" or
			words (1) = "ENT" or words (1) = "relnames"
			then % Handle "built-in constants"
		parseExpression (words, noWords, handledCommand)

	elsif noWords = 1 then
		% An unknown word, such as x; treat as a literal.
		putLiteral (words (1))

	elsif noWords = 5 and words (1) = "cluster" and isRel (words (3))
			then

		% The format of this command is:
		% cluster objects relation newRelName rootName
		% objects = a set of all the objects to cluster
		% relation = interactions between objects are given as a single
		% relation that maybe has to be constructed beforehand
		% newRelName = this is the name of the relation that will be
		% output (could be "contain" or "newcontain" or something)
		% rootName = the anme of the root of the containment tree
		% (this will usually be the project name)

		const MAXINTERFACESIZE := 1
		const MAXSSSIZE := 20
		const Rname := words (3)
		const Rnumber := nameNum (Rname)
		const objectsSetName := words (2)
		const newRelName := words (4)
		const newRelNumber := nameNum (newRelName)
		const newTmpRelName := words (4) + "tmp"
		const newTmpRelNumber := nameNum (newTmpRelName)
		const rootSetName := words (5)

		var rootSetPtr : setPtr
		var rootSetIsTemp : boolean
		evalSet (rootSetName, rootSetPtr, rootSetIsTemp)

		const r := listNodes (rootSetPtr).list (1)
		const rootName := numName (r)

		insertIntoListUnique (relNames, relNamesSize, newRelNumber)
		insertIntoListUnique (relNames, relNamesSize, newTmpRelNumber)

		var objectsSetPtr : setPtr
		var objectsSetIsTemp : boolean
		evalSet (objectsSetName, objectsSetPtr, objectsSetIsTemp)

		var filesToTry, filesTried, filesClustered : setPtr
		var set1, set2, set3 : setPtr
		var rootFile, ssNumber : str
		var ssName : string

		filesToTry := sameSet (objectsSetPtr)
		filesTried := newEmptySet
		filesClustered := newEmptySet

		for i : 1 .. MAXINTERFACESIZE
			loop
				exit when listNodes (filesToTry).size = 0
				rootFile := listNodes (filesToTry).list (1)
				set1 := newSingletonSet (rootFile)
				set2 := coveredSet (set1, Rname)
				%       set3 := setSubtract (set2, set1)
				%      exit when set3 -> size = 0
				%     set1 := setAdd (set1, set3)
				if listNodes (set2).size = 1 then
					filesToTry := setSubtract (filesToTry, set2)
				else
					if listNodes (set2).size < MAXSSSIZE then
						filesToTry := setSubtract (filesToTry, set2)
					else
						filesToTry := setSubtract (filesToTry, set1)
					end if
					ssName := numName (rootFile) + ".ss"

					ssNumber := nameNum (ssName)
					for j : 1 .. listNodes (set2).size
						insertDB (ssNumber, newTmpRelNumber, listNodes (set2).list (j))
					end for
				end if
			end loop
		end for
		makeTree (objectsSetPtr, newRelNumber, newTmpRelNumber, rootName)
		if objectsSetIsTemp then
			freeSet (objectsSetPtr)
		end if


	elsif noWords = 7 and words (2) = ":=" and words (3) = "adopt" and
			isRel (words (5)) then
		const newset := words (1)
		const Rname := words (5)
		const Rnumber := nameNum (Rname)
		const orphanSetName := words (4)
		const setVarName := words (6)
		const prefixSetName := words (7)
		var orphanSetPtr, thisSetPtr, prefixSetPtr : setPtr
		var orphanSetIsTemp, thisSetIsTemp, prefixSetIsTemp : boolean
		evalSet (orphanSetName, orphanSetPtr, orphanSetIsTemp)
		evalSet (setVarName, thisSetPtr, thisSetIsTemp)
		evalSet (prefixSetName, prefixSetPtr, prefixSetIsTemp)

		var votes : array 1 .. listNodes (thisSetPtr).size of vote_record
		var voted : boolean
		var curr_size := 0
		var new_pateras, original_pateras : num
		var adopted := false
		var kidnapped := false
		var look_for_father := true
		var thisSet : setPtr
		const o := listNodes (orphanSetPtr).list (1)
		const oName := numName (o)

		const topLevelSS := nameNum ("REFUGEES.ss")

		const containNumber := nameNum ("contain")
		for i : 1 .. DBSize
			if DBRel (i) = containNumber and DBTrg (i) = o then
				original_pateras := DBSrc (i)
				kidnapped := true
				exit
			end if
		end for
		if kidnapped then
			var count_contents := 0
			look_for_father := false
			for i : 1 .. DBSize
				if DBRel (i) = containNumber and DBSrc (i) =
						original_pateras
						then
					count_contents += 1
					if count_contents > 2 then
						look_for_father := true
						exit
					end if
				end if
			end for
			if original_pateras = nameNum ("support.ss") then
				look_for_father := false
			end if
		end if
		if look_for_father then

			const prefixNumber := nameNum ("prefix")
			for i : 1 .. listNodes (prefixSetPtr).size
				const prfx := listNodes (prefixSetPtr).list (i)
				const prfxName := numName (prfx)

				if length (oName) > length (prfxName) then
					if oName (1 .. length (prfxName)) = prfxName then
						for j : 1 .. DBSize
							if DBRel (j) = prefixNumber and DBTrg (j) =
									prfx
									then
								new_pateras := DBSrc (j)
								exit
							end if
						end for
						thisSet := newSingletonSet (new_pateras)
						adopted := true
						exit
					end if
				end if
			end for
			if not adopted then
				for i : 1 .. DBSize
					if DBRel (i) = Rnumber and isMember (DBTrg (i),
							thisSetPtr)
							then
						voted := false
						for j : 1 .. curr_size
							if votes (j).name = numName (DBSrc (i)) then
								voted := true
								votes (j).number += 1
								exit
							end if
						end for
						if not voted then
							curr_size += 1
							votes (curr_size).name := numName (DBSrc (i))
							votes (curr_size).number := 1
						end if
					end if
				end for
				if curr_size > 0 then
					var pap := 1
					for i : 1 .. curr_size
						if votes (i).number > votes (pap).number then
							pap := i
						end if
					end for
					new_pateras := nameNum (votes (pap).name)
				else
					new_pateras := topLevelSS
				end if
				if kidnapped and new_pateras not= original_pateras then
					if new_pateras = topLevelSS or
							interface_grows (numName (original_pateras),
							oName) then
						if new_pateras = topLevelSS then
							put "toplevel"
						else
							put "interface"
						end if
						new_pateras := original_pateras
					end if
				end if
				thisSet := newSingletonSet (new_pateras)
			end if
		else
			thisSet := newSingletonSet (original_pateras)
		end if
		assignSetVar (newset, thisSet)
		if thisSetIsTemp then
			freeSet (thisSetPtr)
		end if
		if orphanSetIsTemp then
			freeSet (orphanSetPtr)
		end if
		if prefixSetIsTemp then
			freeSet (prefixSetPtr)
		end if

	elsif words (1) = "emitta" and noWords = 4 then
		const ssName := words (2)
		const ffName := words (3)
		const dirName := words (4)
		var ssSet, ffSet, dirSet : setPtr
		var isSsTemp, isFfTemp, isDirTemp : boolean
		evalSet (ssName, ssSet, isSsTemp)
		evalSet (ffName, ffSet, isFfTemp)
		evalSet (dirName, dirSet, isDirTemp)
		emitta (ssSet, ffSet, numName (listNodes (dirSet).list (1)))
		if isSsTemp then
			freeSet (ssSet)
		end if
		if isFfTemp then
			freeSet (ffSet)
		end if
		if isDirTemp then
			freeSet (dirSet)
		end if

	elsif words (1) = "fixfix" then
		fixfix

	elsif words (1) = "tree" and noWords = 2 then
		relTree (words (2))

	elsif words (1) = "createDomination" and noWords = 2 then
		domination (words (2))






	elsif noWords >= 3 and words (2) = ":=" then
		parseAssignment (words, noWords, handledCommand)

	elsif noWords >= 4 and words (1) = "$" and words (3) = ":=" then
		parseDollarAssignment (words, noWords, handledCommand)

	elsif noWords = 2 and words (1) = "delset" then
		const setVarName := words (2)
		var thisSetPtr : setPtr
		var thisSetIsTemp : boolean
		evalSet (setVarName, thisSetPtr, thisSetIsTemp)
		removeSetFromDB (thisSetPtr)
		if thisSetIsTemp then
			freeSet (thisSetPtr)
		end if

	elsif noWords = 3 and words (1) = "delsetset" then
		% Form: delsetset s1 s2
		% Delete all edges in s1 X s2
		const firstSetVarName := words (2)
		const secondSetVarName := words (3)
		var thisSetPtrOne, thisSetPtrTwo : setPtr
		var thisSetIsTempOne, thisSetIsTempTwo : boolean
		evalSet (firstSetVarName, thisSetPtrOne, thisSetIsTempOne)
		evalSet (secondSetVarName, thisSetPtrTwo, thisSetIsTempTwo)
		removeSetsFromDB (thisSetPtrOne, thisSetPtrTwo, "")
		if thisSetIsTempOne then
			freeSet (thisSetPtrOne)
		end if
		if thisSetIsTempTwo then
			freeSet (thisSetPtrTwo)
		end if

	elsif noWords = 4 and words (1) = "delsetsetkeep" and
			isRel (words (4)) then % Form delsetsetkeep s1 s2 R0
		% Delete edges in s1 X s2, except for R0 edges
		const firstSetVarName := words (2)
		const secondSetVarName := words (3)
		const relName := words (4)
		var thisSetPtrOne, thisSetPtrTwo : setPtr
		var thisSetIsTempOne, thisSetIsTempTwo : boolean
		evalSet (firstSetVarName, thisSetPtrOne, thisSetIsTempOne)
		evalSet (secondSetVarName, thisSetPtrTwo, thisSetIsTempTwo)
		removeSetsFromDB (thisSetPtrOne, thisSetPtrTwo, relName)
		if thisSetIsTempOne then
			freeSet (thisSetPtrOne)
		end if
		if thisSetIsTempTwo then
			freeSet (thisSetPtrTwo)
		end if

	elsif noWords = 2 and words (1) = "slice" then
		const setVarName := words (2)
		var thisSetPtr : setPtr
		var thisSetIsTemp : boolean
		evalSet (setVarName, thisSetPtr, thisSetIsTemp)
		sliceSetFromDB (thisSetPtr)
		if thisSetIsTemp then
			freeSet (thisSetPtr)
		end if

	elsif noWords = 2 and words (1) = "wslice" then
		const setVarName := words (2)
		var thisSetPtr : setPtr
		var thisSetIsTemp : boolean
		evalSet (setVarName, thisSetPtr, thisSetIsTemp)
		wsliceSetFromDB (thisSetPtr)
		if thisSetIsTemp then
			freeSet (thisSetPtr)
		end if

	elsif noWords = 4 and words (1) = "addset" then
		const set1Name := words (2)
		const set2Name := words (3)
		const contName := words (4)
		var set1, set2, cont : setPtr
		var set1IsTemp, set2IsTemp, contIsTemp : boolean
		evalSet (set1Name, set1, set1IsTemp)
		set2 := evalSetAsTemp (set2Name)
		evalSet (contName, cont, contIsTemp)
		if listNodes (cont).size > 0 then
			listAdd (listNodes (set2).list, listNodes (set2).size, listNodes (set1).list, listNodes (set1).size)
		end if
		assignSetVar (set2Name, set2)
		if set1IsTemp then
			freeSet (set1)
		end if
		if contIsTemp then
			freeSet (cont)
		end if

	elsif noWords >= 2 then
		parseExpression (words, noWords, handledCommand)

	else
		handledCommand := false
	end if
end otherCommands

include "help.i"

% Translate $n to n-th parameter (which is a string)
fcn resolveParam (token : string, params : array 1 .. * of charstr,
		paramsSize : int) : string
	if isPrefix ("$", token) or isPrefix ("D_", token) then
		var intPart : string
		if isPrefix ("$", token) then
			intPart := token (2 .. *)
		else
			intPart := token (3 .. *)
		end if
		if "1" <= intPart and intPart <= "9" then
			const i := strint (intPart)
			if i <= paramsSize then
				result params (i)
			else
				put "***Missing parameter for ", token
				result token
			end if
		else
			result token
		end if
	else
		result token % Did not translate to parameter
	end if
end resolveParam

proc scanCommandLine (commandLine : string,
		var word : array 1 .. * of charstr, var noWords : int,
		params : array 1 .. * of charstr, paramsSize : int)
	var C := commandLine



	var i := 1
	noWords := 0
	const lengthC := length (C)
	loop
		% Each iteration of this loop scans one token (ident or operator)

		loop % Skip blanks and tab characters
			exit when i > lengthC
			exit when C (i) ~= " " and C (i) ~= "\t"
			i += 1
		end loop
		exit when i > lengthC

		const startChar := C (i)
		assert startChar ~= " "

		exit when startChar = "%" % Skip "%" comments
		% Skip "//" comments
		exit when startChar = "/" & i + 1 <= lengthC and C (i + 1) = "/"

		if noWords = upper (word) then
			put "***Command is too long"
			exit
		end if
		noWords += 1


		word (noWords) := startChar % First character of this token
		i += 1 % Skip start char

		const operatorCharStandAlone := "=+-*/\^()<>#&,{}"
		% ".", "~" omitted on purpose
		const operatorCharWithEqualAfter := "=~<>:"
		const operatorChar := ":=+-*/\^~()<>#&,{}"

		const findOpWithEqAfter := index (operatorCharWithEqualAfter,
			startChar)
		const isOperatorCharWithEqualAfter :=
			findOpWithEqAfter ~= 0 & i <= lengthC and C (i) = "="

		if isOperatorCharWithEqualAfter then
			word (noWords) += "="
			i += 1

		elsif index (operatorCharStandAlone, startChar) ~= 0 then
			% Do nothing, because token is already scanned

		elsif startChar = "\"" or startChar = "'" then % Scan quoted string

			const quoteChar := startChar
			% We retain the quote char's in the string token
			loop
				% Invariant: i locates next (unscanned) character in C
				if i > lengthC then
					put "***Error: Missing final quote on string: '",
						word (noWords), "'"
					word (noWords) += quoteChar
					% Do NOT increment i, cause next char must be scanned
					exit
				end if
				const nextChar := C (i)
				% Invariant: i locates nextChar (just scanned char)
				if nextChar = "\\" & i + 1 <= lengthC &
						(C (i + 1) = "\"" or C (i + 1) = "\'") then
					% Found an embedded quote
					word (noWords) += "\\" + C (i + 1)
					i += 2
				else
					word (noWords) += nextChar
					i += 1
					exit when nextChar = quoteChar
				end if
			end loop
		else % Scan identifier
			loop
				exit when i > lengthC
				const nextChar := C (i)
				exit when nextChar = " " or nextChar = "\t"
				const indexVal := index (operatorChar, nextChar)
				exit when indexVal ~= 0
				word (noWords) += nextChar
				i += 1
			end loop
			word (noWords) := resolveParam (word (noWords),
				params, paramsSize)
		end if
	end loop

end scanCommandLine

proc collectCommandLine (commandFileNo : int, var commandLine : string)
	commandLine := ""
	loop % Collect command line, with possible line extensions
		var inputString : string
		if commandFileNo = stdInput then
			put ">> " ..
			% Seems to need an EOF test here
			if eof then
				put "***End-of-file on interactive input stream"
				% break
				exit
			end if
			get inputString : *
		else
			fileMgr.Get (commandFileNo, inputString)
		end if

		const lenInput := length (inputString)
		const continuation := lenInput >= 1 & inputString (*) = "\\"
		if continuation then
			inputString := inputString (1 .. * - 1)
		end if
		if length (commandLine) + lenInput > 255 then
			put "***ERROR: Grok can't handle long statement: ",
				commandLine, " ... "
			exit
		end if
		commandLine += inputString
		exit when not continuation
	end loop
end collectCommandLine

forward proc ifStatement (commandFileNo : int,
	var quitReason : string,
	params : array 1 .. * of charstr, paramsSize : int,
	words : array 1 .. maxTokens of charstr,
	noWords : int)

forward proc loopStatement (commandFileNo : int,
	var quitReason : string,
	params : array 1 .. * of charstr, paramsSize : int,
	words : array 1 .. maxTokens of charstr,
	noWords : int)

forward proc skipTo (commandFileNo : int,
	terminator : string, var quitReason : string,
	params : array 1 .. * of charstr, paramsSize : int,
	var words : array 1 .. maxTokens of charstr,
	var noWords : int)

forward proc commandLoop (commandFileNo : int,
	var quitReason : string,
	params : array 1 .. * of charstr, paramsSize : int,
	var words : array 1 .. maxTokens of charstr,
	var noWords : int)

forward proc execGrokFile (grokExecFileName : string,
	params : array 1 .. * of charstr, var paramsSize : int,
	var foundExecFile : boolean, var quittingReason : string)

proc evalTest (startWordNo : int,
		var testCondition : string,
		var nextToken : string,
		words : array 1 .. maxTokens of charstr,
		noWords : int)

	if noWords - startWordNo + 1 < 1 then
		put "***Error: Missing test"
		return
	end if

	const testVar := "### TEST-VAR ###"

	const noWordsToCopy := noWords - startWordNo + 1
	var noWordsTmp := noWordsToCopy + 2
	var wordsTmp : array 1 .. maxTokens of charstr
	wordsTmp (1) := testVar
	wordsTmp (2) := ":="
	for i : 3 .. noWordsTmp
		wordsTmp (i) := words (i + startWordNo - 3)
	end for

	var handledCommand : boolean
	parseAssignment (wordsTmp, noWordsTmp, handledCommand)

	if isBOOL (testVar) then
		if evalBOOL (testVar) then
			testCondition := "true"
		else
			testCondition := "false"
		end if
	else
		testCondition := "### gargage ###"
	end if

	if isSTRINGVar (testVar) then
		deleteSTRINGVar (testVar)
	else
		put "***Error: Bad test value: " ..
		if noWords >= 2 then
			put words (2) ..
		end if
		put "  Warning: temporary not deleted"

	end if
end evalTest

const endAnyIfClause := "### end if ###" % ie, "else", "elsif" or "end if"

body proc skipTo % (commandFileNo : int,
		% terminator : string, var quitReason : string,
		% params : array 1 .. * of charstr, paramsSize : int,
		% var words : array 1 .. maxTokens of charstr,
		% var noWords : int)

	pre terminator = "end if" or
		terminator = "end loop" or
		terminator = "end for" or
		terminator = endAnyIfClause


	loop
		if commandFileNo not= stdInput and
				fileMgr.EOF (commandFileNo) then 
			put "***Error: Skipping to ", terminator, " but found EOF"
			quitReason := terminator
			exit
		end if
		var commandLine : string

		collectCommandLine (commandFileNo, commandLine)

		scanCommandLine (commandLine, words, noWords, params,
			paramsSize)

		var command1, command2 := ""

		if noWords >= 1 then
			command1 := words (1)
		end if

		if noWords >= 2 then
			command2 := words (2)
		end if

		quitReason := command1 + " " + command2

		if command1 = "end" then
			exit when quitReason = terminator % "end if" or "end loop"

			exit when quitReason = "end if" & terminator = endAnyIfClause

			put "***Error: Bad ending: '", quitReason, "'"
			quitReason := terminator
			if quitReason ~= "end if" and quitReason ~= "end loop" then
				quitReason := "end if"
			end if
			exit
		elsif terminator = endAnyIfClause &
				(command1 = "else" or command1 = "elsif") then
			quitReason := command1
			exit
		end if

		% Skip nested 'if' and 'loop'
		if command1 = "if" then
			skipTo (commandFileNo, "end if", quitReason, params, paramsSize,
				words, noWords)
		elsif command1 = "loop" then
			skipTo (commandFileNo, "end loop", quitReason, params,
				paramsSize, words, noWords)
		elsif command1 = "for" then
			skipTo (commandFileNo, "end for", quitReason, params,
				paramsSize, words, noWords)
		end if

	end loop
end skipTo

body proc ifStatement % (commandFileNo : int,
		% var quitReason : string,
		% params : array 1 .. * of charstr, paramsSize : int,
		% words : array 1 .. maxTokens of charstr,
		% noWords : int)
	pre words (1) = "if"

	var tmpWords : array 1 .. maxTokens of charstr
	tmpWords := words
	var tmpNoWords : int := noWords

	loop
		assert tmpWords (1) = "if" or tmpWords (1) = "elsif"
		% Evaluate the test condition
		var testCondition : string
		var nextToken : string
		var noWordsTmp2 := tmpNoWords

		if tmpWords (tmpNoWords) = "then" then
			noWordsTmp2 -= 1
		else
			put "***Warning: Missing 'then' in 'if' statement"
		end if

		evalTest (2, testCondition, nextToken, tmpWords, noWordsTmp2)

		if testCondition = "true" then
			var comQuitReason : string
			commandLoop (commandFileNo, comQuitReason, params, paramsSize,
				tmpWords, tmpNoWords)
			quitReason := comQuitReason
			if comQuitReason = "end if" then
				% Do nothing, this is a legal case
			elsif comQuitReason = "else" or comQuitReason = "elsif" or
					comQuitReason = "quit" or comQuitReason = "return" or
					comQuitReason = "exit" then
				var skipQuitReason : string
				skipTo (commandFileNo, "end if",
					skipQuitReason, params, paramsSize, tmpWords, tmpNoWords)
				if comQuitReason = "quit" or comQuitReason = "return" or
						comQuitReason = "exit" then
					quitReason := comQuitReason
				else
					% This is the case which commandLoop stopped due to
					% encountering "else" or "elsif" or wierd stuff
					quitReason := "end if"
				end if
			else
				put "***Error: 'if' statement ends with: '", comQuitReason,
					"'"
			end if
			exit
		else
			if testCondition ~= "false" then
				put "***Error: Bad 'if' test: '", testCondition,
					"' assumed false"
			end if
			skipTo (commandFileNo, endAnyIfClause,
				quitReason, params, paramsSize, tmpWords, tmpNoWords)

			if quitReason = "end if" then
				% Do nothing, we skipped to end of if statement
				exit

			elsif quitReason = "elsif" then
				% Rewind and reparse 'elsif' line, by simply repeating loop
				assert tmpWords (1) = "elsif"

			elsif quitReason = "else" then % Execute else clause
				commandLoop (commandFileNo, quitReason, params, paramsSize,
					tmpWords, tmpNoWords)
				if quitReason = "end if" or
						quitReason = "quit" or
						quitReason = "return" then
					% Do nothing, this is a legal case
				else
					put "***Error: 'if' statement ends with  '", quitReason,
						"'"
					quitReason := "end if"
				end if
				exit
			else
				% This should never happen, cause fixed by 'skipTo'
				put "Error: Bad end of 'if' statement: '", quitReason, "'"
			end if
		end if
	end loop
end ifStatement

var forStmtDepth := 0

body proc loopStatement % (commandFileNo : int,
		% var quitReason : string,
		% params : array 1 .. * of charstr, paramsSize : int,
		% words : array 1 .. maxTokens of charstr,
		% noWords : int)
	pre (words (1) = "loop" & noWords = 1) or
		(words (1) = "for" & words (3) = "in" & noWords >= 4)

	const isForStatement := words (1) = "for"
	var endStmt := "end loop"
	var forTmpSet : string (20)
	var forElement : string

	var forWords : array 1 .. maxTokens of charstr
	var forNoWords : int
	if isForStatement then
		forElement := words (2)
		endStmt := "end for"
		forStmtDepth += 1
		forTmpSet := "### for ###" + intstr (forStmtDepth)

		% Execute: forTmpSet := expn    (based on: for e in expn)
		forWords (1) := forTmpSet
		forWords (2) := ":="
		for i : 4 .. noWords
			forWords (i - 1) := words (i)
		end for
		forNoWords := noWords - 1
		var handledCommand : boolean
		parseAssignment (forWords, forNoWords, handledCommand)
	end if

	var tmpWords : array 1 .. maxTokens of charstr
	tmpWords := words
	var tmpNoWords : int := noWords

	var lineNoAtLoopStart : int
	fileMgr.Tell (commandFileNo, lineNoAtLoopStart)
	% Remember where loop started

	loop
		if isForStatement then

			% Execute: exit when tmpForSet = EMPTYSET   (change later for rel's)
			forWords (1) := forTmpSet
			forWords (2) := "=="
			forWords (3) := "EMPTYSET"
			forNoWords := 3

			var testCondition : string
			var nextToken : string

			evalTest (1, testCondition, nextToken, forWords, forNoWords)
			if testCondition = "true" then
				% This case is just like 'exit when true'
				skipTo (commandFileNo, endStmt,
					quitReason, params, paramsSize, tmpWords, tmpNoWords)
				quitReason := endStmt
				exit
			end if


			% Execute: forElement := pick forTmpSet   (pick an element from set)
			forWords (1) := forElement
			forWords (2) := ":="
			forWords (3) := "pick"
			forWords (4) := forTmpSet
			forNoWords := 4
			var handledCommand : boolean
			parseAssignment (forWords, forNoWords, handledCommand)

			% Execute: forTmpSet := forTmpSet - "{" forElement "}"
			forWords (1) := forTmpSet
			forWords (2) := ":="
			forWords (3) := forTmpSet
			forWords (4) := "-"
			forWords (5) := "{"
			forWords (6) := forElement
			forWords (7) := "}"
			forNoWords := 7
			parseAssignment (forWords, forNoWords, handledCommand)
		end if

		assert tmpWords (1) = "loop" or tmpWords (1) = "for" or
			tmpWords (1) = "exit"
		% In case of repeat for 'exit when true' ???

		var comQuitReason : string
		commandLoop (commandFileNo, comQuitReason, params, paramsSize,
			tmpWords, tmpNoWords)
		if comQuitReason = endStmt then
			% Continue looping, by seeking to beginning of loop
			fileMgr.Seek (commandFileNo, lineNoAtLoopStart - 1)
			var commandLine : string
			fileMgr.Get (commandFileNo, commandLine)
			scanCommandLine (commandLine, tmpWords, tmpNoWords, params,
				paramsSize)
			% Continue looping
		else

			if comQuitReason ~= "exit when" then
				if comQuitReason ~= "exit" and comQuitReason ~= "return" &
						comQuitReason ~= "quit" then
					put "***Error.  Loop ends with: '", quitReason, "'"
				end if
				skipTo (commandFileNo, endStmt,
					quitReason, params, paramsSize, tmpWords, tmpNoWords)
				quitReason := comQuitReason
				exit
			end if

			assert comQuitReason = "exit when"

			% Evaluate the test condition
			var testCondition : string
			var nextToken : string
			var noWordsTmp2 := tmpNoWords

			evalTest (3, testCondition, nextToken, tmpWords, noWordsTmp2)

			if testCondition = "true" then
				% This case is 'exit when true'
				skipTo (commandFileNo, endStmt,
					quitReason, params, paramsSize, tmpWords, tmpNoWords)
				quitReason := endStmt
				exit
			else
				if testCondition ~= "false" then
					put "***Error: Bad 'if' test: '", testCondition,
						"' assumed false"
				end if
				% This case is 'exit when false'
				% Just continue executing, via above call to 'commandLoop'
			end if
		end if
	end loop
	if isForStatement then
		forStmtDepth -= 1
	end if
end loopStatement


body proc commandLoop % (commandFileNo : int,
		% var quitReason : string,
		% params : array 1 .. * of charstr, paramsSize : int,
		% var words : array 1 .. maxTokens of charstr,
		% var noWords : int)

	loop
		if commandFileNo not= stdInput then
			quitReason := "EOF"
			exit when fileMgr.EOF (commandFileNo)
		end if
		var commandLine : string

		collectCommandLine (commandFileNo, commandLine)


		scanCommandLine (commandLine, words, noWords, params,
			paramsSize)
		if echoCommands & noWords > 0 then
			put "COMMAND: " ..
			for i : 1 .. noWords
				put words (i), " " ..
			end for
			put ""
		end if
		var clock1, clock2 : int % To time each command
		if timing then
			clock (clock1)
		end if
		var command := "(no command)"

		if noWords >= 1 & words (1) = "debug" then
			if debugging then
				for i : 2 .. noWords
					words (i - 1) := words (i)
				end for
				noWords -= 1
			else
				noWords := 0
			end if
		end if

		if noWords >= 1 then
			command := words (1)
		end if

		if noWords = 0 then
			% Null command

		elsif noWords = 1 and
				(command = "quit" or command = "q")
				then
			if command = "q" then
				put "***Please use 'quit' to stop Grok"
			end if
			quitReason := "quit"
			exit

		elsif noWords = 1 and command = "return"
				then
			quitReason := "return"
			exit

		elsif command = "end" then
			quitReason := command
			if noWords = 2 then
				quitReason += " " + words (2)
			end if
			exit

		elsif command = "exit" then
			if noWords = 1 then
				quitReason := "exit"
			elsif
					noWords >= 2 and words (2) = "when" then
				quitReason := "exit when"
			else
				put "***Syntax error in: exit ", words (2), " ..."
				quitReason := "exit"
			end if
			exit

		elsif command = "else" or
				command = "elsif" or
				command = "return" then
			quitReason := command
			exit
		end if

		if noWords = 0 then
			% Just a blank line or comment

		elsif command = "if" then
			ifStatement (commandFileNo, quitReason, params,
				paramsSize,
				words, noWords)
			if quitReason = "exit" or quitReason = "exit when" or
					quitReason = "return" or quitReason = "quit" then
				exit
			end if

		elsif command = "loop" & noWords = 1 then
			if commandFileNo = stdInput then
				put "***Error: Ignoring 'loop' in interactive stream"
			else
				loopStatement (commandFileNo, quitReason, params, paramsSize,
					words, noWords)
				if quitReason = "return" or quitReason = "quit" then
					exit
				end if
			end if

		elsif command = "for" & noWords >= 4 & words (3) = "in" then
			if commandFileNo = stdInput then
				put "***Error: Ignoring 'for loop' in interactive stream"
			else
				loopStatement (commandFileNo, quitReason, params, paramsSize,
					words, noWords)
				if quitReason = "return" or quitReason = "quit" then
					exit
				end if
			end if

		elsif command = "exit" & noWords >= 3 & words (2) = "when" then
			put
				"***Error: 'exit when' statement found not in a loop (ignored)"

		elsif command = "option" and noWords = 2 then
			const option := words (2)
			if option = "echo" then
				echoCommands := true
			elsif option = "noecho" then
				echoCommands := false
			elsif option = "unixsort" then
				useUnixSort := true
				useHeapSort := false
				usePigeonSort := false
			elsif option = "heapsort" then
				useUnixSort := false
				useHeapSort := true
				usePigeonSort := false
			elsif option = "pigeonsort" then
				useUnixSort := false
				useHeapSort := false
				usePigeonSort := true
			elsif option = "nosort" then
				useUnixSort := false
				useHeapSort := false
				usePigeonSort := false
			elsif option = "verbal" then
				verbal := true
			elsif option = "noverbal" then
				verbal := false
			elsif option = "timing" then
				timing := true
				clock (clock1)
			elsif option = "notiming" then
				timing := false
			elsif option = "optimize" then
				optimize := true
			elsif option = "nooptimize" then
				optimize := false
			elsif option = "debug" then
				debugging := true
			elsif option = "nodebug" then
				debugging := false
			elsif option = "progress" then
				progress := true
			elsif option = "noprogress" then
				progress := false

			else
				put "***Undefined option: ", option

			end if

		elsif command = "setnames" and noWords = 1 then
			put "There are ", setVarsSize, " set variables:"
			for i : 1 .. setVarsSize
				put INDENT, setVars (i).setName
			end for

		elsif command = "stringnames" and noWords = 1 then
			put "There are ", STRINGVarsSize, " string variables:"
			for i : 1 .. STRINGVarsSize
				put INDENT, STRINGVars (i).STRINGName
			end for

		elsif command = "varnames" and noWords = 1 then
			putLabelledList (stdOutput, relNames,
				relNamesSize, "Relations are", false)

			put "There are ", setVarsSize, " set variables:"
			for i : 1 .. setVarsSize
				put INDENT, setVars (i).setName
			end for

			put "There are ", STRINGVarsSize, " string variables:"
			for i : 1 .. STRINGVarsSize
				put INDENT, STRINGVars (i).STRINGName
			end for

		elsif command = "deldups" then
			deleteDuplicateTuples (1)

		elsif command = "dbsize" then
			put "Size of data base (may contain duplicates) is: ",
				DBSize

		elsif command = "randdb" and noWords = 4 then
			const noTuples := words (2)
			const noRels := words (3)
			const noOperands := words (4)
			put "Generate ", noTuples, " tuples, ", noRels,
				" relations, ",
				noOperands, " operands (possible duplicates!!)"
			for i : 1 .. strint (noTuples)
				var n : int
				randint (n, 1, strint (noOperands))
				const src := "n" + intstr (n)
				var r : int
				randint (r, 1, strint (noRels))
				const rel := "r" + intstr (r)
				randint (n, 1, strint (noOperands))
				const trg := "n" + intstr (n)
				insertDB (nameNum (src), nameNum (rel), nameNum (trg))
			end for
			for r : 1 .. strint (noRels)
				const relName := nameNum ("r" + intstr (r))
				insertIntoListUnique (relNames, relNamesSize, relName)
			end for


		elsif command = "delrel" and noWords = 2 then
			const relName := words (2)
			deleteRelation (relName)

		elsif command = "relToFile" and noWords = 3 then
			% Form: relToFile R F
			const thisRel := words (2)
			const thisRelNumber := nameNum (thisRel)
			const fileName := evalSTRING (words (3))
			var fileNo : int
			openForPut (fileNo, fileName)
			if fileNo >= 0 then
				for i : 1 .. DBSize
					if DBRel (i) = thisRelNumber then
						put : fileNo, thisRel, " ",
							numName (DBSrc (i)),
							" ",
							numName (DBTrg (i))
					end if
				end for
				closeFile (fileNo, fileName)
			end if

		elsif command = "appendRelToFile" and noWords = 3 then
			% Form: appendRelToFile R F
			const thisRel := words (2)
			const thisRelNumber := nameNum (thisRel)
			const fileName := evalSTRING (words (3))
			var fileNo : int
			openForAppend (fileNo, fileName)
			if fileNo >= 0 then
				for i : 1 .. DBSize
					if DBRel (i) = thisRelNumber then
						put : fileNo, thisRel, " ",
							numName (DBSrc (i)),
							" ",
							numName (DBTrg (i))
					end if
				end for
				closeFile (fileNo, fileName)
			end if

		elsif command = "setToFile" and (noWords = 3 or noWords = 4)
				then
			% Form: setToFile s F [D]
			% This form is to be replaced by: putset s [F [D]]
			put "***Error: Please replace 'setToFile' by 'putset'"

		elsif command = "putset" and (noWords >= 2 and noWords <= 4)
				then
			% Form: putset s [F [D]]
			% The replaces the form: setToFile s F [D]
			const setVarName := words (2)
			% If no file name is given, the filename is taken to be the set name
			var fileName : string := setVarName
			var dirPath : string := ""
			if noWords >= 3 then
				fileName := evalSTRING (words (3))
			end if
			if noWords = 4 then
				dirPath := evalSTRING (words (4))
				dirPath += "/"
			end if
			if not isSetVar (setVarName) then
				put "***Sorry, ", setVarName,
					" is not a set in 'putset'"
			else
				var p := findSetValue (setVarName)
				listToFile (listNodes (p).list, listNodes (p).size, dirPath + fileName)
				put "Wrote ", listNodes (p).size, " items to file ", fileName
			end if

		elsif command = "getset" and (noWords >= 2 and noWords <= 4)
				then
			% Form: getset s [F [D]]
			% This obsoletes the form: fileToSet F s [D]"
			const setVarName := words (2)
			var fileName : string := setVarName
			var dirPath : string := ""
			if noWords >= 3 then
				fileName := evalSTRING (words (3))
			end if
			if noWords = 4 then
				dirPath := evalSTRING (words (4))
				dirPath += "/"
			end if
			var p := newEmptySet
			if newEmptySet = nil (listNodes) then
				put "***Sorry, out of memory in getset command"
				quit
			end if
			fileToList (dirPath + fileName, listNodes (p).list, listNodes (p).size)
			assignSetVar (setVarName, p)
			put "Read ", listNodes (p).size, " items from file ", fileName

		elsif command = "getdb" and noWords = 2 then
			getdb (evalSTRING (words (2)), true)

		elsif command = "adddb" and noWords = 2 then
			getdb (evalSTRING (words (2)), false)

		elsif command = "getta" and noWords = 2 then
			getta (evalSTRING (words (2)), true)

		elsif command = "addta" and noWords = 2 then
			getta (evalSTRING (words (2)), false)

		elsif command = "cleardb" and noWords = 1 then
			cleardb

		elsif command = "clearsets" and noWords = 1 then
			clearSetVars

		elsif command = "clearstrings" and noWords = 1 then
			clearSTRINGVars

		elsif command = "reset" and noWords = 1 then
			cleardb
			clearSetVars
			clearSTRINGVars
			resetNameTab

		elsif command = "putdb" and noWords = 2 then
			const fileName := evalSTRING (words (2))
			putdb (fileName)

		elsif command = "putta" and noWords = 2 then
			const fileName := evalSTRING (words (2))
			putta (fileName)

		elsif command = "putgraxfacts" and noWords = 2 then
			const fileName := evalSTRING (words (2))
			putgrax (fileName, true)

		elsif command = "putgraxschema" and noWords = 2 then
			const fileName := evalSTRING (words (2))
			putgrax (fileName, false)

		elsif command = "writedb" and noWords = 2 then
			const fileName := evalSTRING (words (2))
			writedb (fileName)

		elsif command = "readdb" and noWords = 2 then
			const fileName := evalSTRING (words (2))
			readdb (fileName)


		elsif command = "listdb" and noWords = 1 then
			for i : 1 .. DBSize
				put i : 3, " ",
					numName (DBSrc (i)) : 25, " ",
					numName (DBRel (i)) : 25, " ",
					numName (DBTrg (i))
			end for
			put DBSize, " tuples"




		elsif command = "exec" and noWords >= 2 then
			% Form:  exec fileName p1 p2 p3 ...
			const grokFileToExec := evalSTRING (words (2))
			var paramList : array 1 .. maxTokens of charstr
			var paramSize := noWords - 2
			for i : 3 .. noWords
				paramList (i - 2) := words (i)
			end for
			var foundExecFile : boolean
			execGrokFile (grokFileToExec, paramList, paramSize,
				foundExecFile, quitReason)
			if quitReason = "quit" then
				exit
			end if

		elsif command = "for" and noWords >= 3 and
				isSetVar (words (2))
				then
			% Form:  for s exec fileName p2 p3 p4 ...   (p1 is t, a member of s)
			const forSetName := words (2)
			const grokFileToExec := words (4)
			var forSet := evalSetAsTemp (forSetName)
			var paramList : array 1 .. maxTokens of charstr
			var paramSize := noWords - 3
			% First param is a member of s
			% Install parameters p2, p3, p4, ...
			for i : 5 .. noWords
				paramList (i - 3) := words (i)
				% e.g., param 2 = words (5)
			end for
			for i : 1 .. listNodes (forSet).size
				paramList (1) := numName (listNodes (forSet).list (i))
				%  param 1 is member of s
				var foundExecFile : boolean
				execGrokFile (grokFileToExec, paramList, paramSize,
					foundExecFile, quitReason)
				exit when quitReason = "quit"
			end for
			freeSet (forSet)
			exit when quitReason = "quit"

		elsif command = "addprefix" and noWords = 3 and
				isSetVar (words (3))
				then % Form: addprefix prefix s
			const prefix := evalSTRING (words (2))
			const setName := words (3)
			const lenPrefix := length (prefix)
			var p := findSetValue (setName)
			for i : 1 .. listNodes (p).size
				const pListI := listNodes (p).list (i)
				const pListIName := numName (pListI)
				const lenItem := length (pListIName)
				if progress & i mod 1000 = 0 then
					put INDENT, i : 4, " ", pListIName
				end if
				if lenItem + lenPrefix > upper (charstr) then
					put "***Cannot add prefix '", prefix, " to '",
						pListIName, "' because string too long"
				else
					listNodes (p).list (i) := nameNum (prefix + pListIName)
				end if
			end for

		elsif command = "addsuffix" and noWords = 3 and
				isSetVar (words (2))
				then % Form: addsuffix s suffix
			const setName := words (2)
			const suffix := evalSTRING (words (3))
			const lenSuffix := length (suffix)
			var p := findSetValue (setName)
			for i : 1 .. listNodes (p).size
				const pListI := listNodes (p).list (i)
				const pListIName := numName (pListI)
				const lenItem := length (pListIName)
				if progress & i mod 1000 = 0 then
					put INDENT, i : 4, " ", pListIName
				end if
				if lenItem + lenSuffix > upper (charstr) then
					put "***Cannot add suffix '", suffix, " to '",
						pListIName, "' because string too long"
				else
					listNodes (p).list (i) := nameNum (pListIName + suffix)
				end if
			end for
	%%%======================

	elsif command = "addprefixrel" and noWords = 3 and
		isSetVar (words (3))
		then % Form: addprefixrel prefix s
	const additionLoc := 2
	const setLoc := 3
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		const lenItemName := length (itemName)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		if lenItemName + lenAddition > upper (charstr) then
			put "***Cannot catenate '", addition, " to '",
				itemName, "' because string too long"
		else % Change name to prefixed/suffixed name
			const modifiedName := addition + itemName
			const modifiedNum := nameNum (modifiedName)
			for n : 1 .. DBSize
				if DBRel (n) = itemNum then
					DBRel (n) := modifiedNum
				end if
				if progress & n mod 1000 = 0 then
					put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
						" ", DBTrg (n)
				end if
			end for
			insertIntoListUnique (relNames, relNamesSize, modifiedNum)
			deleteFromList (relNames, relNamesSize, itemNum)
		end if
	end for
	%%%========================

	elsif command = "addsuffixrel" and noWords = 3 and
		isSetVar (words (2))
		then % Form: addsuffixrel s suffix
	const additionLoc := 3
	const setLoc := 2
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		const lenItemName := length (itemName)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		if lenItemName + lenAddition > upper (charstr) then
			put "***Cannot catenate '", addition, " to '",
				itemName, "' because string too long"
		else % Change name to prefixed/suffixed name
			const modifiedName := itemName + addition
			const modifiedNum := nameNum (modifiedName)
			for n : 1 .. DBSize
				if DBRel (n) = itemNum then
					DBRel (n) := modifiedNum
				end if
				if progress & n mod 1000 = 0 then
					put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
						" ", DBTrg (n)
				end if
			end for
			insertIntoListUnique (relNames, relNamesSize, modifiedNum)
			deleteFromList (relNames, relNamesSize, itemNum)
		end if
	end for
	%%%======================

	elsif command = "addprefixsrc" and noWords = 3 and
		isSetVar (words (3))
		then % Form: addprefixsrc prefix s
	const additionLoc := 2
	const setLoc := 3
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		for n : 1 .. DBSize
			if DBRel (n) = itemNum then
				const endName := numName(DBSrc (n))
				const lenEndName := length (endName)
				if lenEndName + lenAddition > upper (charstr) then
					put "***Cannot catenate '", addition, " to '",
						endName, "' because string too long"
				else % Change name to prefixed/suffixed name
					DBSrc (n) := nameNum (addition + endName)
				end if
				if progress & n mod 1000 = 0 then
					put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
						" ", DBTrg (n)
				end if
			end if
		end for
	end for
	%%%========================

	elsif command = "addsuffixsrc" and noWords = 3 and
		isSetVar (words (2))
		then % Form: addsuffixsrc s suffix
	const additionLoc := 3
	const setLoc := 2
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		for n : 1 .. DBSize
			if DBRel (n) = itemNum then
				const endName := numName (DBSrc (n))
				const lenEndName := length (endName)
				if lenEndName + lenAddition > upper (charstr) then
					put "***Cannot catenate '", addition, " to '",
						endName, "' because string too long"
				else % Change name to prefixed/suffixed name
					DBSrc (n) := nameNum (endName + addition)
				end if
				if progress & n mod 1000 = 0 then
					put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
						" ", DBTrg (n)
				end if
			end if
		end for
	end for
	%%%======================

	elsif command = "addprefixtrg" and noWords = 3 and
		isSetVar (words (3))
		then % Form: addprefixsrc prefix s
	const additionLoc := 2
	const setLoc := 3
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		for n : 1 .. DBSize
			if DBRel (n) = itemNum then
				const endName := numName(DBTrg (n))
				const lenEndName := length (endName)
				if lenEndName + lenAddition > upper (charstr) then
					put "***Cannot catenate '", addition, " to '",
						endName, "' because string too long"
				else % Change name to prefixed/suffixed name
					DBTrg (n) := nameNum (addition + endName)
				end if
				if progress & n mod 1000 = 0 then
					put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
						" ", DBTrg (n)
				end if
			end if
		end for
	end for
	%%%========================

	elsif command = "addsuffixtrg" and noWords = 3 and
		isSetVar (words (2))
		then % Form: addsuffixsrc s suffix
	const additionLoc := 3
	const setLoc := 2
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		for n : 1 .. DBSize
			if DBRel (n) = itemNum then
				const endName := numName(DBTrg (n))
				const lenEndName := length (endName)
				if lenEndName + lenAddition > upper (charstr) then
					put "***Cannot catenate '", addition, " to '",
						endName, "' because string too long"
				else % Change name to prefixed/suffixed name
					DBTrg (n) := nameNum (endName + addition)
				end if
				if progress & n mod 1000 = 0 then
					put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
						" ", DBTrg (n)
				end if
			end if
		end for
	end for
	%%%========================

	elsif command = "delprefix" and noWords = 3 and
				isSetVar (words (3))
				then % Form:  delprefix prefix s
			const prefix := evalSTRING (words (2))
			const setName := words (3)
			const lenPrefix := length (prefix)
			var p := findSetValue (setName)
			for i : 1 .. listNodes (p).size
				const pListI := listNodes (p).list (i)
				const pListIName := numName (pListI)
				if progress & i mod 1000 = 0 then
					put INDENT, i : 4, " ", pListIName
				end if
				if isPrefix (prefix, pListIName) then
					listNodes (p).list (i) := nameNum (pListIName (lenPrefix
						+ 1
						..
						*))
				end if
			end for

		elsif command = "delsuffix" and noWords = 3 and
				isSetVar (words (2))
				then % Form: delsuffix s suffix
			const setName := words (2)
			const suffix := evalSTRING (words (3))
			const lenSuffix := length (suffix)
			var p := findSetValue (setName)
			for i : 1 .. listNodes (p).size
				const pListI := listNodes (p).list (i)
				const pListIName := numName (pListI)
				if progress & i mod 1000 = 0 then
					put INDENT, i : 4, " ", pListIName
				end if
				if isSuffix (suffix, pListIName) then
					const lenItem := length (pListIName)
					listNodes (p).list (i) := nameNum (
						pListIName (1 .. lenItem - lenSuffix))
				end if
			end for
	%%%======================

	elsif command = "delprefixrel" and noWords = 3 and
		isSetVar (words (3))
		then % Form: delprefixrel prefix s
	const additionLoc := 2
	const setLoc := 3
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		const lenItemName := length (itemName)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		% Change name to  un-prefixed/suffixed name
		var modifiedName := itemName
		if isPrefix (addition, itemName) then
			modifiedName := itemName (lenAddition + 1 .. *)
		end if
		const modifiedNum := nameNum (modifiedName)
		for n : 1 .. DBSize
			if DBRel (n) = itemNum then
				DBRel (n) := modifiedNum
			end if
			if progress & n mod 1000 = 0 then
				put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
					" ", DBTrg (n)
			end if
		end for
		insertIntoListUnique (relNames, relNamesSize, modifiedNum)
		deleteFromList (relNames, relNamesSize, itemNum)
	end for
	%%%========================

	elsif command = "delsuffixrel" and noWords = 3 and
		isSetVar (words (2))
		then % Form: delsuffixrel s suffix
	const additionLoc := 3
	const setLoc := 2
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		const lenItemName := length (itemName)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		% Change name to  un-prefixed/suffixed name
		var modifiedName := itemName
		if isSuffix (addition, itemName) then
			modifiedName := itemName (1 .. lenItemName - lenAddition)
		end if
		const modifiedNum := nameNum (modifiedName)
		for n : 1 .. DBSize
			if DBRel (n) = itemNum then
				DBRel (n) := modifiedNum
			end if
			if progress & n mod 1000 = 0 then
				put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
					" ", DBTrg (n)
			end if
		end for
		insertIntoListUnique (relNames, relNamesSize, modifiedNum)
		deleteFromList (relNames, relNamesSize, itemNum)
	end for
	%%%======================

	elsif command = "delprefixsrc" and noWords = 3 and
		isSetVar (words (3))
		then % Form: delprefixsrc prefix s
	const additionLoc := 2
	const setLoc := 3
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		for n : 1 .. DBSize
			if DBRel (n) = itemNum then
				var endName := numName (DBSrc (n))
				if isPrefix (addition, endName) then
					const lenEndName := length (endName)
					endName := endName (lenAddition + 1 .. *)
					const endNum := nameNum (endName)
					DBSrc (n) := endNum
				end if
				if progress & n mod 1000 = 0 then
					put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
						" ", DBTrg (n)
				end if
			end if
		end for
	end for
	%%%========================

	elsif command = "delsuffixsrc" and noWords = 3 and
		isSetVar (words (2))
		then % Form: delsuffixsrc s suffix
	const additionLoc := 3
	const setLoc := 2
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		for n : 1 .. DBSize
			if DBRel (n) = itemNum then
				var endName := numName (DBSrc (n))
				if isSuffix (addition, endName) then
					const lenEndName := length (endName)
					endName := endName (1 .. lenEndName - lenAddition)
					const endNum := nameNum (endName)
					DBSrc (n) := endNum
				end if
				if progress & n mod 1000 = 0 then
					put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
						" ", DBTrg (n)
				end if
			end if
		end for
	end for
	%%%======================

	elsif command = "delprefixtrg" and noWords = 3 and
		isSetVar (words (3))
		then % Form: delprefixtrg prefix s
	const additionLoc := 2
	const setLoc := 3
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		for n : 1 .. DBSize
			if DBRel (n) = itemNum then
				var endName := numName (DBTrg (n))
				if isPrefix (addition, endName) then
					const lenEndName := length (endName)
					endName := endName (lenAddition + 1 .. *)
					const endNum := nameNum (endName)
					DBTrg (n) := endNum
				end if
				if progress & n mod 1000 = 0 then
					put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
						" ", DBTrg (n)
				end if
			end if
		end for
	end for
	%%%========================

	elsif command = "delsuffixtrg" and noWords = 3 and
		isSetVar (words (2))
		then % Form: delsuffixtrg s suffix
	const additionLoc := 3
	const setLoc := 2
	const addition := evalSTRING (words (additionLoc))
	const setName := words (setLoc)
	const lenAddition := length (addition)
	var p := findSetValue (setName)
	for i : 1 .. listNodes (p).size
		const itemNum := listNodes (p).list (i)
		const itemName := numName (itemNum)
		if progress & i mod 1000 = 0 then
			put INDENT, i : 4, " ", itemName
		end if
		for n : 1 .. DBSize
			if DBRel (n) = itemNum then
				var endName := numName (DBTrg (n))
				if isSuffix (addition, endName) then
					const lenEndName := length (endName)
					endName := endName (1 .. lenEndName - lenAddition)
					const endNum := nameNum (endName)
					DBTrg (n) := endNum
				end if
				if progress & n mod 1000 = 0 then
					put n : 5, " ", DBRel (n) : 8, " ", DBSrc (n) : 14,
						" ", DBTrg (n)
				end if
			end if
		end for
	end for
	%%%========================

		elsif command = "put" then
			for i : 2 .. noWords
				put words (i), " " ..
			end for
			put ""


		elsif command = "?" then
			putHelp
		else
			var handledCommand : boolean
			otherCommands (words, noWords, handledCommand)
			if not handledCommand then
				put "***Sorry, bad command: " ..
				putListOnLine (words, noWords, false)
			end if
		end if
		if timing then
			clock (clock2)
			put "Time for command: ", (clock2 - clock1) / 1000 : 0 :
				1,
				" sec"
		end if
	end loop
end commandLoop


% This is a forwarded proc
body proc execGrokFile % (grokExecFileName : string,
		% params : array 1 .. * of charstr, var paramsSize : int,
		% var foundExecFile : boolean, var quittingReason : string)
	foundExecFile := true
	var commandFileNo : int
	fileMgr.Open (commandFileNo, grokExecFileName)

	if commandFileNo <= 0 then
		foundExecFile := false
		paramsSize := 0
		put "***Error, can't open command file '", grokExecFileName, "'"
		quittingReason := "EOF"
		return
	end if

	var words : array 1 .. maxTokens of charstr
	var noWords : int
	commandLoop (commandFileNo, quittingReason, params, paramsSize,
		words, noWords)
	fileMgr.Close (commandFileNo)

	if quittingReason ~= "EOF" and quittingReason ~= "return" and
			quittingReason ~= "quit" then
		put "***Error: Grok script '", grokExecFileName,
			"' quits early when hits '", quittingReason, "'"
	end if
end execGrokFile

% main program

DBSize := 0
relNamesSize := 0
findRelationNames (1)

var time1, time2 : string
var commandFileNo : int := stdInput
var params : array 1 .. 9 of charstr
var paramsSize : int := 0
var noArgs := nargs
time (time1)
if noArgs > 0 then
	const grokExecFile := fetcharg (1)
	paramsSize := noArgs - 1
	for i : 2 .. noArgs
		params (i - 1) := fetcharg (i)
	end for
	put "Exec ", grokExecFile ..
	for i : 1 .. paramsSize
		put " '", params (i), "'" ..
	end for
	put ""
	var foundExecFile : boolean
	var quittingReason : string
	execGrokFile (grokExecFile, params, paramsSize, foundExecFile,
		quittingReason)
else
	put "Welcome to Grok"
	put "For help, type '?'"
	loop
		var quittingReason : string
		var words : array 1 .. maxTokens of charstr
		var noWords : int
		commandLoop (commandFileNo, quittingReason, params,
			paramsSize, words, noWords)
		exit when quittingReason = "EOF"
		exit when quittingReason = "quit"
		put "***Interactive Grok: Ignoring extra '", quittingReason,
			"'"
	end loop
end if

time (time2)
put "Grok done.  Start time ", time1, ", finish time ", time2

