% File "asgnop.ti"

% This procedure handles basic commands of the form:
%       x = simpleExpn
% Where simpleExpn is one of:
%       string    (could be a variable's name)
%       string infixOp string
%       prefixOp string ...
%       string postfixOp
% This procedure implements all the basic operators of Grok.

% The procedures parseExpression and parseAssignment call assignOpCommands
% to carry out any parsed commands, such as "+.

procedure assignOpCommands
        (words : array 1 .. * of charstr, var noWords : int,
        var handledCommand : boolean)
    pre noWords >= 3 & words (2) = "="
    handledCommand := true

    %-------------Operations on Relations-------------

    if noWords = 5 and words (2) = "=" and words (4) = "+" and
            isRel (words (3)) and isRel (words (5)) then
        % Form: R0 = R1 + R2

        const R0Name := words (1)
        const R1Name := words (3)
        const R2Name := words (5)

        const R0Number := nameNum (R0Name)
        const R1Number := nameNum (R1Name)
        const R2Number := nameNum (R2Name)

        const oldDBSize := DBSize
        % The following does not suppress extra copies of tuples
        var addCount := 0
        for i : 1 .. DBSize
            if DBRel (i) = R1Number or DBRel (i) = R2Number then
                insertDB (DBSrc (i), R0Number, DBTrg (i))
                addCount += 1
                if progress & addCount mod 1000 = 0 then
                    put addCount : 3, " ", numName (DBSrc (i)) : 25, " ",
                        numName (DBTrg (i))
                end if
            end if
        end for
        deleteDuplicateTuples (oldDBSize + 1)
        const actualAddCount := DBSize - oldDBSize
        if verbal then
            put "   Added ", actualAddCount, " tuples. DBSize is ",
                DBSize,
                ", deleted ", addCount - actualAddCount, " duplicates"

        end if
        insertIntoListUnique (relNames, relNamesSize, R0Number)

    elsif noWords = 5 and words (2) = "=" and words (4) = "-" and
            isRel (words (3)) and isRel (words (5)) then
        % Form: R0 = R1 - R2

        const R0Name := words (1)
        const R1Name := words (3)
        const R2Name := words (5)

        const R0Number := nameNum (R0Name)
        const R1Number := nameNum (R1Name)
        const R2Number := nameNum (R2Name)

        relDifference (R0Number, R1Number, R2Number)

    elsif noWords = 5 and words (2) = "=" and words (4) = "\^" and
            isRel (words (3)) and isRel (words (5)) then
        % Form: R0 = R1 ^ R2

        const R0Name := words (1)
        const R1Name := words (3)
        const R2Name := words (5)

        const R0Number := nameNum (R0Name)
        const R1Number := nameNum (R1Name)
        const R2Number := nameNum (R2Name)

        relIntersection (R0Number, R1Number, R2Number)

    elsif noWords = 5 and words (2) = "=" and
            (words (4) = "*" or words (4) = "o") and
            isRel (words (3)) and isRel (words (5)) then
        % Form: R0 = R1 * R2

        const R0Name := words (1)
        const R1Name := words (3)
        const R2Name := words (5)

        const R0Number := nameNum (R0Name)
        const R1Number := nameNum (R1Name)
        const R2Number := nameNum (R2Name)

        % The following does not suppress extra copies of tuples (now ok!!)

        relComposition (R0Number, R1Number, R2Number)

    elsif noWords = 4 and words (2) = "=" and words (3) = "head" and
            isRel (words (4)) then
        % Form: R0 = head R1
        const R0Name := words (1)
        const R1Name := words (4)

        if isTempId (R1Name) then
            put "***Error: Taking 'head' of temporary relation: ", R1Name
        end if

        const R0Number := nameNum (R0Name)
        const R1Number := nameNum (R1Name)

        const oldDBSize := DBSize
        var addCount := 0
        for i : 1 .. DBSize
            if DBRel (i) = R1Number then
                const x := DBSrc (i)
                const y := DBTrg (i)
                % Given (x R1 y), create head edge as ((x R1 y) R0 y)
                const edgeNo := edgeAsString (x, R1Number, y)
                insertDB (edgeNo, R0Number, y)
                addCount += 1
                if progress & addCount mod 1000 = 0 then
                    put addCount : 3, " ", numName (DBSrc (i)) : 25, " ",
                        numName (DBTrg (i))
                end if
            end if
        end for
        deleteDuplicateTuples (oldDBSize + 1)
        const actualAddCount := DBSize - oldDBSize
        if verbal then
            put "   Added ", actualAddCount, " tuples. DBSize is ",
                DBSize,
                ", deleted ", addCount - actualAddCount, " duplicates"

        end if
        insertIntoListUnique (relNames, relNamesSize, R0Number)

    elsif noWords = 4 and words (2) = "=" and words (3) = "tail" and
            isRel (words (4)) then
        % Form: R0 = tail R1
        const R0Name := words (1)
        const R1Name := words (4)

        if isTempId (R1Name) then
            put "***Error: Taking 'tail' of temporary relation: ", R1Name
        end if

        const R0Number := nameNum (R0Name)
        const R1Number := nameNum (R1Name)

        const oldDBSize := DBSize
        var addCount := 0
        for i : 1 .. DBSize
            if DBRel (i) = R1Number then
                const x := DBSrc (i)
                const y := DBTrg (i)
                % Given (x R1 y), create tail edge as (x R0 (x R1 y))
                const edgeNo := edgeAsString (x, R1Number, y)
                insertDB (x, R0Number, edgeNo)
                addCount += 1
                if progress & addCount mod 1000 = 0 then
                    put addCount : 3, " ", numName (DBSrc (i)) : 25, " ",
                        numName (DBTrg (i))
                end if
            end if
        end for
        deleteDuplicateTuples (oldDBSize + 1)
        const actualAddCount := DBSize - oldDBSize
        if verbal then
            put "   Added ", actualAddCount, " tuples. DBSize is ",
                DBSize,
                ", deleted ", addCount - actualAddCount, " duplicates"

        end if
        insertIntoListUnique (relNames, relNamesSize, R0Number)

    elsif noWords = 4 and words (2) = "=" and words (3) = "rel2tripleset" and
            isRel (words (4)) then
        % Form: setVarName = rel2tripleset R1
        const setVarName := words (1)
        const R1Name := words (4)
        var thisSet := newEmptySet
        var thisSetSize := 0

        if isTempId (R1Name) then
            put "***Error: 'rel2tripleset' of temporary relation: ", R1Name
        end if

        const R1Number := nameNum (R1Name)

        deleteDuplicateTuples (DBSize) % So all elements unique

        for i : 1 .. DBSize
            if DBRel (i) = R1Number then
                const x := DBSrc (i)
                const y := DBTrg (i)
                % Given edge (x R1 y), create set member (_x,_R1,_y_)
                const edgeNo := edgeAsString (x, R1Number, y)
                if thisSetSize >= shortListMax then
                    put 
                        "***Error: Set too big. Increase shortListMax beyond ",

                        shortListMax
                    exit
                end if
                thisSetSize += 1
                listNodes (thisSet).list (thisSetSize) := edgeNo
                if progress & thisSetSize mod 1000 = 0 then
                    put thisSetSize : 3, " ", numName (DBSrc (i)) : 25, " ",
                        numName (DBTrg (i))
                end if
            end if
        end for
        listNodes (thisSet).size := thisSetSize
        if verbal then
            put "   Created ", thisSetSize, " elements."
        end if
        assignSetVar (setVarName, thisSet)

    elsif noWords = 4 and words (2) = "=" and words (3) = "inv" and
            isRel (words (4))
            then % Form: R0 = inv R1
        const R0Name := words (1)
        const R1Name := words (4)

        const R0Number := nameNum (R0Name)
        const R1Number := nameNum (R1Name)

        var addCount := 0
        for i : 1 .. DBSize
            if DBRel (i) = R1Number then
                insertDB (DBTrg (i), R0Number, DBSrc (i))
                addCount += 1
                if progress & addCount mod 1000 = 0 then
                    put addCount : 3, " ", numName (DBTrg (i)) : 25, " ",
                        numName (DBSrc (i))
                end if
            end if
        end for
        if verbal then
            put "   Added ", addCount, " tuples. DBSize is ", DBSize
        end if
        insertIntoListUnique (relNames, relNamesSize, R0Number)

    elsif noWords = 5 and words (2) = "=" and
            (words (4) = "." or words (4) = "*") and
            isRel (words (5)) then % Form: s = t . R
        if words (4) = "*" then
            put "***Warning: Use '.' instead of '*' in ",
                words (3), " * ", words (5)
        end if
        const setVarName := words (1)
        const srcItem := words (3)
        const thisRel := words (5)
        var src := evalSetAsTemp (srcItem)
        var trg := srcDotRel (src, thisRel)
        freeSet (src)
        if verbal then
            put "Projection contains ", listNodes (trg).size, " items"
        end if
        assignSetVar (setVarName, trg)

    elsif noWords = 5 and words (2) = "=" and
            (words (4) = "." or words (4) = "*") and
            isRel (words (3)) then % Form: s = R . t
        if words (4) = "*" then
            put "***Warning: Use '.' instead of '*'"
        end if
        const setVarName := words (1)
        const thisRel := words (3)
        const trgItem := words (5)
        var trg : setPtr
        var trgIsTemp : boolean
        evalSet (trgItem, trg, trgIsTemp)
        var src := relDotTrg (thisRel, trg)
        if trgIsTemp then
            freeSet (trg)
        end if
        if verbal then
            put "Projection contains ", listNodes (src).size, " items"
        end if
        assignSetVar (setVarName, src)

    elsif noWords = 4 and words (2) = "=" and words (4) = "+" and
            isRel (words (3)) then % Form: R0 = R1+
        const R0Name := words (1)
        const R1Name := words (3)
        const R0Number := nameNum (R0Name)
        const R1Number := nameNum (R1Name)
        transitiveClosure2 (R0Number, R1Number)

    elsif noWords = 4 and words (2) = "=" and words (4) = "*" and
            isRel (words (3)) then % Form: R0 = R1*
        const R0Name := words (1)
        const R1Name := words (3)
        const R0Number := nameNum (R0Name)
        const R1Number := nameNum (R1Name)
        reflexiveTransitiveClosure (R0Number, R1Number)

    elsif noWords = 3 and words (2) = "=" and words (3) = "EMPTYREL" then
        % Form: R0 := EMPTYREL    This is the empty relation
        const R0Number := nameNum (words (1))
        % Nothing to do to create the empty relation!
        insertIntoListUnique (relNames, relNamesSize, R0Number)

    elsif noWords = 4 and words (2) = "=" and words (3) = "dom" and
            isRel (words (4)) then % Form: s = dom R1
        const setVarName := words (1)
        const thisRel := words (4)
        const thisRelNumber := nameNum (thisRel)
        var thisSet := newEmptySet

        const seen := 0
        const notSeen := 1
        for i : 1 .. nameCount
            Index1 (i) := notSeen
        end for

        for i : 1 .. DBSize
            if DBRel (i) = thisRelNumber then
                Index1 (DBSrc (i)) := seen
            end if
        end for
        % Assert: Index1 (i) = seen iff i is member of dom R1

        var thisSetSize := 0
        for i : 1 .. nameCount
            if Index1 (i) = seen then
                if thisSetSize >= shortListMax then
                    put 
                        "***Error: Set too big.  Increase shortListMax beyond ",

                        shortListMax
                    exit
                end if
                thisSetSize += 1
                listNodes (thisSet).list (thisSetSize) := i
            end if
        end for
        listNodes (thisSet).size := thisSetSize

        if verbal then
            put "Domain contains ", listNodes (thisSet).size, " items"
        end if
        assignSetVar (setVarName, thisSet)

    elsif noWords = 4 and words (2) = "=" and words (3) = "rng" and
            isRel (words (4)) then % Form: s = rng R1
        const setVarName := words (1)
        const thisRel := words (4)
        const thisRelNumber := nameNum (thisRel)
        var thisSet := newEmptySet

        const seen := 0
        const notSeen := 1
        for i : 1 .. nameCount
            Index1 (i) := notSeen
        end for

        for i : 1 .. DBSize
            if DBRel (i) = thisRelNumber then
                Index1 (DBTrg (i)) := seen
            end if
        end for
        % Assert: Index1 (i) = seen iff i is member of rng R1

        var thisSetSize := 0
        for i : 1 .. nameCount
            if Index1 (i) = seen then
                if thisSetSize >= shortListMax then
                    put 
                        "***Error: Set too big.  Increase shortListMax beyond ",

                        shortListMax
                    exit
                end if
                thisSetSize += 1
                listNodes (thisSet).list (thisSetSize) := i
            end if
        end for
        listNodes (thisSet).size := thisSetSize

        if verbal then
            put "Range contains ", listNodes (thisSet).size, " items"
        end if
        assignSetVar (setVarName, thisSet)

    elsif noWords = 4 and words (2) = "=" and words (3) = "ent" and
            isRel (words (4)) then % Form: s = ent R
        % Find all entities that partipated in relation R
        const setVarName := words (1)
        const thisRel := words (4)
        const thisRelNumber := nameNum (thisRel)
        var thisSet := newEmptySet

        const seen := 0
        const notSeen := 1
        for i : 1 .. nameCount
            Index1 (i) := notSeen
        end for

        for i : 1 .. DBSize
            if DBRel (i) = thisRelNumber then
                Index1 (DBSrc (i)) := seen
                Index1 (DBTrg (i)) := seen
            end if
        end for
        % Assert: Index1 (i) = seen iff i is member of dom R1

        var thisSetSize := 0
        for i : 1 .. nameCount
            if Index1 (i) = seen then
                if thisSetSize >= shortListMax then
                    put 
                        "***Error: Set too big.  Increase shortListMax beyond ",

                        shortListMax
                    exit
                end if
                thisSetSize += 1
                listNodes (thisSet).list (thisSetSize) := i
            end if
        end for
        listNodes (thisSet).size := thisSetSize

        if verbal then
            put "Entity set contains ", listNodes (thisSet).size, " items"
        end if
        assignSetVar (setVarName, thisSet)

    elsif noWords = 4 and words (2) = "=" and words (3) = "id"
            then % Form: R0 = id s

        shouldBeSet (words (4))
        const R0Number := nameNum (words (1))
        const orgSetName := words (4)
        var orgSetPtr : setPtr
        var orgSetIsTemp : boolean
        evalSet (orgSetName, orgSetPtr, orgSetIsTemp)

        const oldDBSize := DBSize
        for i : 1 .. listNodes (orgSetPtr).size
            const orgItem := listNodes (orgSetPtr).list (i)
            insertDB (orgItem, R0Number, orgItem)
        end for

        const addCount := DBSize - oldDBSize
        if verbal then
            put "   Added ", addCount, " tuples. DBSize is ", DBSize
        end if

        if orgSetIsTemp then
            freeSet (orgSetPtr)
        end if
        insertIntoListUnique (relNames, relNamesSize, R0Number)

    elsif noWords = 3 and words (2) = "=" and words (3) = "EMPTYSET" then
        % Form: s := EMPTYSET
        const setVarName := words (1)
        var thisSet := newEmptySet
        assignSetVar (setVarName, newEmptySet)

    elsif noWords = 3 and words (2) = "=" and words (3) = "DOM" then
        % Form: s = DOM    Src of all relations
        const setVarName := words (1)
        var thisSet := newEmptySet

        const seen := 0
        const notSeen := 1
        for i : 1 .. nameCount
            Index1 (i) := notSeen
        end for

        for i : 1 .. DBSize
            Index1 (DBSrc (i)) := seen
        end for
        % Assert: Index1 (i) = seen iff i is member of DOM

        var thisSetSize := 0
        for i : 1 .. nameCount
            if Index1 (i) = seen then
                if thisSetSize >= shortListMax then
                    put 
                        "***Error: Set too big.  Increase shortListMax beyond ",

                        shortListMax
                    exit
                end if
                thisSetSize += 1
                listNodes (thisSet).list (thisSetSize) := i
            end if
        end for
        listNodes (thisSet).size := thisSetSize

        if verbal then
            put "Domain contains ", listNodes (thisSet).size, " items"
        end if
        assignSetVar (setVarName, thisSet)

    elsif noWords = 3 and words (2) = "=" and words (3) = "RNG" then
        % Form: s = RNG    Trg of all relations
        const setVarName := words (1)
        var thisSet := newEmptySet

        const seen := 0
        const notSeen := 1
        for i : 1 .. nameCount
            Index1 (i) := notSeen
        end for

        for i : 1 .. DBSize
            Index1 (DBTrg (i)) := seen
        end for
        % Assert: Index1 (i) = seen iff i is member of RNG

        var thisSetSize := 0
        for i : 1 .. nameCount
            if Index1 (i) = seen then
                if thisSetSize >= shortListMax then
                    put 
                        "***Error: Set too big.  Increase shortListMax beyond ",

                        shortListMax
                    exit
                end if
                thisSetSize += 1
                listNodes (thisSet).list (thisSetSize) := i
            end if
        end for
        listNodes (thisSet).size := thisSetSize

        if verbal then
            put "Domain contains ", listNodes (thisSet).size, " items"
        end if
        assignSetVar (setVarName, thisSet)

    elsif noWords = 3 and words (2) = "=" and words (3) = "ENT" then
        % Form: s = ENT    Src and trg of all relations
        const setVarName := words (1)
        var thisSet := newEmptySet

        const seen := 0
        const notSeen := 1
        for i : 1 .. nameCount
            Index1 (i) := notSeen
        end for

        for i : 1 .. DBSize
            Index1 (DBSrc (i)) := seen
            Index1 (DBTrg (i)) := seen
        end for
        % Assert: Index1 (i) = seen iff i is member of ENT

        var thisSetSize := 0
        for i : 1 .. nameCount
            if Index1 (i) = seen then
                if thisSetSize >= shortListMax then
                    put 
                        "***Error: Set too big.  Increase shortListMax beyond ",

                        shortListMax
                    exit
                end if
                thisSetSize += 1
                listNodes (thisSet).list (thisSetSize) := i
            end if
        end for
        listNodes (thisSet).size := thisSetSize

        if verbal then
            put "Domain contains ", listNodes (thisSet).size, " items"
        end if
        assignSetVar (setVarName, thisSet)

    elsif noWords = 3 and words (2) = "=" and words (3) = "relnames" then
        % Form: s = relnames    List of names of all relations
        const setVarName := words (1)
        var thisSet := newEmptySet

        listNodes (thisSet).list := relNames
        listNodes (thisSet).size := relNamesSize

        if verbal then
            put "relnames contains ", listNodes (thisSet).size, " items"
        end if
        assignSetVar (setVarName, thisSet)

    elsif noWords = 3 and words (2) = "=" and words (3) = "ID" then
        % Form: R0 := ID    This is the identity relation
        const R0Number := nameNum (words (1))
        const seen := 0
        const notSeen := 1
        for i : 1 .. maxNames
            nameTemp1 (i) := notSeen
        end for
        for i : 1 .. DBSize
            nameTemp1 (DBSrc (i)) := seen
            nameTemp1 (DBTrg (i)) := seen
        end for
        const oldDBSize := DBSize
        for i : 1 .. maxNames
            if nameTemp1 (i) = seen then
                insertDB (i, R0Number, i)
            end if
        end for
        const addCount := DBSize - oldDBSize
        if verbal then
            put "   Added ", addCount, " tuples. DBSize is ", DBSize
        end if

        insertIntoListUnique (relNames, relNamesSize, R0Number)

        % Do not free right (used by the set variable)

        %--------STRING, real and booleans operations----------

    elsif noWords = 4 and words (2) = "=" and words (3) = "-" and
            isNUMBER (words (4)) then
        % Form: N0 := - N1
        const N0Name := words (1)
        const N1Name := words (4)

        const N1Number := evalNUMBER (N1Name)
        const NUMBERVal := - N1Number
        assignSTRINGVar (N0Name, realstr (NUMBERVal, 0))

    elsif noWords = 4 and words (2) = "=" and
            (words (3) = "not" or words (3) = "~") and
            isBOOL (words (4)) then
        % Form: N0 := not N1
        const N0Name := words (1)
        const N1Name := words (4)

        const N1Number := evalBOOL (N1Name)
        const NUMBERVal := not N1Number
        assignSTRINGVar (N0Name, bool2BOOL (NUMBERVal))

    elsif noWords = 4 and words (2) = "=" and
            words (3) = "#" and isSetVar (words (4)) then
        % Form: N0 := # SetName
        const N0Name := words (1)
        const setVarName := words (4)

        var p := findSetValue (setVarName)
        const cardinality := intstr (listNodes (p).size)
        assignSTRINGVar (N0Name, cardinality)

    elsif noWords = 4 and words (2) = "=" and
            words (3) = "$" and not isSetVar (words (4))
            and not isRel (words (4)) then
        % Form: N0 := $ StringVal   (Use string as name of a variable)
        const N0Name := words (1)
        var stringLit := words (4)
        if isSTRINGVar (stringLit) then
            stringLit := evalSTRING (stringLit)
        end if

        % Set up to recursively evaluate.

        var tempWords : array 1 .. 3 of charstr
        var tempNoWords := 3
        tempWords (1) := words (1)
        tempWords (2) := "="
        tempWords (3) := stringLit
        % Recursive call
        assignOpCommands (tempWords, tempNoWords, handledCommand)

    elsif noWords = 4 and words (2) = "=" and
            words (3) = "pick" and isSetVar (words (4)) then
        % Form: N0 := pick SetName   (Pick an element of the set)
        const N0Name := words (1)
        const setVarName := words (4)

        var p := findSetValue (setVarName)
        var element : STRING
        if listNodes (p).size = 0 then
            element := "NULL"
        else
            element := numName (listNodes (p).list (1))
        end if
        assignSTRINGVar (N0Name, element)

    elsif noWords = 4 and words (2) = "=" and
            words (3) = "#" and isRel (words (4)) then
        % Form: N0 = # R (number of tuples in R)
        % Kludge: relation composition causes duplicates (this is fixed??)
        % which are counted here
        const N0Name := words (1)
        const relName := words (4)
        const relNumber := nameNum (relName)
        var tupleCount := 0
        for i : 1 .. DBSize
            if DBRel (i) = relNumber then
                tupleCount += 1
            end if
        end for
        const cardinality := intstr (tupleCount)
        assignSTRINGVar (N0Name, cardinality)

    elsif noWords = 5 and words (2) = "=" and isNUMBEROp (words (4)) and
            isNUMBER (words (3)) and isNUMBER (words (5)) then
        % Form: N0 = N1 NUMBEROp N2    ie, numeric operator

        const N0Name := words (1)
        const N1Name := words (3)
        const N2Name := words (5)
        const NUMBEROp := words (4)

        const N1Number := evalNUMBER (N1Name)
        const N2Number := evalNUMBER (N2Name)
        var NUMBERVal : real
        if NUMBEROp = "+" then
            NUMBERVal := N1Number + N2Number
        elsif NUMBEROp = "-" then
            NUMBERVal := N1Number - N2Number
        elsif NUMBEROp = "*" then
            NUMBERVal := N1Number * N2Number
        elsif NUMBEROp = "/" then
            NUMBERVal := N1Number / N2Number
        elsif NUMBEROp = "div" then
            NUMBERVal := N1Number div N2Number
        elsif NUMBEROp = "mod" then
            NUMBERVal := N1Number mod N2Number
        else
            put "***Error in Grok: Bad NUMBER op: ", N1Number, " ",
                NUMBEROp, " ", N2Number
            NUMBERVal := 0
        end if
        assignSTRINGVar (N0Name, realstr (NUMBERVal, 0))

    elsif noWords = 5 and words (2) = "=" and isCOMPAREOp (words (4)) and
            isRel (words (3)) and isRel (words (5)) then
        % Form: N0 = N1 COMPAREOp N2    ie, relation compare

        const N0Name := words (1)
        const N1Name := words (3)
        const N2Name := words (5)
        const COMPAREOp := words (4)

        const N1Number := nameNum (N1Name)
        const N2Number := nameNum (N2Name)

        const COMPAREVal := relComparison (COMPAREOp, N1Number, N2Number)

        assignSTRINGVar (N0Name, bool2BOOL (COMPAREVal))

    elsif noWords = 5 and words (2) = "=" and isCOMPAREOp (words (4)) and
            isNUMBER (words (3)) and isNUMBER (words (5)) then
        % Form: N0 = N1 COMPAREOp N2    ie, numeric compare

        const N0Name := words (1)
        const N1Name := words (3)
        const N2Name := words (5)
        const COMPAREOp := words (4)

        const N1Number := evalNUMBER (N1Name)
        const N2Number := evalNUMBER (N2Name)
        var COMPAREVal : boolean
        if COMPAREOp = "==" or COMPAREOp = "=" then
            COMPAREVal := N1Number = N2Number
        elsif COMPAREOp = "~=" then
            COMPAREVal := N1Number ~= N2Number
        elsif COMPAREOp = "<" then
            COMPAREVal := N1Number < N2Number
        elsif COMPAREOp = ">" then
            COMPAREVal := N1Number > N2Number
        elsif COMPAREOp = "<=" then
            COMPAREVal := N1Number <= N2Number
        elsif COMPAREOp = ">=" then
            COMPAREVal := N1Number >= N2Number
        else
            put "***Error in Grok: Bad NUMBER op: ", N1Number, " ",
                COMPAREOp, " ", N2Number
            COMPAREVal := false
        end if
        assignSTRINGVar (N0Name, bool2BOOL (COMPAREVal))

    elsif noWords = 5 and words (2) = "=" and isCOMPAREOp (words (4)) and
            (isSetVar (words (3)) or isSetVar (words (5))) then
        % Form: N0 = S1 COMPAREOp S2    ie, set compare

        const N0Name := words (1)
        const S1SetName := words (3)
        const S2SetName := words (5)
        const COMPAREOp := words (4)

        var S1SetPtr : setPtr
        var S1SetIsTemp : boolean
        evalSet (S1SetName, S1SetPtr, S1SetIsTemp)

        var S2SetPtr : setPtr
        var S2SetIsTemp : boolean
        evalSet (S2SetName, S2SetPtr, S2SetIsTemp)

        const COMPAREVal := setComparison (COMPAREOp, S1SetPtr, S2SetPtr)

        assignSTRINGVar (N0Name, bool2BOOL (COMPAREVal))

        if S1SetIsTemp then
            freeSet (S1SetPtr)
        end if

        if S2SetIsTemp then
            freeSet (S2SetPtr)
        end if

    elsif noWords = 5 and words (2) = "=" and isCOMPAREOp (words (4)) then
        % Form: N0 = N1 COMPAREOp N2    ie, string compare

        const N0Name := words (1)
        const N1Name := words (3)
        const N2Name := words (5)
        const COMPAREOp := words (4)

        const N1Number := evalSTRING (N1Name)
        const N2Number := evalSTRING (N2Name)
        var COMPAREVal : boolean
        if COMPAREOp = "==" or COMPAREOp = "=" then
            COMPAREVal := N1Number = N2Number
        elsif COMPAREOp = "~=" then
            COMPAREVal := N1Number ~= N2Number
        elsif COMPAREOp = "<" then
            COMPAREVal := N1Number < N2Number
        elsif COMPAREOp = ">" then
            COMPAREVal := N1Number > N2Number
        elsif COMPAREOp = "<=" then
            COMPAREVal := N1Number <= N2Number
        elsif COMPAREOp = ">=" then
            COMPAREVal := N1Number >= N2Number
        else
            put "***Error in Grok: Bad NUMBER op: ", N1Number, " ",
                COMPAREOp, " ", N2Number
            COMPAREVal := false
        end if
        assignSTRINGVar (N0Name, bool2BOOL (COMPAREVal))

    elsif noWords = 5 and words (2) = "=" and
            (words (4) = "and" or words (4) = "&" or words (4) = "or") and
            isBOOL (words (3)) and isBOOL (words (5)) then
        % Form: B0 = B1 BOOLOp B2    ie, numeric multiply

        const B0Name := words (1)
        const B1Name := words (3)
        const B2Name := words (5)
        const BOOLOp := words (4)

        const B1Bool := evalBOOL (B1Name)
        const B2Bool := evalBOOL (B2Name)
        var BoolVal : boolean
        if BOOLOp = "and" or BOOLOp = "&" then
            BoolVal := B1Bool and B2Bool
        elsif BOOLOp = "or" then
            BoolVal := B1Bool or B2Bool
        else
            put "***Error in Grok: Bad BOOL op: ", % B1Bool, " ",
                BOOLOp % , " ", B2Bool
            BoolVal := true
        end if
        assignSTRINGVar (B0Name, bool2BOOL (BoolVal))

    elsif noWords = 5 and words (2) = "=" and (
            words (4) = "cat") then
        % Form: S0 = S1 cat S2    ie, string catenation

        const S0Name := words (1)
        const S1Name := words (3)
        const S2Name := words (5)

        const S1STRING := evalSTRING (S1Name)
        const S2STRING := evalSTRING (S2Name)

        assignSTRINGVar (S0Name, S1STRING + S2STRING)

    elsif noWords = 4 and words (2) = "=" and (
            words (3) = "triple2relname") then
        % Form: S0 = triple2relname S1    eg, "(_x,_R,_y_)" to "R"

        const S0Name := words (1)
        const S1Name := words (4)

        var S1STRING := evalSTRING (S1Name)
        
        const prefLoc := index (S1STRING, edgePrefix)
        if prefLoc = 1 then
            S1STRING := S1STRING (lenEdgePrefix+1 .. *)
        else
            put "***Error: Bad triple in 'triple2relname': ", S1STRING
        end if
        
        const suffLoc := index (S1STRING, edgeSuffix)
        if suffLoc ~= 0 & suffLoc = length (S1STRING) - lenEdgeSuffix + 1 then
            S1STRING := S1STRING (1 .. length (S1STRING) - lenEdgeSuffix)
        else
            put "***Error: Bad triple in 'triple2relname': ", S1STRING        
        end if
        
        const sep1 := index (S1STRING, edgeSeparator)
        if sep1 ~= 0 then
            S1STRING := S1STRING (sep1 + lenEdgeSeparator .. *)
        else
            put "***Error: Bad triple in 'triple2relname': ", S1STRING        
        end if
        
        const sep2 := index (S1STRING, edgeSeparator)
        if sep2 ~= 0 then
            S1STRING := S1STRING (1 .. sep2 - 1)
        else
            put "***Error: Bad triple in 'triple2relname': ", S1STRING        
        end if
        
        const rel := S1STRING
        assignSTRINGVar (S0Name, rel)

        %-------------Operations on Sets-------------

    elsif noWords = 5 and words (2) = "=" and words (4) = "+"
            then % Form: s = t + u
        % Warning: All cases of "+" not caught before, fall into here;
        % ie, no explicit check that t and u are sets.
        const setVarName := words (1)
        const leftName := words (3)
        const rightName := words (5)
        var left := evalSetAsTemp (leftName)
        var right : setPtr
        var rightIsTemp : boolean
        evalSet (rightName, right, rightIsTemp)

        left := setAdd (left, right)

        if verbal then
            put "Union contains ", listNodes (left).size, " items"
        end if
        assignSetVar (setVarName, left)
        %  Do not free left (used by the set variable)
        if rightIsTemp then
            freeSet (right)
        end if

    elsif noWords >= 5 and words (2) = "=" and words (3) = "{"
            and words (noWords) = "}" and noWords mod 2 = 1 then
        % Form s = { s1 , s2 , s3  ... }  (We hope)
        %      1 2 3 4  5 6  7  8 ...  n
        var okCommas := true
        % for i : 5 .. noWords - 2 by 2
		var i := 5
		loop
			exit when i > noWords - 2
            if words (i) ~= "," then
                put "***Bad token: '", words (i)
                okCommas := false
            end if
			i += 2
        % end for
		end loop
        if not okCommas then
            put "***Bad set constructor: " ..
            for j : 1 .. noWords
                put words (j), " " ..
            end for
            put ""
        else
            const setVarName := words (1)
            const leftName := words (4)
            var left := evalStringAsSingletonTemp (leftName)
            var right : setPtr
            var rightIsTemp : boolean
            % for i : 6 .. noWords - 1 by 2
			var k := 6
			loop
				exit when k > noWords - 1
                const rightName := words (k)
                evalStringAsSingleton (rightName, right, rightIsTemp)
                left := setAdd (left, right)
				k += 2
            % end for
			end loop

            if verbal then
                put "Union contains ", listNodes (left).size, " items"
            end if
            assignSetVar (setVarName, left)
            %  Do not free left (used by the set variable)
            if noWords > 5 & rightIsTemp then
                freeSet (right)
            end if
        end if

    elsif noWords = 5 and words (2) = "=" and
            words (4) = "-" then % Form: s = t - u

        const setVarName := words (1)
        const leftName := words (3)
        const rightName := words (5)

        if rightName not= "ID" then
            var left := evalSetAsTemp (leftName)
            var right : setPtr
            var rightIsTemp : boolean
            evalSet (rightName, right, rightIsTemp)

            left := setSubtract (left, right)

            if verbal then
                put "Difference contains ", listNodes (left).size, " items"
            end if
            assignSetVar (setVarName, left)
            %  Do not free left (used by the set variable)
            if rightIsTemp then
                freeSet (right)
            end if
        else
            if verbal then
                put "Special case of s = R - ID"
            end if
            assignRelSubtractID (setVarName, leftName)
        end if

    elsif noWords = 5 and words (2) = "=" and
            words (4) = "\^" then % Form: s = t ^ u
        const setVarName := words (1)
        const leftName := words (3)
        const rightName := words (5)
        var left := evalSetAsTemp (leftName)
        var right : setPtr
        var rightIsTemp : boolean
        evalSet (rightName, right, rightIsTemp)

        left := setIntersect (left, right)

        if verbal then
            put "Intersection contains ", listNodes (left).size, " items"
        end if
        assignSetVar (setVarName, left)
        %  Do not free left (used by the set variable)
        if rightIsTemp then
            freeSet (right)
        end if

    elsif noWords = 5 and words (2) = "=" and words (4) = "X"
            then % Form: R = s X t

        const relName := words (1)
        const relNumber := nameNum (relName)
        const leftSetVar := words (3)
        const rightSetVar := words (5)
        var left : setPtr
        var isLeftTemp : boolean
        evalSet (leftSetVar, left, isLeftTemp)
        var right : setPtr
        var isRightTemp : boolean
        evalSet (rightSetVar, right, isRightTemp)
        addCrossProduct (left, relNumber, right)
        if isLeftTemp then
            freeSet (left)
        end if
        if isRightTemp then
            freeSet (right)
        end if

    elsif noWords = 5 and words (2) = "=" and words (3) = "prefix"
            then % Form: s = prefix P t
        const leftName := words (1)
        const prefixName := evalSTRING (words (4))
        const rightName := words (5)
        var right : setPtr
        var isRightTemp : boolean
        evalSet (rightName, right, isRightTemp)
        const prefixSet := subsetWithPrefix (prefixName, right)
        assignSetVar (leftName, prefixSet)
        if isRightTemp then
            freeSet (right)
        end if

    elsif noWords = 5 and words (2) = "=" and words (3) = "suffix"
            then % Form: s = suffix t S
        const leftName := words (1)
        const rightName := words (4)
        const suffixName := evalSTRING (words (5))
        var right : setPtr
        var isRightTemp : boolean
        evalSet (rightName, right, isRightTemp)
        const suffixSet := subsetWithSuffix (suffixName, right)
        assignSetVar (leftName, suffixSet)
        if isRightTemp then
            freeSet (right)
        end if

    elsif noWords = 4 and words (2) = "=" and words (3) = "id"
            then % Form: R0 = id s

        shouldBeSet (words (4))
        const R0Number := nameNum (words (1))
        const orgSetName := words (4)
        var orgSetPtr : setPtr
        var orgSetIsTemp : boolean
        evalSet (orgSetName, orgSetPtr, orgSetIsTemp)

        const oldDBSize := DBSize
        for i : 1 .. listNodes (orgSetPtr).size
            const orgItem := listNodes (orgSetPtr).list (i)
            insertDB (orgItem, R0Number, orgItem)
        end for

        const addCount := DBSize - oldDBSize
        if verbal then
            put "   Added ", addCount, " tuples. DBSize is ", DBSize
        end if

        if orgSetIsTemp then
            freeSet (orgSetPtr)
        end if
        insertIntoListUnique (relNames, relNamesSize, R0Number)

        %-----Form: x = y -------------------------


    elsif noWords = 3 and words (2) = "=" and isRel (words (3))
            then % Form: R0 = R1
        const R0Name := words (1)
        const R1Name := words (3)

        const R0Number := nameNum (R0Name)
        const R1Number := nameNum (R1Name)
        var noAdded := 0
        for i : 1 .. DBSize
            if DBRel (i) = R1Number then
                noAdded += 1
                if progress & noAdded mod 1000 = 0 then
                    put INDENT, noAdded : 3, " ", numName (DBSrc (i)),
                        " ",
                        numName (DBRel (i)),
                        " ", numName (DBTrg (i))
                end if
                insertDB (DBSrc (i), R0Number, DBTrg (i))
            end if
        end for
        insertIntoListUnique (relNames, relNamesSize, R0Number)
        if verbal then
            put "Added ", noAdded, " tuples for relation ", R0Name
        end if

        %------Danger: order of the following case clauses is critical --------

    elsif noWords = 3 and words (2) = "=" and isSTRINGLiteral (words (3))
            then % Form: stringvar1 = stringLiteral
        const leftName := words (1)
        const stringLiteral := words (3)
        var right := unquote (stringLiteral)
        assignSTRINGVar (leftName, right)
        % Do not free right (used by the set variable)

    elsif noWords = 3 and words (2) = "=" and isSTRINGVar (words (3))
            then % Form: stringvar1 = stringvar2
        const leftName := words (1)
        const rightName := words (3)
        var right := STRINGVars (findSTRINGVar (rightName)).String
        assignSTRINGVar (leftName, right)
        % Do not free right (used by the set variable)

    elsif noWords = 3 and words (2) = "=" and
            (isNUMBER (words (3)) or isBOOL (words (3)))
            then % Form: stringvar1 = NUMBEROrBOOLLiteral
        const leftName := words (1)
        var right := words (3)
        assignSTRINGVar (leftName, right)

    elsif noWords = 3 and words (2) = "=" and isSetVar (words (3))
            then % Form: s = t
        const leftName := words (1)
        const rightName := words (3)
        var right := evalSetAsTemp (rightName)
        if verbal then
            put "Set contains ", listNodes (right).size, " items"
        end if
        assignSetVar (leftName, right)
        % Do not free right (used by the set variable)

    elsif noWords = 3 and words (2) = "=" then
        % Form: s = t    Where t is a singleton set, given by literal
        const leftName := words (1)
        const rightName := words (3)
        var right := evalSetAsTemp (rightName)
        if verbal then
            put "Set contains ", listNodes (right).size, " items"
        end if
        assignSetVar (leftName, right)

    else
        handledCommand := false
    end if

end assignOpCommands

