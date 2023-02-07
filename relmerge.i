function EQ_Pair12 (i, j : int) : boolean
    const I := Index1 (i)
    const J := Index2 (j)

    result DBSrc (I) = DBSrc (J) &
        DBTrg (I) = DBTrg (J)
end EQ_Pair12

function GE_Pair12 (i, j : int) : boolean
    const I := Index1 (i)
    const J := Index2 (j)

    const si := DBSrc (I)
    const sj := DBSrc (J)
    if si > sj then
        result true
    end if
    if si < sj then
        result false
    end if

    const ti := DBTrg (I)
    const tj := DBTrg (J)
    result ti >= tj
end GE_Pair12

function LE_Pair1 (i, j : int) : boolean
    const I := Index1 (i)
    const J := Index1 (j)

    const si := DBSrc (I)
    const sj := DBSrc (J)
    if si < sj then
        result true
    end if
    if si > sj then
        result false
    end if

    const ti := DBTrg (I)
    const tj := DBTrg (J)
    result ti <= tj
end LE_Pair1

function LE_Pair12 (i, j : int) : boolean
    const I := Index1 (i)
    const J := Index2 (j)

    const si := DBSrc (I)
    const sj := DBSrc (J)
    if si < sj then
        result true
    end if
    if si > sj then
        result false
    end if

    const ti := DBTrg (I)
    const tj := DBTrg (J)
    result ti <= tj
end LE_Pair12

function LE_Pair2 (i, j : int) : boolean
    const I := Index2 (i)
    const J := Index2 (j)

    const si := DBSrc (I)
    const sj := DBSrc (J)
    if si < sj then
        result true
    end if
    if si > sj then
        result false
    end if

    const ti := DBTrg (I)
    const tj := DBTrg (J)
    result ti <= tj
end LE_Pair2

function LT_Pair12 (i, j : int) : boolean
    const I := Index1 (i)
    const J := Index2 (j)

    const si := DBSrc (I)
    const sj := DBSrc (J)
    if si < sj then
        result true
    end if
    if si > sj then
        result false
    end if

    const ti := DBTrg (I)
    const tj := DBTrg (J)
    result ti < tj
end LT_Pair12

function GT_Pair12 (i, j : int) : boolean
    const I := Index1 (i)
    const J := Index2 (j)

    const si := DBSrc (I)
    const sj := DBSrc (J)
    if si > sj then
        result true
    end if
    if si < sj then
        result false
    end if

    const ti := DBTrg (I)
    const tj := DBTrg (J)
    result ti > tj
end GT_Pair12

function EQ_Pair23 (i, j : int) : boolean
    const I := Index2 (i)
    const J := Index3 (j)

    result DBSrc (I) = DBSrc (J) &
        DBTrg (I) = DBTrg (J)
end EQ_Pair23

function GE_Pair23 (i, j : int) : boolean
    const I := Index2 (i)
    const J := Index3 (j)

    const si := DBSrc (I)
    const sj := DBSrc (J)
    if si > sj then
        result true
    end if
    if si < sj then
        result false
    end if

    const ti := DBTrg (I)
    const tj := DBTrg (J)
    result ti >= tj
end GE_Pair23

function LE_Pair23 (i, j : int) : boolean
    const I := Index2 (i)
    const J := Index3 (j)

    const si := DBSrc (I)
    const sj := DBSrc (J)
    if si < sj then
        result true
    end if
    if si > sj then
        result false
    end if

    const ti := DBTrg (I)
    const tj := DBTrg (J)
    result ti <= tj
end LE_Pair23

function LT_Pair23 (i, j : int) : boolean
    const I := Index2 (i)
    const J := Index3 (j)

    const si := DBSrc (I)
    const sj := DBSrc (J)
    if si < sj then
        result true
    end if
    if si > sj then
        result false
    end if

    const ti := DBTrg (I)
    const tj := DBTrg (J)
    result ti < tj
end LT_Pair23

function GT_Pair23 (i, j : int) : boolean
    const I := Index2 (i)
    const J := Index3 (j)

    const si := DBSrc (I)
    const sj := DBSrc (J)
    if si > sj then
        result true
    end if
    if si < sj then
        result false
    end if

    const ti := DBTrg (I)
    const tj := DBTrg (J)
    result ti > tj
end GT_Pair23

% In Dec 98, I (Holt) combined relDiff, relIntersection and relComp
% into the following basic procedure.

% Basic procedure to implement the basis for relation:
%   intersection (^),
%   difference (-), and
%   comparisons (==, ~=, >, >=, <, <=)
%
% R0Number is set only for ^ and -.
% excessOnRight/Left are always set but are meaningful only for comparisons.

procedure relMerge (operator : string (2),
        R0Number, R1Number, R2Number : str,
        var excessOnLeft, excessOnRight : boolean)

    const isComparison :=
        operator = "==" or operator = "=" or operator = "~=" or
        operator = "<" or operator = "<=" or
        operator = ">" or operator = ">="

    const isIntersection := operator = "\^"
    const isDifference := operator = "-"

    assert isComparison or isIntersection or isDifference

    excessOnLeft := false
    excessOnRight := false

    %-----------Old bodies from relIntersection, relDiff, relComp,
    %           with flags used to differentiate their actions

    const oldDBSize := DBSize

    % Fill index arrays for R1 and R2
    var R1Count, R2Count : int
    collectRelIndex (R1Number, R1Count, Index1)
    collectRelIndex (R2Number, R2Count, Index2)

	PigeonHoleSort (DBTrg, R1Count, Index1, tempIndex1, nameTemp1, tempIndex2)
	PigeonHoleSort (DBSrc, R1Count, Index1, tempIndex1, nameTemp1, tempIndex2)

	PigeonHoleSort (DBTrg, R2Count, Index2, tempIndex1, nameTemp1, tempIndex2)
	PigeonHoleSort (DBSrc, R2Count, Index2, tempIndex1, nameTemp1, tempIndex2)

    var addCount := 0
    var k1, k2 := 1 % Count through the 2 index arrays
    loop % Merge with common link from R1 to R2
        exit when k1 > R1Count or (k2 > R2Count & ~isDifference) % Was bug!
        if k2 > R2Count or LT_Pair12 (k1, k2) then
            loop % Advance the k1 counter till match R2 (or exhaust k1)

                if isDifference then
                    addCount += 1
                    insertDB (DBSrc (Index1 (k1)), R0Number,
                        DBTrg (Index1 (k1)))
                end if
                excessOnLeft := true

                k1 += 1
                exit when k1 > R1Count
                exit when k2 <= R2Count & GE_Pair12 (k1, k2)
            end loop
        end if
        exit when k1 > R1Count

        if GT_Pair12 (k1, k2) then
            loop % Advance the k2 counter till match R1 (or exhaust k2)

                excessOnRight := true

                k2 += 1
                exit when k2 > R2Count
                exit when LE_Pair12 (k1, k2)
            end loop
        end if

        if k1 <= R1Count & k2 <= R2Count & EQ_Pair12 (k1, k2) then

            % Following two loops only needed if DB contains duplicates
            var lastLink1 := R1Count
            for i : k1 + 1 .. R1Count
                if not EQ_Pair12 (i, k2) then
                    lastLink1 := i - 1
                    exit
                end if
            end for
            var lastLink2 := R2Count
            for i : k2 + 1 .. R2Count
                if not EQ_Pair12 (k1, i) then
                    %-                 if not EQ_Pair12 (i, k1) then
                    lastLink2 := i - 1
                    exit
                end if
            end for

            if isIntersection then
                addCount += 1
                insertDB (DBSrc (Index1 (k1)), R0Number, DBTrg (Index2 (k2)))
            end if

            k1 := lastLink1 + 1
            k2 := lastLink2 + 1
        end if
    end loop
    if isIntersection or isDifference then
        if verbal then
            put "   Added ", addCount, " tuples. DBSize is ", DBSize
        end if
        insertIntoListUnique (relNames, relNamesSize, R0Number)
        % No dup's created, so no: deleteDuplicateTuples (oldDBSize, DBSize)
    else
        assert isComparison
        if k1 <= R1Count then
            excessOnLeft := true
        end if
        if k2 <= R2Count then
            excessOnRight := true
        end if
    end if
end relMerge

procedure relIntersection (R0Number, R1Number, R2Number : str)

    var excessOnLeft, excessOnRight : boolean % Unused dummy parameters
    relMerge ("\^", R0Number, R1Number, R2Number, excessOnLeft, excessOnRight)

end relIntersection


% Rel Diff (Clone of relIntersection except for placement of "insertDB")
procedure relDifference (R0Number, R1Number, R2Number : str)

    var excessOnLeft, excessOnRight : boolean % Unused dummy parameters
    relMerge ("-", R0Number, R1Number, R2Number, excessOnLeft, excessOnRight)

end relDifference

function relComparison (COMPAREOp : string (2), R1Number, R2Number : str) :
        boolean

    var excessOnLeft, excessOnRight : boolean
    var R0Number : str := -1 % Unused dummy parameter

    relMerge (COMPAREOp, R0Number, R1Number, R2Number,
        excessOnLeft, excessOnRight)

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
        put "***Error in Grok: Bad COMPARE op: ", R1Number, " ",
            COMPAREOp, " ", R2Number
        result false
    end if

end relComparison

