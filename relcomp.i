% Work functions used for speedy version of rel composition (etc)
function LE_Trg1 (i, j : int) : boolean
    result DBTrg (Index1 (i)) <= DBTrg (Index1 (j))
end LE_Trg1

function LE_Src2 (i, j : int) : boolean
    result DBSrc (Index2 (i)) <= DBSrc (Index2 (j))
end LE_Src2

function LE_Tup1 (i, j : int) : boolean
    const I := Index1 (i)
    const J := Index1 (j)

    const ri := DBRel (I)
    const rj := DBRel (J)
    if ri < rj then
        result true
    end if
    if ri > rj then
        result false
    end if

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
end LE_Tup1

function LE_Tup2 (i, j : int) : boolean
    const I := Index2 (i)
    const J := Index2 (j)

    const ri := DBRel (I)
    const rj := DBRel (J)
    if ri < rj then
        result true
    end if
    if ri > rj then
        result false
    end if

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
end LE_Tup2

procedure deleteDuplicateTuples (first : int)
    const last := DBSize
    
    if first = 0 or first > last then
        return
    end if

    % Fill index array
    const range := last - first + 1
    var n := first
    for i : 1 .. range
        Index2 (i) := n
        n += 1
    end for

	PigeonHoleSort (DBTrg, range, Index2, tempIndex1, nameTemp1, tempIndex2)
	PigeonHoleSort (DBSrc, range, Index2, tempIndex1, nameTemp1, tempIndex2)
	PigeonHoleSort (DBRel, range, Index2, tempIndex1, nameTemp1, tempIndex2)

    % Mark repeated tuples
    const markedTup := - 1
    for j : 2 .. range
        const this := Index2 (j)
        const prev := Index2 (j - 1)
        if DBSrc (this) = DBSrc (prev) &
                DBRel (this) = DBRel (prev) &
                DBTrg (this) = DBTrg (prev) then
            DBRel (prev) := markedTup
        end if
    end for

    % Delete marked tuples
    var noDeleted := 0
    for i : first .. last
        if DBRel (i) = markedTup then
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

end deleteDuplicateTuples

procedure collectRelIndex (RelNumber : str, var RelCount : int,
        var Index : array 1 .. * of str)
    RelCount := 0
    for i : 1 .. DBSize
        if RelNumber = DBRel (i) then
            RelCount += 1
            Index (RelCount) := i
        end if
    end for
end collectRelIndex

% Relational composition using heap sort.
% Use index arrays to locate all of each of R1 and R2 tuples
% Sort Index1 array by target and Index2 by source,
% so we can effectively merge these two.
procedure relComposition (R0Number, R1Number, R2Number : str)
    const oldDBSize := DBSize

    % Fill index arrays for R1 and R2
    var R1Count, R2Count : int

    collectRelIndex (R1Number, R1Count, Index1)
    collectRelIndex (R2Number, R2Count, Index2)

	PigeonHoleSort (DBTrg, R1Count, Index1, tempIndex1, nameTemp1, tempIndex2)
	PigeonHoleSort (DBSrc, R2Count, Index2, tempIndex1, nameTemp1, tempIndex2)

    var addCount := 0
    var k1, k2 := 1 % Count through the 2 index arrays
    loop % Merge with common link from R1 to R2
        exit when k1 > R1Count or k2 > R2Count
        const trg1 := DBTrg (Index1 (k1))
        const src2 := DBSrc (Index2 (k2))
        if trg1 < src2 then
            loop % Advance the k1 counter till match R2 (or exhaust k1)
                k1 += 1
                exit when k1 > R1Count
                exit when DBTrg (Index1 (k1)) >= src2
            end loop
        end if
        exit when k1 > R1Count

        if trg1 > src2 then
            loop % Advance the k2 counter till match R1 (or exhaust k2)
                k2 += 1
                exit when k2 > R2Count
                exit when trg1 <= DBSrc (Index2 (k2))
            end loop
        end if
        exit when k2 > R2Count % No more matching links

        if DBTrg (Index1 (k1)) = DBSrc (Index2 (k2)) then
            const link := DBTrg (Index1 (k1))
            var lastLink1 := R1Count
            for i : k1 + 1 .. R1Count
                if DBTrg (Index1 (i)) not= link then
                    lastLink1 := i - 1
                    exit
                end if
            end for
            var lastLink2 := R2Count
            for i : k2 + 1 .. R2Count
                if DBSrc (Index2 (i)) not= link then
                    lastLink2 := i - 1
                    exit
                end if
            end for
            % For this matching link value, add all tuples to DB
            for m1 : k1 .. lastLink1
                for m2 : k2 .. lastLink2
                    insertDB (DBSrc (Index1 (m1)), R0Number,
                        DBTrg (Index2 (m2)))
                    addCount += 1
                    if progress & addCount mod 1000 = 0 then
                        put addCount : 3, " ",
                            numName (DBSrc (Index1 (m1))) : 25, " ",
                            numName (DBTrg (Index2 (m2)))
                    end if

                end for
            end for
            k1 := lastLink1 + 1
            k2 := lastLink2 + 1
        end if
        exit when k1 > R1Count or k2 > R2Count
    end loop
    if verbal then
        put "   Added ", addCount, " tuples. DBSize is ", DBSize
    end if
    insertIntoListUnique (relNames, relNamesSize, R0Number)
    deleteDuplicateTuples (oldDBSize + 1)
end relComposition

