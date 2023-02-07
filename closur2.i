% Compute transitive closure (Second version, using sorting)
% Dec 1997, Ric Holt

% Algorithm uses new fast Rel Composition algorithm.
% Algorithm attempts to minimize sorting
proc transitiveClosure2 (R0Number, R1Number : str)
    % Implement transitive closure: R0 = R1+

    % Definitions:  Sn = R**1 + R**2 + ... + R**n
    %               Un = R**n - Sn-1 (Unique new part of Sn due to R**n)
    % Hence:        Sn =  Sn-1 + Un
    % In this we consider R to be R1 and we compute
    %               R0 := R1+, which is the limit as n -> infinity of Sn
    % Basic algorithm is:

    % STEP 0.  n := 1
    % STEP 1.  S := EMPTY  (S represents Sn-1)
    % STEP 2.  U := R1     (U represents Un)
    % STEP 3.  loop           (n increases by 1 each time thru the loop
    %              assert S = Sn-1  &  U = Un  &  Sn = S + U
    % STEP 4.      P := U * R1
    % STEP 5.      S := S + U
    %              assert S = Sn+1
    % STEP 6.      U := P - S
    %              assert U = Un+1
    % STEP 7.      exit when U = EMPTY
    % STEP 8.      n += 1
    % STEP 9.  end loop
    %          assert S = R1+

    % We consider n to be the 'ply' level, ie, how deep in jumps
    % along arcs we have progressed.  Let P be the number of plies
    % to finish algorithm.  (Note: P <= N , the number of tuples).
    % The big O timing is P * C where C is the time to perform steps 4-8.
    % Since each step in 4-8 is bounded by time proportional to the number
    % of tuples (t) (actually, by the number of tuples in a relation),
    % the big O timing is P * t.  For a worst-case (dense) graph, this 
    % will be n**4, but effectively much faster for typical cases.
    % There are transitive closure algorithms that run in worst-case
    % time n**3, which might possibly be better.

    % In the implemented algorithm: n is redundant (but is computed anyway)

    % Use of Index arrays:   R1: Index1
    %                         S: Index3
    %                         P: Index2
    %                         U: Index2
    %                   delDups: Index2
    %                         T: tempIndex1 (temporary copy for S)

    % Make sure names are unique (use space inside name)
    const S := nameNum ("S ")
    const U := nameNum ("U ")
    const P := nameNum ("P ")

    % Collect R1 and sort index by src (for repeated use in U * R1);
    %           this will be the only sort of R1 in entire algorithm.
    % R1 uses Index1.
    var R1Count : int
    collectRelIndex (R1Number, R1Count, Index1)
    PigeonHoleSort (DBSrc, R1Count, Index1, tempIndex1, nameTemp1, tempIndex2)

    % STEP 0.  n := 1
    var N := 1

    const oldDBSize := DBSize

    % STEP 1.  S := EMPTY
    const SStart := DBSize + 1
    var SCount := 0

    % STEP 2.  U := R1
    var UCount := 0
    var UStart := DBSize + 1
    for i : 1 .. DBSize
        if DBRel (i) = R1Number then
            insertDB (DBSrc (i), U, DBTrg (i))
            UCount += 1
        end if
    end for
    % Eliminate any possible dup's in U, in case R1 had dup's
    deleteDuplicateTuples (UStart) % Could optimize this
    % Danger: 'deleteDuplicatePairsSpecial' clobbers work array Index2.
    UCount := DBSize - UStart + 1

    loop % STEP  3. loop  (in basic algorithm)
        exit when R1Count = 0 % Special case of null relation

        % Assert S = Sn-1  &  U = Un  &  Sn = S + U

        % STEP 4.  P := U * R1 (set up for this step)

        % Create index for U, sorted by target (for use in U * R1).
        % U uses Index2.  Note that U tuples are contiguous.
        const UStartLess1 := UStart - 1
        for i : 1 .. UCount
            Index2 (i) := i + UStartLess1
        end for
        PigeonHoleSort (DBTrg, UCount, Index2, tempIndex1, nameTemp1, tempIndex2)

        % Assert: R1 is sorted by src (done preceding loop body)

        % STEP 4.  P := U * R1 (Rel. Composition loop to carry out this step)
        var PCount := 0
        var PStart := DBSize + 1
        var kU, k1 := 1 % Count through the 2 index arrays (for U & R1)
        loop % Using common link (U trg to R1 src) merge U and R1
            const trgU := DBTrg (Index2 (kU))
            const src1 := DBSrc (Index1 (k1))
            if trgU < src1 then
                loop
                    % Advance the kU counter till match R1 (or exhaust kU)
                    kU += 1
                    exit when kU > UCount
                    exit when DBTrg (Index2 (kU)) >= src1
                end loop
            end if
            exit when kU > UCount

            if trgU > src1 then
                loop
                    % Advance the k1 counter till match U (or exhaust k1)
                    k1 += 1
                    exit when k1 > R1Count
                    exit when trgU <= DBSrc (Index1 (k1))
                end loop
            end if
            exit when k1 > R1Count % No more matching links

            if DBTrg (Index2 (kU)) = DBSrc (Index1 (k1)) then
                const link := DBTrg (Index2 (kU))

                var lastLinkU := UCount
                for i : kU + 1 .. UCount
                    if DBTrg (Index2 (i)) not= link then
                        lastLinkU := i - 1
                        exit
                    end if
                end for

                var lastLink1 := R1Count
                for i : k1 + 1 .. R1Count
                    if DBSrc (Index1 (i)) not= link then
                        lastLink1 := i - 1
                        exit
                    end if
                end for

                % For this matching link value, add all tuples to DB
                for mU : kU .. lastLinkU
                    for m1 : k1 .. lastLink1
                        PCount += 1
                        insertDB (DBSrc (Index2 (mU)), P,
                            DBTrg (Index1 (m1)))
                    end for
                end for
                kU := lastLinkU + 1
                k1 := lastLink1 + 1
            end if
            exit when kU > UCount or k1 > R1Count
        end loop % End of loop for P := U * R1 (STEP 4)
        % Assert: P now contains U * R1
        % In most cases, P includes dups,
        %       which will be effectively deleted in STEP 6. U := P - S

        % STEP 5. S := S + U  (set up by sorting U by pairs)
        % Assert: S and U are disjoint  (cause U is unique new part of Sn)
        % Assert: U's index is already in Index2; sort this index by pairs
        PigeonHoleSort (DBTrg, UCount, Index2, tempIndex1, nameTemp1, tempIndex2)
        PigeonHoleSort (DBSrc, UCount, Index2, tempIndex1, nameTemp1, tempIndex2)

        % STEP 5. S := S + U  (loop to do the union)
        % We know that S's tuples lie contiguously just beyond oldDBSize and
        % that U's tuples lie contiguously just beyond that.
        % To represent S, we simply extend (merge into) S's index,
        % so it also contains the items from U's index.

        % Create a copy T of S's index (Index3) to build a new index for S.
        var TCount := 0 % Temporary copy of S's index in tempIndex1
        % During merging of S and U, index for the union goes into tempIndex1

        % Count through the 2 index arrays
        kU := 1
        var kS := 1
        loop % Loop for S := S + U.  Maintains S's index sorted by pairs!
            % Consider sum to be written with U first: U + S
			loop
				exit when kU > UCount
				exit when kS <= SCount & GE_Pair23 (kU, kS)
				% Advance the kU counter till match S (or exhaust kU)
				% Found pair in U but not in S.
				% So, add it to T (temporary for S).
				TCount += 1
				tempIndex1 (TCount) := Index2 (kU)
				DBRel (Index2 (kU)) := S  % Redundant (for assertions)
				kU += 1
			end loop

			loop
				exit when kS > SCount
				exit when kU <= UCount & LE_Pair23 (kU, kS)
				% Advance the kS counter till match U (or exhaust kS)
				% Found pair in S but not in U.
				% So, add it to T (temporary for S).
				TCount += 1
				tempIndex1 (TCount) := Index3 (kS)
				DBRel (Index3 (kS)) := S
				kS += 1
			end loop
		
			exit when kS > SCount & kU > UCount

			% Since S and U are disjoint, the following is redundant
			if kU <= UCount & kS <= SCount & EQ_Pair23 (kU, kS) then

				% Following two loops only needed if DB contains duplicates
                var lastLinkU := UCount
                for i : kU + 1 .. UCount
                    put "###ERROR: Bug in Transitive Closure (1)"
                    quit
                    if not EQ_Pair23 (i, kS) then
                        lastLinkU := i - 1
                        exit
                    end if
                end for
                var lastLinkS := SCount
                for i : kS + 1 .. SCount
                    put "###ERROR: Bug in Transitive Closure (2)"
                    if not EQ_Pair23 (kU, i) then
                        lastLinkS := i - 1
                        exit
                    end if
                end for

                kU := lastLinkU + 1
                kS := lastLinkS + 1
            end if
        end loop % End of loop for union S + U in STEP 5
        assert TCount = SCount + UCount % T is merge of disjoint S and U
        SCount := TCount
        for i : 1 .. SCount % Copy S's new index into S's proper index
            Index3 (i) := tempIndex1 (i)
        end for

        % Assert S = Sn+1

        % STEP 6.  U := P - S  (Set up for Set Difference)
        % Assert: S is sorted by pairs (previous loop maintains this ordering)

        % Sort P by pairs (src & trg) for use in P - S
        % P uses Index2 (which is not longer used by U, which is now empty)
        % P is contiguous.
        const PStartLess1 := PStart - 1
        for i : 1 .. PCount
            Index2 (i) := i + PStartLess1
        end for
        PigeonHoleSort (DBTrg, PCount, Index2, tempIndex1, nameTemp1, tempIndex2)
        PigeonHoleSort (DBSrc, PCount, Index2, tempIndex1, nameTemp1, tempIndex2)

        % STEP 6.  U := P - S  (Loop to do Set Diff using sorted P and S)
        UCount := 0 % All of U's old elements were absorbed by S
        % Count through the 2 index arrays
        kS := 1
        var kP := 1
        loop % Loop for P - S.  Merge with common link from P to S
            exit when kP > PCount
            if kS > SCount or LT_Pair23 (kP, kS) then
                loop
                    % Advance the kP counter till match S (or exhaust kP)
                    % Found pair in P but not in S
                    % So, add it to U (for U := P - S),
                    const src := DBSrc (Index2 (kP))
                    const trg := DBTrg (Index2 (kP))
                    % Ignore dup's in P
                    if kP = PCount or
                            src not= DBSrc (Index2 (kP + 1)) or
                            trg not= DBTrg (Index2 (kP + 1)) then
                        DBRel (Index2 (kP)) := U % Just reuse P's element
                        UCount += 1
                    end if
                    kP += 1
                    exit when kP > PCount
                    exit when kS <= SCount & GE_Pair23 (kP, kS)
                end loop
            end if
            exit when kP > PCount
            assert kS <= SCount
            % To see this, inspect exits in preceding loop

            if GT_Pair23 (kP, kS) then
                loop
                    % Advance the kS counter till match P (or exhaust kS)
                    % Found pair in S but not in P; just ignore it.
                    kS += 1
                    exit when kS > SCount
                    exit when LE_Pair23 (kP, kS)
                end loop
            end if

            if kP <= PCount & kS <= SCount & EQ_Pair23 (kP, kS) then

                % Following two loops only needed if DB contains duplicates
                var lastLinkP := PCount
                for i : kP + 1 .. PCount
                    if not EQ_Pair23 (i, kS) then
                        lastLinkP := i - 1
                        exit
                    end if
                end for
                var lastLinkS := SCount
                for i : kS + 1 .. SCount
                    if not EQ_Pair23 (kP, i) then
                        lastLinkS := i - 1
                        exit
                    end if
                end for

                kP := lastLinkP + 1
                kS := lastLinkS + 1
            end if
        end loop % End of STEP 6. U := P - S

        % Delete relation P (value is now dead)
        deleteRelationTuples (P, PStart) % Could optimize this
        UStart := PStart
        assert UCount = DBSize - UStart + 1

        % Assert: U = Un+1

        % STEP 7. exit when U = EMPTY
        exit when UCount = 0

        % STEP 8. n += 1
        N += 1

    end loop % STEP 9.  end loop
    % Assert S: = R1+

    % Delete U
    DBSize := oldDBSize + SCount

    % Rename S as R0
    for i : oldDBSize + 1 .. DBSize
        assert DBRel (i) = S
        DBRel (i) := R0Number
    end for

    if verbal then
        put "   Added ", SCount, " tuples. DBSize is ", DBSize
    end if
    insertIntoListUnique (relNames, relNamesSize, R0Number)
end transitiveClosure2

procedure reflexiveTransitiveClosure (R0Number, R1Number : str)
    % Use (non-reflexive) transitive closure.
    const oldDBSize := DBSize % Assumes knowledge that
    %      transive closure algorithm leaves R0 tuples at end of DB arrays.
    transitiveClosure2 (R0Number, R1Number)
    % Then add the ID relation to R0 (then delete dup's)
    const notSeen := 1
    const seen := 0
    for i : 1 .. maxNames
        nameTemp1 (i) := notSeen
    end for
    for i : 1 .. oldDBSize
        nameTemp1 (DBSrc (i)) := seen
        nameTemp1 (DBTrg (i)) := seen
    end for
    for i : 1 .. maxNames
        if nameTemp1 (i) = seen then
            insertDB (i, R0Number, i)
        end if
    end for
    deleteDuplicateTuples (oldDBSize)  % Could optimize
end reflexiveTransitiveClosure
