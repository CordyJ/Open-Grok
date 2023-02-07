% file "pigsort.tu"

% This pigeon hole sort assumes that the Index array locates 'last'
% elements in the 'contents' array.  The result of the sort is that
% the Index array elements are re-ordered, so that in order they
% located the lowest to highest elements in the range.

% This sort is written such that it can be used repeatedly in
% a radix sort.

% unit
% module Pigeon

    procedure PigeonHoleSort (contents : array 1 .. * of num, last : int,
            var Index, tempIndex : array 1 .. * of int, % size = DBMax
            var first : array 1 .. * of int, % size = maxNames
            var next : array 1 .. * of int) % size = DBMax
            
        assert last <= DBMax
            
        assert upper (Index) = DBMax
        assert upper (tempIndex) = DBMax
        assert upper (first) = maxNames
        assert upper (next) = DBMax

        var clock1, clock2 : int
        if timing then 
            clock (clock1)
        end if

        % Clear all the 'first' pointers (initialize lists for all values)
        for value : 1 .. nameCount
            first (value) := 0
        end for

        % Put each (pointer to) value into a pigeon hole
        for slot : 1 .. last
            const value := contents (Index (slot))
            % Fetch value, eg, src of relation
            next (slot) := first (value)
            % Link next free slot to old first value
            first (value) := slot
            % First location for this value is this new slot
        end for

        % Go backwards through slots, so values come out in
        % the right order, in case this is used in a radix sort.
        var indexPtr := last + 1
        for decreasing value : nameCount .. 1
            var slot := first (value)
            loop
                exit when slot = 0
                indexPtr -= 1
                tempIndex (indexPtr) := Index (slot)
                slot := next (slot)
            end loop
        end for
        assert indexPtr = 1
        for i : 1 .. last
            Index (i) := tempIndex (i)
        end for

        if timing then
            clock (clock2)
             put " Pigeon hole sort time [", last,"]: ", 
                (clock2 - clock1) / 1000:0:1
        end if
    end PigeonHoleSort

% end Pigeon


