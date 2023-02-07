% File "nametab.tu".  The Name Table

% Translates names to numbers and back.
% Distinct names get distinct numbers.

% This can be optimized two ways:
%    For speed, hash the string values to avoid the serial look up.
%           (this has been done).
%    For space, place all string contiguously in a flat char vector
%        to avoid wasted space at ends of strings.

% unit
% module nameTab

	var numName : 
		% flexible 
		array 1 .. maxNames of charstr
    var nextName : 
		% flexible 
		array 1 .. maxNames of int % - 1 .. maxNames
    var nameCount : int
    var hashTable : array 0 .. hashTableSize - 1 of int % 0 .. maxNames

    procedure increaseNames
        const namesAddOn := 10000
        maxNames += namesAddOn
        put "***Grok increasing max names to: ", maxNames
        flushstreams
        % new numName, maxNames
        % new nextName, maxNames
        % new nameTemp1, maxNames
    end increaseNames

    proc resetNameTab
        for i : 1 .. maxNames
            nextName (i) := - 1 % Only used for checking in assertions
        end for
        nameCount := 0

        for i : 0 .. hashTableSize - 1
            hashTable (i) := 0
        end for
    end resetNameTab

    resetNameTab

    proc writeNames (fileNo : int)
        for i : 1 .. nameCount
            write : fileNo, numName (i)
        end for
    end writeNames

    % Constants used to compute the hash of a name
    const nameSlices := 4
    const hashMultiplier := floor (sqrt (sqrt (hashTableSize)))
    const hashMultiplierTo2 := hashMultiplier * hashMultiplier
    const hashMultiplierTo3 := hashMultiplier * hashMultiplierTo2

    fcn hashName (name : charstr) : num
        % Compute hash from 4 char's in the string.
        % Multiply their ords to spread out these values
        %     across the hash table indexes

        const len := length (name)
        const lenDiv := len div nameSlices
        var hash : int := len
        if len ~= 0 then
            hash += ord (name (1)) +
                hashMultiplier * ord (name (1 + lenDiv)) +
                hashMultiplierTo2 * ord (name (1 + 2 * lenDiv)) +
                hashMultiplierTo3 * ord (name (len))
        end if

        hash := hash mod hashTableSize
        result hash
    end hashName

    fcn addNewName (name : charstr) : num
        if nameCount >= maxNames div 2 then 
            increaseNames
        end if
        nameCount += 1
        if nextName (24) > 20000 then
            put "***Error: Internal addressing bug in Grok name table"
            put "### addNewName name, nameCount: ", name,
                " ", nameCount, "    ", nextName (24)
        end if
        numName (nameCount) := name
        nextName (nameCount) := 0
        if nextName (24) > 20000 then
            put "***Error: Internal addressing bug in Grok name table"
            put "### addNewName name, nameCount: ", name, "=",
                numName (nameCount),
                " ", nameCount, "  nextName(24): ", nextName (24)
        end if
        result nameCount
    end addNewName

    fcn nameNum (name : charstr) : num

        const hash := hashName (name)
        assert 0 <= hash & hash <= hashTableSize

        var nameLoc := hashTable (hash)
        if nameLoc = 0 then % This hash bucket not previously used
            const newNameLoc := addNewName (name)
            hashTable (hash) := newNameLoc
            result newNameLoc
        else % This hash bucket previously used
            loop
                if nameLoc <= 0 or nameLoc > maxNames then
                    put "***Error: Grok failure, clobbered name table!!"
                    put "nameTab:###loop name, nameLoc: ", name, " ", nameLoc
                    put "nameCount: ", nameCount
                    put "hash: ", hash
                    var p := hashTable (hash)
                    var fi : int
                    open : fi, "grok.bug", put
                    put : fi, "Trace for locating bug in Grok", skip
                    put : fi, "loc" : 5, "numName" : 10, "nextName"
                    for i : 1 .. nameCount
                        put : fi, i : 5, " ", numName (i) : 10, "=",
                            hashName (numName (i)) : 7, " ",
                            nextName (i) : 8
                    end for
                    loop
                        const nm := numName (p)
                        const n := nextName (p)
                        put p : 10, " ", nm : 9, ":", hashName (nm) : 10,
                            " ", n : 10
                        p := n
                    end loop
                end if
                assert nameLoc > 0 and nameLoc <= maxNames
                if name = numName (nameLoc) then
                    % Name is already in Name Table
                    result nameLoc
                end if
                if nextName (nameLoc) = 0 then
                    % Add name to this bucket
                    const newNameLoc := addNewName (name)
                    nextName (nameLoc) := newNameLoc
                    if newNameLoc > 50000 then
                        put "###namtab: newNameLoc: ", newNameLoc
                    end if
                    result newNameLoc
                end if
                nameLoc := nextName (nameLoc)
            end loop
        end if
    end nameNum

    proc readNames (fileNo : int, noOfNames : int)
        loop
            exit when noOfNames <= maxNames
            increaseNames
        end loop

        if nameCount ~= 0 then
            put "***Error: Attempt to read when name table is non-empty"
        end if
        resetNameTab % Paranoia

        var clock1, clock2, clock3 : int
        if timing then
            clock (clock1)
        end if
        for i : 1 .. noOfNames
            read : fileNo, numName (i)
        end for
        if timing then
            clock (clock2)
            put " Time to read names: ", (clock2 - clock1) / 1000 : 0 : 1
        end if

        for i : 1 .. noOfNames % Rebuild hash table nextName links
            const hash := hashName (numName (i))
            var nameLoc := hashTable (hash)
            hashTable (hash) := i
            nextName (i) := nameLoc
        end for
        if timing then
            clock (clock3)
            put " Time to hash names: ", (clock3 - clock2) / 1000 : 0 : 1
        end if

        nameCount := noOfNames
    end readNames

% end nameTab

