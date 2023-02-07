% unit
% module Globals

    var DBMax := 200000 % Max tuples (size of DB tables)
    var commandLinesMax := 200 % Max tuples (default no. of command lines)

    var maxNames := 10000 % 100000 % Size of Name Table

    const shortListMax := 50000 % 20000  (990520 Holt for Carriere)
	% Max size of any set (990323 for Bowman)

    const hashTableSize := 10000 div 2 % For fast name lookup
    const maxSets := 100 % Max number of sets
    const maxSTRINGs := 100 % Max number of STRING variables
    const maxTokens := 20 % Max tokens on a Grok command line (9903 to fit
            % Winoot stack limits --- was 40)
    const strSize := 4095 %103 	%% Turing+ 6.0 JRC 15mar2019 
	% Max string len; keep modest so sets aren't too big
    % Changed for Carriere (20 May 99 --- RCH)
    assert (strSize +1 ) mod 8 = 0  % Kludge, to avoid compiler alignment bug,
        % but likely this didn't avoid the bug.

    % Flags set by "option flag"
    var useHeapSort := false % Previously: New, fast way for rel *, ^ and -.
    var useUnixSort := false % Only used by testing old method
    var usePigeonSort := true % New, faster way.

    var echoCommands := false % Set in options statement
    var verbal := false % Lots of tracing messages
    var timing := false % Give time taken by each command
    var optimize := true % Check speed of finding relation names
    var debugging := false % Execute commands guarded by "debug"
    var progress := false % Show progress of large operations (is verbose)"

    type * STRING : string   % Values that are scalars
    
    type * charstr : string %% (strSize) % Replaces "str" when value
    %             actually stores characters in the string
    type * str : int % Was originally string (strSize), but now
    %             stores index to actual char's of the string
    type * num : int % Used to make sure value is a number


    % Rel = name of relation
    % Src (source) = domain of relation
    % Trg (target) = range of relation

    % In-core fact base
    var DBRel, DBSrc, DBTrg :  
		% flexible 
		array 1 .. DBMax of str
    var DBSize : int % Size of in-core fact base

    % Work arrays used for speedy version of rel composition (etc)
    % These are kludgy in the sense that they are
    % globally shared.  Be careful when using them.
    var Index1, Index2, Index3, tempIndex1, tempIndex2 :
         % flexible 
		 array 1 .. DBMax of int

    var nameTemp1 :  
		% flexible 
		array 1 .. maxNames of int

    procedure increaseDB
        const DBAddOn := 100000
        DBMax += DBAddOn
        put "***Grok increasing max tuples (DB size) to: ", DBMax
        % new DBRel, DBMax
        % new DBSrc, DBMax
        % new DBTrg, DBMax
        % new Index1, DBMax
        % new Index2, DBMax
        % new Index3, DBMax
        % new tempIndex1, DBMax
        % new tempIndex2, DBMax
    end increaseDB

    const stdOutput := - 1
    const stdInput := - 2

% end Globals
