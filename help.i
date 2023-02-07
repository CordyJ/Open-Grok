var helpFileNo := stdOutput

proc PUT (s : string)
    put : helpFileNo, s
end PUT

procedure putHelpTypesAndLiterals
    PUT ("Grok has three major types: relations, sets and strings")
    PUT ("")
    PUT ("Within strings, there are two sub-types: numbers and booleans")
    PUT ("")
    PUT ("String literals must be quoted")
    PUT ("Number literals are written as integers or in")
    PUT ("   floating point notation")
    PUT ("Boolean literals are 'true' and 'false'")
    PUT ("Strings with number (or boolean) values can be")
    PUT ("   treated as numbers (or as booleans)")
end putHelpTypesAndLiterals

procedure putHelpIO
    PUT ("Grok input/output commands")
    PUT ("")
    PUT ("Grok input commands:")
    PUT ("   getdb F              Read RSF data base from file F")
    PUT ("   adddb F              Add new tuples to data base from file F")
    PUT ("   readdb F             Read (compressed) RSF data base from file F")


    PUT ("   getta F              Read TA data base from file F")
    PUT ("   addta F              Add more TA to date base from file F")
    PUT ("   getset s [F [D]]     Read  set s [from file F [in dir D]]")
    PUT ("")
    PUT ("Grok output commands:")
    PUT ("   putdb F              Write RSF data base to file F")
    PUT ("   writedb F            Write data base (compressed) to file F")
    PUT ("   putset s [F [D]]     Write set s [to file F [in dir D]]")
    PUT ("   relToFile R F        Write rel R to file F")
    PUT ("   appendRelToFile R F  Append rel R to file F")
    PUT ("   putta F              Write TA data base to file F")
    PUT ("   put msg              Print message msg")
    PUT ("")
    PUT ("The file format for relations for get/put is ASCII triples, eg,")
    PUT ("      R a b   where R relates a to b (this format is called 'RSF').")



    PUT ("The file format for sets is blank-separated strings.")
    PUT ("Beware that the read/write (compressed) format isn't portable.")
    PUT ("")
    PUT ("When TA is read in, attributes are treated as relations (with @_")
    PUT ("added as a prefix).  The relations and attributes at the scheme level")

    PUT ("have $_ prepended to their names.")

end putHelpIO

procedure putHelpRelOperations
    PUT ("Grok relation operators:")
    PUT ("   R1 + R2              Union of relations")
    PUT ("   R1 - R2              Difference of relations")
    PUT ("   R1 \^ R2              Intersection of relations")
    PUT ("   R1 o R2              Relational composition [was previously '*']")



    PUT ("   inv R                Inverse of R")
    PUT ("   s . R                Project set s through rel R [was *]")
    PUT ("   R . s                Project set s through rel R backwards [was *]")



    PUT ("   R+                   Transitive closure of R")
    PUT ("   R*                   Reflective transitive closure of R")
    PUT ("   R1 cmp R2            Compare: cmp is == [or =], ~=, <, <=, >, >=")



    PUT ("   EMPTYREL             The empty relation")
    PUT ("   # R                  Cardinality (number of tuples) of R")
    PUT ("   dom R                Domain of R (a set)")
    PUT ("   rng R                Range of R (a set)")
    PUT ("   ent R                Range + domain of R (all entities in R)")
    PUT ("   id s                 Identity relation on set s")
    PUT ("   DOM                  Domain of all relations in data base (a set)")



    PUT ("   RNG                  Range of all relations in data base (a set)")



    PUT ("   ENT                  Range + domain of all relations (a set)")
    PUT ("   ID                   The identity relation (based on ENT)")
    PUT ("   head R               Head of R, eg (x y) in R produces ((x R y) head y)")
    PUT ("   tail R               Tail of R, eg (x y) in R produces (x tail (x R y))")
    PUT ("   rel2tripleset R      Make R in to a set, eg (x y) produces (x R y)")
    PUT ("   tripl2relname        Given '(x R y)' produce 'R'")
PUT ("")
end putHelpRelOperations

procedure putHelpOtherRels
    PUT ("Other relation commands:")
    PUT ("   relnames             Print the names of relations")
    PUT ("   delrel R             Remove R's tuples data base")
    PUT ("   listdb               Print entire set of relational tuples")
    PUT ("   sample R [b [n] ]    Print sample from R.  Begin with")
    PUT ("                          number b (default 1) & print n (10)")
    PUT ("   slice s              Remove tuples that reference other than s")
    PUT ("   wslice s             Remove tuples that do not reference s")
    PUT ("   delset s             Remove tuples from data base that reference s")



    PUT ("   delsetset s t        Remove tuples with source from s and target from t")



    PUT ("   deldups              Delete any duplicate tuples in data base")
    PUT ("   dbsize               Number of tuples in data base")
    PUT ("   cleardb              Delete all relations")
end putHelpOtherRels

procedure putHelpSetOperations
    PUT ("Grok set operators:")
    PUT ("   s1 + s2              Union of sets")
    PUT ("   s1 - s2              Difference of sets")
    PUT ("   s1 \^ s2              Intersection of sets")
    PUT ("   s1 cmp s2            Compare: cmp is == [or =], ~=, <, <=, >, >=")



    PUT ("   # s                  Cardinality (number of elements) of s")
    PUT ("   s1 X s2              Cross product s1 X s2 (a relation)")
    PUT ("   { e1, e2, ... }      Construct set containing e1, e2, ...")
    PUT ("   EMPTYSET             The empty set.  You can use { } instead")
    PUT ("   pick s               Pick a value (a string) from s")
    PUT ("   prefix Q s           Set of items in s with prefix Q")
    PUT ("   suffix s Q           Set of items in s with suffix Q")
end putHelpSetOperations

proc putHelpOtherSets
    PUT ("Other set commands:")
    PUT ("   setnames             Print the names of set variables")
    PUT ("   sample s [b [n] ]    Print sample from s.  Begin with")
    PUT ("                          number b (default 1) & print n (10)")
    PUT ("   clearsets            Delete all sets")

    PUT ("   addprefix Q s        Add prefix Q to items in set s")
    PUT ("   addsuffix s Q        Add suffix Q to items in set s")
    PUT ("   addprefixrel Q s     Add prefix Q to rel names in set s")
    PUT ("   addsuffixrel s Q     Add suffix Q to rel names in set s")
    PUT ("   addprefixsrc Q s     Add prefix Q to src of rel names in set s")
    PUT ("   addsuffixsrc s Q     Add suffix Q to src of rel names in set s")    
    PUT ("   addprefixtrg Q s     Add prefix Q to trg of rel names in set s")
    PUT ("   addsuffixtrg s Q     Add suffix Q to trg of rel names in set s")

    PUT ("   delprefix Q s        Delete prefix Q to items in set s")
    PUT ("   delsuffix s Q        Delete suffix Q to items in set s")   
    PUT ("   delprefixrel Q s     Delete prefix Q to rel names in set s")
    PUT ("   delsuffixrel s Q     Delete suffix Q to rel names in set s")
    PUT ("   delprefixsrc Q s     Delete prefix Q to src of rel names in set s")
    PUT ("   delsuffixsrc s Q     Delete suffix Q to src of rel names in set s")    
    PUT ("   delprefixtrg Q s     Delete prefix Q to trg of rel names in set s")
    PUT ("   delsuffixtrg s Q     Delete suffix Q to trg of rel names in set s")

    PUT ("   duplicate s          Creates new set with elements e.dup for")
    PUT ("                           each element e of s, with same")
    PUT ("                           connecting relations as in s")
end putHelpOtherSets

procedure putHelpNUMBEROperations
    PUT ("Grok number operators:")
    PUT ("   n1 + n2              Addition")
    PUT ("   n1 - n2              Subtraction")
    PUT ("   n1 * n2              Multiplication")
    PUT ("   n1 / n2              Real division")
    PUT ("   n1 div n2            Truncating division")
    PUT ("   n1 mod n2            Modulo")
    PUT ("   n1 cmp n2            Compare: cmp is == [or =], ~=, <, <=, >, >=")



end putHelpNUMBEROperations

procedure putHelpSTRINGOperations
    PUT ("Grok string operators")
    PUT ("   S1 cat S2            Catenate strings S1 and S2")
    PUT ("   S1 cmp S2            Compare: cmp is == [or =], ~=, <, <=, >, >=")



end putHelpSTRINGOperations

procedure putHelpBOOLOperations
    PUT ("Grok boolean operators:")
    PUT ("   b1 and b2            Conjunction")
    PUT ("   b1 or b2             Disjunction")
    PUT ("   not b                Boolean negation")
end putHelpBOOLOperations

procedure putHelpPrecedence
    PUT ("Levels of precedence are, from low to high (most binding):")
    PUT ("   and")
    PUT ("   or")
    PUT ("   cmp                     (==, ~=, <, <=, >, >=)")
    PUT ("   + - cat                 (additions)")
    PUT ("   * X ^ o . / mod div     (multiplications)")
    PUT ("   prefixOperators         (inv dom rng ent - not ~ # pick $)")
    PUT ("   suffixOperators         (*, +, ie, transitive closure)")
    PUT ("")
    PUT ("Parentheses (...) can be used to specify order of operations")
end putHelpPrecedence

procedure putHelpStatements
    PUT ("Statements:")
    PUT ("   x := expn            Assign expn to variable x (an identifier)")
    PUT (" $ x := expn            Assign expn to variable whose name is in x")



    PUT ("       (The 'x = expn' form is considered to be obsolete.)")
    PUT ("")
    PUT ("   if expn then        Cascaded 'if' statement, with optional")
    PUT ("       statements      'elsif' and 'else' clauses")
    PUT ("   {elsif expn then")
    PUT ("       statements}")
    PUT ("   [else")
    PUT ("       statements]")
    PUT ("   end if")
    PUT ("")
    PUT ("   loop                Loop statement, stopped by 'exit'")
    PUT ("       statements")
    PUT ("   end loop")
    PUT ("")
    PUT ("   for e in s          Repeat with each element e from set s")
    PUT ("       statements           (exits are supported)")
    PUT ("   end for")
    PUT ("")
    PUT ("   exit                Exit from loop/for")
    PUT ("   exit when expn      Exit from loop/for when expn is true")
    PUT ("   return              Return from current Grok script")
    PUT ("   quit                Quit (halt Grok)")
end putHelpStatements

procedure putHelpOther
    PUT ("Other Grok commands:")
    PUT ("varnames                List of all relation, set and string variables")



    PUT ("   $ str                String 'str' is taken as a variable's name")



    PUT ("   expn                 The value of expn (expression) is printed")
    PUT ("   exec F p1 ...        Exec file F (Grok script) with parameters p1 ...")



    PUT ("                             where t is each member of s")
    PUT ("   randdb t r n         Generate t tuples with r relations and")
    PUT ("                             n nodes.")
    % *PUT("   prolog               Emit relations in data base as Prolog facts")
    PUT ("   ?                    Print the help menu")
    PUT ("   reset                Restart (clear relations and variables)")
    PUT ("")
    PUT ("Use '\\' in last column to extend commands across line boundaries")
    % PUT("   relToFile R F Write relation R to file F")
    % *PUT("   prolog      Emit prolog for relations")
    % *PUT("   emitsil S [D]   Emit subsystem for S (and descendents) [in dir d]")
    % *PUT("   emitsupplier S Emit supplier S")
    % *PUT("   emitclient S D C Emit client C (subsystem S, descendent D)")
    % *PUT("   emittopss T C S E Emit top ss T (client C, supplier S, client C)")
    % *PUT("   elide S     Elide all contents of subsystem S")
    % *PUT("3 Tobey specific commands (based on access and declare):")
    % *PUT("   ssprefix P  From prefix P, emit subsystem SIL")
    % *PUT("   ssfile F S  Emit subsystem SIL: contents in file F,")
    % *PUT("                        name of ss is S")
    % *PUT("   driver i1 ... F  From driver items d1 ..., put contained")
    % *PUT("                        files in F")

    % * means the routine has been commented out in Grok (Dec 98)

end putHelpOther

procedure putHelpOptions
    PUT ("Setting Grok options:")
    PUT ("     option Q    Set option Q")
    PUT ("The options that can be set include:")
    PUT ("     timing      Print time to complete each operation")
    PUT ("     echo        Echo each command (useful for tracing scripts)")
    PUT ("     debug       Interpret 'debug comand' as 'command' (default off)")



    PUT ("     verbal      Print out extra info during commands")
    PUT ("     progress    Show progress on large operations (verbose)")
    PUT ("     optimize    Run fast (default)")
    PUT ("     heapsort    Use older sort (new pigeon hole sort is faster)")
    PUT ("The first six of these are")



    PUT ("turned off using 'no', as in 'option notiming'.")
    PUT ("The heapsort option can be turned off by 'option pigeonsort'")
    PUT ("The original slow operations can be turned on by 'option nosort'")
end putHelpOptions

proc putHelpLimits
    PUT ("Limits on Sizes in Grok")
    PUT ("")
    PUT ("As of January 1998, many data sizes are dynamically increased to")
    PUT ("up to the maximum available memory.")
    PUT ("The dynamically adjusted sizes are for:")
    PUT ("       Number of tuples")
    PUT ("       Number of names")
    PUT ("          (Along with internal tables using for sorting, etc.)")
    PUT ("Sizes which are statically fixed are:")
    PUT ("       Maximum length of name; strSize=" + intstr (strSize) +
        " bytes, can increase to 256 at most")
    PUT ("       Maximum elements in a set; shortListMax=" +
        intstr (shortListMax))
    PUT ("       Maximum number of sets; maxSets=" + intstr (maxSets))
    PUT ("       Maximum tokens on a source Grok line; maxTokens=" +
        intstr (maxTokens))
end putHelpLimits

proc putHelpVersion
    PUT ("Grok Version")
    PUT ("")
    PUT ("This documents version R34 of Grok")
    PUT ("Modified 11 Apr 2000 99 by R.C. Holt")
    PUT ("")
    PUT ("Grok was written in the Turing language by R.C. Holt.  V. Tzerpos")
    PUT ("has written some parts of it.")
end putHelpVersion

forward procedure putHelpPart (partNo : int)

proc putAllOfHelpToFile
    put "Give name of file to receive help info: " ..
    var fileName : string
    get fileName : *
    open : helpFileNo, fileName, put
    if helpFileNo <= 0 then
        helpFileNo := stdOutput
        put "***Error: Can't open file '", fileName, "'"
        return
    end if
    put : helpFileNo, "GROK: SUMMARY OF FEATURES", skip
    for i : 1 .. 14
        put : helpFileNo, skip, i, ". " ..
        putHelpPart (i)
    end for
    put "   Wrote 'help' info to file '", fileName, "'"
end putAllOfHelpToFile

body procedure putHelpPart % (partNo : int)
    case partNo of
        label 0 :
            putHelpTypesAndLiterals
        label 1 :
            putHelpIO
        label 2 :
            putHelpRelOperations
        label 3 :
            putHelpOtherRels
        label 4 :
            putHelpSetOperations
        label 5 :
            putHelpOtherSets
        label 6 :
            putHelpNUMBEROperations
        label 7 :
            putHelpSTRINGOperations
        label 8 :
            putHelpBOOLOperations
        label 9 :
            putHelpPrecedence
        label 10 :
            putHelpStatements
        label 11 :
            putHelpOther
        label 12 :
            putHelpOptions
        label 13 :
            putHelpLimits
        label 14 :
            putHelpVersion
        label 15 :
            putAllOfHelpToFile
        label :
            put "Exiting from Help"
    end case
end putHelpPart

proc putHelp
    helpFileNo := stdOutput
    put "Welcome to 'help' for GROK."
    put "Please type the number for kind of help wanted:"
    put "  0. Types and literals"
    put "  1. File input/output"
    put "  2. Basic relation operations"
    put "  3. Other relation operations"
    put "  4. Set operations"
    put "  5. Other set operations"
    put "  6. Number operations"
    put "  7. String operations"
    put "  8. Boolean operations"
    put "  9. Precedence"
    put " 10. Statements"
    put " 11. Other operations"
    put " 12. Options"
    put " 13. Limits on sizes"
    put " 14. Version"
    put " 15. Output file containing all help info"
    put " 16. None (quit help)"
    put "Type number: " ..
    var command : string
    get command : *
    var commandNo : int
    % if strintok (command) then
        commandNo := strint (command)
    % else
        % commandNo := - 1
    % end if
    put ""
    var stop := false
    putHelpPart (commandNo)
end putHelp

