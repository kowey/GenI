%% Verbs

*ENTRY : aimer 
*CAT : v
*SEM : aimer<agent,theme>
*ACC : 1
*FAM : n0Vn1
*FILTERS : []
*EX:{}
*EQUATIONS: 
*COANCHORS:

*ENTRY : regarder
*CAT : v
*SEM : regarder<agent,theme>
*ACC : 1
*FAM : n0Vn1
*FILTERS : []
*EX:{}
*EQUATIONS: 
*COANCHORS:

*ENTRY : peser
*CAT : v
*SEM : peser
*ACC : 1
*FAM : n0Vn1
*FILTERS : [passivable = +]       
*EX:{}
*EQUATIONS:
*COANCHORS:

*ENTRY : avoir
*CAT : v
*SEM : avoir
*ACC : 1
*FAM : n0Vn1
*FILTERS : []
*EX:{}
*EQUATIONS:
*COANCHORS:

%% Proper nouns
*ENTRY : Jean
*CAT :    n
*SEM : jean
*ACC : 1
*FAM : propername
*FILTERS : []
*EX:{}
*EQUATIONS: anchor -> gen = m
*COANCHORS:

*ENTRY : Marie
*CAT :    n
*SEM : marie
*ACC : 1
*FAM : propername
*FILTERS : []
*EX:{}
*EQUATIONS: anchor -> gen = f 
*COANCHORS:

%% Determiners

*ENTRY : la
*CAT : d
*SEM : the
*ACC : 1
*FAM : stddeterminer
*FILTERS : []
*EX:{}
*EQUATIONS: anchor -> gen = f
            anchor -> num = sg
*COANCHORS:



*ENTRY : un
*CAT : d
*SEM : exists
*ACC : 1
*FAM : stddeterminer
*FILTERS : []
*EX:{}
*EQUATIONS: anchor ->  gen = m
            anchor -> num = sg
*COANCHORS:

%% Adjectives

*ENTRY : beau
*CAT : adj
*SEM : pretty
*ACC : 1
*FAM : adjectifEpithete
*FILTERS : []
*EX:{}
*EQUATIONS: 
*COANCHORS:

% Nouns

*ENTRY : telescope
*CAT : n
*SEM : telescope
*ACC : 1
*FAM : commonNoun
*FILTERS : []
*EX:{}
*EQUATIONS: anchor -> gen = m
            anchor -> num = sg
*COANCHORS:

*ENTRY : fille
*CAT : n
*SEM : girl
*ACC : 1
*FAM : commonNoun
*FILTERS : []
*EX:{} 
*EQUATIONS: anchor -> gen = f
            anchor -> num = sg
*COANCHORS:

% Prepositions

*ENTRY : avec
*CAT : p
*SEM : with
*ACC : 1
*FAM : prepositionN
*FILTERS : []
*EX:{}
*EQUATIONS:
*COANCHORS:

