*ENTRY : depart
*CAT : n
*SEM : partir<agent>
*ACC : 1
*FAM : relationalNoun
*FILTERS : []
*EX:{}
*EQUATIONS: 
*COANCHORS: 
det -> le/v
prep -> de/p

*ENTRY : parte
*CAT : v
*SEM : partir<agent>
*ACC : 1
*FAM : n0V
*FILTERS : []
*EX:{}
*EQUATIONS: 
*COANCHORS: 


% Prepositions

*ENTRY : de
*CAT : p
*SEM : with
*ACC : 1
*FAM : prepositionN
*FILTERS : []
*EX:{}
*EQUATIONS:
*COANCHORS:

*ENTRY : s etonne
*CAT : v
*SEM : etonner<exp,cause>
*ACC : 1
*FAM : n0ClVden1
*FILTERS : []
*EX:{}
*EQUATIONS: 
*COANCHORS: 

*ENTRY : etonne
*CAT : v
*SEM : etonner<cause,exp>
*ACC : 1
*FAM : n0Vn1
*FILTERS : []
*EX:{}
*EQUATIONS: 
*COANCHORS: 

*ENTRY : frappe
*CAT : v
*SEM : etonner<cause,exp>
*ACC : 1
*FAM : n0Vn1
*FILTERS : [passiveShort = -]
*EX:{}
*EQUATIONS: 
*COANCHORS: 

*ENTRY : etonnement
*CAT : n
*SEM : etonner<exp,cause>
*ACC : 1
*FAM : n0vPNpn1
*FILTERS : []
*EX:{}
*EQUATIONS: 
*COANCHORS: 
vsup -> eprouve/v
vppPrep -> de/p
obliquePrep -> de/p

*ENTRY : passion
*CAT : n
*SEM : passionner<exp,cause>
*ACC : 1
*FAM : n0vPNpn1
*FILTERS : []
*EX:{}
*EQUATIONS: 
*COANCHORS: 
vsup -> eprouve/v
vppPrep -> de/p
obliquePrep -> pour/p

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
