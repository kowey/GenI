% semantics : [ jean(j) flatter(l) cause(l j) exp(l m) marie(m) ]

*ENTRY : flatter
*CAT : v
*SEM : flatter<cause,exp>         
*ACC : 1
*FAM : n0Vn1
*FILTERS : []
*EX:{Jean-flatte-Marie}
*EQUATIONS: 
*COANCHORS: 



*ENTRY : enorgueillit
*CAT : v
*SEM : flatter<cause,exp>
*ACC : 1
*FAM : n0Vn1
*FILTERS : []
*EX:{Jean-enorgueillit-Marie}
*EQUATIONS: 
*COANCHORS: 



ENTRY : orgueil
*CAT : n
*SEM : flatter<exp,cause>
*ACC : 1
*FAM : n0vPNden1
*FILTERS : []
*EX:{Marie-eprouve-de-l-orgueil-de-Marie}
*EQUATIONS: 
*COANCHORS: 
vsup -> eprouve/v
vppPrep -> de/p
obliquePrep -> de/p



ENTRY : orgueil
*CAT : n
*SEM : flatter<cause,exp>
*ACC : 1
*FAM : n0vNden1
*FILTERS : []
*EX:{Jean-est-l-orgueil-de-Marie}
*EQUATIONS: 
*COANCHORS: 
vsup -> etre/v





% Jean procure de l\'honneur à Marie
ENTRY : honneur
*CAT : n
*SEM : flatter<exp,cause>
*ACC : 1
*FAM : n0vPNan1
*FILTERS : []
*EX:{Jean-procure-de-l-honneur-a-Marie}
*EQUATIONS: 
*COANCHORS: 
vsup -> procure/v
vppPrep -> de/p

