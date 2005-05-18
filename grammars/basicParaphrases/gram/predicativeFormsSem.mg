% predicate trees for light verb constructions based on N, A or PP

% s(vn(v[cop = +] X))
class predicativeForm
export  xHead
declare
	?xR ?xVN ?xCop ?xHead ?fX ?fY ?fZ ?fU ?fW

{
	<syn>{
		node xR(color=black)[cat = s,bot=[mode = ?fX]]{
			node xVN(color=black)[cat = vn,top=[mode = ?fX],bot=[mode = ?fY,gen = ?fU,num = ?fZ,pers=?fW]]{
				node xCop(color=black,name=vsup)[cat = v,top=[vsup = +, mode= ?fY,pers=?fW,num=?fZ]]
				node xHead(color=black)[cat = @{adj,n,pp},top=[gen=?fU,num=?fZ]]
			}
		}
	}
}

% Nominal predicate
% s(vn(v[vsup = +] <n>)) 
% pousser un cri

class ** NominalPredicativeform
import
	predicativeForm[]
{
	<syn>{	node xHead(mark=anchor)[cat = n]}
}

% Adjectival predicate
% s(vn(v[vsup = +] <adj>))
% etre apte

class ** AdjectivalPredicativeform
import
	predicativeForm[]
{
	<syn>{	node xHead(mark=anchor)[cat = adj]}
}

% Prepositional predicate
% s(vn(v[vsup = +] pp(prep <n>)))
% eprouver de l'etonnement

class ** PrepositionalPredicativeform
import
	predicativeForm[]
declare
	?xPrep ?xN
{
	<syn>{	node xHead[cat = pp]{
			node xPrep(color=black,name = vppPrep)[cat = p]
			node xN(color=black,mark=anchor)[cat = n]
			}
		}
}


	