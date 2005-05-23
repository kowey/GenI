% predicate trees for light verb constructions based on N, A or PP

% s(vn(v[cop = +] X))
class predicativeForm
export  xHead
declare
	?xR ?xVN ?xCop ?xLexCop ?xHead ?fX ?fY ?fZ ?fU ?fW

{
	<syn>{
		node xR(color=black)[cat = s,bot=[mode = ?fX]]{
			node xVN(color=black)[cat = vn,top=[mode = ?fX],bot=[mode = ?fY,gen = ?fU,num = ?fZ,pers=?fW]]{
				node xCop(color=black)[cat =
	v,top=[vsup = +, mode= ?fY,pers=?fW,num=?fZ]]{
		    node xLexCop(color=black,mark=flex,name=vsup)}
				node xHead(color=black)[cat = @{adj,n,pp},top=[gen=?fU,num=?fZ]]
			}
		}
	}
}

% Nominal predicate
% s(vn(v[vsup = +] n(det,<n>)) )
% pousser un cri

class ** NominalPredicativeform
import
	predicativeForm[]
declare
	?xDet ?xN
{
	<syn>{	node xHead[cat = n]{
			node xDet(color=black,name = vNdet)[cat = det]
			node xN(color=black,mark=anchor)[cat = n]}
}}

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
	?xPrep ?xPrep2 ?xN ?xDet ?xPred ?xLexDet
{
	<syn>{	node xHead[cat = pp]{
			node xPrep(color=black)[cat =
			p]{
			     node xPrep2(color=black,name = vppPrep,mark=flex)}
			node xN(color=black)[cat = n]{
			     node xDet(color=black)[cat=det]{
		    node xLexDet(color=black,mark=flex,name=vppDet)}
			     node xPred(color=black,mark=anchor)[cat=n]}
			}
		}
}


	