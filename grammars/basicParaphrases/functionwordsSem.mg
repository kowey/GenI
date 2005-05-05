% Standard determiners : un, le, chaque (semantique = quantifieur)
% Prepositions (N attachment)

class determiner
export
	xR xFoot xAnc 
declare
	?xR ?xFoot ?xAnc ?fX ?fY ?fZ ?fT ?fU ?fW
{
	<syn>{
		node xR(color=red)[cat = n, bot = [def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU]]{
			node xAnc(color=red,mark=anchor)[cat = d,bot=[num = ?fY,gen = ?fZ]]
			node xFoot(color=red,mark=foot)[cat = n,top=[def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU]]
		}
	}
}

class pureDeterminer
import 
	determiner[]
{
	<syn>{
		node xR[bot = [det = +]];
		node xFoot[top = [det = -]]
	}
}

class stddeterminer
import 
	pureDeterminer[]
declare 
	?fR ?X ?LV ?LN ?LQ ?Q 
{
	<syn>{
		node xR[bot = [wh = ?fR],top=[idx=X,label=LV]];
		node xFoot [top = [wh = ?fR],bot=[idx=X,label=LN]]
	};
	<sem>{
	        LQ:Q(X,LN,LV)
	     }		
}

class prepositionN
declare 
	 ?LN ?X ?Y ?xR ?xFoot ?xGP ?xPrep ?xN ?P
{
	<syn>{
		node xR(color=red)[cat = n, bot = [idx = X, label = LN]]{
		 node xFoot(color=red,mark=foot) [cat = n, bot = [idx=X,label=LN]]
		 node xGP(color=red) [cat = gp]
		      {node xPrep(color=red,mark=anchor) [cat = p]
		       node xN(color=red,mark=subst) [cat = n, bot = [idx=Y]]
		       }
		       }		       
	};
	<sem>{
	        LN:P(X,Y)
	     }		
}