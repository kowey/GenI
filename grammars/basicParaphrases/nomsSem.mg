%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%* Nominal forms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Jean, Paul
class propername
declare
	?xN ?I ?L ?P
{
	<syn>{
		node xN(color=red,mark=anchor)[cat = n,bot=[pers = 3,
		det = +],top=[idx=I,label=L, det = +]]
	};
	<sem>{
		L:P(I) 
	      }
}

% chien, chat, (la) France
class commonNoun
declare
	?xN ?I ?L ?P
{
	<syn>{
		node xN(color=red,mark=anchor)[cat = n,bot=[pers = 3, det = - ],top=[idx=I,label=L]]
	};
	<sem>{
		L:P(I) 
	      }
}

% moi, toi
class pronoun
declare
	?xN ?I
{
	<syn>{
	       node xN(color=red,mark=anchor)[cat = n,bot=[det = +],top=[idx=I]]		};
	<sem>{
		I
	      }
}

% je, tu,le, me, se 
class CliticT
declare
	?xCl ?I
{
	<syn>{
		node xCl(color=red,mark=anchor)[cat = cl,top=[idx=I]]	};
	<sem>{
		I  
	     }
}