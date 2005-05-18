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
		det = +],top=[idx=I,label=L, det = + , wh = - ]]
	};
	<sem>{
		L:P(I) 
	      }*=[idx=I,rel=P]
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
	      }*=[idx=I,rel=P]
}
% le depart de X 
% Generation Hack: the determiner is included in the tree

class ** relationalNoun
declare	 ?xRoot ?xDet ?xN ?xAnchor ?xPP  ?xPrep ?xArg ?I ?L ?P ?E ?X ?Theta1
{
  <syn>{
	node xRoot(color=black)[cat = n,bot=[pers = 3,det = +
	],top=[idx=E, det = + , wh = - ]]{
	     node xDet(color=black,name=det)[cat = det]
	     node xN(color=black)[cat = n, bot = [det = - ]]{
		  node xAnchor(color = black,mark = anchor)[cat = n]
		  node xPP(color = black)[cat = pp]{
		       node xPrep(color = black,name = prep)[cat = p]
		       node xArg(color = black,mark = subst)[cat = n,
		       top = [idx = X]]}}}};
	<sem>{
		L:P(E); L:Theta1(E,X) 
	      }*=[evt=E,rel=P,arg1=X,theta1=Theta1]
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
	      }*=[idx=I]
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
	     }*=[idx=I]
}
