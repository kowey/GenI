%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%PREDICATIVE NOUNS (= basic support to make the grammar work, should be seriously extended)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class noun

declare
	?xN
{
	<syn>{
		node xN(color=red,mark=anchor)[cat = n,bot=[det = -,pers = 3,wh = -,bar = 0]]
	}
}

class nounWithCompl
declare 
	?xR ?xHead
{
	<syn>{
		node xR(color=black)[cat = n,bot=[bar = 2]]{
			node xHead(color=black,mark=anchor)[cat = n,bot=[bar = 0]]
		}
	}
}

%% s tree for intransitive nominal predicative and support verb construction  
%% "jean est un angoisse", "jean pousse un cri"
class n0vN[E,X,T1,P]
{	unaryRel[]*=[index=E,arg1=X,theta1=T1,rel=P] ;
	Subject[]*=[subjectI=X];
	NominalPredicativeform[]*=[vbI=E] 
}

% etre le chef du departement
class n0vNden1[E,X,Y,T1,T2,P]
{	binaryRel[]*=[index=E,arg1=X,theta1=T1,arg2=Y,theta2=T2,rel=P];
	Subject[]*=[subjectI=X];
	NominalPredicativeform[]*=[vbI=E];
	Genitive[]*=[genitiveI=Y]
}
%  faire un cadeau a marie
class n0vNan1[E,X,Y,T1,T2,P]
{	binaryRel[]*=[index=E,arg1=X,theta1=T1,arg2=Y,theta2=T2,rel=P];
	Subject[]*=[subjectI=X];
	NominalPredicativeform[]*=[vbI=E];
	Iobject[]*=[iobjectI=Y]
}
% commettre une agression contre marie 		
class n0vNpn1[E,X,Y,T1,T2,P]
{	binaryRel[]*=[index=E,arg1=X,theta1=T1,arg2=Y,theta2=T2,rel=P];
	Subject[]*=[subjectI=X];
	NominalPredicativeform[]*=[vbI=E];
	Oblique[]*=[obliqueI=Y]
}


