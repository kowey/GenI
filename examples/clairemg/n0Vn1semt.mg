% -------------------------------------------------------
% A FRENCH METAGRAMMAR with semantics
% -------------------------------------------------------
% author = Benoit Crabbe ; Claire Gardent
% date = May 2004

%type declarations


%type declarations

type CAT={n,np,v,vn,s,pp,c,p,cl,adj,coord,d,à,de,par,que,qui,dont,si,il,se}
type PERSON=[1..3]
type GENDER={m,f}
type NUMBER={sg,pl}
type MODE={ind,inf,subj,imp,ppart}
type COLOR ={red,black,white}
type WH={rel,+,-}
type LABEL !
type CASE={nom,acc,acc3rd,dat3rd,dat,gen,locative,ce}

type MARK  = {subst,nadj,foot,anchor,flex,none}
type RANK  = [1..7]
type AUX = {etre,avoir,-}
type TENSE = {present,past}
type INVERSION = {+, n, -}


type ATOMIC=[
	mode : MODE,
 	num : NUMBER,
 	gen : GENDER,
 	pers : PERSON,
 	refl : bool,      
 	loc : bool,
 	wh : WH,
 	case : CASE,	
	pp-num: NUMBER,
	pp-gen: GENDER,
	control-num : NUMBER,
	control-pers : PERSON,
	control-gen : GENDER,
	aux-pass : bool,
	tense: TENSE,
	aux: AUX,
	det : bool,
	inv : INVERSION,
	cop : bool
]

%property declarations

property color : COLOR
property mark       : MARK
property extracted  : bool { extraction = + }
property xcomp : bool 
property rank : RANK{
        i_   = 1 ,
        ii_  = 2 ,
        iii_ = 3 ,
        iv_  = 4 ,
        v_  = 5 ,
	vi_ = 6
}

%feature declarations
feature idx : LABEL
feature top : ATOMIC
feature bot : ATOMIC
%feature mode : LABEL
%feature num : NUMBER
%feature pers : PERSON
%feature wh : WH
%feature loc : bool
%feature refl : bool
%feature case : CASE
type lexique = {run}
%feature declarations
feature subjectI : LABEL
feature objectI : LABEL
feature iobjectI : LABEL
feature cagentI : LABEL
feature genitiveI : LABEL
feature obliqueI : LABEL
feature locativeI : LABEL
feature phon : LABEL
feature vbI : LABEL
feature evt : LABEL
feature arg1 : LABEL
feature arg2 : LABEL
feature arg3 : LABEL
feature rel3 : LABEL
feature rel2 : LABEL
feature rel1 : LABEL
feature theta1 : LABEL
feature theta2 : LABEL
feature theta3 : LABEL
feature exp : LABEL
feature agent : LABEL
feature theme : LABEL
feature beneficiary : LABEL


%% Semantic classes
class semRel
export E 
declare ?Rel ?E !L0
      {
	<sem>{
		L0:Rel(E) 
	      }
	      *=[rel=Rel,evt=E]
	  }
class unaryRel
import semRel[]
declare ?X ?T1 !L1 
      {
	<sem>{
		 L1:T1(E,X)
	      }
	      *=[arg1=X,theta1=T1] 
	  }
class binaryRel
import unaryRel[]
export T2
declare ?X ?T2 !L2 
      {
	<sem>{
		 L2:T2(E,X)
	      }
	      *=[arg2=X,theta2=T2] 
	  }
class ternaryRel[]
import binaryRel[]
declare ?X ?T3 !L3 
      {
	<sem>{
		 L3:T3(E,X)
	      }
	      *=[arg3=X,theta3=T3]
}


%Class definitions

%* <Syntactic classes>
%%%%%%%%%%%%%%%%%%%%%%%%%%

class TopLevelClass
%# dummy class that provides a single root to the inh. hierarchy

class VerbalMorphology
export
   xS xVN xV
declare
   ?xS ?xVN ?xV ?fU ?fV ?fW ?fX ?I
{
        <syn>{
                node xS(color=black)[cat = s,bot=[mode=?fX]]{
                        node xVN(color=black)[cat = vn,top=[mode=?fX]]{
                                node xV(mark=anchor,color=black)[cat = v,idx=I]
                        }
                }
        }*=[vbI=I]       
}



class ** activeVerbMorphology
import
	VerbalMorphology[]
declare 
	?fX ?fY ?fZ ?fW ?fS ?fT ?fO ?fP
{
	<syn>{
		node xVN[bot=[mode=?fX, num = ?fY,gen = ?fZ,pers=?fW,pp-gen = ?fS,pp-num=?fT,inv = ?fO]]{
			node xV[top=[mode=?fX,num = ?fY,gen = ?fZ,pers=?fW,inv = ?fO,pp-gen= ?fS, pp-num = ?fT],bot=[inv = -]]
		}			
	}
}


class VerbalArgument
import 
	TopLevelClass[]
export
        xS xVN
declare
        ?xS ?xVN     
{
        <syn>{
                node xS(color=white)[cat=s]{
                        node xVN(color=white)[cat = vn]
                }               
        }
}

%% SUBJECT

class BoundedExtraction
import 
	VerbalArgument[]
	SubjectAgreement[]
	NonInvertedNominalSubject[]
export 
	xArg
declare 
	?xSubj ?xArg
{
	<syn>{
		xArg = xSubj
	};
	xSubj = xSubjAgr;
	xVN = xVNAgr
}


class CanonicalArgument
import 
	VerbalArgument[]

%# dummy for now

class EmptySubject
import 
	VerbalArgument[]

class ** InvertedNominalSubject
import 
	VerbalArgument[]
	SubjectAgreement[]
export
	xSubj
declare
	?xSubj ?I
{
		<syn>{
			node xS[bot = [wh = -,inv = n]]{
				node xVN
				,,,node xSubj(color = red,mark=subst)[cat = n,idx=I,top = [det = +]]
			}
		}*=[subjectI=I];
		xSubjAgr = xSubj;
		xVNAgr = xVN
}


class NonInvertedNominalSubject

declare
	?xS ?xVN ?fX ?VA
{
	<syn>{
		node xS[bot = [inv = ?fX]];
		node xVN[top = [inv = ?fX]]
	};
	VA = VerbalArgument[] ;
	xS = VA.xS ;
	xVN = VA.xVN
}

class ** InfinitiveSubject
import 
	EmptySubject[]
declare
	?fX ?fY ?fZ ?I
{
	<syn>{	
		node xS[bot=[control-num=?fX,control-gen=?fY,control-pers=?fZ]]{			
                            node xVN[idx=I,top=[num=?fX,gen=?fY,pers=?fZ,mode=inf]]
		}
	}*=[subjectI=I]
}
class ** ImperativeSubject
import 
	EmptySubject[]
declare  ?I
{
	<syn>{
		node xVN[idx=I,top=[mode=imp,pers=@{1,2}]]
	}*=[subjectI=I]
}

class SubjectAgreement
%# Accord Sujet -- Verbe
export
	xSubjAgr xVNAgr
declare 
	?xSubjAgr ?xVNAgr ?fX ?fY ?fZ
{
	<syn>{
		node xSubjAgr[top=[num = ?fX, gen = ?fY, pers = ?fZ]];
		node xVNAgr[top=[num = ?fX, gen = ?fY, pers = ?fZ]]
	}
}

class NonCanonicalObject 
%# This class sets up Past Participle agreement w/ auxiliary 'avoir'
export 
	xVNAgr xObjAgr
declare 
	?xObjAgr ?xVNAgr ?fX ?fY
{
	<syn>{
		node xVNAgr[cat=vn,bot=[pp-gen=?fX,pp-num=?fY]];
		node xObjAgr[top=[num=?fY,gen=?fX]]
	}
}

class RealisedNonExtractedSubject 
import 
	CanonicalArgument[]
	SubjectAgreement[]
	NonInvertedNominalSubject[]
export
	xSubj
declare
        ?xSubj ?fX ?fY ?fZ
{
	<syn>{
		node xS[bot=[wh = -]]{
			node xSubj(color=red)[top=[det = +]]
			node xVN(color=white)[cat = vn]
		};
		xSubj = xSubjAgr;
		xVN = xVNAgr
	}
}

class ** CanonicalSubject
import
        RealisedNonExtractedSubject[]
declare ?I
{
        <syn>{
             node xSubj(mark=subst)[cat = n,idx=I]
	 }*=[subjectI=I]
}

class ** CliticSubject
import
       	RealisedNonExtractedSubject[]
declare ?I
{
        <syn>{            
             node xSubj(color=red,mark=subst)[cat = cl,top=[case=nom,idx=I]]
        }*=[subjectI=I]
}

class ImpersonalSubject
import
        RealisedNonExtractedSubject[]
declare 
	?xil
{
	<syn>{                
             node xSubj[cat = cl,top=[num = sg, pers=3,gen=m]]{
		   node xil(color=red,mark=flex)[cat=il] 
             }                                                    	
	}
}

class CanonicalSententialSubjectFinite
%#Que Jean aille à la montagne plait à Marie
import
	CanonicalArgument[]
declare
	?xSubj ?xTop ?xComp ?xQue
{
	<syn>{
		node xS{
			node xTop(color=red)[cat=s]{
				node xComp(color=red)[cat=c]{
					node xQue(color=red,mark=flex)[cat=que]
				}
				node xSubj(color=red,mark=subst)[cat=s]
			}
			node xVN
		}
	}
}

class CanonicalSententialSubjectInFinitive 
%#Aller à la montagne plait à Marie
import 
	CanonicalArgument[]
declare
	?xSubj
{
	<syn>{
		node xS{
			node xSubj(color=red,mark=subst)[cat=s]
			node xVN
		}
	}
}


class ** whSubject
import 
	BoundedExtraction[]
declare ?I
{
	<syn>{
		node xS[bot=[wh = +]]{ %should be wh = + better w/ a default ?
			node xArg(color=red,mark=subst,extracted = +)[cat=n,idx=I,top=[wh = +]]
			node xVN
		}		
	}*=[subjectI=I]
}

class ** RelativeSubject
import 
	BoundedExtraction[]
declare
	?xfoot ?xRel ?I
{
	<syn>{
		node xRel(color=red)[cat = n]{
			node xfoot(color=red,mark=foot)[cat=n]
			node xS(mark=nadj)[bot=[mode=ind,wh = -]]{
				node xArg(color=red,mark=subst,extracted = +)[cat=n,idx=I,top=[wh=rel]]
				node xVN
			}
		}
	}*=[subjectI=I]
}

class ** CleftSubject
import 
	BoundedExtraction[]
declare
	?xSe ?xVNCleft ?xCl ?xVcleft ?xComp ?xCompl ?I

{
	<syn>{
		node xSe(color=red)[cat = s, bot=[wh = -]]{
			node xVNCleft(color=red)[cat=vn]{
				node xCl(color=red,mark=subst)[cat=cl,top=[cat=ce]]
				node xVcleft(color=red,mark=subst)[cat=v,top=[pers = 3,mode=ind]]
			}
			node xArg(color=red,mark=subst,extracted = +)[cat=n,idx=I]
			node xS[bot=[wh = -, mode = ind]]{
				node xComp(color=red)[cat=c]{
					node xCompl(color=red,mark=flex)[cat=qui]
				}
				node xVN
			}
		}
	}*=[subjectI=I]
}


class Subject
{
	CanonicalSubject[]
	|CliticSubject[]
	|whSubject[]
	|RelativeSubject[]
	|CleftSubject[]
	|InfinitiveSubject[]
 	|ImperativeSubject[]
 	|InvertedNominalSubject[]
}

%%% OBJECT %%%%

class CanonicalnonSubjectArg 
%# = postverbal arg
import
        CanonicalArgument[]
export
        xtop
declare
        ?xtop
{
        <syn>{
                node xS{
                        node xVN
                        ,,,node xtop(color=red) 
                }
        }
}

class ** CanonicalObject
import
        CanonicalnonSubjectArg[]
declare ?I
{
        <syn>{
                node xtop(mark=subst)[cat = n,idx=I,top=[det = +]]
        }*=[objectI=I]
}

class UnboundedExtraction
import 
	VerbalArgument[]
export
	xSe xtop
declare
	?xSe ?xtop ?fX
{
	<syn>{
		node xSe(color=red,extracted)[cat = s]{
			node xtop(color=red)
			node xS[top=[wh = -,inv = @{n,-}]]   
		}
	}
}
class UnboundedQuestion
import 
	UnboundedExtraction[]
declare 
	?fX
{
	<syn>{
		node xSe[top = [princ = +],bot=[wh = +,princ = ?fX, inv = ?fX]];
		node xS[top=[inv = ?fX]]
	}
}

class ** whObject
import
	UnboundedQuestion[]	
declare
	?NCO ?I
{
	<syn>{
		node xtop(mark=subst)[cat = n,idx=I,top=[wh = +]];		
		NCO = NonCanonicalObject[];
		xVN = NCO.xVNAgr;
		xtop = NCO.xObjAgr
	}*=[objectI=I]
}
class Clitic
import 
	VerbalArgument[]
export 
	xCl xV
declare
	?xCl ?xV
{
	<syn>{
		node xVN{
			node xCl(color=red,mark=subst)[cat=cl]
			,,,node xV(color=white)[cat=v]
		}
	}
}

class nonReflexiveClitic
import 
	Clitic[]
{
	<syn>{
		node xCl[top=[refl = -]]
	}
}

class ** CliticObjectII
%# Clitic subject ? see above (near canonical subject)
%# me te le les la nous vous leur leurs
import 
	nonReflexiveClitic[]	
declare
	?NCO ?I
{
	<syn>{
		node xCl(rank=2)[idx=I,top=[case=acc]];
		NCO = NonCanonicalObject[];
		xCl = NCO.xObjAgr;
		xVN = NCO.xVNAgr
	}*=[objectI=I]
}
class CliticIobjectII
import 
	nonReflexiveClitic[]
	
{
	<syn>{
		node xCl(rank=2)[top=[case=dat]]
	}
}
class ** CliticObject3
import 
	nonReflexiveClitic[]	
declare
	?NCO ?I
{
	<syn>{
		node xCl(rank=3)[idx=I,top=[case=acc3rd]]
		
	}*=[objectI=I];
	NCO = NonCanonicalObject[];
	xCl = NCO.xObjAgr;
	xVN = NCO.xVNAgr
}

class CliticIobject3
import 
	nonReflexiveClitic[]
{
	<syn>{
		node xCl(rank=4)[top=[case=dat3rd]]
	}
}

class reflexiveClitic
%# Special clitics : reflexive
%# me te se nous vous se 
import 
	Clitic[]
{
	<syn>{
		node xCl[top=[refl = +]]
	}
}


class ** reflexiveAccusative
import 
	reflexiveClitic[]
declare 
	?fX ?fY ?NCO ?I
{
	<syn>{
		node xCl(mark=subst,rank=2)[idx=I,top=[case=acc,num=?fX,pers = ?fY]];
		node xVN[top=[num=?fX,pers=?fY]]
	}*=[objectI=I];
	NCO = NonCanonicalObject[];
	xCl = NCO.xObjAgr;
	xVN = NCO.xVNAgr
}
class UnboundedRelative
import 
	UnboundedExtraction[]
export	?xRelf
declare
	?xRelf ?xReltop
{
	<syn>{
		node xReltop(color=red)[cat = n]{
			node xRelf(mark=foot,color=red)[cat = n]
			node xSe(mark=nadj)
		}
	}
}

class ** RelativeObject
import 
	UnboundedRelative[]
declare
	?NCO ?I
{
	<syn>{
		node xRelf[idx=I];
		node xtop(mark=subst)[cat = n]	
	}*=[objectI=I];
	NCO = NonCanonicalObject[];
	xVN = NCO.xVNAgr;
	xtop = NCO.xObjAgr	
}

class UnboundedCleft
import
	UnboundedExtraction[]
export 
	xClefttop
declare
	?xCleft ?xVNCleft ?xClCleft ?xAuxCleft ?xClefttop
{
	<syn>{
		node xCleft(color=red)[cat = s,bot=[wh = -]]{
			node xVNCleft(color=red)[cat=vn]{
				node xClCleft(color=red,mark=subst)[cat=cl,top=[case = ce]]
				node xAuxCleft(color=red,mark=subst)[cat=v,top=[mode=@{ind,subj},pers=3,phon=etre]]
			}
			node xClefttop(color=red)
			node xSe(mark=nadj)	
		}		
	}
}
class NominalCleft
import
	UnboundedCleft[]
export 
	xComp
declare
	?xComp
{
	<syn>{
		node xClefttop(mark=subst)[cat = n,top=[det = +,wh = -, num = sg]];
		node xtop[cat = c]{
			node xComp(color=red,mark=flex)
		}	
	}
}

class ** CleftObject
import 
	NominalCleft[]	
declare
	?NCO ?I
{
	<syn>{	node xClefttop[idx=I] ;
		node xComp[cat = que]					
	}*=[objectI=I,cleft=I];
	NCO=NonCanonicalObject[];
	xVN = NCO.xVNAgr;
	xClefttop = NCO.xObjAgr
}
class Object
{
	CanonicalObject[]
	|CliticObjectII[]
	|CliticObject3[]
	|reflexiveAccusative[]
	|whObject[]
	|RelativeObject[]
	|CleftObject[]
}

%%%%

class dian0V[E,X]{
	{Subject[]*=[subjectI=X] | ImpersonalSubject[] ; Object[]*=[objectI=X]} ; 
	activeVerbMorphology[]*=[vbI=E]
}

% class dian0V[E,X]{
% 	{Subject[]*=[subjectI=X]  ; activeVerbMorphology[]}
% }

%%%%
class dian0Vn1Active[E,X,Y]{
                Subject[]*=[subjectI=X] ; Object[]*=[objectI=Y] ; 
		activeVerbMorphology[]*=[vbI=E]       
}
% class dian0Vn1middle{
%                 Subject[] ; middleVerbMorphology[]        
% }
% class dian0Vn1Passive{
%                 Subject[] ; CAgent[] ; passiveVerbMorphology[]        
% }
% class dian0Vn1dePassive{
%                 Subject[] ; Genitive[] ; passiveVerbMorphology[]        
% }
% class dian0Vn1ShortPassive{
%                 Subject[] ; passiveVerbMorphology[]        
% }
% class dian0Vn1ImpersonalPassive{
%                 ImpersonalSubject[] ; Object[]; passiveVerbMorphology[]        
% }

%* <Familles>
%%%%%%%%%%%%%%%%%%%%%%%%%%

class n0V[E,X]{
	dian0V[E,X]*=[evt=E,arg1=X]
}

% class n0Vn1passivable{
%                 dian0Vn1Active[]
% 		| dian0Vn1Passive[]
% 		| dian0Vn1dePassive[]
% 		| dian0Vn1ShortPassive[]
% 		| dian0Vn1ImpersonalPassive[]
% 		| dian0Vn1middle[]
% }
class n0Vn1nonpassivable[E,X,Y]{
                dian0Vn1Active[E,X,Y]*=[evt=E,arg1=X,arg2=Y]
%		| dian0Vn1middle[]
}
%% class call {ternaryRel[]*=[theta1=agent,theta2=theme,theta3=beneficiary]}
%% value call

%% Help verbs

class cleftEtre
declare ?xN
{
	<syn>{
		node xN(color=red)[cat = v,bot=[mode = ind, pers = 3,phon = etre]]
	}
}

%* NP Classes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class commonNounNP[I]

declare
	?xN
{
	<syn>{
		node xN(color=red,mark=anchor)[cat = n,idx=I,bot=[det = -,pers = 3]]
	}
}

class properNameNP
declare
	?xN ?G ?I
{
	<syn>{
		node xN(color=red,mark=anchor)[cat = n,idx=I,bot=[det = +, pers = 3,gen=G]]
	}*=[gen=G,idx=I]
}

class cliticNP
declare
	?xN ?C ?G ?N ?P ?R ?I
{
	<syn>{
		node xN(color=red,mark=anchor)[cat = cl,idx=I,bot=[det = +,case=C,gen=G,num=N,pers=P,refl=R]]
	}*=[case=C,gen=G,num=N,pers=P,refl=R,idx=I]
}
class quNP
declare
	?xN ?C ?W ?I
{
	<syn>{
		node xN(color=red,mark=anchor)[cat = n,idx=I,bot=[wh = W,case=C]]}*=[case=C,wh=W,idx=I]}

%% Lexicon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class court 
declare
	?E ?X
	{
	unaryRel[]*=[rel=run,theta1=agent,evt=E,arg1=X];
	n0V[E,X]
	}

%% calls
value n0Vn1nonpassivable
value n0V

% lexical categories calls

value commonNounNP
value properNameNP
value cliticNP
value quNP

%lexical entries calls
%subject "il"
class IL
	{cliticNP[]*=[case=nom,gen=m,num=sg,pers=3,refl= -]}
value IL

%Proper names : Jean, Marie
class JEAN
	{properNameNP[]*=[gen=m]}
value JEAN

