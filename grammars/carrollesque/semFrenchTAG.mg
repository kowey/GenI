% -------------------------------------------------------
% A FRENCH METAGRAMMAR with a syntax/semantic interface
% -------------------------------------------------------
% author = Benoit Crabbe ; Claire Gardent
% date = May - December 2004
% comments = mostly follows (Abeillé02)
% 
% ASSOCIATED FILES (azim's parser): 
% 	Benoit4.lex : syntactic database
%	Benoit4.mph : morphological database
%			
%
% Summary of the things covered so far :
%     Constructions :
%        * Nominal args : cleft, clitic, canonical, whquestions, relatives
% 	 * Sentential args : canonical, infinitives, completives
%        		     indirect questions	 
%     Diathesis :
%	 * Active, passive, middle, reflexive
%     Functions :
%        * Subject, Object, Iobject, Genitive, Locative, Obliques
%     Complementation :
%	 * Families of (Abeille 02)
%     Agreement :		
%        * Subject-Verb
%	 * Subject-Past Participle (avoir -- être), check w/ middle and reflexives
%     Tense auxiliaries
%	 * avoir -- être 
%     Raising vs Equi-Deletion
% 	 *Subject Control 
%	 *Subject Raising
%     Subject ellipsis (imperative,infinitives,etc.)
%     Subject inversion (nominal, clitic, complex)
%		Long inversion in ExtrContext is badly handled (TAG failure): Jean demande à qui veut parler Paul	
%     Predicative adjectives (nominal dependants)
%	complementation
%     Tough adjectives (a simple example that should be extended)
%
%   STUFF IMPLEMENTED TO MAKE THE GRAMMAR WORK
%   (what follows should be seriously revised)
%     Basic NP syntax (determiners etc.)
%     Negation (badly handled, overgenerates, etc. should be completely revised)
%     Coordination (constituent only)
%	n n
%	adj adj
%	adv adv
%	s s
%     Adverbs
%        s-adverbs and v-adverbs
%     PP-Modifiers
%	 s-modifiers v-modifiers
%     CP-Modifiers (parce_que, etc...)
%     Noun noun modifiers (Monsieur Leber)	
%
% TODO
% Semantics
% Cleft locative
% Causative (beuark...)
% Sentential Subject : check for passive
% 
% * functional unicity
% * Subject inversion : le sujet nominal ne peut etre inversé si l'objet nominal est en position canonique.


use rank    with()          dims(syn)
use unicity with(extracted) dims(syn)
use unicity with(rank=1) dims(syn)
use unicity with(rank=2) dims(syn)
use unicity with(rank=3) dims(syn)
use unicity with(rank=4) dims(syn)
use unicity with(rank=5) dims(syn)
use unicity with(rank=6) dims(syn)
use unicity with(rank=7) dims(syn)
use color   with()       dims(syn)
% type declarations

type CAT={n,np,v,vn,s,pp,c,p,cl,adj,adv,coord,d,a,de,par,que,qui,dont,si,il,se}
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
	cop : bool,
	loc : bool
]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% IMPLICITS HFPs 
% NP : det,def,num,gen,pers,wh
      % Classes concernées : EpithAnte,

% V : mode,num,gen,pers,pp-num,pp-gen,inv,aux,aux-pass
% VN : mode,num,gen,pers,inv
% Adj : num,gen
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



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
feature top : ATOMIC
feature bot : ATOMIC

%feature declarations (for the syntax/semantics interface)
% in tree
feature idx : LABEL
feature controlIdx : LABEL
feature vlabel : LABEL
feature nlabel : LABEL

% in interface
feature subjectI : LABEL
feature objectI : LABEL
feature iobjectI : LABEL
feature cagentI : LABEL
feature genitiveI : LABEL
feature obliqueI : LABEL
feature locativeI : LABEL
feature sobjectI : LABEL
feature controlI : LABEL
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

%feature mode : LABEL
%feature num : NUMBER
%feature pers : PERSON
%feature wh : WH
%feature loc : bool
%feature refl : bool
%feature case : CASE

%Class definitions

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
declare ?X !L1 
      {
	<sem>{
		 L1:theta1(E,X)
	      }
	      *=[arg1=X] 
	  }
class binaryRel
import unaryRel[]
declare ?X !L2 
      {
	<sem>{
		 L2:theta2(E,X)
	      }
	      *=[arg2=X] 
	  }
class ternaryRel[]
import binaryRel[]
declare ?X !L3 
      {
	<sem>{
		 L3:theta3(E,X)
	      }
	      *=[arg3=X]
}


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
                                node xV(mark=anchor,color=black)[cat = v,top=[idx=I]]
                        }
                }
        }*=[vbI=I]       
}

% factor it with verbalForm (undone actually)
class adjectiveForm
declare
	?xR ?xVN ?xCop ?xHead ?fX ?fY ?fZ ?fU ?fW

{
	<syn>{
		node xR(color=black)[cat = s,bot=[mode = ?fX]]{
			node xVN(color=black)[cat = vn,top=[mode = ?fX],bot=[mode = ?fY,gen = ?fU,num = ?fZ,pers=?fW]]{
				node xCop(color=black,mark=subst)[cat = v,top=[cop = +, mode= ?fY,pers=?fW,num=?fZ]]
				node xHead(color=black,mark=anchor)[cat = adj,top=[gen=?fU,num=?fZ]]
			}
		}
	}
}
% factor it with verbalForm and adjectivalForm(undone actually)
class nominalForm
declare
	?xR ?xVN ?xCop ?xHead ?fX ?fY ?fZ ?fU ?fW

{
	<syn>{
		node xR(color=black)[cat = s,bot=[mode = ?fX]]{
			node xVN(color=black)[cat = vn,top=[mode = ?fX],bot=[mode = ?fY,gen = ?fU,num = ?fZ,pers=?fW]]{
				node xCop(color=black,mark=subst)[cat = v,top=[cop = +, mode= ?fY,pers=?fW,num=?fZ]]
				node xHead(color=black,mark=anchor)[cat = n,top=[gen=?fU,num=?fZ]]
			}
		}
	}
}


class activeVerbMorphology
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

class passiveVerbMorphology
import
   VerbalMorphology[]
export
   xInfl
declare
   ?xInfl ?fS ?fT ?fU ?fV ?fX ?fY ?fZ ?fW ?f0
{
        <syn>{
		     node xVN[bot=[num = ?fX, gen = ?fY, pers = ?fZ, mode = ?fW,inv = ?f0]]{
                     	node xInfl(color=black,mark=subst)[cat = v,top=[num = ?fX, gen = ?fY, pers = ?fZ, mode = ?fW,cop = +,inv = ?f0],bot=[inv = -, num = ?fT,gen = ?fU, pers = ?fV, mode = ?fS]]
                     	node xV(color=black)[cat = v,top=[mode = ppart,pp-gen=?fY,pp-num=?fX,aux-pass= -]]
		     }		              
        }
}

%I put the middle and intrinsic reflexives here (and not w/ clitics) because not inflected
class affixalVerbMorphology
import
   VerbalMorphology[]
export
   xClitic
declare
   ?xClitic ?fX ?fY ?fZ ?fW
{
        <syn>{
	    node xVN[bot=[num = ?fX, pers = ?fY, gen=?fZ, mode = ?fW]]{
               	node xClitic(color=red,rank=2)[cat = cl]
               	,,,node xV(color=black)[cat = v,top=[num = ?fX, pers = ?fY, gen=?fZ,mode=?fW],bot=[aux=etre]]
	    }		              
        }    
}

class middleVerbMorphology
import
   affixalVerbMorphology[]
declare
   ?xSe
{
        <syn>{	    
		node xVN[bot=[pers=3,tense=present]]{
               		node xClitic{	
				node xSe(mark=flex,color=red)[cat=se]		
			}
		}
        }     
}

class properReflexive
import
   affixalVerbMorphology[]
declare
   ?fX ?fY
{
	<syn>{
		node xVN[bot=[num=?fX,pers=?fY]]{
			node xClitic(mark=subst)[top=[num=?fX,pers=?fY,refl = +]]
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
                node xS(color=white)[cat = @{s,n}]{
                        node xVN(color=white)[cat = @{vn,adj,n}]
                }               
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



class CanonicalArgument
import 
	VerbalArgument[]

%# dummy for now

class EmptySubject
import 
	VerbalArgument[]

class InvertedNominalSubject
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
				,,,node xSubj(color = red,mark=subst)[cat = n,top = [idx=I,det = +]]
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

class InfinitiveSubject
import 
	EmptySubject[]
declare
	?fX ?fY ?fZ ?CI
{
	<syn>{	
		node xS[bot=[controlIdx = ?CI, control-num=?fX,control-gen=?fY,control-pers=?fZ]]{			
                            node xVN[top=[num=?fX,gen=?fY,pers=?fZ,mode=inf]]
		}
	}*=[subjectI=CI]
}
class ImperativeSubject
import 
	EmptySubject[]
{
	<syn>{
		node xVN[top=[mode=imp,pers=@{1,2}]]
	}
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

class CanonicalSubject
import
        RealisedNonExtractedSubject[]
declare ?I
{
        <syn>{
             node xSubj(mark=subst)[cat = n,top=[idx=I]]
	 }*=[subjectI=I]
}

class CliticSubject
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

class CanonicalObject
import
        CanonicalnonSubjectArg[]
declare ?I
{
        <syn>{
                node xtop(mark=subst)[cat = n,top=[det = +]]
        }
}

class CanonicalAttribute
import 
	CanonicalnonSubjectArg[]
declare
	?fX ?fY
{
	<syn>{
		node xtop(mark=subst)[top=[num = ?fX, gen = ?fY]];
		node xVN[top=[num = ?fX, gen = ?fY]]
	}
}


class CanonicalPP
import
        CanonicalnonSubjectArg[]
export
        xArg xPrep
declare
        ?xArg ?xX ?xPrep
{
        <syn>{
                node xtop[cat = pp]{
                        node xX (color=red)[cat = p]{
                                node xPrep(color=red,mark=flex)
                        }
                        node xArg(mark=subst,color=red)[cat = n, top = [det = +]]
                }
        }
}

class CanonicalCAgent
import 
	CanonicalPP[]
declare ?I
{
	<syn>{
		node xPrep[cat = par];
		node xArg[top=[idx = I]]	
	}*=[cagentI=I]
}

class CanonicalGenitive
import 
	CanonicalPP[]
declare ?I
{
	<syn>{
		node xPrep[cat = de];
		node xArg[top=[idx = I]]	
	}*=[genitiveI=I]
}

class CanonicalIobject
import 
	CanonicalPP[]
declare ?I
{
	<syn>{
		node xPrep[cat = a];
		node xArg[top=[idx = I]]	
	}*=[iobjectI=I]
}

class CanonicalOblique
import 
	CanonicalPP[]
declare ?I
{
	<syn>{
		node xPrep;
		node xArg[top=[idx = I]]	
	}*=[obliqueI=I]
}

class CanonicalLocative
import 
	 CanonicalnonSubjectArg[]
declare ?I
{
	<syn>{
		node xtop(mark=subst)[cat=pp,top=[loc = +,idx=I]]
	}*=[locativeI=I]
}

class Infinitive
export
	xCtrollee
declare 
	?xCtrollee
{
	<syn>{
		node xCtrollee(color=red)[top = [mode = inf]]
	}
}

class Subject-Control
%# Implements subject-control
import 
	Infinitive[]
export 
	xVNCtroller
declare 
	?xVNCtroller ?fX ?fY ?fZ
{
	<syn>{
		node xCtrollee[top=[control-num = ?fX, control-gen = ?fY, control-pers = ?fZ]];
		node xVNCtroller(color=white)[top=[num = ?fX, gen = ?fY, pers = ?fZ]]
	}
}

class CanonicalSententialXObjectWithComplementizer
import
	CanonicalnonSubjectArg[]
export 
	xComp xArg
declare
	?xX ?xComp ?xArg
{
	<syn>{
		node xtop(mark=nadj)[cat=s]{
			node xX (color = red)[cat=c]{
				node xComp(color=red,mark=flex)
			}
			node xArg(color = red)[cat = s,top=[wh = -]]
		}		
	}
}

class CanonicalSententialXObjectWithComplementizerNonBounding
import
	CanonicalSententialXObjectWithComplementizer[]
{
	<syn>{ 
		node xArg(xcomp)		
	}
}

class CanonicalSententialXObjectWithComplementizerBounding
import
	CanonicalSententialXObjectWithComplementizer[]
{
	<syn>{
		node xArg(mark=subst)
	}
}

class CanonicalSententialObjectFinite
import
	CanonicalSententialXObjectWithComplementizerNonBounding[]
{
	<syn>{
		node xComp[cat=que];
		node xArg[top=[mode = @{ind,subj}]]
	}
}

class CanonicalSententialXObjectwithoutComplementizer
import
	CanonicalnonSubjectArg[]
{
	<syn>{
		node xtop(color=red)[cat=s]
	}
}

class CanonicalSententialXObjectwithoutComplementizerBounding
import 
	CanonicalnonSubjectArg[]
{
	<syn>{
		node xtop(color=red,mark=subst)[cat=s]
	}
}

class CanonicalSententialXObjectwithoutComplementizerNonBounding
import 
	CanonicalnonSubjectArg[]

{
	<syn>{
		node xtop(color=red,xcomp)[cat=s]  
	}
}

class CanonicalSententialObjectInFinitive
import
	CanonicalSententialXObjectwithoutComplementizerNonBounding[]
	Subject-Control[]
declare
	?fX ?fY ?fZ
{
	<syn>{
		node xtop[top=[mode=inf]];
		xCtrollee = xtop;
		xVNCtroller = xVN
	}
}

class CanonicalSententialObjectInFinitiveDe
import
	CanonicalSententialXObjectWithComplementizerNonBounding[]
	Subject-Control[]
declare
	?CI ?I
{
	<syn>{
		node xComp[cat=de];
		xCtrollee = xArg;
		xVNCtroller = xVN;
		node xCtrollee(mark=subst)[bot=[idx=I,controlIdx=CI]]
	}*=[sobjectI=I,controlI=CI]
}

class CanonicalSententialObjectInFinitiveA
import
	CanonicalSententialXObjectWithComplementizerNonBounding[]
	Subject-Control[]
{
	<syn>{
		node xComp[cat=a];
		xCtrollee = xArg;
		xVNCtroller = xVN				
	}
}

class CanonicalSententialObjectInterrogativeFinite 
import
	CanonicalSententialXObjectWithComplementizerBounding[]
{
	<syn>{
		node xComp(color=red)[cat=si]
	};
	<syn>{
		node xArg(color = red)[cat = s,top=[wh = -]]
	}
}

class CanonicalSententialObjectInterrogativeInFinitive
import  
	CanonicalSententialXObjectwithoutComplementizerBounding[]
	Infinitive[]
{
	<syn>{
		node xtop [top =[wh = +]]; %nothing to say for now (features)
		xtop = xCtrollee
	}
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

class CliticObjectII
%# Clitic subject ? see above (near canonical subject)
%# me te le les la nous vous leur leurs
import 
	nonReflexiveClitic[]	
declare
	?NCO ?I
{
	<syn>{
		node xCl(rank=2)[top=[idx=I,case=acc]];
		NCO = NonCanonicalObject[];
		xCl = NCO.xObjAgr;
		xVN = NCO.xVNAgr
	}*=[objectI=I]
}
class CliticIobjectII
import 
	nonReflexiveClitic[]
declare ?I	
{
	<syn>{
		node xCl(rank=2)[top=[idx=I,case=dat]]
	}*=[objectI=I]
}
class CliticObject3
import 
	nonReflexiveClitic[]	
declare
	?NCO ?I

{
	<syn>{
		node xCl(rank=3)[top=[case=acc3rd]]
		
	};
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
class CliticGenitive
import 
	nonReflexiveClitic[]
declare ?I
{
	<syn>{
		node xCl(rank=5)[top=[case=gen,idx=I]]
	}*=[genitiveI=I]
}

class CliticLocative
import 
	nonReflexiveClitic[]
declare ?I
{
	<syn>{
		node xCl(rank=6)[top=[case=locative,idx=I]]
	}*=[locativeI=I]
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


class reflexiveAccusative
import 
	reflexiveClitic[]
declare 
	?fX ?fY ?NCO ?I
{
	<syn>{
		node xCl(mark=subst,rank=2)[top=[idx=I,case=acc,num=?fX,pers = ?fY]];
		node xVN[top=[num=?fX,pers=?fY]]
	}*=[objectI=I];
	NCO = NonCanonicalObject[];
	xCl = NCO.xObjAgr;
	xVN = NCO.xVNAgr
}

class reflexiveDative
import
	reflexiveClitic[]
{
	<syn>{
		node xCl(mark=subst,rank=2)[top=[case=dat]]
	}
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

class whObject
import
	UnboundedQuestion[]
declare
	?NCO ?I
{
	<syn>{
		node xtop(mark=subst)[cat = n,top=[idx=I,wh = +]];		
		NCO = NonCanonicalObject[];
		xVN = NCO.xVNAgr;
		xtop = NCO.xObjAgr
	}*=[objectI=I]
}

class whLocative
import
	UnboundedQuestion[]
declare ?I
{
	<syn>{
		node xtop(mark=subst)[cat = pp, top=[loc = +,wh = +,idx=I]]		
	}*=[locativeI=I]
}

class whPrep
import
	UnboundedQuestion[]
export 
	xArg xPrep
declare
	?xArg ?xPrep ?xX
{
	<syn>{
		node xtop[cat = pp]{
			node xX(color=red)[cat = p]{
				node xPrep(color=red,mark=flex)
			}
			node xArg(mark=subst,color=red)[cat = n,top=[wh = +, det = +]]
		}
	}
}

class whIobject
import
	 whPrep[]
{
	<syn>{
		node xPrep[cat = a]
	}
}

class whGenitive
import
	 whPrep[]
declare ?I
{
	<syn>{
		node xPrep[cat = de];
		node xArg[top=[idx = I]]
	}*=[genitiveI = I]
}

class whCAgent
import
	 whPrep[]
declare ?I
{
	<syn>{
		node xPrep[cat = par];
		node xArg[top=[idx = I]]
	}*=[cagentI=I]
}

class whOblique
import
	 whPrep[]
declare ?I
{
	<syn>{
		node xPrep;
		node xArg[top=[idx = I]] %nothing much for now
	}*=[obliqueI = I]
}

class UnboundedRelative
import 
	UnboundedExtraction[]
export	?xRelf
declare
	?xRelf ?xReltop ?fX ?fT ?fY ?fZ ?fU ?fW
{
	<syn>{
		node xReltop(color=red)[cat = n,bot=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]{
			node xRelf(mark=foot,color=red)[cat = n,top=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]
			node xSe(mark=nadj)
		}
	}
}

class RelativeObject
import 
	UnboundedRelative[]
declare
	?NCO ?I
{
	<syn>{
		node xRelf[top=[idx=I]];
		node xtop(mark=subst)[cat = n]	
	}*=[objectI=I];
	NCO = NonCanonicalObject[];
	xVN = NCO.xVNAgr;
	xtop = NCO.xObjAgr	
}


class RelativeLocative
import 
	UnboundedRelative[]
declare ?I
{
	<syn>{
		node xtop(mark=subst)[cat = pp,loc = +,top=[idx=I]]
	}*=[locativeI=I]
}

class RelativePP
import 
	UnboundedRelative[]
export 
	xArg xPrep	
declare
	?xArg ?xPrep ?xX
{
	<syn>{
		node xtop[cat = pp]{
			node xX(color=red)[cat = p]{
				node xPrep(mark=flex,color=red)
			}
			node xArg(color=red,mark = subst)[cat = n,top=[det = +, wh = +]]
		}
	}
}

class RelativeIobject
import 
	RelativePP[]
declare ?I
{
	<syn>{
		node xPrep[cat = a];
		node xRelf[top=[idx=I]]
	}*=[iobjectI=I]
}

class RelativeGenitive
import 
	RelativePP[]
declare ?I
{
	<syn>{
		node xPrep[cat = de];
		node xArg[top=[idx = I]]
	}*=[genitiveI=I]
}

class RelativeCAgent
import 
	RelativePP[]
declare ?I
{
	<syn>{
		node xPrep[cat = par];
		node xArg[top=[idx = I]]
	}*=[cagentI=I]
}

class RelativeOblique
import 
	RelativePP[]
declare ?I
{
	<syn>{
		node xPrep;
		node xArg[top=[idx = I]] % nothing much for now
	}*=[obliqueI=I]
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
				node xClCleft(color=red)[cat=cl,phon=ce,top=[case = ce]]
				node xAuxCleft(color=red)[cat=v,phon=etre,top=[mode=@{ind,subj},pers=3]]
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
	?xComp ?I
{
	<syn>{
		node xClefttop(mark=subst)[cat = n,idx=I,top=[det = +,wh = -, num = sg]];
		node xtop[cat = c]{
			node xComp(color=red,mark=flex)
		}	
	}*=[objectI=I,cleft=I]
}

class CleftObject
import 
	NominalCleft[]	
declare
	?NCO ?I
{
	<syn>{
		node xClefttop[top=[idx=I]] ;
		node xComp[cat = que]					
	}*=[objectI=I];
	NCO=NonCanonicalObject[];
	xVN = NCO.xVNAgr;
	xClefttop = NCO.xObjAgr
}

class CleftDont
import 
	NominalCleft[]
declare ?I
{
	<syn>{
		node xComp[cat = dont];
		node xClefttop[idx=I]			
	}*=[genitiveI=I]
}

class PrepCompCleft
import
	UnboundedCleft[]
export 
	xPrep xArg
declare 
	?xComp ?xX ?xPrep ?xArg
{
	<syn>{
		node xClefttop[cat = pp]{
			node xX(color=red)[cat=p]{
				node xPrep(mark=flex,color=red)
			}
			node xArg(color=red,mark=subst)[cat=n,top=[det = +,wh = -, num = sg]]		
		} 		
	};
	<syn>{
		node xtop[cat=c]{
			node xComp(color=red,mark=flex)[cat = que]
		}
	}
}

class CleftIobjectOne
import 
	PrepCompCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = a];
		node xArg[top=[idx = I]]
	}*=[iobjectI=I]	
}

class CleftGenitiveOne
import 
	PrepCompCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = de];
		node xArg[top=[idx = I]]
	}*=[genitiveI=I]	
}

class CleftCAgentOne
import 
	PrepCompCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = par];
		node xArg[top=[idx = I]]
	}*=[cagentI=I]		
}

class CleftObliqueOne
import 
	PrepCompCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = par];
		node xArg[top=[idx = I]]
	}*=[obliqueI=I]	
}

class NominalPrepCleft
import 
	UnboundedCleft[]
export
	xPrep xArg 
declare
	?xX ?xArg ?xPrep 
{
	<syn>{
		node xClefttop(mark=subst)[cat = n] 
	};
	<syn>{
		node xtop[cat = pp]{
			node xX(color =red)[cat = p]{
				node xPrep(mark=flex,color=red)
			}
			node xArg(color=red,mark=subst)[cat = n]
		}
	}
}	
class CleftIobjectTwo
import 
	NominalPrepCleft[]
{
	<syn>{
		node xPrep[cat = a]
	}	
}
class CleftGenitiveTwo
import 
	NominalPrepCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = de];
		node xArg[idx = I]
	}*=[genitiveI=I]	
}
class CleftCAgentTwo
import 
	NominalPrepCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = par];
		node xClefttop[top=[idx=I]]
	}*=[cagentI=I]
}

class CleftObliqueTwo
import 
	NominalPrepCleft[]
declare ?I
{
	<syn>{
		node xPrep;
		node xArg[top=[idx = I]] %nothing much for now
	}*=[obliqueI=I]	
}

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


%# extraction du sujet est bornée : 
%# La fille qui voit Jean ?
%# *La fille qui tu crois que voit Jean ?
%# Dummy class for now

class whSubject
import 
	BoundedExtraction[]
declare ?I
{
	<syn>{
		node xS[bot=[wh = +]]{ %should be wh = + better w/ a default ?
			node xArg(color=red,mark=subst,extracted = +)[cat=n,top=[idx=I,wh = +]]
			node xVN
		}		
	}*=[subjectI=I]
}

class RelativeSubject
import 
	BoundedExtraction[]
declare
	?xfoot ?xRel ?fT ?fU ?fW ?fX ?fY ?fZ ?I
{
	<syn>{
		node xRel(color=red)[cat = n,bot=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]{
			node xfoot(color=red,mark=foot)[cat=n,top=[idx=I,det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]
			node xS(mark=nadj)[bot=[mode=ind,wh = -]]{
				node xArg(color=red,mark=subst,extracted = +)[cat=n,top=[wh=rel,idx=I]]
				node xVN
			}
		}
	}*=[subjectI=I]
}

class CleftSubject
import 
	BoundedExtraction[]
declare
	?xSe ?xVNCleft ?xCl ?xVcleft ?xComp ?xCompl ?I

{
	<syn>{
		node xSe(color=red)[cat = s, bot=[wh = -]]{
			node xVNCleft(color=red)[cat=vn]{
				node xCl(color=red)[cat=cl,phon=ce,top=[cat=cl]]
				node xVcleft(color=red)[cat=v,phon=etre,top=[pers = 3,mode=ind]]
			}
			node xArg(color=red,mark=subst,extracted = +)[cat=n,top=[idx=I]]
			node xS[bot=[wh = -, mode = ind]]{
				node xComp(color=red)[cat=c]{
					node xCompl(color=red,mark=flex)[cat=qui]
				}
				node xVN
			}
		}
	}*=[subjectI=I]
}

%PredicativeAdj
class EpithAnte
declare
	?I ?xR ?xHead ?xFoot ?fT ?fU ?fW ?fX ?fY ?fZ
{
	<syn>{
		node xR(color=red)[cat=n, idx=?I, bot=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]{
			node xHead(color=red,mark=anchor)[cat = adj, top=[num = ?fY,gen = ?fZ]]
			node xFoot(color=red,mark=foot)[cat = n, idx=?I, top=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]
		}
	}*=[idx=I]
}
class EpithPost
declare
	?I ?xR ?xHead ?xFoot ?fT ?fU ?fW ?fX ?fY ?fZ
{
	<syn>{
		node xR(color=black)[cat=n, idx=?I, bot=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]{
			node xFoot(color=red,mark=foot)[cat = n, idx=?I, top=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]
			node xHead(color=black,mark=anchor)[cat = adj,top=[num = ?fY,gen = ?fZ]]
		}
	}*=[idx=I]
}

%%%%%%%%%%%%%%%%%%%
% LEXICAL CLASSES %
%%%%%%%%%%%%%%%%%%%


%* <Fonctions>
%%%%%%%%%%%%%%

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




class SententialSubject{
	CanonicalSententialSubjectFinite[]
	|CanonicalSententialSubjectInFinitive[]
	|Subject[] %always true ? it seems...
}
class SententialCObject{
	CanonicalSententialObjectFinite[]
	|CanonicalSententialObjectInFinitive[]
}
class SententialDeObject{
	CanonicalSententialObjectFinite[]
	|CanonicalSententialObjectInFinitiveDe[]
}
class SententialAObject{
	CanonicalSententialObjectFinite[]
	|CanonicalSententialObjectInFinitiveA[]
}
class SententialInterrogative{
	CanonicalSententialObjectInterrogativeFinite[]
	|CanonicalSententialObjectInterrogativeInFinitive[]
}

class Iobject
{
	CanonicalIobject[]
	|whIobject[]
	|CliticIobjectII[]
	|CliticIobject3[]
	|CliticLocative[] %Jean pense à Marie / y pense (pas de distinction ici) 
	|reflexiveDative[]
	|RelativeIobject[]
	|CleftIobjectOne[]
	|CleftIobjectTwo[]
}

class CAgent
{
	CanonicalCAgent[]
	|whCAgent[]
	|RelativeCAgent[]
	|CleftCAgentOne[]
	|CleftCAgentTwo[]
}

class Oblique
{
	CanonicalOblique[]
	|whOblique[]
	|RelativeOblique[]
	|CleftObliqueOne[]
	|CleftObliqueTwo[]
}

class Locative
{
	CanonicalLocative[]
	|whLocative[]
	|RelativeLocative[]
	|CliticLocative[]
	%|CleftLocativeOne[]
	%|CleftLocativeTwo[]
}


class Genitive
{
	CanonicalGenitive[]
	|CliticGenitive[]
	|whGenitive[]
	|RelativeGenitive[]
	|CleftGenitiveOne[]
	|CleftGenitiveTwo[]
	|CleftDont[]
}


%* <Alternances de diathèse>
%%%%%%%%%%%%%%%%%%%%%%%%%%

class dian0V[E,X]{
	{Subject[]*=[subjectI=X] | ImpersonalSubject[] ; Object[]*=[objectI=X]} ; 
	activeVerbMorphology[]*=[vbI=E]
}

class dian0Vn1Active[E,X,Y]{
                Subject[]*=[subjectI=X] ; Object[]*=[objectI=Y] ; activeVerbMorphology[]*=[vbI=E]          
}
class dian0Vn1Passive[E,X,Y]{
                Subject[]*=[subjectI=Y] ; CAgent[]*=[cagentI=X] ; passiveVerbMorphology[]*=[vbI=E]        
}
class dian0Vn1dePassive[E,X,Y]{
                Subject[]*=[subjectI=Y] ; Genitive[]*=[genitiveI=X] ;
                passiveVerbMorphology[]*=[vbI=E] }

class dian0Vn1ShortPassive[E,X,Y]{
                Subject[]*=[subjectI=Y] ; passiveVerbMorphology[]*=[vbI=E]                
}
class dian0Vn1ImpersonalPassive[E,X,Y]{
                ImpersonalSubject[] ; Object[]*=[objectI=Y]; passiveVerbMorphology[]*=[vbI=E]                
}
class dian0Vn1middle[E,X,Y]{
                Subject[]*=[subjectI=Y] ; middleVerbMorphology[]*=[vbI=E]       
}
class dian0Van1[E,X,Y]{
		Subject[]*=[subjectI=X] ; Iobject[]*=[iobjectI=Y] ; activeVerbMorphology[]*=[vbI=E]     
}
class dian0Vden1[E,X,Y]{
		Subject[]*=[subjectI=X] ; Genitive[]*=[genitiveI=Y] ; activeVerbMorphology[]*=[vbI=E] }


%* <Familles>
%%%%%%%%%%%%%%%%%%%%%%%%%%


class n0V[E,X]{
	unaryRel[]*=[evt=E,arg1=X] ;
	dian0V[E,X]
}
class noVinf[E,X]{
	unaryRel[]*=[evt=E,arg1=X] ;
	InfinitiveSubject[]*=[subjectI=X] ; activeVerbMorphology[]*=[vbI=E]     }
class n0Vn1[E,X,Y]{
 		binaryRel[]*=[evt=E,arg1=X,arg2=Y] ;
               { dian0Vn1Active[E,X,Y]
		| dian0Vn1Passive[E,X,Y]
		| dian0Vn1dePassive[E,X,Y]
		| dian0Vn1ShortPassive[E,X,Y]
		| dian0Vn1ImpersonalPassive[E,X,Y]
		| dian0Vn1middle[E,X,Y]
		}
}
class ilV[E]{
	semRel[]*=[evt=E];      
	{ImpersonalSubject[];activeVerbMorphology[]*=[vbI=E]}
}
class n0Vn1an2[E,X,Y,Z]{
		ternaryRel[]*=[evt=E,arg1=X,arg2=Y,arg3=Z] ;
		{n0Vn1[E,X,Y];
 		Iobject[]*=[iobjectI=Z]}
}

class n0ClV[E,X]{
	unaryRel[]*=[evt=E,arg1=X] ;
	{Subject[]*=[subjectI=X]; properReflexive[]*=[vbI=E]}
}
class n0ClVn1[E,X,Y]{
	binaryRel[]*=[evt=E,arg1=X,arg2=Y] ;
	{Subject[]*=[subjectI=X]; Object[]*=[objectI=Y]; properReflexive[]*=[vbI=E]  }
}
class n0Van1[E,X,Y]{
	binaryRel[]*=[evt=E,arg1=X,arg2=Y] ;
        dian0Van1[E,X,Y]
}
class n0Vden1[E,X,Y]{
      binaryRel[]*=[evt=E,arg1=X,arg2=Y] ;
	dian0Vden1[E,X,Y]
}
class n0ClVden1[E,X,Y]{
      binaryRel[]*=[evt=E,arg1=X,arg2=Y] ;
      {Subject[]*=[subjectI=X]; Genitive[]*=[genitiveI=Y]; properReflexive[]*=[vbI=E]}
}
class n0ClVpn1[E,X,Y]{
      binaryRel[]*=[evt=E,arg1=X,arg2=Y] ;
      {Subject[]*=[subjectI=X]; Oblique[]*=[obliqueI=Y]; properReflexive[]*=[vbI=E]  	}
}
class n0Vloc1[E,X,Y]{
      binaryRel[]*=[evt=E,arg1=X,arg2=Y] ;
      {	Subject[]*=[subjectI=X];Locative[]*=[locativeI=Y];activeVerbMorphology[]*=[vbI=E]
}}
class n0Vpn1[E,X,Y]{
      binaryRel[]*=[evt=E,arg1=X,arg2=Y] ;
      {	Subject[]*=[subjectI=X];Oblique[]*=[obliqueI=Y];activeVerbMorphology[]*=[vbI=E]
}}
class n0Van1des2_0[E,X,Y,Z]{
        ternaryRel[]*=[evt=E,arg1=X,arg2=Y,arg3=Z] ;
        Subject[]*=[subjectI=X];
	CanonicalSententialObjectInFinitiveDe[]*=[sobjectI=Z,controlI=X];
	Iobject[]*=[iobjectI=Y];activeVerbMorphology[]*=[vbI=E] 
}
class n0Vn1des2_1[E,X,Y,Z]{
        ternaryRel[]*=[evt=E,arg1=X,arg2=Y,arg3=Z] ;
        Subject[]*=[subjectI=X];
	CanonicalSententialObjectInFinitiveDe[]*=[sobjectI=Z,controlI=Y];
	Object[]*=[objectI=Y];activeVerbMorphology[]*=[vbI=E] 
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADJECTIVAL NOMINAL FAMILIES
% (epith. are missing due to a probable bug in the compiler)
% missing: adjectival diathesis

% FIXME: ask Claire how to deal with this
% We only want the EpithAnte and EpithPost classes
% GenI has this restriction that tree features must be a 
% superset of the lexical item features, but stuff like
% adjectiveForm has subjectO but not idx
class n0vApre {
  {Subject[]; adjectiveForm[]}
  |EpithAnte[]
}
class n0vApost {
  {Subject[]; adjectiveForm[]}
  |EpithPost[]
}


class n0vA
{
	{Subject[]; adjectiveForm[]}
	|EpithAnte[]
	|EpithPost[]
}
class s0vA
{
	{SententialSubject[];adjectiveForm[]}
	|EpithAnte[]
	|EpithPost[]
}
class n0vAden1
{
	{
		{Subject[];adjectiveForm[];Genitive[]}
		|{EpithPost[];CanonicalGenitive[]}
	}
}
class n0vAan1
{
	{
		{Subject[];adjectiveForm[];Iobject[]}
		|{EpithPost[];CanonicalIobject[]}
	}
}
class n0vApn1
{
	{
		{Subject[];adjectiveForm[];Oblique[]}
		|{EpithPost[];CanonicalOblique[]}
	}
}
class n0vAan1pn2
{
	{
		{Subject[];adjectiveForm[];Iobject[];Oblique[]}
		|{EpithPost[];CanonicalIobject[];CanonicalOblique[]}
	}
}
class n0vAan1den2
{
	{
		{Subject[];adjectiveForm[];Iobject[];Genitive[]}
		|{EpithPost[];CanonicalIobject[];CanonicalGenitive[]}
	}
}

%ADJECTIVAL FAMILIES W/SENTENTIAL ARGS
%todo

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%PREDICATIVE NOUNS (= basic support to make the grammar work, should be seriously extended)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class noun

declare
	?xN
{
	<syn>{
		node xN(color=red,mark=anchor)[cat = n,bot=[det = -,pers = 3]]
	}
}

class nounWithCompl
declare 
	?xR ?xHead
{
	<syn>{
		node xR(color=black)[cat = n]{
			node xHead(color=black,mark=anchor)[cat = n]
		}
	}
}


class n0vN
{
	{Subject[];nominalForm[]}
	|noun[]
}

% Sample subcats
class n0vNden1
{
	{Subject[];nominalForm[];Genitive[]}
	|{nounWithCompl[];CanonicalGenitive[]}
}
class n0vNan1
{	{Subject[];nominalForm[];Iobject[]}
	|{nounWithCompl[];CanonicalIobject[]}
}



% NON FULL PREDICATIVE UNITS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%* <Auxiliaires>
%%%%%%%%%%%%%%%%%%%

class TenseAux
export 
	xTop xL xR
declare
	?xTop ?xL ?xR ?fM ?fP ?fN ?fO
{
	<syn>{
		node xTop(color=red)[cat = v,bot=[mode=?fM,num=?fN,pers=?fP,aux-pass= +, inv = ?fO]]{
			node xL(mark=anchor,color=red)[cat = v, top=[mode=?fM, num=?fN, pers=?fP,inv = ?fO]]
			node xR(mark=foot,color=red)[cat = v,top=[mode = ppart]]
		}
	}
}

class AvoirAux
import
	TenseAux[]
declare
	?fN ?fG
{
	<syn>{
		node xTop[bot=[pp-num = ?fN, pp-gen = ?fG]]{
			node xR[top=[pp-num = ?fN, pp-gen = ?fG, aux = avoir]]
		}
	}
}

class EtreAux
import
	TenseAux[]
declare
	?fN ?fG
{
	<syn>{
		node xTop[bot=[num = ?fN, gen = ?fG]]{
			node xR[top=[pp-num = ?fN, pp-gen = ?fG, aux = etre]]
		}
	}
}


%* <Copule> 
%%%%%%%%%%%%%%%%%%%%%%%%
class Copule
declare 
	?xV
{
	<syn>{
		node xV(color=red,mark=anchor)[cat = v,bot=[cop = +,aux = avoir]]
	}
}


% SEMI-AUXILIARIES (Subject Raising verbs)
%%%%%%%%%%%%%%%%%%%%%%%%%

class SemiAux

declare
	?xTop ?xL ?xR ?fM ?fP ?fN ?fM ?fK ?fO
{
	<syn>{
		node xTop(color=red)[cat = vn,bot=[mode=?fM,num=?fN,pers=?fP,inv = ?fO, gen = ?fK]]{
			node xL(mark=anchor,color=red)[cat = v, top=[mode=?fM, num=?fN, pers=?fP,inv = ?fO]]
			node xR(mark=foot,color=red)[cat = vn,top=[mode = inf, num = ?fN,gen = ?fK]]
		}
	}
}
% TOUGH ADJECTIVES (Subject raising adjectives)
%%%%%%%%%%%%%%%%%%%%%%%%%%
% experimental (further constraints on these adjectives ?)
% other prepositions than specifically de ? sure ! which ones ?
class toughDe
declare
	?xR ?xCop ?xHead ?xPP ?xP ?xde ?xFoot ?fM ?fN ?fP ?fO ?fK
{
	<syn>{
		node xR(color=red)[cat = vn, bot = [mode=@{subj,ind},num=?fN,pers=?fP,inv = ?fO, gen = ?fK]]{
			node xCop(color=red,mark=subst)[cat = v,top=[cop = +, num = ?fN, pers = ?fP]]
			node xHead(color=red,mark=anchor)[cat = adj,top=[gen = ?fK, num = ?fN]]
			node xPP(color=red)[cat = pp]{
				node xP(color=red,mark=anchor)[cat = p]{
					node xde(color=red,mark=flex)[cat = de]
				}
				node xFoot(color=red,mark=foot)[cat = vn, top= [mode=inf,num=?fN,pers=?fP,inv = ?fO, gen = ?fK]]
			}		
		}
	}
}
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%* <MISC>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


class propername
declare
	?xN ?LPN ?Rel ?X
{
	<syn>{
		node xN(color=red,mark=anchor)[cat = n,bot=[det = +,pers = 3,idx=X]]
	}*=[idx=X];
	<sem>{
		LPN:Rel(X)
		}
}
class commonNoun
declare
	?xN ?LN ?Rel ?X
{
	<syn>{
		node xN(color=red,mark=anchor)[cat = n,idx=X,bot=[det = -,pers = 3,nlabel=LN]]
	}*=[idx=X];
	<sem>{
		LN:Rel(X)
		}
}
class pronoun
declare
	?xN
{
	<syn>{
	       node xN(color=red,mark=anchor)[cat = n,bot=[det = +]]
	}
}

class n0Nmod
declare 
	?xR ?xFoot ?xHead ?fX ?fT ?fY ?fZ ?fU ?fW
{
	<syn>{
		node xR(color=red)[cat = n,bot=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]{
			node xFoot(color=red,mark=foot)[cat = n,top=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]
			node xHead(color=red,mark=nadj,mark=anchor)[cat = n]
		}
	}
}


class Subjclitic
declare
	?xN
{
	<syn>{
		node xN(color=red,mark=anchor)[cat = cl,bot=[det = +]]
	}
}



class determiner
export
	xR xFoot xAnc 
declare
	?xR ?xFoot ?xAnc ?fX ?fY ?fZ ?fT ?fU ?fW
{
	<syn>{
		node xR(color=red)[cat = n, bot = [det = +, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU]]{
			node xAnc(color=red,mark=anchor)[cat = d,bot=[num = ?fY,gen = ?fZ]]
			node xFoot(color=red,mark=foot)[cat = n,top=[det = -, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU]]
		}
	}
}	

class stddeterminer
import 
	determiner[]
declare
	?I ?L0 ?Rel ?X ?H1 ?H2 ?LN ?LV
{
	<syn>{
		node xR[top=[wh = -,idx=I, vlabel = LV],bot=[wh = -]];
		node xFoot[bot=[idx=I, nlabel = LN]]
	}*=[idx=I];
	<sem>{
		L0:Rel(X,H1,H2);  LN <* H1;  LV <* H2
	     }

}


class whdeterminer
import
	determiner[]
{
	<syn>{
		node xR[bot = [wh = +]];
		node xFoot[top = [wh = -]]
	}
}




class PrepositionN
% as postnominal modifier
declare
	?A ?B ?xroot ?xfoot ?xprepph ?xprep ?xnoun ?fX ?fT ?fY ?fZ ?fU ?fW ?LP ?PrepRel ?X ?Y
{
	<syn>{
		node xroot(color=red)[cat = n,idx=?A, bot=[det = -, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]{
			node xfoot(color=red,mark=foot)[cat = n,top=[det = -, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW,idx=X]]
			node xprepph(color=red)[cat = pp]{
				node xprep (color=red,mark=anchor)[cat = p]
				node xnoun (color=red,mark=subst)[cat = n,idx=?B]
			}
		}
	} *= [idx=?A,idx2=?B];
	<sem>{
		LP:PrepRel(X,Y)
		}	
}
class PrepositionV
% as postnominal modifier
declare
	?xroot ?xfoot ?xprepph ?xprep ?xnoun ?fX ?fT ?fY ?fZ ?fU ?fW ?LP ?PrepRel ?X ?Y
{
	<syn>{
		node xroot(color=red)[cat = v,bot=[det = -, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]{
			node xfoot(color=red,mark=foot)[cat = v,top=[det = -, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW,idx=X]]
			node xprepph(color=red)[cat = pp]{
				node xprep (color=red,mark=anchor)[cat = p]
				node xnoun (color=red,mark=subst)[cat = n,idx=Y]
			}
		}
	};
	<sem>{
		LP:PrepRel(X,Y)
		}	
}
class CliticT
declare
	?xCl
{
	<syn>{
		node xCl(color=red,mark=anchor)[cat = cl]
	}
}

class InvertedSubjClitic
declare
	?xR ?xfoot ?xCl	?fX ?fY ?fZ ?fU ?fV ?fW
{
	<syn>{
		node xR(color=red)[cat = v,bot = [inv = @{+,-},mode = ?fX, num = ?fY, pers= ?fZ, gen = ?fU,aux = ?fV ,aux-pass = ?fW]]{
			node xfoot(color=red,mark=foot)[cat = v,top=[inv = -, mode = ?fX, num = ?fY, pers = ?fZ, gen = ?fU, aux = ?fV, aux-pass=?fW]]
			node xCl(color=red,mark=anchor)[cat = cl]
		}
	}
}
%Adverbes

class advAnte
export 
	xR xFoot
declare
	?xR ?xFoot ?xHead

{
	<syn>{
		node xR(color=red){	
			node xHead(color=red,mark=anchor)[cat = adv]
			node xFoot(color=red,mark=foot)
		}
	}
}

class advPost
export
	xR xFoot
declare
	?xR ?xFoot ?xHead

{	
	<syn>{
		node xR(color=red){	
			node xFoot(color=red,mark=foot)
			node xHead(color=red,mark=anchor)[cat = adv]
		}
	}
}

class advSAnte
import
	advAnte[]
{
	<syn>{
		node xR[cat = s];
		node xFoot[cat = s]	
	}
}
class advSPost
import
	advPost[]
{
	<syn>{
		node xR[cat = s];
		node xFoot[cat = s]	
	}
}
class advVPost
import
	advPost[]
declare 
	?fX ?fY ?fZ ?fU ?fV ?fW ?fR ?fS ?fT

{
	<syn>{
		node xR[cat = v,bot=[mode = ?fX, num = ?fY, gen = ?fZ, pers = ?fU, pp-num = ?fV,pp-gen = ?fW, inv = ?fR, aux = ?fS, aux-pass = ?fT]];
		node xFoot[cat = v,top=[mode = ?fX, num = ?fY, gen = ?fZ, pers = ?fU, pp-num = ?fV,pp-gen = ?fW, inv = ?fR, aux = ?fS, aux-pass = ?fT]]	
	}
}

class advAdjAnte
import
	advAnte[]
declare 
	?fX ?fY
{
	<syn>{
		node xR[cat = adj,bot=[num = ?fX, gen = ?fY]];
		node xFoot[cat = adj,top=[num = ?fX,gen = ?fY]]	
	}
}
class advAdvAnte
import
	advAnte[]
{
	<syn>{
		node xR[cat = adv];
		node xFoot[cat = adv]	
	}
}

class advLoc
declare
	?xR ?xHead ?fX
{
	<syn>{
		node xR(color=red)[cat = pp,bot=[loc = +,wh=?fX]]{
			node xHead(color=red,mark=anchor)[cat = adv,top=[wh=?fX]]
		}
	}
}
class prepLoc
declare
	?xHead ?xArg ?xP ?fX
{
	<syn>{
		node xHead(color=red)[cat = pp,bot=[loc = +,wh=?fX]]{
			node xP(color=red,mark=anchor)[cat = p]
			node xArg(color=red,mark=subst)[cat = n,top=[wh = ?fX]]
		}
	}
}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%PP MODIFIERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class abstractRightModifier
export
	xR xFoot xBarBar xBar xComp		
declare
	?xR ?xFoot ?xBarBar ?xBar ?xComp
{
	<syn>{
		node xR(color=red){
			node xFoot(color=red,mark=foot)
			node xBarBar(color=red){
				node xBar(color=red,mark=anchor)
				node xComp(color=red)
			}
		}
	}
}

class GenericPPModifier
import 
	abstractRightModifier[]
{
	<syn>{
		node xBarBar[cat = pp];
		node xBar[cat = p]
	}

} 
class ppSModifier
import 
	GenericPPModifier[]
{
	<syn>{
		node xR[cat=s];
		node xFoot[cat=s]
	}
}
class ppNModifier
import 
	GenericPPModifier[]
declare
	?fX ?fT ?fY ?fZ ?fU ?fW
{
	<syn>{
		node xR[cat=n,bot=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]];
		node xFoot[cat=n,top=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]];
		node xComp(mark=subst)
	}
}
class s0Pn1
import 
	ppSModifier[]
{
	<syn>{
		node xComp(mark=subst)[cat = n]
	}
}
class s0PLoc1
import 
	ppSModifier[]
{
	<syn>{
		node xComp(mark=subst)[cat = pp,top=[loc = +]]
	}
}

class n0Pn1
import 
	ppNModifier[]
{
	<syn>{
		node xComp[cat = n]
	}
}

class s0PNullCOmplementizer
import 
	ppSModifier[]
{
	<syn>{
		node xComp(mark = subst)[cat = s, top=[mode = inf]]
	}
}


class s0PComplementizer
import 
	ppSModifier[]
export 
	xC xCLex xS
declare
	?xC ?xCLex ?xS
{
	<syn>{
		node xComp[cat = s]{
			node xC(color = red)[cat = c]{
				node xCLex(color = red,mark=flex)
			}
			node xS(color=red,mark=subst)[cat = s]
		}
	}	
}
class s0Pdes1
import 
	s0PComplementizer[]
{
	<syn>{
		node xCLex[cat = de];
		node xS[top=[mode = inf]]
	}
}
class s0Pques1
import 
	s0PComplementizer[]
{
	<syn>{
		node xCLex[cat = que,top=[mode= @{ind,subj}]]
	}
}
class s0Ps1
{
	n0Pn1[]|s0PNullCOmplementizer[]|s0Pques1[]
}
class s0Pcs1
{
	n0Pn1[]|s0PNullCOmplementizer[]|s0Pdes1[]
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% !!!! AWFUL HACK  !!!! for implementing negation (due to Evalda)
% snif snif it sucks 
% remove it asap
% More incorrect and imprecise than that you die :-(
% should work to achieve parses on common negation cases in finite clauses

class negLeft
declare 
	?xR ?xFoot ?xHead ?fX ?fY ?fZ ?fU ?fV ?fW ?fR ?fS ?fT
{
	<syn>{
		node xR(color=red)[cat = v,bot=[mode = ?fX, num = ?fY, gen = ?fZ, pers = ?fU, pp-num = ?fV,pp-gen = ?fW, inv = ?fR, aux = ?fS, aux-pass = ?fT]]{
			node xHead(color=red,mark=anchor)[cat = cl]	
			node xFoot(color=red,mark=foot)[cat = v,top=[mode = ?fX, num = ?fY, gen = ?fZ, pers = ?fU, pp-num = ?fV,pp-gen = ?fW, inv = ?fR, aux = ?fS, aux-pass = ?fT]]
		} 
	}
}

%use it for : rien jamais, etc as well
class negPas
import 
	advPost[]
declare
	?fX ?fY ?fZ ?fU ?fV ?fW ?fR ?fS ?fT
{
	<syn>{
		node xR[cat = v,bot=[mode = ?fX, num = ?fY, gen = ?fZ, pers = ?fU, pp-num = ?fV,pp-gen = ?fW, inv = ?fR, aux = ?fS, aux-pass = ?fT]];
		node xFoot[cat = v,top=[mode = ?fX, num = ?fY, gen = ?fZ, pers = ?fU, pp-num = ?fV,pp-gen = ?fW, inv = ?fR, aux = ?fS, aux-pass = ?fT]]
	}
}

%END OF THE HACK	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Coordination
% Jean et marie mangent
% Jean ou marie mange
% Un professeur gentil et bienveillant
% Jean parle et Marie dort
% Eric parle de son pays et des ses voyages
% Jean parle lentement et avec assurance (not handled)
% Jean va soit à Paris soit à Minneapolis (not handled)

class ConstituentCoord
export
	xCoordtop xCoordFoot xCoord xCoordSubst
declare 
	?xCoordtop ?xCoordFoot ?xCoord ?xCoordSubst
{
	<syn>{
		node xCoordtop(color=red){
			node xCoordFoot(color=red,mark=foot)
			node xCoord(color=red,mark=anchor)[cat = coord]
			node xCoordSubst(color=red,mark=subst)
		}
	}
}

class NominalCoord
import
	ConstituentCoord[]
{
	<syn>{
		node xCoordtop[cat = n];
		node xCoordFoot[cat = n];
		node xCoordSubst[cat = n]		
	}
}

class SententialCoord
import
	ConstituentCoord[]
{
	<syn>{
		node xCoordtop[cat = s];
                node xCoordFoot[cat = s];
                node xCoordSubst[cat = s]
	}
}

class PrepCoord
import
        ConstituentCoord[]
{
        <syn>{
                node xCoordtop[cat = pp];
                node xCoordFoot[cat = pp];
                node xCoordSubst[cat = pp]
        }
}

class AdjCoord
import
        ConstituentCoord[]
{
        <syn>{
                node xCoordtop[cat = adj];
                node xCoordFoot[cat = adj];
                node xCoordSubst[cat = adj]
        }
}
class AdvCoord
import
        ConstituentCoord[]
{
        <syn>{
                node xCoordtop[cat = adv];
                node xCoordFoot[cat = adv];
                node xCoordSubst[cat = adv]
        }
}

class Coordination
{
	NominalCoord[]
	|SententialCoord[]
	|AdjCoord[]
	|PrepCoord[]
	|AdvCoord[]
}



