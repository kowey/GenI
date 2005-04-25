%-------------
%  MG de Test 
%-------------

% principles
use color with () dims ()
use rank with () dims ()
use unicity with (extracted) dims ()

%type declarations

type CAT={n,np,v,vn,s,vs,pp,acc,p,cl,c,à,de,par,que,qui,dont,si,il,se}
type PERSON=[1..3] 
type GENDER={m,f}
type NUMBER={sg,pl}
type COLOR ={red,black,white} 
type AGR=[cat    : CAT,
	  person : PERSON,
	  test   : bool
]

type CASE={nom,acc,acc3rd,dat3rd,dat,gen,locative}
type LABEL !
type PHON !

type LEXIQUE = {manger}
type SEM = {arg1,arg2,subst}

type MARK  = {subst,nadj,foot,none,anchor}
type RANK  = [1..7]

type WH={rel,+,-}

%property declarations
property color : COLOR 
property mark       : MARK
property extracted  : bool { extraction = + }
property xcomp : bool
property rank       : RANK {
	i_   = 1 ,
	ii_  = 2 ,
	iii_ = 3 ,
	iv_  = 4 ,
	v_   = 5 ,
        vi_  = 6 ,
 	vii_ = 7}

%feature declarations
feature idx : LABEL
feature top : LABEL
feature bot : LABEL

feature phon : PHON
feature anch : PHON
feature suj : LABEL
feature obj : LABEL

feature mode : LABEL
feature num  : NUMBER
feature pers : PERSON

%class definitions
%include test.mg

class Top
export
  S
declare
  ?S
{ <syn> { 
            node S (color=black)[cat=s] 
	}  
}

class SujCan
export
  N  
declare 
  ?N
{ <syn> {
          node N (color = red)[cat=n]
	} 
} 

class VerbTrans
import Top[]
export
  V Y
declare 
  ?V ?Y
{ <syn> {node S {
          node V (color = black)[cat = Y ] } 
		}*=[active= +, ditransitive= +] ; 
	Y = @{v,vn}  
}

class ObjCan
export
  O
declare 
  ?O
{ <syn> {
          node O (extracted,color = red)[cat=n]} 
} 

class ZeSem
declare
  ?I
{ <sem> { I }}

class Tree
%% in this class, you can check the atomic disjunction in either the virtual machine (by commenting node V1 and uncommenting node V)
%% or in the solver (current settings)
%% besides, we test here the dot operation on avms (identifier 'U')
import
  SujCan[]
  ObjCan[]
  VerbTrans[]
  ZeSem[]

declare
   ?X ?V1

{ <syn> 
	 { node S { 
		node N 
		node V1 (color=white)[cat= @{v,vn}]
		node O 
	        }
         }
}

value Tree


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class subjCan 
export S
declare ?N ?M ?S
{ <syn>{
	node S (color=white)[cat = s]{
		node(color=red,mark=subst)[cat=n,top = [num = ?N, pers=?M]]
		node(color=white)[cat = v, top=[num=?N, pers=?M]]
	}
}
}

class objCan 
export S
declare ?S
{ <syn>{
	node S { 
		node(color=white)[cat = v]
	     ,,,node(color=red,mark=subst)[cat = n]	
             }
       }
}

class activeMorph 
declare ?M ?N ?X
{ <syn>{
	node(color=black)[cat=s]{
		node(color=black)[cat=v, bot=[mode=?N]]{
			node(color=black)[cat=v, top=[mode = ?N]]{
				node(color=black)[phon=?X]
			}
		}
	}
}*=[anch=?X, active= +, ditransitive= +]
}

class transitifDirect 
%% in this class, we test the extended call, along with the dot operation on these calls
declare 
	?X ?Y
{ 

	X = subjCan[] ; 
	Y = objCan[] ; 
	activeMorph[] ;
	X.S = Y.S
}

class BinaryRel[Pred]
declare !L !E ?X ?Y
{ <sem>{!L:Pred(!E,?X,?Y)}}

class lexemeManger 
{ transitifDirect[] ; BinaryRel[manger]}

%Simple transitive
value lexemeManger

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extract from Benoit2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class TopLevelClass
% dummy class that provides a single root to the inh. hierarchy

class VerbalMorphology
export
   xS xVN xV
declare
   ?xS ?xVN ?xV
{
        <syn>{
                node xS(color=black)[cat = s]{
                        node xVN(color=black)[cat = vn]{
                                node xV(mark=anchor,color=black)[cat = v]
                        }
                }
        }       
}

class activeVerbMorphology
import
	VerbalMorphology[]


class passiveVerbMorphology
import
   VerbalMorphology[]
export
   xInfl
declare
   ?xInfl
{
        <syn>{
		     node xVN{
                     	node xInfl(color=black,mark=subst)[cat = v]
                     	node xV(color=black)[cat = v]
		     }		              
        }*=[passive= +]       
}

%I put the middle and intrinsic reflexives here (and not w/ clitics) because not inflected
class affixalVerbMorphology
import
   VerbalMorphology[]
export 
   xClitic
declare
   ?xClitic

{
        <syn>{
	    node xVN{
               	node xClitic(color=red,rank=2)[cat = cl]
               	,,,node xV(color=black)[cat = v]
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
               	node xClitic{
			node xSe(color=red)[cat=se]
		}	              
        }       
}
class properReflexive
import
   affixalVerbMorphology[]
{
	<syn>{
		node xClitic(mark=subst)[refl = +]
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

class CanonicalArgument
import 
	VerbalArgument[]

%dummy for now

        
class CanonicalSubject
import
        CanonicalArgument[]
declare
        ?xSubj
{
        <syn>{
                node xS{
                        node xSubj(color=red,mark=subst)[cat = n]
                        node xVN(color=white)[cat = vn]
                }                       
        }
}

class CliticSubject
import
        CanonicalArgument[]
declare
        ?xSubj
{
        <syn>{
                node xS{
                        node xSubj(color=red,mark=subst)[cat = cl,case=nom]
                        node xVN(color=white)[cat = vn]
                }                       
        }
}

class ImpersonalSubject
import
        CanonicalArgument[]
declare 
	?xSubj ?xil
{
	<syn>{
                node xS{
                        node xSubj(color=red)[cat = cl]{
				node xil(color=red)[cat=il] 
			}
                        node xVN(color=white)[cat = vn]
                }                       		
	}
}

class CanonicalSententialSubjectFinite%Que Jean aille à la montagne plait à Marie
import
	CanonicalArgument[]
declare
	?xSubj ?xTop ?xComp ?xQue
{
	<syn>{
		node xS{
			node xTop(color=red)[cat=s]{
				node xComp(color=red)[cat=c]{
					node xQue(color=red)[cat=que]
				}
				node xSubj(color=red,mark=subst)[cat=s]
			}
			node xVN
		}
	}
}

class CanonicalSententialSubjectInFinitive %Aller à la montagne plait à Marie
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


class CanonicalnonSubjectArg % = postverbal arg
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
{
        <syn>{
                node xtop(mark=subst)[cat = n]
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
                                node xPrep(color=red)
                        }
                        node xArg(mark=subst,color=red)[cat = n]
                }
        }
}

class CanonicalCAgent
import 
	CanonicalPP[]

{
	<syn>{
		node xPrep[cat = par]	
	}
}

class CanonicalGenitive
import 
	CanonicalPP[]

{
	<syn>{
		node xPrep[cat = de]	
	}
}

class CanonicalIobject
import 
	CanonicalPP[]

{
	<syn>{
		node xPrep[cat = à]	
	}
}

class CanonicalOblique
import 
	CanonicalPP[]

{
	<syn>{
		node xPrep %nothing much for now	
	}
}

class CanonicalLocative
import 
	 CanonicalnonSubjectArg[]
{
	<syn>{
		node xtop(mark=subst)[cat=pp,loc = +]
	}

}

class CanonicalSententialXObjectWithComplementizer
import
	CanonicalnonSubjectArg[]
export xComp xArg

declare
	?xX ?xComp ?xArg
{
	<syn>{
		node xtop[cat=s]{
			node xX (color = red)[cat =c]{
				node xComp(color=red)
			}
			node xArg(color = red)[cat = s]
		}		
	}
}

class CanonicalSententialXObjectWithComplementizerNonBounding
import
	CanonicalSententialXObjectWithComplementizer[]

{
	<syn>{
		node xArg(mark=foot)%replace by p2		
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
		node xComp[cat=que]
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
		node xtop(color=red,mark=foot)[cat=s] %replace foot by p2 
	}
}

class CanonicalSententialObjectInFinitive
import
	CanonicalSententialXObjectwithoutComplementizerNonBounding[]
{
	<syn>{
		node xtop % nothing much for now
	}
}

class CanonicalSententialObjectInFinitiveDe
import
	CanonicalSententialXObjectWithComplementizerNonBounding[]

{
	<syn>{
		node xComp[cat=de]			
	}
}

class CanonicalSententialObjectInFinitiveA
import
	CanonicalSententialXObjectWithComplementizerNonBounding[]

{
	<syn>{
		node xComp[cat=à]			
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
		node xArg(color = red)[cat = s]
	}
}

class CanonicalSententialObjectInterrogativeInFinitive
import 
	CanonicalSententialXObjectwithoutComplementizerBounding[]
	
{
	<syn>{
		node xtop %nothing to say for now (features)
	}
}

class Clitic
import 
	VerbalArgument[]
export 
	xCl
declare
	?xV ?xCl
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
		node xCl[refl = -]
	}
}

%Clitic subject ? see above (near canonical subject)
% me te le les la nous vous leur leurs
class CliticObjectII
import nonReflexiveClitic[]

{
	<syn>{
		node xCl(rank=2)[case=acc]
	}
}
class CliticIobjectII
import nonReflexiveClitic[]

{
	<syn>{
		node xCl(rank=2)[case=dat]
	}
}
class CliticObject3
import nonReflexiveClitic[]

{
	<syn>{
		node xCl(rank=3)[case=acc3rd]
	}
}

class CliticIobject3
import nonReflexiveClitic[]

{
	<syn>{
		node xCl(rank=4)[case=dat3rd]
	}
}
class CliticGenitive
import nonReflexiveClitic[]

{
	<syn>{
		node xCl(rank=5)[case=gen]
	}
}
class CliticLocative
import 
	nonReflexiveClitic[]
{
	<syn>{
		node xCl(rank=6)[case=locative]
	}
}

% Special clitics : reflexive
% me te se nous vous se 
class reflexiveClitic
import 
	Clitic[]
{
	<syn>{
		node xCl[refl = +]
	}
}


class reflexiveAccusative
import 
	reflexiveClitic[]
{
	<syn>{
		node xCl(mark=subst,rank=2)[case=acc]
	}	
}

class reflexiveDative
import
	reflexiveClitic[]
{
	<syn>{
		node xCl(mark=subst,rank=2)[case=dat]
	}	
}

class UnboundedExtraction
import 
	VerbalArgument[]
export
	%xS xVN 
	xSe xtop
declare
	?xSe ?xtop
{
	<syn>{
		node xSe(color=red,extracted = +)[cat = s]{
			node xtop(color=red)
			node xS
		}
	}	
}


class UnboundedQuestion
import 
	UnboundedExtraction[]
%dummy class for now

class whObject
import
	UnboundedQuestion[]
{
	<syn>{
		node xtop(mark=subst)[cat = n]		
	}
}

class whLocative
import
	UnboundedQuestion[]
{
	<syn>{
		node xtop(mark=subst)[cat = pp, loc = +]		
	}
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
				node xPrep(color=red)
			}
			node xArg(mark=subst,color=red)[cat = n]
		}
	}
}

class whIobject
import
	 whPrep[]
{
	<syn>{
		node xPrep[cat = à]
	}
}

class whGenitive
import
	 whPrep[]
{
	<syn>{
		node xPrep[cat = de]
	}
}

class whCAgent
import
	 whPrep[]
{
	<syn>{
		node xPrep[cat = par]
	}
}

class whOblique
import
	 whPrep[]
{
	<syn>{
		node xPrep %nothing much for now
	}
}

class UnboundedRelative
import 
	UnboundedExtraction[]	
declare
	?xRelf ?xReltop
{
	<syn>{
		node xReltop(color=red)[cat = n]{
			node xRelf(mark=foot,color=red)[cat = n]
			node xSe
		}
	}
}

class RelativeObject
import 
	UnboundedRelative[]
{
	<syn>{
		node xtop(mark=subst)[cat = n]
	}
}


class RelativeLocative
import 
	UnboundedRelative[]
{
	<syn>{
		node xtop(mark=subst)[cat = pp,loc = +]
	}
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
				node xPrep(color=red)
			}
			node xArg(color=red,mark = subst)[cat = n]
		}
	}
}

class RelativeIobject
import 
	RelativePP[]
{
	<syn>{
		node xPrep[cat = à]
	}
}

class RelativeGenitive
import 
	RelativePP[]
{
	<syn>{
		node xPrep[cat = de]
	}
}

class RelativeCAgent
import 
	RelativePP[]
{
	<syn>{
		node xPrep[cat = par]
	}
}

class RelativeOblique
import 
	RelativePP[]
{
	<syn>{
		node xPrep % nothing much for now
	}
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
		node xCleft(color=red)[cat = s]{
			node xVNCleft(color=red)[cat=vn]{
				node xClCleft(color=red,mark=subst)[cat=cl]
				node xAuxCleft(color=red,mark=subst)[cat=v]
			}
			node xClefttop(color=red)
			node xSe	
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
		node xClefttop(mark=subst)[cat = n]
	};
	<syn>{
		node xtop[cat = c]{
			node xComp(color=red)
		}
	}
}

class CleftObject
import 
	NominalCleft[]
{
	<syn>{
		node xComp[cat = que]				
	}
}

class CleftDont
import 
	NominalCleft[]
{
	<syn>{
		node xComp[cat = dont]				
	}
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
				node xPrep(color=red)
			}
			node xArg(color=red,mark=subst)[cat=n]		
		} 		
	};
	<syn>{
		node xtop[cat=c]{
			node xComp(color=red)[cat = que]
		}
	}
}

class CleftIobjectOne
import 
	PrepCompCleft[]
{
	<syn>{
		node xPrep[cat = à]
	}	
}

class CleftGenitiveOne
import 
	PrepCompCleft[]
{
	<syn>{
		node xPrep[cat = de]
	}	
}

class CleftCAgentOne
import 
	PrepCompCleft[]
{
	<syn>{
		node xPrep[cat = par]
	}	
}

class CleftObliqueOne
import 
	PrepCompCleft[]
{
	<syn>{
		node xPrep[cat = par]
	}	
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
				node xPrep(color=red)
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
		node xPrep[cat = à]
	}	
}
class CleftGenitiveTwo
import 
	NominalPrepCleft[]
{
	<syn>{
		node xPrep[cat = de]
	}	
}
class CleftCAgentTwo
import 
	NominalPrepCleft[]
{
	<syn>{
		node xPrep[cat = par]
	}	
}

class CleftObliqueTwo
import 
	NominalPrepCleft[]
{
	<syn>{
		node xPrep %nothing much for now
	}	
}

class BoundedExtraction
import VerbalArgument[]

%extraction du sujet est bornée : 
%  La fille qui voit Jean ?
%  *La fille qui tu crois que voit Jean ?
%Dummy class for now

class whSubject
import BoundedExtraction[]
declare
	?xArg
{
	<syn>{
		node xS{
			node xArg(color=red,mark=subst,extracted = +)[cat=n]
			node xVN
		}
	}
}

class RelativeSubject
import BoundedExtraction[]
declare
	?xArg ?xfoot ?xRel
{
	<syn>{
		node xRel(color=red)[cat = n]{
			node xfoot(color=red,mark=foot)[cat=n]
			node xS{
				node xArg(color=red,mark=subst,extracted = +)[cat=n,wh=rel]
				node xVN
			}
		}
	}
}

class CleftSubject
import BoundedExtraction[]
declare
	?xSe ?xVNCleft ?xCl ?xVcleft ?xArg ?xComp ?xCompl

{
	<syn>{
		node xSe(color=red)[cat = s]{
			node xVNCleft(color=red)[cat=vn]{
				node xCl(color=red,mark=subst)[cat=cl]
				node xVcleft(color=red,mark=subst)[cat=v]
			}
			node xArg(color=red,mark=subst,extracted = +)[cat=n]
			node xS{
				node xComp(color=red)[cat=c]{
					node xCompl(color=red)[cat=qui]
				}
				node xVN
			}
		}
	}
}

%%%%%%%%%%%%%%%%%%%
% LEXICAL CLASSES %
%%%%%%%%%%%%%%%%%%%


% FONCTIONS  
%%%%%%%%%%%%%%

class Subject
{
	CanonicalSubject[]
	|CliticSubject[]
	|whSubject[]
	|RelativeSubject[]
	|CleftSubject[]
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


% DIATHESIS ALTERNATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%

class dian0V{
	{Subject[] | ImpersonalSubject[];Object[]} ; activeVerbMorphology[]  	
}

class dian0Vn1Active{
                Subject[] ; Object[] ; activeVerbMorphology[]          
}
class dian0Vn1Passive{
                Subject[] ; CAgent[] ; passiveVerbMorphology[]        
}
class dian0Vn1dePassive{
                Subject[] ; Genitive[] ; passiveVerbMorphology[]        
}
class dian0Vn1ShortPassive{
                Subject[] ; passiveVerbMorphology[]        
}
class dian0Vn1ImpersonalPassive{
                ImpersonalSubject[] ; Object[]; passiveVerbMorphology[]        
}
class dian0Vn1middle{
                Subject[] ; middleVerbMorphology[]        
}
class dian0Van1{
		Subject[] ; Iobject[] ; activeVerbMorphology[]       
}
class dian0Vden1{
		Subject[] ; Genitive[] ; activeVerbMorphology[]       
}


% FAMILIES
%%%%%%%%%%%%%%%%%%%%%%%%%%

class n0V{
	dian0V[]
}
class n0Vn1{
                dian0Vn1Active[] 
		| dian0Vn1Passive[] 
		| dian0Vn1dePassive[] 
		| dian0Vn1ShortPassive[]    
		| dian0Vn1ImpersonalPassive[]
		| dian0Vn1middle[]            
}
class n0Vn1an2{
		n0Vn1[];Iobject[]
}
class n0Vn1den2{
		n0Vn1[];Genitive[]
}
class n0Van1{
                dian0Van1[]
}
class n0Vden1{
		dian0Vden1[]
}
class n0Vpn1{
		Subject[];Oblique[];activeVerbMorphology[]
}
class n0Vloc1{
	Subject[];Locative[];activeVerbMorphology[]
}	
class n0Van1den2{
		n0Van1[];Genitive[]
}
class n0Vden1pn2{
		Subject[];Oblique[];Genitive[];activeVerbMorphology[]
}
class n0Vn1loc2{
		n0Vn1[];Locative[]
}
class n0Vcs1{
		Subject[]; SententialCObject[]; activeVerbMorphology[]   
}
class n0Vas1{
		Subject[]; SententialAObject[]; activeVerbMorphology[]   
}
class n0Vdes1{
		Subject[]; SententialDeObject[]; activeVerbMorphology[]   
}
class n0Vcs1an2{
		Subject[]; SententialCObject[]; Iobject[]; activeVerbMorphology[]   
}
class n0Vs1int{
	Subject[]; SententialInterrogative[]; activeVerbMorphology[]   
}
class n0Vs1intan2{
	Subject[]; SententialInterrogative[]; activeVerbMorphology[]   
}
class n0Vn1des2{
	n0Vn1[]; SententialDeObject[]
}
class n0Vn1as2{
	n0Vn1[]; SententialAObject[]
}
class n0Vn1cs2{
	n0Vn1[]; SententialCObject[]
}
class n0Vs1des2{
	Subject[]; SententialCObject[]; SententialDeObject[]; activeVerbMorphology[]   
}
class n0Vcs1des2{
	Subject[]; SententialCObject[]; SententialDeObject[]; activeVerbMorphology[]
}
class n0Vdes1den2{
	Subject[]; SententialDeObject[]; Genitive[]; activeVerbMorphology[]   
}
class s0V{
	SententialSubject[]; activeVerbMorphology[]
}
class s0Vn1{
	SententialSubject[]; Object[]; activeVerbMorphology[]
}
class s0Van1{
	SententialSubject[]; Iobject[]; activeVerbMorphology[]
}
class s0Vcs1{
	SententialSubject[]; SententialCObject[]; activeVerbMorphology[]
}
class s0Vn1as2{
	SententialSubject[]; Object[]; SententialAObject[]; activeVerbMorphology[]
}
class n0Vdes1pn2{
	Subject[]; Oblique[]; SententialDeObject[]; activeVerbMorphology[]
}
class n0ClV{
	Subject[]; properReflexive[]  
}
class n0ClVn1{
	Subject[]; Object[]; properReflexive[]  
}
class n0ClVpn1{
	Subject[]; Oblique[]; properReflexive[]  	
}
class n0ClVden1{
	Subject[]; Genitive[]; properReflexive[]  	
}



% VALUATION
%%%%%%%%%%%%%%%%%%%%%%%%%


%Nominal
value n0V 		%Jean dort
value n0ClV		%Jean s'évanouit
value n0Vn1		%Jean regarde Marie
value n0ClVn1		%L'enfant s'appelle Marie
value n0Van1		%Jean parle à Marie
value n0Vden1		%Jean parle de ses vacances
value n0ClVden1		%Jean se souvient de Marc
value n0Vpn1		%Jean parle avec Marie
value n0ClVpn1		%Jean se bat contre Paul
value n0Vloc1		%Jean va à Paris
value n0Vn1an2		%Jean donne un cadeau à Marie
value n0Van1den2	%Jean parle de ses vacances à Marie
value n0Vn1den2		%Jean reçoit un cadeau de Marie
value n0Vden1pn2	%Jean parle de ce livre avec Marie
value n0Vn1loc2		%Jean envoie la lettre à la poste
