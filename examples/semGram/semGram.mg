% -------------------------------------------------------
% A FRENCH METAGRAMMAR with semantics
% -------------------------------------------------------
% author = Benoit Crabbe ; Claire Gardent
% date = May 2004

%type declarations

type CAT={n,np,v,vn,s,pp,c,p,cl,à,de,par,que,qui,dont,si,il,se}
type PERSON=[1..3] 
type GENDER={m,f}
type NUMBER={sg,pl}
type MODE={ind,inf,subj,imp}
type COLOR ={red,black,white}
type AGR=[cat : CAT,
          person : PERSON]
type WH={rel,+,-}
type LABEL !
type CASE={nom,acc,acc3rd,dat3rd,dat,gen,locative}

type MARK  = {subst,nadj,foot,anchor,none}
type RANK  = [1..7]

type lexique = {give,donner,persuader,convince,manger,eat,dormir,sleep}

type TOP=[
	mode : MODE,
 	num : NUMBER,
 	gen : GENDER,
 	pers : PERSON,
 	refl : bool,      
 	loc : bool,
 	wh : WH,
 	case : CASE	
]
%property declarations
property color      : COLOR
property mark       : MARK
property extracted  : bool { extraction = + }
property xcomp      : bool
property rank       : RANK {
        i_   = 1 ,
        ii_  = 2 ,
        iii_ = 3 ,
        iv_  = 4 ,
        v_  = 5 ,
	vi_ = 6
}

%feature declarations
feature idx : LABEL
feature top : TOP
feature subjectIdx : LABEL
feature objectIdx : LABEL
feature iobjectIdx : LABEL
feature cagentIdx : LABEL
feature genitiveIdx : LABEL
feature obliqueIdx : LABEL
feature locativeIdx : LABEL
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
feature exp : LABEL

%% Semantic classes
class ternaryRel
declare ?Rel ?E ?X ?Y ?Z !L
      {
	<sem>{
		L:Rel(E,X,Y,Z)
	      }
	      *=[rel3=Rel,evt=E,arg1=X,arg2=Y,arg3=Z] 
	  }
class binaryRel
declare ?Rel ?E ?X ?Y !L
      {
	<sem>{
		L:Rel(E,X,Y)
	      }
	      *=[rel2=Rel,evt=E,arg1=X,arg2=Y] 
	  }

class unaryRel
declare ?Rel ?E ?X ?T1 !L1 !L2
      {
	<sem>{
		L1:Rel(E) ; L2:T1(E,X)
	      }
	      *=[rel1=Rel,evt=E,arg1=X,theta1=T1] 
	  }

%%% Classes syntaxiques 

class TopLevelClass
% dummy class that provides a single root to the inh. hierarchy

class VerbalMorphology
export
   xS xVN xV
declare
   ?xS ?xVN ?xV ?Anchor ?VbI
{
        {<syn>{
                node xS(color=black)[cat = s]{
                        node xVN(color=black)[cat = vn]{
                                node xV(mark=anchor,color=black)[cat = v,
					idx=VbI, phon=Anchor]
                        }
                }
        }
	*=[phon=Anchor,vbI=VbI] 
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
        }       
}

% %I put the middle and intrinsic reflexives here (and not w/ clitics) because not inflected
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
declare ?I
{
	<syn>{
		node xClitic(mark=subst)[refl = + , idx=I]
	}
	*=[iobjectIdx=I]
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
 	?xSubj ?SubjI
{
        <syn>{
                node xS{
                       node xSubj(color=red,mark=subst)[cat = n,idx=SubjI]
                        node xVN(color=white)[cat = vn]
                }                       
        }
	*=[subjectIdx=SubjI]
}

class CliticSubject
import
        CanonicalArgument[]
declare
         ?xSubj ?SubjI
{
        <syn>{
                node xS{
                       node xSubj(color=red,mark=subst)[cat = cl,case=nom,idx=SubjI]
                        node xVN(color=white)[cat = vn]
                }                       
        }
	*=[subjectIdx=SubjI]
}

class ImpersonalSubject
import
        CanonicalArgument[]
declare 
	?xSubj ?xil ?I
{
	<syn>{
                node xS{
                        node xSubj(color=red)[cat = cl,idx=I]{
				node xil(color=red)[cat=il] 
			}
                        node xVN(color=white)[cat = vn]
                }                       		
	}*=[subjectIdx=I]
}

class CanonicalSententialSubjectFinite%Que Jean aille à la montagne plait à Marie
import
	CanonicalArgument[]
declare
	?xSubj ?xTop ?xComp ?xQue ?I
{
	<syn>{
		node xS{
			node xTop(color=red)[cat=s]{
				node xComp(color=red)[cat=c]{
					node xQue(color=red)[cat=que]
				}
				node xSubj(color=red,mark=subst)[cat=s,idx=I]
			}
			node xVN
		}
	}*=[subjectIdx=I]
}

class CanonicalSententialSubjectInFinitive %Aller à la montagne plait à Marie
import 
	CanonicalArgument[]
declare
	?xSubj ?I
{
	<syn>{
		node xS{
			node xSubj(color=red,mark=subst)[cat=s,idx=I]
			node xVN
		}
	}
	*=[subjectIdx=I]
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
declare ?ObjI
{
        <syn>{
                node xtop(mark=subst)[cat = n,idx=ObjI]
        }
	*=[objectIdx=ObjI]
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
declare ?I
{
	<syn>{
		node xPrep[cat = par]	
	};
	<syn>{
		node xArg[idx = I]	
	}
	*=[cagentIdx=I]
}

class CanonicalGenitive
import 
	CanonicalPP[]
declare ?I
{
	<syn>{
		node xPrep[cat = de]	
	};
	<syn>{
		node xArg[idx = I]	
	}
	*=[genitiveIdx=I]
}

class CanonicalIobject
import 
	CanonicalPP[]
declare ?I
{
	<syn>{
		node xPrep[cat = à]	
	};
	<syn>{
		node xArg[idx = I]	
	}
	*=[iobjectIdx=I]
}

class CanonicalOblique
import 
	CanonicalPP[]
declare ?I
{
	<syn>{
		node xPrep};
	<syn>{
		node xArg[idx = I]	
	}
	*=[obliqueIdx=I]
}

class CanonicalLocative
import 
	 CanonicalnonSubjectArg[]	
declare ?I
{
	<syn>{
		node xtop(mark=subst)[cat=pp,loc = +,idx=I]
	}
	*=[locativeIdx=I]
}

% class CanonicalSententialXObjectWithComplementizer
% import
% 	CanonicalnonSubjectArg[]
% export xComp xArg

% declare
% 	?xX ?xComp ?xArg
% {
% 	<syn>{
% 		node xtop[cat=s]{
% 			node xX (color = red)[cat =c]{
% 				node xComp(color=red)
% 			}
% 			node xArg(color = red)[cat = s]
% 		}		
% 	}
% }

% class CanonicalSententialXObjectWithComplementizerNonBounding
% import
% 	CanonicalSententialXObjectWithComplementizer[]
% declare ?I
% {
% 	<syn>{
% 		node xArg(mark=foot)[idx=I]%replace by p2		
% 	}
% }

% % class CanonicalSententialXObjectWithComplementizerBounding
% % import
% % 	CanonicalSententialXObjectWithComplementizer[]

% % {
% % 	<syn>{
% % 		node xArg(mark=subst)		
% % 	}
% % }


% % class CanonicalSententialObjectFinite
% % import
% % 	CanonicalSententialXObjectWithComplementizerNonBounding[]

% % {
% % 	<syn>{
% % 		node xComp[cat=que]
% % 	}
% % }

% % class CanonicalSententialXObjectwithoutComplementizer
% % import 
% % 	CanonicalnonSubjectArg[]

% % {
% % 	<syn>{
% % 		node xtop(color=red)[cat=s]
% % 	}
% % }

% % class CanonicalSententialXObjectwithoutComplementizerBounding
% % import 
% % 	CanonicalnonSubjectArg[]
% % {
% % 	<syn>{
% % 		node xtop(color=red,mark=subst)[cat=s]
% % 	}
% % }

% % class CanonicalSententialXObjectwithoutComplementizerNonBounding
% % import 
% % 	CanonicalnonSubjectArg[]
% % {
% % 	<syn>{
% % 		node xtop(color=red,mark=foot)[cat=s] %replace foot by p2 
% % 	}
% % }

% % class CanonicalSententialObjectInFinitive
% % import
% % 	CanonicalSententialXObjectwithoutComplementizerNonBounding[]
% % {
% % 	<syn>{
% % 		node xtop % nothing much for now
% % 	}
% % }

% class CanonicalSententialObjectInFinitiveDe
%  import
%  	CanonicalSententialXObjectWithComplementizerNonBounding[]
%  {
%  	<syn>{
%  		node xComp[cat=de]			
%  	}
%  }

% % class CanonicalSententialObjectInFinitiveA
% % import
% % 	CanonicalSententialXObjectWithComplementizerNonBounding[]

% % {
% % 	<syn>{
% % 		node xComp[cat=à]			
% % 	}
% % }

% % class CanonicalSententialObjectInterrogativeFinite 
% % import
% % 	CanonicalSententialXObjectWithComplementizerBounding[]
% % {
% % 	<syn>{
% % 		node xComp(color=red)[cat=si]
% % 	};
% % 	<syn>{
% % 		node xArg(color = red)[cat = s]
% % 	}
% % }

% % class CanonicalSententialObjectInterrogativeInFinitive
% % import 
% % 	CanonicalSententialXObjectwithoutComplementizerBounding[]
	
% % {
% % 	<syn>{
% % 		node xtop %nothing to say for now (features)
% % 	}
% % }

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

% % Clitic subject ? see above (near canonical subject)
% % me te le les la nous vous leur leurs
class CliticObjectII
import nonReflexiveClitic[]
declare ?I
{
	<syn>{
		node xCl(rank=2)[case=acc,idx=I]
	}
	*=[objectIdx=I]
}
class CliticIobjectII
import nonReflexiveClitic[]
declare ?I
{
	<syn>{
		node xCl(rank=2)[case=dat,idx=I]
	}
	*=[iobjectIdx=I]
}

class CliticObject3
import nonReflexiveClitic[]
declare ?I
{
	<syn>{
		node xCl(rank=3)[case=acc3rd,idx=I]
	}
	*=[objectIdx=I]
}

class CliticIobject3
import nonReflexiveClitic[]
declare ?I
{
	<syn>{
		node xCl(rank=4)[case=dat3rd,idx=I]
	}
	*=[iobjectIdx=I]
}
class CliticGenitive
import nonReflexiveClitic[]
declare ?I
{
	<syn>{
		node xCl(rank=5)[case=gen,idx=I]
	}
	*=[genitiveIdx=I]
}
class CliticLocative
import 
	nonReflexiveClitic[]
declare ?I
{
	<syn>{
		node xCl(rank=6)[case=locative,idx=I]
	}
	*=[locativeIdx=I]
}

% % % Special clitics : reflexive
% % % me te se nous vous se 
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
declare ?I
{
	<syn>{
		node xCl(mark=subst,rank=2)[case=acc,idx=I]
	}
	*=[objectIdx=I]	
}

class reflexiveDative
import
	reflexiveClitic[]
declare ?I
{
	<syn>{
		node xCl(mark=subst,rank=2)[case=dat,idx=I]
	}
	*=[iobjectIdx=I]		
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
declare ?I
{
	<syn>{
		node xtop(mark=subst)[cat = n,idx=I]		
	}
	*=[objectIdx=I]
}

class whLocative
import
	UnboundedQuestion[]
declare ?I
{
	<syn>{
		node xtop(mark=subst)[cat = pp, loc = +,idx=I]		
	}
	*=[locativeIdx=I]
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
declare ?I
{
	<syn>{
		node xPrep[cat = à]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[iobjectIdx = I]
}

class whGenitive
import
	 whPrep[]
declare ?I
{
	<syn>{
		node xPrep[cat = de]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[genitiveIdx = I]
}
class whCAgent
import
	 whPrep[]
declare ?I
{
	<syn>{
		node xPrep[cat = par]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[cagentIdx = I]
}

class whOblique
import
	 whPrep[]
declare ?I
{
	<syn>{
		node xPrep 
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[obliqueIdx = I]
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
declare ?I
{
	<syn>{
		node xtop(mark=subst)[cat = n,idx=I]
	}
	*=[objectIdx=I]
}


class RelativeLocative
import 
	UnboundedRelative[]
declare ?I
{
	<syn>{
		node xtop(mark=subst)[cat = pp,loc = +,idx=I]
	}
	*=[locativeIdx=I]
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
declare ?I
{
	<syn>{
		node xPrep[cat = à]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[iobjectIdx=I]
}

class RelativeGenitive
import 
	RelativePP[]
declare ?I
{
	<syn>{
		node xPrep[cat = de]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[genitiveIdx=I]
}

class RelativeCAgent
import 
	RelativePP[]
declare ?I
{
	<syn>{
		node xPrep[cat = par]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[cagentIdx=I]
}

class RelativeOblique
import 
	RelativePP[]
declare ?I
{
	<syn>{
		node xPrep 
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[obliqueIdx=I]
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
	%xClefttop
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
declare ?I
{
	<syn>{
		node xComp[cat = que]
	};
	<syn>{
		node xClefttop[idx=I]
	     }
	*=[objectIdx=I]
}


class CleftDont
import 
	NominalCleft[]
declare ?I
{
	<syn>{
		node xComp[cat = dont]				
	};
	<syn>{
		node xClefttop[idx=I]
	     }
	*=[genitiveIdx=I]
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
declare ?I
{
	<syn>{
		node xPrep[cat = à]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[iobjectIdx=I]		
}

class CleftGenitiveOne
import 
	PrepCompCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = de]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[genitiveIdx=I]	
}

class CleftCAgentOne
import 
	PrepCompCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = par]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[cagentIdx=I]	
}

class CleftObliqueOne
import 
	PrepCompCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = par]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[obliqueIdx=I]	
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
declare ?I
{
	<syn>{
		node xPrep[cat = à]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[iobjectIdx=I]
}
class CleftGenitiveTwo
import 
	NominalPrepCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = de]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[genitiveIdx=I]	
}
class CleftCAgentTwo
import 
	NominalPrepCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = par]
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[cagentIdx=I]	
}

class CleftObliqueTwo
import 
	NominalPrepCleft[]
declare ?I
{
	<syn>{
		node xPrep 
	};
	<syn>{
		node xArg[idx = I]
	}
	*=[obliqueIdx=I]	
}

class BoundedExtraction
import VerbalArgument[]

% %Dummy class for now

class whSubject
import BoundedExtraction[]
declare
	?xArg ?SubjI
{
	<syn>{
		node xS{
			node xArg(color=red,mark=subst,extracted = +)[cat=n,idx=SubjI]
			node xVN
		}
	}
	*=[subjectIdx=SubjI]
}

class RelativeSubject
import BoundedExtraction[]
declare
	?xArg ?xfoot ?xRel ?SubjI
{
	<syn>{
		node xRel(color=red)[cat = n]{
			node xfoot(color=red,mark=foot)[cat=n,idx=SubjI]
			node xS{
				node xArg(color=red,mark=subst,extracted = +)[cat=n,wh=rel]
				node xVN
			}
		}
	}
	*=[subjectIdx=SubjI]
}

class CleftSubject
import BoundedExtraction[]
declare
	?xSe ?xVNCleft ?xCl ?xVcleft ?xArg ?xComp ?xCompl ?I

{
	<syn>{
		node xSe(color=red)[cat = s]{
			node xVNCleft(color=red)[cat=vn]{
				node xCl(color=red,mark=subst)[cat=cl]
				node xVcleft(color=red,mark=subst)[cat=v]
			}
			node xArg(color=red,mark=subst,extracted = +)[cat=n,idx=I]
			node xS{
				node xComp(color=red)[cat=c]{
					node xCompl(color=red)[cat=qui]
				}
				node xVN
			}
		}
	}
	*=[subjectIdx=I]
}

% %%%%%%%%%%%%%%

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
% class SententialCObject{
% 	CanonicalSententialObjectFinite[]
% 	|CanonicalSententialObjectInFinitive[]
% }
% class SententialDeObject{
% % 	CanonicalSententialObjectFinite[]|
% 	CanonicalSententialObjectInFinitiveDe[]
% }
% class SententialAObject{
% 	CanonicalSententialObjectFinite[]
% 	|CanonicalSententialObjectInFinitiveA[]
% }
% class SententialInterrogative{
% 	CanonicalSententialObjectInterrogativeFinite[]
% 	|CanonicalSententialObjectInterrogativeInFinitive[]
% }

class Iobject
{
	CanonicalIobject[]
  	|whIobject[]
  	|CliticIobjectII[]
  	|CliticIobject3[]
% % 	|CliticLocative[] %Jean pense à Marie / y pense (pas de distinction ici) 
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

% Diathesis ALTERNATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%

% class dian0V{
%  	{Subject[] | ImpersonalSubject[];Object[]} ; activeVerbMorphology[]  	
%  }

% class dian0V{
%  	Subject[] ; activeVerbMorphology[]  	
%  }

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


% % FAMILIES
% %%%%%%%%%%%%%%%%%%%%%%%%%%

% class n0V{
%  	dian0V[]
%  }


% class n0Vn1den2{
% 		n0Vn1[];Genitive[]
% }
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
% class n0Vn1loc2{
% 		n0Vn1[];Locative[]
% }
% class n0Vcs1{
% 		Subject[]; SententialCObject[]; activeVerbMorphology[]   
% }
% class n0Vas1{
% 		Subject[]; SententialAObject[]; activeVerbMorphology[]   
% }
% class n0Vdes1{
% 		Subject[]; SententialDeObject[]; activeVerbMorphology[]   
% }
% class n0Vcs1an2{
% 		Subject[]; SententialCObject[]; Iobject[]; activeVerbMorphology[]   
% }
% class n0Vs1int{
% 	Subject[]; SententialInterrogative[]; activeVerbMorphology[]   
% }
% class n0Vs1intan2{
% 	Subject[]; SententialInterrogative[]; activeVerbMorphology[]   
% }
% class n0Vn1des2{
%  	n0Vn1[]; SententialDeObject[]
% }
% class n0Vn1as2{
% 	n0Vn1[]; SententialAObject[]
% }
% class n0Vn1cs2{
% 	n0Vn1[]; SententialCObject[]
% }
% class n0Vs1des2{
% 	Subject[]; SententialCObject[]; SententialDeObject[]; activeVerbMorphology[]   
% }
% class n0Vcs1des2{
% 	Subject[]; SententialCObject[]; SententialDeObject[]; activeVerbMorphology[]
% }
% class n0Vdes1den2{
% 	Subject[]; SententialDeObject[]; Genitive[]; activeVerbMorphology[]   
% }
class s0V{
	SententialSubject[]; activeVerbMorphology[]
}
class s0Vn1{
	SententialSubject[]; Object[]; activeVerbMorphology[]
}
class s0Van1{
	SententialSubject[]; Iobject[]; activeVerbMorphology[]
}
% % class s0Vcs1{
% % 	SententialSubject[]; SententialCObject[]; activeVerbMorphology[]
% % }
% % class s0Vn1as2{
% % 	SententialSubject[]; Object[]; SententialAObject[]; activeVerbMorphology[]
% % }
% % class n0Vdes1pn2{
% % 	Subject[]; Oblique[]; SententialDeObject[]; activeVerbMorphology[]
% % }
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
% % VALUATION
% %%%%%%%%%%%%%%%%%%%%%%%%%

% % class n0Vn1an2
% %       {
% % 		n0Vn1[];Iobject[]
% % }

class binaryPredicates
declare ?E ?A ?B 
{
		binaryRel[]*=[evt=E,arg1=A,arg2=B];
                {dian0Vn1Active[]*=[vbI=E,subjectIdx=A,objectIdx=B]
		| dian0Vn1Passive[]*=[vbI=E,subjectIdx=B,cagentIdx=A]
 		| dian0Vn1dePassive[]*=[vbI=E,subjectIdx=B,genitiveIdx=A] 
  		| dian0Vn1ShortPassive[]*=[vbI=E,subjectIdx=B]    
  		| dian0Vn1ImpersonalPassive[]*=[vbI=E,subjectIdx=B,objectIdx=A]
  		| dian0Vn1middle[]*=[vbI=E,subjectIdx=B]
		| dian0Van1[]*=[vbI=E,subjectIdx=A,iobjectIdx=B]  
		| dian0Vden1[]*=[vbI=E,subjectIdx=A,genitiveIdx=B] 
		| n0Vpn1[]*=[vbI=E,subjectIdx=A,obliqueIdx=B]      
		| n0Vloc1[]*=[vbI=E,subjectIdx=A,locativeIdx=B] 
		| n0ClV[]*=[vbI=E,subjectIdx=A,iobjectIdx=B] 
		| s0Vn1[]*=[vbI=E,subjectIdx=A,objectIdx=B] 
		}     
}

class ternaryPredicates
declare ?E ?A ?B ?C
{
		ternaryRel[]*=[evt=E,arg1=A,arg2=B,arg3=C];
                {
		 {dian0Vn1Active[];Iobject[]}*=[vbI=E,subjectIdx=A,objectIdx=B,iobjectIdx=C]
		|{dian0Vn1Passive[];Iobject[]}*=[vbI=E,subjectIdx=B,cagentIdx=A,iobjectIdx=C]
		|{dian0Vn1dePassive[];Iobject[]}*=[vbI=E,subjectIdx=B,genitiveIdx=A,iobjectIdx=C] 
  		| {dian0Vn1ShortPassive[];Iobject[]}*=[vbI=E,subjectIdx=B,iobjectIdx=C]    
  		| {dian0Vn1ImpersonalPassive[];Iobject[]}*=[vbI=E,subjectIdx=B,
			objectIdx=A,iobjectIdx=C]		
 		| {dian0Vn1middle[];Iobject[]}*=[vbI=E,subjectIdx=B,iobjectIdx=C]
		| {dian0Vn1Active[];Genitive[]}*=[vbI=E,subjectIdx=A,objectIdx=B,genitiveIdx=C]
		| {dian0Vn1Passive[];Genitive[]}*=[vbI=E,subjectIdx=B,cagentIdx=A,genitiveIdx=C]
  		| {dian0Vn1ShortPassive[];Genitive[]}*=[vbI=E,subjectIdx=B,genitiveIdx=C]    
  		| {dian0Vn1ImpersonalPassive[];Genitive[]}*=[vbI=E,subjectIdx=B,
			objectIdx=A,genitiveIdx=C]
		| {dian0Vn1middle[];Genitive[]}*=[vbI=E,subjectIdx=B,genitiveIdx=C]
		| n0Van1den2[]*=[vbI=E,subjectIdx=A,iobjectI=C,genitiveIdx=B]
		| n0Vden1pn2[]*=[vbI=E,subjectIdx=A,obliqueI=C,genitiveIdx=B]
		| {dian0Vn1Active[];Locative[]}*=[vbI=E,subjectIdx=A,objectIdx=B,locativeIdx=C]
		| {dian0Vn1Passive[];Locative[]}*=[vbI=E,subjectIdx=B,cagentIdx=A,locativeIdx=C]
 		| {dian0Vn1dePassive[];Locative[]}*=[vbI=E,subjectIdx=B,genitiveIdx=A,locativeIdx=C] 
  		| {dian0Vn1ShortPassive[];Locative[]}*=[vbI=E,subjectIdx=B,locativeIdx=C]    
  		| {dian0Vn1ImpersonalPassive[];Locative[]}*=[vbI=E,subjectIdx=B,objectIdx=A,
			locativeIdx=C] 
  		| {dian0Vn1middle[];Locative[]}*=[vbI=E,subjectIdx=B,locativeIdx=C]
  		| n0ClVn1[]*=[vbI=E,subjectIdx=A,iobjectIdx=C]
 		| n0ClVden1[]*=[vbI=E,subjectIdx=A,genitiveIdx=B,iobjectIdx=C]
 		| n0ClVpn1[]*=[vbI=E,subjectIdx=A,obliqueIdx=B,iobjectIdx=C]
}}

value binaryPredicates
value ternaryPredicates
value RelativeObject
