

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
                node xS(color=black)[cat = s,bot=[mode=?fX],bot=[idx=I]]{
                        node xVN(color=black)[cat = vn,top=[mode=?fX],bot=[idx=I]]{
                                node xV(mark=anchor,color=black)[cat = v,bot=[idx=I]]
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
				node xCop(color=black,mark=flex)[cat = v, phon=etre, top=[cop = +, mode= ?fY,pers=?fW,num=?fZ]]
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

class ** passiveVerbMorphology
import
   VerbalMorphology[]
export
   xInfl
declare
   ?xInfl ?fS ?fT ?fU ?fV ?fX ?fY ?fZ ?fW ?f0
{
        <syn>{
		     node xVN[bot=[num = ?fX, gen = ?fY, pers = ?fZ, mode = ?fW,inv = ?f0]]{
                     	node xInfl(color=black,mark=flex)[cat = v,phon=etre,top=[num = ?fX, gen = ?fY, pers = ?fZ, mode = ?fW,cop = +,inv = ?f0],bot=[inv = -, num = ?fT,gen = ?fU, pers = ?fV, mode = ?fS]]
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

class ** middleVerbMorphology
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

class ** properReflexive
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
        ?xS ?xVN ?E 
{
        <syn>{
                node xS(color=white)[cat = @{s,n},bot=[idx=?E]]{
                        node xVN(color=white)[cat = @{vn,adj,n},bot=[idx=?E]]
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

class ** InfinitiveSubject
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
class ** ImperativeSubject
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

class ** CanonicalSubject
import
        RealisedNonExtractedSubject[]
declare ?I
{
        <syn>{
             node xSubj(mark=subst)[cat = n,top=[idx=I]]
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

class ** ImpersonalSubject
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



class ** CanonicalSententialSubjectInFinitive 
%#Aller à la montagne plait à Marie
import 
	CanonicalArgument[]
declare
	?xSubj ?I
{
	<syn>{
		node xS{
			node xSubj(color=red,mark=subst)[cat=s,top=[idx=I]]
			node xVN
		}
	}*=[subjectI=I]
}

class ** CanonicalSententialSubjectFinite
%#Que Jean aille à la montagne plait à Marie
import
	CanonicalArgument[]
declare
	?xSubj ?xTop ?xComp ?xQue ?I
{
	<syn>{
	     node xS{
		node xTop(color=red)[cat=s,bot=[idx = I]]{
			node xComp(color=red)[cat=c]{
				node xQue(color=red,mark=flex)[cat=que,phon=que]				}
			node xSubj(color=red,mark=subst)[cat=s,top=[idx=I]]}			node xVN
		}
	}*=[subjectI=I]
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

class ** CanonicalObject
import
        CanonicalnonSubjectArg[]
declare ?I
{
        <syn>{
                node xtop(mark=subst)[cat = n,top=[det = +,idx=I]]
        }*=[objectI=I]
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

%% pp(p([prep]) !n)
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

class ** CanonicalCAgent
import 
	CanonicalPP[]
declare ?I
{
	<syn>{
		node xPrep[cat = par];
		node xArg[top=[idx = I]]	
	}*=[cagentI=I]
}

class ** CanonicalGenitive
import 
	CanonicalPP[]
declare ?I
{
	<syn>{
		node xPrep[cat = de];
		node xArg[top=[idx = I]]	
	}*=[genitiveI=I]
}

class ** CanonicalIobject
import 
	CanonicalPP[]
declare ?I
{
	<syn>{
		node xPrep[cat = a];
		node xArg[top=[idx = I]]	
	}*=[iobjectI=I]
}

class ** CanonicalOblique
import 
	CanonicalPP[]
declare ?I
{
	<syn>{
		node xPrep(name=obliquePrep);
		node xArg[top=[idx = I]]	
	}*=[obliqueI=I]
}

class ** CanonicalLocative
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

class  ** CanonicalSententialObjectFinite
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

class ** CanonicalSententialObjectInFinitive
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

class  ** CanonicalSententialObjectInFinitiveDe
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

class ** CanonicalSententialObjectInFinitiveA
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

class ** CanonicalSententialObjectInterrogativeFinite 
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

class ** CanonicalSententialObjectInterrogativeInFinitive
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

class ** CliticObjectII
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
class ** CliticIobjectII
import 
	nonReflexiveClitic[]
declare ?I	
{
	<syn>{
		node xCl(rank=2)[top=[idx=I,case=dat]]
	}*=[objectI=I]
}
class  ** CliticObject3
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

class ** CliticIobject3
import 
	nonReflexiveClitic[]
{
	<syn>{
		node xCl(rank=4)[top=[case=dat3rd]]
	}
}
class ** CliticGenitive
import 
	nonReflexiveClitic[]
declare ?I
{
	<syn>{
		node xCl(rank=5)[top=[case=gen,idx=I]]
	}*=[genitiveI=I]
}

class ** CliticLocative
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


class ** reflexiveAccusative
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

class  ** reflexiveDative
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
	?xSe ?xtop ?fX ?E
{
	<syn>{
		node xSe(color=red,extracted)[cat = s,bot=[idx=?E]]{
			node xtop(color=red)[top=[wh = +]]
			node xS[top=[wh = -,inv = @{n,-}],bot=[idx=?E]]   
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
		node xtop(mark=subst)[cat = n,top=[idx=I,wh = +]];		
		NCO = NonCanonicalObject[];
		xVN = NCO.xVNAgr;
		xtop = NCO.xObjAgr
	}*=[objectI=I]
}

class  ** whLocative
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

class ** whIobject
import
	 whPrep[]
{
	<syn>{
		node xPrep[cat = a]
	}
}

class ** whGenitive
import
	 whPrep[]
declare ?I
{
	<syn>{
		node xPrep[cat = de];
		node xArg[top=[idx = I]]
	}*=[genitiveI = I]
}

class ** whCAgent
import
	 whPrep[]
declare ?I
{
	<syn>{
		node xPrep[cat = par];
		node xArg[top=[idx = I]]
	}*=[cagentI=I]
}

class ** whOblique
import
	 whPrep[]
declare ?I
{
	<syn>{
		node xPrep(name=obliquePrep);
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

class ** RelativeObject
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


class  ** RelativeLocative
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

class ** RelativeIobject
import 
	RelativePP[]
declare ?I
{
	<syn>{
		node xPrep[cat = a];
		node xRelf[top=[idx=I]]
	}*=[iobjectI=I]
}

class ** RelativeGenitive
import 
	RelativePP[]
declare ?I
{
	<syn>{
		node xPrep[cat = de];
		node xArg[top=[idx = I]]
	}*=[genitiveI=I]
}

class  ** RelativeCAgent
import 
	RelativePP[]
declare ?I
{
	<syn>{
		node xPrep[cat = par];
		node xArg[top=[idx = I]]
	}*=[cagentI=I]
}

class ** RelativeOblique
import 
	RelativePP[]
declare ?I
{
	<syn>{
		node xPrep(name=obliquePrep);
		node xArg[top=[idx = I]] % nothing much for now
	}*=[obliqueI=I]
}

class UnboundedCleft
import
	UnboundedExtraction[]
export 
	xClefttop
declare
	?xCleft ?xVNCleft ?xClCleft ?xAuxCleft ?xClefttop ?E
{
	<syn>{
		node xCleft(color=red)[cat = s,bot=[wh = - , idx=?E]]{
			node xVNCleft(color=red)[cat=vn]{
				node xClCleft(color=red)[cat=cl,phon=ce,top=[case = ce]]
				node xAuxCleft(color=red)[cat=v,phon=etre,top=[mode=@{ind,subj},pers=3]]
			}
			node xClefttop(color=red)
			node xSe(mark=nadj)[bot=[idx=?E]]	
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

class ** CleftObject
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

class ** CleftDont
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
			node xArg(color=red,mark=subst)[det = +, cat = @{qui,n},top=[wh = -, num = sg]]		
		} 		
	};
	<syn>{
		node xtop[cat=c]{
			node xComp(color=red,mark=flex)[cat = que]
		}
	}
}

class ** CleftIobjectOne
import 
	PrepCompCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = a];
		node xArg[top=[idx = I]]
	}*=[iobjectI=I]	
}

class ** CleftGenitiveOne
import 
	PrepCompCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = de];
		node xArg[top=[idx = I]]
	}*=[genitiveI=I]	
}

class  ** CleftCAgentOne
import 
	PrepCompCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = par];
		node xArg[top=[idx = I]]
	}*=[cagentI=I]		
}

class ** CleftObliqueOne
import 
	PrepCompCleft[]
declare ?I
{
	<syn>{
		node xPrep(name=obliquePrep);
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
			node xArg(color=red,mark=flex)[cat = qui,
			top=[wh = +]]
		}
	}
}	
class  ** CleftIobjectTwo
import 
	NominalPrepCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = a];
		node xClefttop[top=[idx = I]]
	}*=[iobjectI=I]	
}

class ** CleftGenitiveTwo
import 
	NominalPrepCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = de];
		node xArg[idx = I]
	}*=[genitiveI=I]	
}
class ** CleftCAgentTwo
import 
	NominalPrepCleft[]
declare ?I
{
	<syn>{
		node xPrep[cat = par];
		node xClefttop[top=[idx=I]]
	}*=[cagentI=I]
}

class  ** CleftObliqueTwo
import 
	NominalPrepCleft[]
declare ?I
{
	<syn>{
		node xPrep(name=obliquePrep);
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

class  ** whSubject
import 
	BoundedExtraction[]
declare ?I
{
	<syn>{
		node xS[top=[wh = +]]{ %should be wh = + better w/ a default ?
			node xArg(color=red,mark=subst,extracted = +)[cat=n,top=[idx=I,wh = +]]
			node xVN
		}		
	}*=[subjectI=I]
}

class ** RelativeSubject
import 
	BoundedExtraction[]
declare
	?xfoot ?xRel ?fT ?fU ?fW ?fX ?fY ?fZ ?I
{
	<syn>{
		node xRel(color=red)[cat = n,bot=[idx=I,det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]{
			node xfoot(color=red,mark=foot)[cat=n,top=[idx=I,det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]
			node xS(mark=nadj)[bot=[mode=ind,wh = -]]{
				node xArg(color=red,mark=subst,extracted = +)[cat=n,top=[wh=rel,idx=I]]
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
	?I ?xR ?xHead ?xFoot ?fT ?fU ?fW ?fX ?fY ?fZ !L ?P
{
	<syn>{
		node xR(color=red)[cat=n, idx=?I, 
                                   bot =[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]
                {
			node xHead(color=red,mark=anchor)[cat = adj, top=[num = ?fY,gen = ?fZ]]
			node xFoot(color=red,mark=foot)[cat = n,
			idx=?I, top=[det = ?fX, def = ?fT, num = ?fY,
			gen = ?fZ,pers = ?fU, wh = ?fW]]
		}; fX= -
		}*=[arg1=I];	
	<sem>{
		L:P(I)
		}
}

class EpithPost
declare
	?I ?xR ?xHead ?xFoot ?fT ?fU ?fW ?fX ?fY ?fZ !L ?P 
{
	<syn>{
		node xR(color=black)[cat=n, idx=?I, 
                                     bot=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]
                {
			node xFoot(color=red,mark=foot)[cat = n, idx=?I, top=[det = ?fX, def = ?fT, num =?fY,gen = ?fZ,pers = ?fU, wh = ?fW]]
			node xHead(color=black,mark=anchor)[cat = adj,top=[num = ?fY,gen = ?fZ]]
		}
                ; fX= -
	}*=[arg1=I];	
	<sem>{
		L:P(I)
		}
}


%%%%%%%%%%%%%%%%%%%
% LEXICAL CLASSES %
%%%%%%%%%%%%%%%%%%%


%* <Fonctions>
%%%%%%%%%%%%%%

class Subject
{
	ImperativeSubject[]
	|InfinitiveSubject[]
	|CanonicalSubject[]
	|CliticSubject[]
	|whSubject[]
	|RelativeSubject[]
	|CleftSubject[]
}

class Object
{
	whObject[]
	|RelativeObject[]
	|CanonicalObject[]
	|CliticObjectII[]
	|CliticObject3[]
	|reflexiveAccusative[]	
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
	|CliticLocative[] %Jean pense à Marie / y pense (pas de distinction ici)	|reflexiveDative[]
	|RelativeIobject[]
	|CleftIobjectOne[]
	|CleftIobjectTwo[]
}


class CAgent
{
	whCAgent[]
	|RelativeCAgent[]
	|CleftCAgentOne[]
	|CleftCAgentTwo[]
	|CanonicalCAgent[]
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
