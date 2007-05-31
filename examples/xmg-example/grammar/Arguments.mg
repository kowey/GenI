class  VerbalArgument
export
        ?xS ?xV
declare
        ?xS ?xV
{
        <syn>{
                node ?xS(color=white)[cat = s]{
                        node ?xV(color=white)[cat = v]
                }
        }
}

class  SubjectAgreement
export
        ?xSubjAgr ?xVAgr
declare
        ?xSubjAgr ?xVAgr ?fX ?fY ?fZ
{
        <syn>{
                node ?xSubjAgr[top=[num = ?fX, gen = ?fY, pers = ?fZ]];
                node ?xVAgr[top=[num = ?fX, gen = ?fY, pers = ?fZ]]
        }
}

class  CanSubject
import
        VerbalArgument[]
        SubjectAgreement[]
export
        ?xSubj
declare
        ?xSubj
{
        <syn>{
                node ?xS[cat = s]{
                        node ?xSubj(color=red)[cat = @{cl,n}]
                        node ?xV[cat = v]
                };
                ?xSubj = ?xSubjAgr;
                ?xV = ?xVAgr
        }
}

class  CanonicalSubject
import
        CanSubject[]
	SubjectSem[]
{
        <syn>{
             node ?xSubj(color=red,mark=subst)[cat = n]
        };
	?xSubj = ?xSem
}

class  CliticSubject
import
        CanSubject[]
	SubjectSem[]
{
        <syn>{
             node ?xSubj(color=red,mark=subst)[cat = cl]
        };
	?xSubj = ?xSem
}

class  RelativeSubject
import
        VerbalArgument[]
        SubjectAgreement[]
	SubjectSem[]
declare
        ?xSubj ?xfoot ?xRel ?fU ?fY ?fZ ?xQui
{
        <syn>{
                node ?xRel(color=red)[cat = n,bot=[num = ?fY,gen = ?fZ,pers = ?fU]]{
                        node ?xfoot(color=red,mark=foot)[cat=n,top=[num = ?fY,gen = ?fZ,pers = ?fU]]
                        node ?xS(mark=nadj){
                                node ?xSubj(color=red,extracted = +)[cat=c]{
                                        node ?xQui(color=red,mark=flex)[cat=qui]
                                }
                                node ?xV
                        }
                }
        };
        ?xfoot = ?xSubjAgr;
        ?xV = ?xVAgr;
	?xfoot = ?xSem
}

class  CanonicalObject
import
        VerbalArgument[]
	ObjectSem[]
export
        ?xObj
declare
        ?xObj
{
        <syn>{
                node ?xS{
                        node ?xV
                        node ?xObj(mark=subst, color=red)[cat = n]
                }
        };
	?xObj = ?xSem
}

class  CanonicalCAgent
import
        VerbalArgument[]
	CAgentSem[]
export
        ?xtop ?xArg ?xX
declare
        ?xtop ?xArg ?xX ?xPrep
{
        <syn>{
                node ?xtop[cat = pp](color=red){
                        node ?xX (color=red)[cat = p] {
				node ?xPrep(mark=flex,color=red)[cat=par]
			}
                        node ?xArg(mark=subst,color=red)[cat = n]
                }
        } ;
	?xArg = ?xSem ;
	<syn>{ ?xS -> ?xtop ; ?xV >> ?xtop }
}


class Subject[I,L]
{
	CanonicalSubject[]*=[subjectI = I, subjectL = L]
	| RelativeSubject[]*=[subjectI = I, subjectL = L]
	| CliticSubject[]*=[subjectI = I, subjectL = L]
}

class Object[I,L]
{
	CanonicalObject[]*=[objectI = I, objectL = L]
}

class CAgent[I,L]
{
	CanonicalCAgent[]*=[cagentI = I, cagentL = L]
}
