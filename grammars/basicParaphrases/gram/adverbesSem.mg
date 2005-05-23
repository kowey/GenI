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
	?fX ?fY ?fZ ?fU ?fV ?fW ?fR ?fS ?fT ?E

{
	<syn>{
		node xR[cat = v,bot=[idx=E,mode = ?fX, num = ?fY, gen = ?fZ, pers = ?fU, pp-num = ?fV,pp-gen = ?fW, inv = ?fR, aux = ?fS, aux-pass = ?fT]];
		node xFoot[cat = v,top=[idx=E,mode = ?fX, num = ?fY, gen = ?fZ, pers = ?fU, pp-num = ?fV,pp-gen = ?fW, inv = ?fR, aux = ?fS, aux-pass = ?fT]]	
	};
	semRel[]*=[evt=E]
		
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
		node xR[cat=n,bot=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW,bar=2]];
		node xFoot[cat=n,top=[det = ?fX, def = ?fT, num = ?fY,gen = ?fZ,pers = ?fU, wh = ?fW,bar=@{0,1,2}]];
		node xComp(mark=subst)
	}
}
class s0Pn1can
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

class abstractCleftPPMod
export 
	?xTop ?xVN ?xV ?xCl ?xArg ?xS ?xPied ?xFoot
declare
	?xTop ?xVN ?xV ?xCl ?xArg ?xS ?xPied ?xFoot
{
	<syn>{
		node xTop(color=red)[cat = s, bot = [wh = -]]{
			node xVN(color=red)[cat = vn]{
				node xV(color=red,mark=subst)[cat = v, top = [pers = 3,mode = @{ind,subj}]]
				node xCl(color=red,mark=subst)[cat = cl,top=[case = ce]]
			}
			node xArg(color=red)
			node xS(color=red,mark=nadj)[cat = s]{
				node xPied(color=red)
				node xFoot(color=red,mark=foot)[cat = s,top=[wh = -]]
			}
		}
	}
}

class cleftPPModOne
import 
	abstractCleftPPMod[]
declare
	?xQue ?xP ?xN
{
	<syn>{
		node xArg(color=red)[cat=pp]{
			node xP(color=red,mark=anchor)[cat = p]
			node xN(color=red,mark=subst)[cat = n, top=[det = +]]
		};
		node xPied(color=red)[cat = c]{
			node xQue(color=red,mark=flex)[cat = que]
		}
	}
}

class cleftPPModTwo
import 
	abstractCleftPPMod[]
declare
	?xP ?xN
{
	<syn>{
		node xArg(color=red,mark=subst)[cat = n, top=[det = +, def = +]];
		node xPied(color=red)[cat = pp]{
			node xP(color=red,mark=anchor)[cat = p]
			node xN(color=red,mark = subst)[cat = n, top=[wh = +]]
		}	
	}
}	

class ExtractedPPMod
export
	xE xPP xP xN xS
declare
	?xE ?xPP ?xP ?xN ?xS
{
	<syn>{
		node xE(color=red)[cat = s,bot=[wh = +]]{
			node xPP(color=red)[cat = pp]{
				node xP(color=red,mark=anchor)[cat = p]
				node xN(color=red,mark=subst)[cat = n,top=[det = +,wh = +]]
			}
			node xS(color=red,mark=foot)[cat = s,top = [wh = -]]
		}
	}
}

class whPPMod
import 
	 ExtractedPPMod[]
{
	<syn>{
		node xS[top = [inv = +,wh = +]]
	}
}
class  RelPPMod
import 
	ExtractedPPMod[]
declare
	?xTop ?xFoot ?fX ?fY ?fZ ?fU ?fW
{
	<syn>{
		node xTop(color=red)[cat = n,bot = [det = ?fX, def = ?fY, num = ?fZ, gen = ?fU, pers= ?fW]]{
			node xFoot(color=red,mark=foot)[cat = n,top=[det = ?fX, def = ?fY, num =  ?fZ, gen = ?fU, pers= ?fW]]
			node xE(color=red,mark=nadj)[cat = s,bot = [wh = +]]
		}
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
class s0Pn1
{
	s0Pn1can[]
	|whPPMod[]
	|RelPPMod[]
	|cleftPPModOne[]
	|cleftPPModTwo[]
}
