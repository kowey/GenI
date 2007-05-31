class VerbalMorphology
export
   ?xS ?xVN ?xV
declare
   ?xS ?xVN ?xV ?I ?L
{
        <syn>{
                node ?xS(color=black)[cat = s, top = [mode=ind]]{
                        node ?xVN(color=black)[cat = v]{
                                node ?xV(mark=anchor,color=black)[cat = v,top = [idx=I,label=L]]
                        }
                }
        }*=[vbI=I,vbL = L]
}

class activeVerbMorphology
import
        VerbalMorphology[]
declare
        ?fY ?fZ ?fW
{
        <syn>{
                node ?xVN[bot=[num = ?fY,gen = ?fZ,pers=?fW]]{
                        node ?xV[top=[num = ?fY,gen = ?fZ,pers=?fW]]
                }
        }
}

class passiveVerbMorphology
import
   VerbalMorphology[]
export
   ?xInfl
declare
   ?xInfl ?fX ?fY ?fZ
{
        <syn>{
                     node ?xVN[bot=[num = ?fX, gen = ?fY, pers = ?fZ]]{
                        node ?xInfl(color=black,mark=subst)[cat = v,top=[num = ?
fX, gen = ?fY, pers = ?fZ]]
                        node ?xV(color=black)[cat = v]
                     }
        }
}

class dian0Vactive[L,E,X]
{
        Subject[X,L]
	; activeVerbMorphology[]*=[vbI=E,vbL = L]
}

class dian0Vn1Active[L,E,X,Y]
{
        dian0Vactive[L,E,X]
	; Object[Y,L]
}

class dian0Vn1Passive[L,E,X,Y]
{
        Subject[Y,L]
	; CAgent[X,L]
	; passiveVerbMorphology[]*=[vbI=E,vbL = L]
}

class n0V[L,E,X]
{
	unaryRel[]*=[label0=L,arg0=E,arg1=X];
        dian0Vactive[L,E,X]
}

class n0Vn1[L,E,X,Y]
{
	 binaryRel[]*=[label0=L,arg0=E,arg1=X,arg2=Y] ;
	{
         dian0Vn1Active[L,E,X,Y]
         | dian0Vn1Passive[L,E,X,Y]
	}
}
