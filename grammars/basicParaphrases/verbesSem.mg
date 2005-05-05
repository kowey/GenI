
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
                Subject[]*=[subjectI=Y] ; CAgent[]*=[cagentI=X]  ; passiveVerbMorphology[]*=[vbI=E]        
}
class dian0Vn1dePassive[E,X,Y]{
                Subject[]*=[subjectI=Y] ; Genitive[]*=[genitiveI=X]  ; passiveVerbMorphology[]*=[vbI=E]        
}
class dian0Vn1ShortPassive[E,X,Y]{
                Subject[]*=[subjectI=Y] ; passiveVerbMorphology[]*=[vbI=E]        
}
class dian0Vn1ImpersonalPassive[E,X,Y]{
                ImpersonalSubject[] ; Object[]*=[objectI=X]; passiveVerbMorphology[]*=[vbI=E]        
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


class n0V[E,X,T1,P]{
	unaryRel[]*=[evt=E,arg1=X,theta1=T1,rel=P] ;
	dian0V[E,X]
}
class n0Vinf[E,X,T1,P]{
	unaryRel[]*=[evt=E,arg1=X,theta1=T1,rel=P] ;
	InfinitiveSubject[]*=[subjectI=X] ; activeVerbMorphology[]*=[vbI=E]     }
class n0Vn1[E,X,Y,T1,T2,P]{
 		binaryRel[]*=[evt=E,arg1=X,arg2=Y,theta1=T1,theta2=T2,rel=P] ;
               { dian0Vn1Active[E,X,Y]
		| dian0Vn1Passive[E,X,Y]
		| dian0Vn1dePassive[E,X,Y]
		| dian0Vn1ShortPassive[E,X,Y]
		| dian0Vn1ImpersonalPassive[E,X,Y]
		| dian0Vn1middle[E,X,Y]
		}
}
class ilV[E,P]{
	semRel[]*=[evt=E,rel=P];      
	{ImpersonalSubject[];activeVerbMorphology[]*=[vbI=E]}
}

class n0Van1[E,X,Y,T1,T2,P]{
	binaryRel[]*=[evt=E,arg1=X,arg2=Y,theta1=T1,theta2=T2,rel=P] ;
        dian0Van1[E,X,Y]
}
class n0Vn1an2[E,X,Y,Z,T1,T2,T3,P]{
		ternaryRel[]*=[evt=E,arg1=X,arg2=Y,arg3=Z,theta1=T1,theta2=T2,theta3=T3,rel=P] ;
		{n0Vn1[E,X,Y,T1,T2,P];
 		Iobject[]*=[iobjectI=Z]}
}
class n0ClV[E,X,T1,P]{
	unaryRel[]*=[evt=E,arg1=X,theta1=T1,rel=P] ;
	{Subject[]*=[subjectI=X]; properReflexive[]*=[vbI=E]}
}
class n0ClVn1[E,X,Y,T1,T2,P]{
	binaryRel[]*=[evt=E,arg1=X,arg2=Y,theta1=T1,theta2=T2,rel=P] ;
	{Subject[]*=[subjectI=X]; Object[]*=[objectI=Y]; properReflexive[]*=[vbI=E]  }
}
class n0Vden1[E,X,Y,T1,T2,P]{
      binaryRel[]*=[evt=E,arg1=X,arg2=Y,theta1=T1,theta2=T2,rel=P] ;
	dian0Vden1[E,X,Y]
}
class n0ClVden1[E,X,Y,T1,T2,P]{
      binaryRel[]*=[evt=E,arg1=X,arg2=Y,theta1=T1,theta2=T2,rel=P] ;
      {Subject[]*=[subjectI=X]; Genitive[]*=[genitiveI=Y]; properReflexive[]*=[vbI=E]}
}
class n0ClVpn1[E,X,Y,T1,T2,P]{
      binaryRel[]*=[evt=E,arg1=X,arg2=Y,theta1=T1,theta2=T2,rel=P] ;
      {Subject[]*=[subjectI=X]; Oblique[]*=[obliqueI=Y]; properReflexive[]*=[vbI=E]  	}
}
class n0Vloc1[E,X,Y,T1,T2,P]{
      binaryRel[]*=[evt=E,arg1=X,arg2=Y,theta1=T1,theta2=T2,rel=P] ;
      {	Subject[]*=[subjectI=X];Locative[]*=[locativeI=Y];activeVerbMorphology[]*=[vbI=E]
}}
class n0Vpn1[E,X,Y,T1,T2,P]{
      binaryRel[]*=[evt=E,arg1=X,arg2=Y,theta1=T1,theta2=T2,rel=P] ;
      {	Subject[]*=[subjectI=X];Oblique[]*=[obliqueI=Y];activeVerbMorphology[]*=[vbI=E]
}}
class n0Van1des2_0[E,X,Y,Z,T1,T2,T3,P]{
        ternaryRel[]*=[evt=E,arg1=X,arg2=Y,arg3=Z,theta1=T1,theta2=T2,theta3=T3,rel=P] ;
        Subject[]*=[subjectI=X];
	CanonicalSententialObjectInFinitiveDe[]*=[sobjectI=Z,controlI=X];
	Iobject[]*=[iobjectI=Y];activeVerbMorphology[]*=[vbI=E] 
}
class n0Vn1des2_1[E,X,Y,Z,T1,T2,T3,P]{
        ternaryRel[]*=[evt=E,arg1=X,arg2=Y,arg3=Z,theta1=T1,theta2=T2,theta3=T3,rel=P] ;
        Subject[]*=[subjectI=X];
	CanonicalSententialObjectInFinitiveDe[]*=[sobjectI=Z,controlI=Y];
	Object[]*=[objectI=Y];activeVerbMorphology[]*=[vbI=E] 
}

class s0Vn1[E,X,Y,T1,T2,P]{
	binaryRel[]*=[evt=E,arg1=X,arg2=Y,theta1=T1,theta2=T2,rel=P] ;
	SententialSubject[]*=[subjectI=X]; 
	Object[]*=[objectI=Y]; 
	activeVerbMorphology[]*=[vbI=E] 
}
