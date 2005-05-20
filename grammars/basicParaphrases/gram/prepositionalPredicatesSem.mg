
%% "jean est en colere"
class n0vPN[E,X,T1,P]
{	unaryRel[]*=[evt=E,arg1=X,theta1=T1,rel=P] ;
	Subject[]*=[subjectI=X];
	PrepositionalPredicativeform[]*=[vbI=E] 
}

% etre dans l'ignorance de 
class n0vPNden1[E,X,Y,T1,T2,P]
{
	binaryRel[]*=[evt=E,arg1=X,theta1=T1,arg2=Y,theta2=T2,rel=P] ;
	Subject[]*=[subjectI=X];
	PrepositionalPredicativeform[]*=[vbI=E];
	Genitive[]*=[genitiveI=Y]
}
%  ??
class n0vPNan1[E,X,Y,T1,T2,P]
{	binaryRel[]*=[evt=E,arg1=X,theta1=T1,arg2=Y,theta2=T2,rel=P] ;
	Subject[]*=[subjectI=X];
	PrepositionalPredicativeform[]*=[vbI=E];
	Iobject[]*=[iobjectI=X]
}
% eprouver de la passion pour
class n0vPNpn1[E,X,Y,T1,T2,P]
{	
	binaryRel[]*=[evt=E,arg1=X,theta1=T1,arg2=Y,theta2=T2,rel=P] ;
	Subject[]*=[subjectI=X];
	PrepositionalPredicativeform[]*=[vbI=E];
	Oblique[]*=[obliqueI=Y]
}

