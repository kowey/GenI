%----------------------------------------%
%  MG de Test pour la production d'une   %
%  grammaire TAG ancrée                  %
%----------------------------------------%

%type declarations

type CAT={n,np,v,vn,s,vs}
type PERSON=[1..3] 
type GENDER={m,f}
type NUMBER={sg,pl}
type COLOR ={red,black,white} 
type AGR=[cat    : CAT,
	  person : PERSON,
	  test   : bool
]

type LABEL !
type PHON !

type LEXIQUE = {manger}

type MARK  = {subst,nadj,foot,none}
type RANK  = [1..5]


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
	v_   = 5 }

%feature declarations

feature idx : LABEL
feature top : LABEL

feature phon : PHON
feature anch : PHON
feature suj : LABEL
feature obj : LABEL

feature mode : LABEL
feature num  : NUMBER
feature pers : PERSON

%class definitions


class subjCan 
declare ?W ?N ?M
{ <syn>{
	node(color=white)[cat = s]{
		node(color=red,mark=subst)[cat=n,top = [idx=?W,num = ?N, pers=?M]]
		node(color=white)[cat = v, top=[num=?N, pers=?M]]
	}
}*=[suj = ?W]
}

class objCan 
declare ?W 
{ <syn>{
	node(color=white)[cat = s]{
		node(color=white)[cat = v]
	     ,,,node(color=red,mark=subst)[cat = n,top=[idx=?W]]
	}
}*=[obj = ?W]
}

class activeMorph 
declare ?M ?N ?X
{ <syn>{
	node(color=black)[cat=s, bot=[mode=?M]]{
		node(color=black)[cat=v, top=[mode =?M],bot=[mode=?N]]{
			node(color=black)[cat=v, top=[mode = ?N]]{
				node(color=black)[phon=?X]
			}
		}
	}
}*=[anch=?X]
}

class transitifDirect 
{ 
	subjCan ; 
	objCan ; 
	activeMorph }

class BinaryRel[Pred]
declare !L !E ?X ?Y
{ <sem>{!L:Pred(!E,?X,?Y)}
*= [arg0 = ?X,arg1 = ?Y] }

class lexemeManger 
declare ?X ?Y
{ transitifDirect*=[suj=?X,obj=?Y,anch=manger] ; BinaryRel[manger]*=[arg0 =?X,arg1 = ?Y] }

%Simple transitive
value lexemeManger
