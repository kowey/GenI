% -------------------------------------------------------
% A FRENCH TOY METAGRAMMAR
% -------------------------------------------------------
%
% date   = january 2007
%
%
% Demo MetaGrammar inspired by B. Crabbe and C. Gardent French TAG
%
%
% ASSOCIATED FILES (for SemConst):
% 	demo.lex : lemma database
%	demo.mph : morphological database
%	
%
% Contact: parmenti@loria.fr
%		
% -------------------------------------------------------


%% Principles instanciation

use color with () dims (syn)
use unicity with (extracted = +) dims (syn)

%% Type declarations

type CAT={n,np,v,vn,s,pp,c,p,cl,par,qui}
type PERSON=[1..3]
type GENDER={m,f}
type NUMBER={sg,pl}
type MODE={ind,subj}

type MARK={subst,foot,none,nadj,anchor,coanchor,flex}
type COLOR ={red,black,white}

type LABEL !
type IDX !

%% Property declarations

property color      : COLOR
property mark       : MARK
property extracted  : bool

%% Feature declarations

feature cat  : CAT
feature gen  : GENDER
feature num  : NUMBER
feature pers : PERSON
feature mode : MODE

feature idx  : IDX
feature label: LABEL

%%%%%%%%%%%%%%%%%%%
% MUTUAL EXCLUSION
%%%%%%%%%%%%%%%%%%%
%
% (functionality not used in this toy metagrammar)
%
% mutex MUTEX-SET1            %% Mutex declaration
% mutex MUTEX-SET1 += Class1  %% Mutex filling
% mutex MUTEX-SET1 += Class2
