% -------------------------------------------------------
% A FRENCH METAGRAMMAR with a semantic dimension
% -------------------------------------------------------
% author = Benoit Crabbe & Claire Gardent
% date = February 2005


use color with () dims ()
use rank with () dims ()
use unicity with (extraction) dims ()
use unicity with (rank = 1) dims (syn)
use unicity with (rank = 2) dims (syn)
use unicity with (rank = 3)  dims (syn)
use unicity with (rank = 4)  dims (syn)
use unicity with (rank = 5)  dims (syn)
use unicity with (rank = 6)  dims (syn)
use unicity with (rank = 7)  dims (syn)

%type declarations

type CAT={n,np,v,vn,s,pp,c,p,cl,adj,adv,coord,d,à,a,de,par,que,qui,dont,si,il,se,gp}
type PERSON=[1..3]
type GENDER={m,f}
type NUMBER={sg,pl}
type MODE={ind,inf,subj,imp,ppart}
type COLOR ={red,black,white}
type WH={rel,+,-}
type LABEL !
type CASE={nom,acc,acc3rd,dat3rd,dat,gen,locative,ce}

type MARK  = {subst,nadj,foot,anchor,flex,none}
type NAME  = {subject,object,iobject}
type RANK  = [1..7]
type AUX = {etre,avoir,-}
type TENSE = {present,past}
type INVERSION = {+, n, -}
type BARV = [0..3]

type ATOMIC=[
	passivable : bool,
	mode : MODE,
 	num : NUMBER,
 	gen : GENDER,
 	pers : PERSON,
 	refl : bool,      
 	loc : bool,
 	wh : WH,
 	case : CASE,	
	pp-num: NUMBER,
	pp-gen: GENDER,
	control-num : NUMBER,
	control-pers : PERSON,
	control-gen : GENDER,
	aux-pass : bool,
	tense: TENSE,
	aux: AUX,
	det : bool,
	inv : INVERSION,
	cop : bool,
	loc : bool,
	bar : BARV
]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% IMPLICITS HFPs 
% NP : det,def,num,gen,pers,wh
      % Classes concernées : EpithAnte,

% V : mode,num,gen,pers,pp-num,pp-gen,inv,aux,aux-pass
% VN : mode,num,gen,pers,inv
% Adj : num,gen
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%property declarations

property color : COLOR
property mark       : MARK
property extracted  : bool { extraction = + }
property xcomp : bool 
property rank : RANK{
        i_   = 1 ,
        ii_  = 2 ,
        iii_ = 3 ,
        iv_  = 4 ,
        v_  = 5 ,
	vi_ = 6
}
property name       : NAME

%feature declarations
feature idx : LABEL
feature control-idx : LABEL
feature top : ATOMIC
feature bot : ATOMIC
%feature mode : LABEL
%feature num : NUMBER
%feature pers : PERSON
%feature wh : WH
%feature loc : bool
%feature refl : bool
%feature case : CASE
feature subjectI : LABEL
feature objectI : LABEL
feature iobjectI : LABEL
feature cagentI : LABEL
feature genitiveI : LABEL
feature obliqueI : LABEL
feature locativeI : LABEL
feature reflexiveI : LABEL
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
feature theta2 : LABEL
feature theta3 : LABEL
feature exp : LABEL
feature cause : LABEL
feature agent : LABEL
feature theme : LABEL
feature beneficiary : LABEL

%%%%%%%%%%%%%%%%%%%%
% MUTUAL EXCLUSION %
%%%%%%%%%%%%%%%%%%%%

mutex EXTR-IMPERATIVE1
mutex EXTR-IMPERATIVE1 += UnboundedExtraction
mutex EXTR-IMPERATIVE1 += ImperativeSubject

mutex EXTR-IMPERATIVE2
mutex EXTR-IMPERATIVE2 += BoundedExtraction
mutex EXTR-IMPERATIVE2 += ImperativeSubject

mutex SUBJ-INV
mutex SUBJ-INV += CanonicalObject
mutex SUBJ-INV += InvertedNominalSubject

mutex SEN-SUBJ
mutex SEN-SUBJ += SententialSubject
mutex SEN-SUBJ += UnboundedExtraction


mutex INFINITIVE-SUBJECT1
mutex INFINITIVE-SUBJECT1 += RelativeObject
mutex INFINITIVE-SUBJECT1 += InfinitiveSubject

mutex INFINITIVE-SUBJECT2
mutex INFINITIVE-SUBJECT2 +=  CleftObject
mutex INFINITIVE-SUBJECT2 += InfinitiveSubject

class imperativeSubjectMutex { whObject[] | RelativeObject[] |
      CleftObject[] | whCAgent[] | RelativeCAgent[] | CleftCAgentOne[]
      | CleftCAgentTwo[] | whGenitive[] | RelativeGenitive[] }
      

mutex IMPERATIVE-SUBJECT1
mutex IMPERATIVE-SUBJECT1 += whObject
mutex IMPERATIVE-SUBJECT1 += ImperativeSubject

mutex IMPERATIVE-SUBJECT2
mutex IMPERATIVE-SUBJECT2 += RelativeObject
mutex IMPERATIVE-SUBJECT2 += ImperativeSubject

mutex IMPERATIVE-SUBJECT3
mutex IMPERATIVE-SUBJECT3 += CleftObject
mutex IMPERATIVE-SUBJECT3 += ImperativeSubject

mutex IMPERATIVE-SUBJECT4
mutex IMPERATIVE-SUBJECT4 += whCAgent
mutex IMPERATIVE-SUBJECT4 += ImperativeSubject

mutex IMPERATIVE-SUBJECT5
mutex IMPERATIVE-SUBJECT5 += RelativeCAgent
mutex IMPERATIVE-SUBJECT5 += ImperativeSubject

mutex IMPERATIVE-SUBJECT6
mutex IMPERATIVE-SUBJECT6 += CleftCAgentOne
mutex IMPERATIVE-SUBJECT6 += ImperativeSubject

mutex IMPERATIVE-SUBJECT7
mutex IMPERATIVE-SUBJECT7 += CleftCAgentTwo
mutex IMPERATIVE-SUBJECT7 += ImperativeSubject

mutex IMPERATIVE-SUBJECT8
mutex IMPERATIVE-SUBJECT8 += whGenitive
mutex IMPERATIVE-SUBJECT8 += ImperativeSubject

mutex IMPERATIVE-SUBJECT9
mutex IMPERATIVE-SUBJECT9 += RelativeGenitive
mutex IMPERATIVE-SUBJECT9 += ImperativeSubject

mutex IMPERATIVE-SUBJECT10
mutex IMPERATIVE-SUBJECT10 += CleftGenitiveOne
mutex IMPERATIVE-SUBJECT10 += ImperativeSubject

mutex IMPERATIVE-SUBJECT11
mutex IMPERATIVE-SUBJECT11 += CleftDont
mutex IMPERATIVE-SUBJECT11 += ImperativeSubject

mutex IMPERATIVE-SUBJECT12
mutex IMPERATIVE-SUBJECT12 += CleftGenitiveTwo
mutex IMPERATIVE-SUBJECT12 += ImperativeSubject

class invertedNominalSubjectMutex { CanonicalObject[] |
      CliticObjectII[] | CliticObject3[] |  reflexiveAccusative[] |
      CanonicalCAgent[] | CanonicalGenitive[] | CliticGenitive[] }

mutex INVERTEDNOMINAL-SUBJECT1
mutex INVERTEDNOMINAL-SUBJECT1 += CanonicalObject
mutex INVERTEDNOMINAL-SUBJECT1 += InvertedNominalSubject

mutex INVERTEDNOMINAL-SUBJECT2
mutex INVERTEDNOMINAL-SUBJECT2 += CliticObjectII
mutex INVERTEDNOMINAL-SUBJECT2 += InvertedNominalSubject

mutex INVERTEDNOMINAL-SUBJECT3
mutex INVERTEDNOMINAL-SUBJECT3 += CliticObject3
mutex INVERTEDNOMINAL-SUBJECT3 += InvertedNominalSubject

mutex INVERTEDNOMINAL-SUBJECT4
mutex INVERTEDNOMINAL-SUBJECT4 += reflexiveAccusative
mutex INVERTEDNOMINAL-SUBJECT4 += InvertedNominalSubject

mutex INVERTEDNOMINAL-SUBJECT5
mutex INVERTEDNOMINAL-SUBJECT5 += CanonicalCAgent
mutex INVERTEDNOMINAL-SUBJECT5 += InvertedNominalSubject


mutex INVERTEDNOMINAL-SUBJECT6
mutex INVERTEDNOMINAL-SUBJECT6 += CanonicalGenitive
mutex INVERTEDNOMINAL-SUBJECT6 += InvertedNominalSubject

mutex INVERTEDNOMINAL-SUBJECT7
mutex INVERTEDNOMINAL-SUBJECT7 += CliticGenitive
mutex INVERTEDNOMINAL-SUBJECT7 += InvertedNominalSubject

class impersonalSubjectMutex { CliticObjectII[] |CliticObject3[] |
reflexiveAccusative[] | whObject[] | RelativeObject[] | CleftObject[]
}

class impersonalSubjMutex
import impersonalSubjectMutex[]

mutex IMPERSONAL-SUBJECT
mutex IMPERSONAL-SUBJECT += impersonalSubjMutex
mutex IMPERSONAL-SUBJECT += ImpersonalSubject

