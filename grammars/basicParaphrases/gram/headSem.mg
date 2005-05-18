include declarations.mg
include semRel.mg
include grammaticalFunctionsSem.mg
include verbesSem.mg
include functionwordsSem.mg
include adjectifsSem.mg
include nomsSem.mg

%% predicative constructions
include predicativeFormsSem.mg
include nominalPredicatesSem.mg
include adjectivalPredicatesSem.mg
include prepositionalPredicatesSem.mg

%include verbes.mg
%include adjectifs.mg

%include adverbes.mg
%include misc.mg

 
% VALUATION
%%%%%%%%%%%%


%% calls

%Impersonal
%value ilV		%il pleut
%value ilVcs1		%il faut que Jean vienne/venir

%Nominal
%value n0V 		%Jean dort			15 trees
%value n0ClV		%Jean s'évanouit		8 trees
value n0Vn1		%Jean regarde Marie		165 trees > 107
value n0Van1		%Jean parle à Marie		65 trees

%value n0ClVn1		%L'enfant s'appelle Marie	35 trees

%value n0Vden1		%Jean parle de ses %vacances	47 trees
value n0ClVden1		%Jean se souvient de Marc	47 trees
%value n0Vpn1		%Jean parle avec Marie		33 trees
%value n0ClVpn1		%Jean se bat contre Paul	33 trees
%value n0Vloc1		%Jean %va à Paris		29 trees
%value n0Vn1an2		%Jean donne un cadeau à Marie

%value n0Van1den2	%Jean parle de ses vacances à Marie
%value n0Vn1den2		%Jean reçoit un cadeau de Marie
%value n0Vden1pn2	%Jean parle de ce livre avec Marie
%value n0Vn1loc2		%Jean envoie la lettre à la poste	-- 629 trees


%Sentential
%value s0V		%Qu'il neige en mai arrive parfois
%value s0Vn1		%Que Jean soit parti désole Marie
%value s0Van1		%Que Jean soit parti déplait à Marie
%value s0Vcs1		%Qu'il neige prouve qu'il fait froid
%value n0Vcs1		%Jean pense qu'il a raison

% Light verb constructions
%value n0vN
%value n0vNden1
%value n0vNan1
%value n0vNpn1
value n0vPNpn1

%% Noms

value propername
%value commonNoun
%value CliticT
%value pronoun
value relationalNoun

% Function words
%value stddeterminer
%value prepositionN

% Adjectifs
%value adjectifEpithete
