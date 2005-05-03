include cgTAG.mg

% VALUATION
%%%%%%%%%%%%%%%%%%%%%%%%%

value PrepositionN

%Nominal
value ilV		%il pleut
value n0V 		%Jean dort
value n0Vn1		%Jean regarde Marie
value s0Vn1
value n0Vn1an2		%Jean donne un cadeau à Marie

value n0ClV		%Jean s'évanouit
value n0ClVn1		%L'enfant s'appelle Marie
value n0Van1		%Jean parle à Marie
value n0Vden1		%Jean parle de ses vacances
value n0ClVden1		%Jean se souvient de Marc
value n0Vpn1		%Jean parle avec Marie
value n0ClVpn1		%Jean se bat contre Paul
value n0Vloc1		%Jean va à Paris
value n0Van1des2_0	%Jean promet à Marie de venir 
			% subject control
value n0Vn1des2_1	%Jean persuade Marie de venir 
			% subject control
%value n0Van1des2	%Jean promet à Marie qu'il partira
value noVinf		% partir

% PREDICATIVE ADJECTIVES
value n0vApre  % un heureux événement
value n0vApost % un garçon heureux
value n0vA		%Jean est heureux, un garçon heureux, un heureux événement
value s0vA		%Que Marie parte était imprévu, Le départ imprévu
value n0vAden1		%Le père est fier de sa fille, un père fier de sa fille
value n0vAan1		%L'enfant est attentif à ce projet, L'enfant attentif à ce projet
value n0vApn1		%Un enfant fort en maths, L'enfant est fort en maths
value n0vAan1pn2	%Un enfant supérieur à Luc en math, Cet enfant est supérieur à Luc en maths
value n0vAan1den2	%Un ami redevable à Paul de ses conseils, Ce bonhomme est redevable à Paul de ses conseils

%PREDICATIVE NOUNS
value n0vN		%Jean , La *France*, Jean est un *garçon*
value n0vNan1		%  
value n0vNden1		%Marie est la *femme* de Jean; La *femme* de Jean s'appelle Marie

%NonVerbalVerbs (!)
value AvoirAux		%Jean *a* mangé
value EtreAux		%Jean *est* venu
value SemiAux		%Jean *semble* partir
value Copule		%Jean *est* aimé par Marie

% TOUGH adjectives
%(only subject to subject raising)
value toughDe		%Il est susceptible de pleuvoir


% Adverbs
%value advArgMan	%Jean court vite / *Jean court
value advLoc		%Les enfants viennent *ici*, *Où vont les enfants*
value prepLoc		%Dans quelle ville vont les enfants, les enfants vont chez la mère grand
value advSAnte          %Hier Jean est venu, *jean hier est venu, *jean est hier venu,...
value advSPost		%Jean viendra demain *jean est demain venu...
value advVPost		%Jean a vraiment vu un monstre !
value advAdjAnte	%Jean est très petit
value advAdvAnte	%Jean court très vite
% PPModifiers
value s0Pcs1		%Jean veut qu'on se rencontre avant le match,que tu partes, de partir
value s0Ps1		%Jean veut qu'on se rencontre après le match,que tu partes, après être partis
value s0PLoc1		%Nous viendrons jusque chez vous
value s0Pn1		%Un livre avec une couverture bleue

% Misc.
value CliticT		%Tree for any clitic : Jean *le* donne
value InvertedSubjClitic%Tree for inverted subject clitic in context of questions : Semble*-t-il* venir ? Viendra*-t-il* ?
value Subjclitic	  %Tree for clitic subject (je,tu,il,elle,on,ce,nous,vous,ils,elles)
%value PrepositionalPhrase %Tree for postnominal PP modifier
value propername	%Marie
value commonNoun        %chat
value n0Nmod		%monsieur *Machin*
value stddeterminer	%*Le* lutin
value whdeterminer	%*Quel* lutin
value Coordination	%Any simple constituent coordination :Jean *et* Marie mangent

%negative hack :-(
%A truly dirty hack requested for Evalda (see above) 
value negLeft		%ne implemented as adjunct >-(    Jean ne vient pas
value negPas		%pas implemented as adjunct >-(   Jean ne mange pas, Jean ne mange guère...
% really a shame...


%EVALDA FEEDBACK

% participe présent :

% (priorité faible) les coteaux environnant la ville 

% 4) Attributs 
%-> Attribut de l'objet

%???? Elle traite Jean d'imbécile (Elle traite Jean de Jean être un imbécile)
%
%???? La plus belle de la collection est la verte 

%???? Certains enseignants se déclarent choqués

%en quelle année a-t-on vraiment construit la première automobile ?

% Déterminants quantifieurs
%une centaine de -> 2 familles ? think more about this...
%beaucoup de     ->
%trop de         ->

% La solution retenue fut celle proposée par Jean
% La solution fut proposée de telle sorte que tt le monde soit d'accord.
