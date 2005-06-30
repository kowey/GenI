in order to make geni work without semantics,
the Geni/.genirc file must set IgnoreSemantics
to true. here is an example .genirc:

Grammar  = examples/media/index

% True or False
Graphical  = True 
IgnoreSemantics = True 
MaxTrees = 3 

% Optimisations should be a comma delimited list containing any 
% number of the following items: 
%  Polarised, PolSig, ChartSharing, 
%  SemFiltered, FootConstraint 
%  There is also PolOpts (all polarity optimisations) 
%  and AdjOpts (all adjunction optimisations) 

Optimisations = FootConstraint