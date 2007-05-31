%% Semantic classes
class  basicProperty
export ?E ?L0 ?Rel
declare ?E ?L0 ?Rel
      {
        <sem>{
                L0:Rel(E)
              }
              *=[label0 = L0, rel =Rel,arg0=E]
          }

% unary relation with event variable
class  unaryRel
import basicProperty[]
declare ?X ?Theta1
      {
        <sem>{
                 L0:Theta1(E,X)
              }
              *=[arg1=X,theta1 =Theta1]
          }

% binary relation with event variable
class  binaryRel
import unaryRel[]
declare ?X ?Theta2
      {
        <sem>{
                 L0:Theta2(E,X)
              }
              *=[arg2=X,theta2 =Theta2]
          }

% Verb arguments
class  SubjectSem
export
        ?xSem
declare
        ?xSem ?X ?L
{
        <syn>{
                node xSem[top=[idx=X,label=L]]
        }*=[subjectI = X,subjectL = L]
}

class  ObjectSem
export
        ?xSem
declare
        ?xSem ?X ?L
{
        <syn>{
                node xSem[top=[idx=X,label=L]]
        }*=[objectI = X,objectL = L]
}

class  CAgentSem
export
        ?xSem
declare
        ?xSem ?X ?L
{
        <syn>{
                node xSem[top=[idx=X,label=L]]
        }*=[cagentI = X,cagentL = L]
}

%% Noun semantics
class  nSem
export
        ?xSem
declare
        ?xSem ?X ?L
{
        <syn>{
                node ?xSem[cat=@{cl,n},top=[idx=X, label=L]]
        };
        basicProperty[]*=[arg0=X]
}

