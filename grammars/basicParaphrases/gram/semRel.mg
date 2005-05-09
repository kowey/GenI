
%% Semantic classes
class semRel
export E 
declare ?Rel ?E !L0
      {
	<sem>{
		L0:Rel(E) 
	      }
	      *=[rel=Rel,evt=E]
	  }
class unaryRel
import semRel[]
declare ?X !L1 ?Theta1
      {
	<sem>{
		 L1:Theta1(E,X)
	      }
	      *=[arg1=X,theta1=Theta1] 
	  }
class binaryRel
import unaryRel[]
declare ?X !L2  ?Theta2
      {
	<sem>{
		 L2:Theta2(E,X)
	      }
	      *=[arg2=X,theta2=Theta2] 
	  }
class ternaryRel[]
import binaryRel[]
declare ?X !L3  ?Theta3
      {
	<sem>{
		 L3:Theta3(E,X)
	      }
	      *=[arg3=X,theta3=Theta3]
}


