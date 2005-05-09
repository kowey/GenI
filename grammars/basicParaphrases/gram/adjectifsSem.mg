% Adjectifs epithetes

class adjectifEpithete
declare
	?xR ?xFoot ?xAnc ?L ?P ?X
{
	<syn>{
		node xR(color=red)[cat = n, top = [det = - ], bot = [idx=X,label=L]]{
			node xAnc(color=red,mark=anchor)[cat = adj]
			node xFoot(color=red,mark=foot)[cat =
			n,bot=[idx=X,label=L,det = - ]]
		}
	};
	<sem>{
		L:P(X)
		}
}