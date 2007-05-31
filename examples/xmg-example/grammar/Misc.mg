class propername
import
	nSem[]
declare
        ?xN
{
        <syn>{
                node xN(color=red,mark=anchor)[cat = n,bot=[pers = 3]]
        };
	?xN = ?xSem
}

class Copule
declare
        ?xV
{
        <syn>{
                node xV(color=red,mark=anchor)[cat = v]
        }
}

class Clitic
import
	nSem[]
declare
        ?xCl
{
        <syn>{
                node xCl(color=red,mark=anchor)[cat = cl]
        };
	?xCl = ?xSem
}
