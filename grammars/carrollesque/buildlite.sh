MetaTAG --xml -o semFrenchLite.xml cgTAG.mg 
rm semFrenchLite.geni
../../geniconvert --macros < semFrenchLite.xml > semFrenchLite.geni
