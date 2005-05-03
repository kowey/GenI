MetaTAG --xml -o gram/semFrenchLite.xml gram/semFrenchLite.mg 
rm gram/semFrenchLite.geni
../../geniconvert --macros < gram/semFrenchLite.xml > gram/semFrenchLite.geni
