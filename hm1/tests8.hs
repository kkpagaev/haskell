showRE (simplify re5) == "(ab|)dd*"

isTerminal  nda1 2 ==  True
isTerminal da4 3 == True
isTerminal nda2 5 == False

isEssential nda1 2 == True
isEssential nda2 5 == True
isEssential nda2 6 == False

transitionsFrom ndaFigure 5 ==  [(5,7,Eps), (5,9,Eps)]
transitionsFrom nda3 10 == [(10,6, C 'b')]

labels [(1,2,Eps)] == []
labels ((\(_,_, tr) -> tr) nda3)  == [C 'a', C 'c', C 'b']

acceptsDA da1 "x1" == True
acceptsDA da1 "12" == False
acceptsDA da1 "x21" == False

stStep nda1 4 Eps ==  [9, 11]
setStep nda4 ([2,4,5]) (C 'a') ==  sort [6,2] 
closure ndaTest ( [2])  ==  [1..5]

accepts ndaFigure "ac" == True
accepts ndaFigure "aac" == True
accepts ndaFigure "ad" == False 

makeNDA reFigure == ndaFigure 
makeNDA re1 == nda1
makeNDA re4 == nda4  

parseReg "(a?)a" == Just (Seq (Opt (Term 'a')) (Term 'a'))
parseReg "ab(+)" == Nothing  

makeDA ndaFigure == daFigure
makeDA nda1 == da1
makeDA nda3 == da3