import CL7a

countModels :: Wff Atom -> Int
countModels wff = length [ v | v <- envs [A,B,C,D,W,X,Y,Z] , undefined ]

