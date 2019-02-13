module DataSamples2 exposing (..)


csvTest =
    "x,y\n0,0\n1,0\n1,1\n0,1\n"


csvTest2 =
    "This data describes a square\nx,y\n0,0\n1,0\n1,1\n0,1\n"


csvTest32 =
    "x, y\n  0, 0 \n1,0\n1,1\n0,1\n"


{-| RawData.get3 '\t' tabTest
Just (["x","y"],[["0","0"],["1","0"],["1","1"],["0","1"]])
-}
tabTest =
    "x\ty\n0\t0\n1\t0\n1\t1\n0\t1\n"


tabTest2 =
    "This data describes a square\nx\ty\n0\t0\n1\t0\n1\t1\n0\t1\n"


{-| RawData.get3 ' ' spaceTest
Just (["x","y"],[["0","0"],["1","0"],["1","1"],["0","1"]])
-}
spaceTest =
    "x y\n0 0\n1 0\n1 1\n0 1\n"


spaceTest2 =
    "This data describes a square\nx y\n0 0\n1 0\n1 1\n0 1\n"
