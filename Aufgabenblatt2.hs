--Aufgabe 3: 
--Teil 1

data MyLabel = -- name of type
    LabelKlickbar 
        String --Link
        String --Text
        
                        
    deriving Show
 
olat= LabelKlickbar"https://lms.uibk.ac.at" "Olat"
fp= LabelKlickbar"http://cl-informatik.uibk.ac.at/teaching/ws21/fp" "FP"

--Teil 2
data Menu2= --menü dass ein oder 2 elemente enthalten kann
    Menu1 --ein Element
        MyLabel --Item1
    |Menu2 --zwei Elemente 
        MyLabel --Item1
        MyLabel --Item2

    deriving Show

olatandfp= Menu2 olat fp

--Teil 3
data Menu =
    Empty
    |Cons MyLabel Menu 
    deriving Show

test= LabelKlickbar "testlink" "test"
onelabel= Cons olat Empty
twolabels= Cons olat (Cons fp Empty)
threelabels= Cons olat(Cons fp(Cons test Empty))
        





