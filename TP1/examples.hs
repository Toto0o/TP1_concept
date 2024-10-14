
-- Exemple simple de fix avec une seul declaration
-- readSexp "(fix ((x 2)) (+ x 3))" ↝
Snode (Ssym "fix") [Snode (Snode (Ssym "x") [Snum 2]) [],Snode (Ssym "+") [Ssym "x",Snum 3]]


-- Pour le rapport : pour le cas fix -> on avait assumer une forme (Snode (Ssym)) alors que c'est pas nencessairement ca
--                   daans appel de fonction -> on avait assumer que c'etait (Snode (Ssym)) alors que c'est pas bla bla...

-- Exemple simple de fix avec deux declaration
-- readSexp "(fix ((x 2) (y 3)) (+ x y))" ↝
-- Lfix [(Var, Lexp) (Var, Lexp)] Lexp
Snode (Ssym "fix") [Snode (Snode (Ssym "x") [Snum 2]) [Snode (Ssym "y") [Snum 3]],Snode (Ssym "+") [Ssym "x",Ssym "y"]]  

Snode (Ssym "fix") [Snode (Snode (Ssym "x") [Snum 2]) [Snode (Ssym "y") [Snum 3],Snode (Ssym "z") [Snum 2]],Snode (Ssym "+") [Snode (Ssym "+") [Ssym "x",Ssym "y"],Ssym "z"]]

Snode (Ssym "fix") [Snode (Snode (Ssym "y") [Snum 10]) [Snode (Snode (Ssym "div2") [Ssym "x"]) [Snode (Ssym "/") [Ssym "x",Snum 2]]],Snode (Ssym "div2") [Ssym "y"]]

-- Example pour débug le pattern match
--s2l (Snode (Ssym "fix") [Snode (Snode (Ssym "x") [Snum 2]) [],Snode (Ssym "+") [Ssym "x",Snum 3]]) = Lnum 42 

-- Sucre syntaxique 
-- (fix ((y 10) ((div2 x) (/ x 2))) (div2 y))
-- Snode (Ssym "fix") [Snode (Snode (Ssym "y") [Snum 10]) [Snode (Snode (Ssym "div2") [Ssym "x"]) [Snode (Ssym "/") [Ssym "x",Snum 2]]],Snode (Ssym "div2") [Ssym "y"]]
Lfix [("y",Lnum 10),("div2",Lfob [] (Lsend (Lvar "/") [Lvar "x",Lnum 2]))] (Lsend (Lvar "div2") [Lvar "y"])

--readSexp "(fix (((even x) 42) ((odd x)  43)) (odd 42))"      
Snode (Ssym "fix") [Snode (Snode (Snode (Ssym "even") [Ssym "x"]) [Snum 42]) [Snode (Snode (Ssym "odd") [Ssym "x"]) [Snum 43]],Snode (Ssym "odd") [Snum 42]]

Snode (Ssym "fix") [Snode (Snode (Ssym "y") [Snum 10]) [Snode (Snode (Ssym "div2") [Ssym "x"]) [Snode (Ssym "/") [Ssym "x",Snum 2]]],Snode (Ssym "div2") [Ssym "y"]]


-- (fix (((even x)  (if (= x 0) true  (odd  (- x 1)))) ((odd x)   (if (= x 0) false (even (- x 1))))) (odd 42))
Lfix [
    ("even", Lfob ["x"] 
        (Ltest 
            (Lsend (Lvar "=") [Lvar "x",Lnum 0]) 
            (Lbool True) 
            (Lsend (Lvar "odd") [Lsend (Lvar "-") [Lvar "x",Lnum 1]])
        )
    ),
    
    ("odd", Lfob ["x"] 
        (Ltest 
            (Lsend (Lvar "=") [Lvar "x",Lnum 0]) 
            (Lbool False) 
            (Lsend (Lvar "even") [Lsend (Lvar "-") [Lvar "x",Lnum 1]])
        )
    )
]       
(Lsend (Lvar "odd") [Lnum 42])