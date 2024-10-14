
-- Exemple simple de fix avec une seul declaration
-- readSexp "(fix ((x 2)) (+ x 3))" ↝
Snode (Ssym "fix") [Snode (Snode (Ssym "x") [Snum 2]) [],Snode (Ssym "+") [Ssym "x",Snum 3]]


-- Exemple simple de fix avec deux declaration
-- readSexp "(fix ((x 2) (y 3)) (+ x y))" ↝
Snode (Ssym "fix") [Snode (Snode (Ssym "x") [Snum 2]) [Snode (Ssym "y") [Snum 3]],Snode (Ssym "+") [Ssym "x",Ssym "y"]]  