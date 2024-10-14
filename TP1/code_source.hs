-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr
import Distribution.Simple.Setup (trueArg)
import Language.Haskell.TH (AnnTarget(ValueAnnotation))

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [Var] Lexp      -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp

-- Entier signé en décimal (n)
s2l (Snum n) = Lnum n

-- Boolean
s2l (Ssym "true") = Lbool True
s2l (Ssym "false") = Lbool False

-- Varibale (x)
s2l (Ssym s) = Lvar s

-- Cas de liste vide "()"
s2l Snil = error "Illegal empty list"

-- Expression conditionelle (if e ethen eelse)
s2l (Snode (Ssym "if") [expression, true, false]) =
    Ltest (s2l expression) (s2l true) (s2l false)

-- *** ((fob (x) x) 2) ***
-- *** Snode (Snode (Ssym "fob") [Snode (Ssym "x") [],Ssym "x"]) [Snum 2] ***
-- Fonction (fob (x1 ... xn) e)
s2l (Snode (Ssym "fob") parameters) =
    let
        -- args doit être de type [var]; on construit la liste par extension
        args = [arg | Snode (Ssym arg) [] <- init parameters]
        -- on utilise "last" pour extraire le body (corps de la fonction)
        -- last extrait le dernier élément d'une liste
        body = last parameters
    in Lfob args (s2l body)

-- Déclaration locale simple (let x e1 e2)
s2l (Snode (Ssym "let") [Ssym x, e1, e2]) =
    -- s2l sur e1 et e2 pour avoir une Lexp
    Llet x (s2l e1) (s2l e2)



-- !!! continuer ici !!!
-- Déclaration locales récursives : (fix (d1 ... dn) e) 

-- *** (fix ((x 2)) (+ x 3)) ***
-- ***ex: Snode (Ssym "fix") [Snode (Snode (Ssym "x") [Snum 2]) []   ,Snode (Ssym "+") [Ssym "x",Snum 3]]***
-- *** Type de retour : Lfix [(Var, Lexp)] Lexp ***

-- Snode (Snode (Ssym "div2") [Ssym "x"]) [Snode (Ssym "/") [Ssym "x",Snum 2]]
s2l (Snode (Ssym "fix") [declarations, exp]) = 
    case declarations of 
        --Snode (Snode (Ssym x) [val]) [] -> Lfix [(x, s2l val)] (s2l exp)
        Snode firstd restD -> Lfix (map extractPair (firstd:restD)) (s2l exp)
        {- Snode (Snode (Ssym x) [Ssym y]) [Snode (Ssym z) body] -> Lfix ([x, ]) (s2l exp) -}
        _ -> error "invalid"




-- Opérateur binaire (+ | - | * | \ |...) 
-- *** comment DOM : redondant avec le patern matching subséquent? ***
-- [s2l eleft, s2l eright] et (map s2l args) vont faire la même chose anyway
--s2l (Snode (Ssym op) [eleft, eright]) = 
--    Lsend (Lvar op) [s2l eleft, s2l eright]

-- Appel de fonction (e0 e1...en)
s2l (Snode f args) =
    -- s2l sur f pour avoir une lexp
    -- map avec s2l sur args pour avoir [lexp]
    Lsend (s2l f) (map s2l args)

s2l se = error ("Expression Psil inconnue: " ++ showSexp se)


-- Fonction auxiliaire permettant de traiter plusieurs déclarations dans un fix

-- Snode (Snode (Ssym "div2") [Ssym "x"]) [Snode (Ssym "/") [Ssym "x",Snum 2]]
extractPair :: Sexp -> (Var, Lexp)
extractPair (Snode (Ssym y) [val]) = (y, s2l val)
extractPair (Snode (Snode (Ssym var) args) [body]) = (var, Lfob [arg | (Ssym arg) <- args] (s2l body))
---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv [Var] Lexp

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type VEnv = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")

          in [("+", binop Vnum (+)),
              ("*", binop Vnum (*)),
              ("/", binop Vnum div),
              ("-", binop Vnum (-)),
              ("<", binop Vbool (<)),
              (">", binop Vbool (>)),
              ("≤", binop Vbool (<=)),
              ("≥", binop Vbool (>=)),
              ("=", binop Vbool (==)),
              ("true",  Vbool True),
              ("false", Vbool False)]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: VEnv -> Lexp -> Value
-- ¡¡ COMPLETER !!
-- Cas de base (trivial)
eval _ (Lnum n) = Vnum n

eval _ (Lbool True) = Vbool True

eval _ (Lbool False) = Vbool False

-- Evaluation d'une variable
eval env (Lvar x) = case lookup x env of
    Just value ->  value
    Nothing -> error ("Variable" ++ x ++ "not defined")

eval env (Llet x e1 e2) = eval ((x, eval env e1):env) e2

-- *** (Lfob [var] Lexp)
-- *** Retour: Vfob VEnv [Var] Lexp ***
eval env (Lfob arg body) = Vfob env arg body

-- *** Lsend (Lfob ["x"] (Lvar "x")) [Lnum 2] ***
eval env (Lsend f arg) = case eval env f of -- on evalue la fonction appelée
    Vbuiltin fop -> fop (map (eval env) arg) -- si on appel une fonction primitive tel +, -, ...
    Vfob env var body ->  eval ((zipWith makePair var (map (eval env) arg)) ++ env) body

-- *** Ltest Lexp Lexp Lexp
eval env (Ltest exp etrue efalse) = case eval env exp of 
    Vbool True -> eval env etrue
    Vbool False -> eval env efalse

-- *** Lfix [(Var, Lexp)] Lexp
eval env (Lfix declarations exp) =  



-- Fonction auxiliaire qui cree une paire; utilisation dans le traitement de Lsend (Lfob ...)
makePair:: a -> b -> (a,b)
makePair x y = (x,y)

    -- !!! continuer ici !!!

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode 
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf