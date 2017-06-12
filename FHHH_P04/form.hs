--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 3: Relación de alpha-equivalencia y Algoritmo de Martelli         --
-- Montanari                                                                  --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con la sintaxis y semántica de las expresiones del    --
-- lenguaje FORM y unificación                                                --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------
module PrimerOrden where

-- Sinónimo para representar los nombres de variables y funciones.
type Nombre = String

-- Sinónimo para representar sustituciones.
type Sust = [(Nombre,Termino)]

-- Gramática para representar términos.
data Termino = V Nombre
             | F Nombre [Termino] deriving (Eq)

-- Gramática para representar fórmulas de la Lógica de Primer Orden.
data FORM = TrueF
             | FalseF
             | Pr Nombre [Termino]
             | Eq Termino Termino
             | Neg FORM
             | Conj FORM FORM
             | Disy FORM FORM
             | Impl FORM FORM
             | Equi FORM FORM
             | PT Nombre FORM
             | EX Nombre FORM deriving(Eq)
             
             
-- Hace parte de la familia Show al tipo Atomo.
instance Show Termino where
   show (V v) = show v
   show (F c []) = show c ++ "[]"
   show (F n (x:xs)) = n++"("++(show x) ++(auxShowT xs)++")"

instance Show FORM where
  show (TrueF) = "T"
  show (FalseF) = "F"
  show (Pr n ls) = n++"("++auxShowT ls++")"
  show (Eq t1 t2) = "("++show t1 ++ "==" ++ show t2++")"
  show (Neg f) = "¬("++show f ++")"
  show (Conj f1 f2) = "("++show f1 ++"/\\"++ show f2++")"
  show (Disy f1 f2) = "("++show f1 ++"\\/"++ show f2++")"
  show (Impl f1 f2) = "("++show f1 ++"=> "++ show f2++")"
  show (Equi f1 f2) = "("++show f1 ++"<=>"++ show f2++")"
  show (PT n f) = "V"++n++ show f
  show (EX n f) = "Э"++n++show f

auxShowT :: [Termino] -> String
auxShowT [] = ""
auxShowT (x:xs) = show x++auxShowT xs


consT :: Termino -> [Nombre]
consT (V n) = []
consT (F n []) = [n]
consT (F n l) = constAux l

constAux :: [Termino] -> [Nombre]
constAux [] = []
constAux ((V n):xs) = constAux xs
constAux ((F n []):xs) =  [n] ++ constAux xs
constAux ((F n l):xs) = constAux l ++ constAux xs

varT :: Termino -> [Nombre]
varT (V n) = [n]
varT (F n []) = []
varT (F n l) = varAux l

varAux :: [Termino] -> [Nombre]
varAux [] = []
varAux ((V n):xs) = [n] ++ varAux xs
varAux ((F n []):xs) =  varAux xs
varAux ((F n l):xs) = varAux l ++ varAux xs

funT :: Termino -> [Nombre]
funT (V n) = []
funT (F n []) = []
funT (F n l) = [n] ++ funTAux l

funTAux :: [Termino] -> [Nombre]
funTAux [] = []
funTAux ((V n):xs) = funTAux xs
funTAux ((F n []):xs) = funTAux xs
funTAux ((F n l):xs) = [n] ++ funTAux l ++ funTAux xs

consF :: FORM -> [Nombre]
consF FalseF = ["False"]
consF TrueF = ["True"]
consF (Pr n l) = consFAux l
consF (Eq a b) = consFAux [a] ++ consFAux [b]
consF (Neg p) = consF p
consF (Conj a b) = consF a ++ consF b
consF (Disy a b) = consF a ++ consF b
consF (Impl a b) = consF a ++ consF b
consF (Equi a b) = consF a ++ consF b
consF (PT x a) = consF a
consF (EX x a) = consF a

consFAux :: [Termino] -> [Nombre]
consFAux [] = []
consFAux ((V n):xs) = consFAux xs
consFAux ((F n []):xs) =  [n] ++ consFAux xs
consFAux ((F n l):xs) = consFAux l ++ consFAux xs

varF :: FORM -> [Nombre]
varF FalseF = []
varF TrueF = []
varF (Pr n l) = varFAux l
varF (Eq a b) = varFAux [a] ++ varFAux [b]
varF (Neg p) = varF p
varF (Conj a b) = varF a ++ varF b
varF (Disy a b) = varF a ++ varF b
varF (Impl a b) = varF a ++ varF b
varF (Equi a b) = varF a ++ varF b
varF (PT x a) = varF a
varF (EX x a) = varF a

varFAux :: [Termino] -> [Nombre]
varFAux [] = []
varFAux ((V n):xs) = [n] ++ varFAux xs
varFAux ((F n []):xs) = varFAux xs
varFAux ((F n l):xs) = varFAux l ++ varFAux xs

funF :: FORM -> [Nombre]
funF FalseF = []
funF TrueF = []
funF (Pr n l) =  funFAux l
funF (Neg p) = funF p
funF (Conj a b) = funF a ++ funF b
funF (Disy a b) = funF a ++ funF b
funF (Impl a b) = funF a ++ funF b
funF (Equi a b) = funF a ++ funF b
funF (PT x a) = funF a
funF (EX x a) = funF a

funFAux :: [Termino] -> [Nombre]
funFAux [] = []
funFAux ((V n):xs) = funFAux xs
funFAux ((F n []):xs) = funFAux xs
funFAux ((F n l):xs) = [n] ++ funFAux l ++ funFAux xs

subT :: Termino -> [Termino]
subT (V n) = [(V n)]
subT (F n [] ) = [(F n [])]
subT (F n l) = uniont [F n l] (subTAux l)

uniont :: [Termino] -> [Termino] -> [Termino]
uniont [] b = b
uniont (x:xs) b = if elem x b then uniont xs b else x:(uniont xs b)

subTAux :: [Termino] -> [Termino]
subTAux [] = []
subTAux ((V n):xs) = [(V n)] ++ (subTAux xs)
subTAux ((F n []):xs) = [(F n [])] ++ (subTAux xs)
subTAux ((F n l ): xs) =  (uniont [(F n l)] (subTAux l)) ++ subTAux xs

subF :: FORM -> [FORM]
subF FalseF = [FalseF]
subF TrueF = [TrueF]
subF (Pr n l) = [(Pr n l)]
subF (Eq a b) = [(Eq a b)]
subF (Neg p) = unionT [Neg p] (subF p)
subF (Conj p q) = unionT (subF p) (subF q)
subF (Disy p q) = unionT (subF p) (subF q)
subF (Impl p q) = unionT (subF p) (subF q)
subF (Equi p q) = unionT (subF p) (subF q)
subF (PT n q) = subF q

unionT::[FORM] -> [FORM] -> [FORM]
unionT [] b = b
unionT (x:xs) b = if elem x b then unionT xs b else x:(unionT xs b)

cuantF :: FORM ->[FORM]
cuantF FalseF = []
cuantF TrueF = []
cuantF (Eq a b) = []
cuantF (Pr n l) = []
cuantF (Neg p) = (cuantF p)
cuantF (Conj p q) = unionT (cuantF p) (cuantF q)
cuantF (Disy p q) = unionT (cuantF p) (cuantF q)
cuantF (Impl p q) = unionT (cuantF p) (cuantF q)
cuantF (Equi p q) = unionT (cuantF p) (cuantF q)
cuantF (PT n q) = [PT n q] ++ cuantF q
cuantF (EX n q) = [EX n q] ++ cuantF q

bv :: FORM -> [Nombre]
bv FalseF = []
bv TrueF =[]
bv (Pr n l) = []
bv (Eq a b) = []
bv (Neg p) = bv p
bv (Conj p q) = (bv p) ++ (bv q)
bv (Disy p q) = (bv p) ++ (bv q)
bv (Impl p q) = (bv p) ++ (bv q)
bv (Equi p q) = (bv p) ++ (bv q)
bv (PT x f) = elimina x (varF f)
bv (EX x f) = elimina x (varF f)

termB:: Nombre -> FORM -> [Nombre]
termB _ FalseF = []
termB _ TrueF = []
termB a (Pr n l) = (auxVar a l)

auxVar:: Nombre -> [Termino] -> [Nombre]
auxVar _ [] = []
auxVar a [(V y)] = if a == y then [y] else []
auxVar a [(F n [])] = []
auxVar a [(F n (x:xs))] = (auxVar a [x]) ++ (auxVar a xs)

fv :: FORM -> [Nombre]
fv FalseF = []
fv TrueF = []
fv (Pr p ls) = varAux ls
fv (Eq a b) = []
fv (Neg p) = fv p
fv (Conj p q) = (fv p) ++ (fv q)
fv (Disy p q) = (fv p) ++ (fv q)
fv (Impl p q) = (fv p) ++ (fv q)
fv (Equi p q) = (fv p) ++ (fv q)
fv (PT x f) = elimina x (varF f)
fv (EX x f) = elimina x (varF f)

termF:: Nombre -> FORM -> [Nombre]
termF _ FalseF = []
termF _ TrueF = []
termF a (Pr n l) = (auxVarF a l)
termF a (Eq b c) = []
termF a (Neg p) = termF a p
termF a (Conj p q) = (termF a p) ++ (termF a q)
termF a (Disy p q) = (termF a p) ++ (termF a q)
termF a (Impl p q) = (termF a p) ++ (termF a q)
termF a (Equi p q) = (termF a p) ++ (termF a q)
termF a (PT n q) = filter (/= a) (termF n q)

auxVarF:: Nombre -> [Termino] -> [Nombre]
auxVarF _ [] = []
auxVarF a [(V y)] = if a /= y then [y] else []
auxVarF a [(F n [])] = []
auxVarF a [(F n (x:xs))] = (auxVarF a [x]) ++ (auxVarF a xs)

getTerm:: [Termino] -> [Nombre]
getTerm [] = []
getTerm [(V n)] = [n]
getTerm [(F n [])] = []
getTerm [(F n (x:xs))] = (getTerm [x]) ++ (getTerm xs)


elimina :: Nombre -> [Nombre] -> [Nombre]
elimina n [] = []
elimina n (x:xs) = if (n == x) then (elimina n xs) else x:(elimina n xs)


-- Función que verifica si dos fórmulas son alpha-equivalentes.
vAlfaEq :: FORM -> FORM -> Bool
vAlfaEq f g = error "Función no implementada"

-- Función que renombra las variables ligadas de una fórmula de manera que las
-- listas de variables libres y ligadas que sean ajenas. Estos es un caso 
-- particular de la siguiente función.
renVL :: FORM -> FORM
renVL f = error "Función no implementada"

-- Función que renombra la variables ligadas de una fórmula de forma que sus 
-- nombres sean ajenos a los de una lista dada.
renVLconj :: FORM -> [Nombre] -> FORM
renVLconj f ls = error "Función no implementada"

-- Función que implementa la sustitución en fórmulas usano la 
-- alpha-equivalencia.
apsubF2 :: FORM -> Sust -> FORM
apsubF2 f s = error "Función no implementada"

-- Función que dada una sustitución, elimina de ella los pares con componentes
-- iguales correspondientes a sustituciones de la forma x:=x.
simpSus :: Sust -> Sust
simpSus [] = []
simpSus ((n, (V m)):xs) = if n == m then simpSus xs else (n,(V m)):(simpSus xs)
simpSus (x:xs) = x:(simpSus xs)

-- Función que dadas dos sustituciones devuelve su composición.
compSus :: Sust -> Sust -> Sust
compSus s t = error "Función no implementada"

-- Función que dados dos términos devuelve una lista de sustituciones de tal
-- forma que:
-- · Si t1, t2 no son unificables la lista es vacía.
-- · Si sí lo son, la lista contiene como único elemento al unificador 
--   correspondiente.
unifica :: Termino -> Termino -> [Sust]
unifica (V x) (V y) = if x == y then [] else [[(x, (V y))]]
unifica (V x) (F f ls)
  | (elem x (varAux ls)) = []
  | otherwise = [[(x, (F f ls))]]
unifica (F f ls) (V x) = unifica (V x) (F f ls)
unifica (F f fs) (F g gs)
  | not(f == g) || not((length fs) == (length gs)) = error "FALLA"
  | otherwise = []

-- Función que implementa el caso general para unificar un conjunto (lista)
-- W = {t1,..,tn}
unificaConj :: [Termino] -> [Sust]
unificaConj ls = error "Función no implementada"
