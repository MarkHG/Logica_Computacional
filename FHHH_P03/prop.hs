--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 2: Gramáticas / Sintaxis y semántica del lenguaje PROP.           --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con la sintaxis y semántica de las expresiones del    --
-- lenguaje PROP.                                                             --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------

module PROPP2 where
import Data.List (nub)

-- Sinónimo para representar estados
type Estado = (VarP, Booleano)

-- Gramática para constantes lógicas
data Booleano = V | F deriving (Show,Eq)

-- Gramática para variables proposicionales
data VarP = A|B|C|D|E|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|W|X|Y|Z deriving (Show, Eq)

-- Gramática para representar átomos
data Atomo = Var VarP | Cte Booleano deriving (Eq)

-- Gramática para representar a los operadores binarios.
data OpBin = Conj | Disy | Impl | Syss deriving(Eq)

-- Gramática para representar expresiones del lenguaje Prop.
data Prop = FA Atomo
          | Neg Prop
          | Op Prop OpBin Prop deriving(Eq)

-- Gramática para literales.
data Literal = LAtom Atomo 
          | NegAt Literal deriving (Eq)

-- Gramática para cláusulas.
data Clausula = CLite Literal 
          | NCLite Clausula OpBin Clausula deriving (Eq)
              
instance Show Literal where
   show (LAtom a) = show a
   show (NegAt a) = "¬("++show a++")"

instance Show Clausula where
   show (CLite (LAtom l)) = show l
   show (CLite (NegAt l)) = "¬("++show l++")" 
   show (NCLite l1 o l2) = show l1 ++ show o ++ show l2

-- Hace parte de la familia Show al tipo  Atomo.
instance Show Atomo where
   show (Var v) = show v
   show (Cte b) = show b

-- Hace parte de la familia Show al tipo OpBin.
instance Show OpBin where
   show (Conj) = " ∧ "
   show (Disy) = " ∨ "
   show (Impl) = " => "
   show (Syss) = " <=> "

-- Hace parte de la familia Show al tipo Prop.
instance Show Prop where
   show (FA a) = show a
   show (Neg p) = "¬(" ++ show p ++ ")"
   show (Op p o q) = "(" ++ show p ++ show o ++ show q ++ ")"

-- Función que realiza la sustitución simultánea dada una lista con las
-- sustituciones.
sustSimult :: Prop -> [(VarP, Prop)] -> Prop
sustSimult (FA (Cte c)) l = (FA (Cte c))
sustSimult (FA (Var v)) l = buscaSustitucion v l
sustSimult (Neg x) l = Neg (sustSimult x l)
sustSimult (Op p o q) l = Op (sustSimult p l) o (sustSimult q l)


buscaSustitucion::VarP ->[(VarP, Prop)]-> Prop
buscaSustitucion x [] = (FA (Var x))
buscaSustitucion x ((v,p):ys)
  | x == v = p
  | otherwise = buscaSustitucion x ys


-- Función que regresa el valor de interpretación aplicada a una función en los
-- estados recibidos como parámetros.
interpreta :: Prop -> [Estado] -> Booleano
interpreta (FA (Cte V)) _ = V
interpreta (FA (Cte F)) _ = F
interpreta (FA (Var c)) e = interpreta (auxInterpreta (FA (Var c)) e) e
interpreta (Neg (FA (Cte F))) e = V
interpreta (Neg (FA (Cte V))) e = F
interpreta (Neg f) e = interpreta (Neg (FA (Cte (interpreta f e)))) e
interpreta (Op f Conj g) e
  | ((interpreta f e) == V) == ((interpreta g e) == V) = V
  | ((interpreta f e) == F) || ((interpreta g e) == F) = F
interpreta(Op f Disy g) e
  | ((interpreta f e) == V) || ((interpreta g e) == V) = V
  | ((interpreta f e) == F && (interpreta g e) == F) = F
interpreta(Op f Impl g) e
  | ((interpreta f e) == V) && ((interpreta g e) == F) = F
  | otherwise = V
interpreta(Op f Syss g) e
  | ((interpreta f e) == (interpreta g e)) = V
  | otherwise = F


auxInterpreta :: Prop -> [Estado] -> Prop
auxInterpreta (FA (Var c)) [] = error "vacio"
auxInterpreta (FA (Var f)) ((var, b):ys)
  | f == var = (FA (Cte b))
  | otherwise = auxInterpreta (FA (Var f)) ys


-- Función que regresa la forma normal negativa de una expresión
formaNN :: Prop -> Prop
formaNN f = negacionAtomica (eliminaImplicaciones (eliminaEquivalencias f))

-- Función que regresa la forma normal conjuntiva de una expresión
formaNC :: Prop -> Prop
formaNC f = disyuncionAtomica (formaNN f)

-- Función que verifica si una fórmula es tautología
esTautologia :: Prop -> Booleano
esTautologia p
    | esTautologiaAux p = V
    |otherwise = F

esTautologiaAux :: Prop -> Bool
esTautologiaAux p =
    and [ transforma (interpreta p e) | e <- interpretaciones p]



--------------------------------------------------------------------------------
--                MODIFICACIONES A LA PRÁCTICA 3                              --
-- Las funciones anteriores debieron implementarse en la práctica 2 y pueden  --
-- servir como auxiliares para esta práctica.                                 --
--------------------------------------------------------------------------------

-- Función que toma dos fórmulas proposicionales e indica si son equivalentes.
equivalentes :: Prop -> Prop -> Booleano
equivalentes p q
  | [ transforma (interpreta p e) | e <- interpretaciones p] == [ transforma (interpreta q e) | e <- interpretaciones q] = V
  | otherwise = F

-- Función que dada una fórmula, la simplifica. Esta es una versión mejorada a
-- la versión de la práctica 2.
-- Version anterior:
-- Función que dada una fórmula, elimina: dobles negaciones, disyunciones o
-- conjunciones de la misma variable y disyunciones con constantes.
simplifica :: Prop -> Prop
simplifica (FA (Cte c)) = (FA (Cte c))
simplifica (FA (Var v)) = (FA (Var v))
simplifica (Neg (Neg f)) = simplifica f
simplifica (Neg f) = Neg (simplifica f)
simplifica (Op p Conj q) = conjuncion (simplifica p) (simplifica q)
    where conjuncion (FA (Cte V))  q = q
          conjuncion (FA (Cte F)) q = (FA (Cte F))
          conjuncion q (FA (Cte V)) = q
          conjuncion q (FA (Cte F)) = (FA (Cte F))
          conjuncion p q | p == q = p
                         | otherwise = (Op p Conj q)

simplifica (Op p Disy q) = disyuncion (simplifica p) (simplifica q)
    where disyuncion (FA (Cte V)) q = (FA (Cte V))
          disyuncion (FA (Cte F)) q = q
          disyuncion q (FA (Cte V)) = (FA (Cte V))
          disyuncion q (FA (Cte F)) = q
          disyuncion (Neg x) f = (FA (Cte V))
          disyuncion f (Neg x) = (FA (Cte V))
          disyuncion p q | p == q = p
                         | otherwise =  (Op p Disy q)


-- Función que toma dos cláusulas y una literal como parámetro y regresa su
-- resolución binaria.
resBin :: Clausula -> Clausula -> Literal -> Clausula
resBin p q l = if elem l (listaLiteralesClausula p) &&  elem (NegAt l) (listaLiteralesClausula q) then (resolvente (eliminaLiteral(listaLiteralesClausula p) l) (eliminaLiteral (listaLiteralesClausula q) (NegAt l) ) ) else error "La literal no está en las cláusulas"

--Función que recibe dos listas de literales y devuelve una cláusula con todas las literales
resolvente :: [Literal] -> [Literal] -> Clausula
resolvente [] l = auxResolvente l
resolvente l [] = auxResolvente l
resolvente l1 l2 = NCLite (auxResolvente l1) Disy (auxResolvente l2)  

--Función auxiliar de resolvente que resive una lista de literales y devuelve una clausula con todas las literales
auxResolvente:: [Literal] -> Clausula
auxResolvente (x:[]) = (CLite x)
auxResolvente (x: xs) = NCLite (CLite x) Disy (auxResolvente xs)

--Función que recibe una lista de literales y una literal. Elimina la literal de la lista de literales.
eliminaLiteral:: [Literal] -> Literal -> [Literal]
eliminaLiteral (x:xs) a = if a==x then xs else x:(eliminaLiteral xs a) 

--Función auxiliar que recibe una cláusula y regresa una lista de literales
listaLiteralesClausula :: Clausula -> [Literal]
listaLiteralesClausula (CLite(LAtom(Var x))) = [(LAtom(Var x))]
listaLiteralesClausula (CLite(LAtom(Cte x))) = [(LAtom(Cte x))]
listaLiteralesClausula (CLite(NegAt(LAtom(Var x)))) = [(NegAt(LAtom(Var x)))]
listaLiteralesClausula (CLite(NegAt(LAtom(Cte x)))) = [(NegAt(LAtom(Cte x)))]
listaLiteralesClausula (NCLite c1 Disy c2) = (listaLiteralesClausula c1) ++ (listaLiteralesClausula c2)


---Auxiliares ---

eliminaEquivalencias :: Prop -> Prop
eliminaEquivalencias (FA (Cte V)) = (FA (Cte V))
eliminaEquivalencias (FA (Cte F)) = (FA (Cte F))
eliminaEquivalencias (FA (Var f)) = (FA (Var f))
eliminaEquivalencias (Neg f) = eliminaEquivalencias f
eliminaEquivalencias (Op f Conj g) = (Op (eliminaEquivalencias f) Conj (eliminaEquivalencias g))
eliminaEquivalencias (Op f Disy g) = (Op (eliminaEquivalencias f) Disy (eliminaEquivalencias g))
eliminaEquivalencias (Op f Impl g) = (Op (eliminaEquivalencias f) Impl (eliminaEquivalencias g))
eliminaEquivalencias (Op f Syss g) = (Op (Op (eliminaEquivalencias f) Impl (eliminaEquivalencias g)) Conj (Op (eliminaEquivalencias g) Impl (eliminaEquivalencias f)))


eliminaImplicaciones :: Prop -> Prop
eliminaImplicaciones (FA (Cte V)) = (FA (Cte V))
eliminaImplicaciones (FA (Cte F)) = (FA (Cte F))
eliminaImplicaciones (FA (Var f)) = (FA (Var f))
eliminaImplicaciones (Neg f) = eliminaImplicaciones f
eliminaImplicaciones (Op f Conj g) = (Op (eliminaImplicaciones f) Conj (eliminaImplicaciones g))
eliminaImplicaciones (Op f Disy g) = (Op (eliminaImplicaciones f) Disy (eliminaImplicaciones g))
eliminaImplicaciones (Op f Impl g) = (Op (Neg (eliminaImplicaciones f)) Conj (eliminaImplicaciones g))


negacionAtomica :: Prop -> Prop
negacionAtomica (FA (Cte V)) = (FA (Cte V))
negacionAtomica (FA (Cte F)) = (FA (Cte F))
negacionAtomica (FA (Var f)) = (FA (Var f))
negacionAtomica (Neg f) = auxNegacionAtomica f
negacionAtomica (Op f Conj g) = (Op (negacionAtomica f) Conj (negacionAtomica g))
negacionAtomica (Op f Disy g) = (Op (negacionAtomica f) Disy (negacionAtomica g))


auxNegacionAtomica :: Prop -> Prop
auxNegacionAtomica (FA (Cte V)) = (FA (Cte F))
auxNegacionAtomica (FA (Cte F)) = (FA (Cte V))
auxNegacionAtomica (FA (Var f)) = Neg((FA (Var f)))
auxNegacionAtomica (Neg f) = negacionAtomica f
auxNegacionAtomica (Op f Conj g) = (Op (auxNegacionAtomica f) Disy (auxNegacionAtomica g))
auxNegacionAtomica (Op f Disy g) = (Op (auxNegacionAtomica f) Conj (auxNegacionAtomica g))


disyuncionAtomica :: Prop -> Prop
disyuncionAtomica (FA (Var f)) = (FA (Var f))
disyuncionAtomica (Op (Op f Conj g) Disy h) =
    disyuncionAtomica (Op (Op (disyuncionAtomica f) Disy (disyuncionAtomica h)) Conj (Op (disyuncionAtomica g) Disy (disyuncionAtomica h)))
disyuncionAtomica (Op f Disy (Op g Conj h)) =
    disyuncionAtomica (Op (Op (disyuncionAtomica f) Disy (disyuncionAtomica g)) Conj (Op (disyuncionAtomica f) Disy (disyuncionAtomica h)))
disyuncionAtomica (Op f Conj g) = (Op (disyuncionAtomica f) Conj (disyuncionAtomica g))


variables :: Prop -> [VarP]
variables (FA (Cte _))  = []
variables (FA (Var f))  = [f]
variables (Neg p) = variables p
variables (Op p Conj q) = variables p ++ variables q
variables (Op p Disy q) = variables p ++ variables q
variables (Op p Impl q) = variables p ++ variables q
variables (Op p Syss q) = variables p ++ variables q

--es la lista de las interpretaciones con n variables.
interpretacionesVar :: Int -> [[Booleano]]
interpretacionesVar 0 = [[]]
interpretacionesVar n =
    map (F:) bss ++ map (V:) bss
    where bss = interpretacionesVar (n-1)


interpretaciones :: Prop -> [[Estado]]
interpretaciones p =
        [zip var boleano | boleano <- interpretacionesVar (length var)]
        where var = nub(variables p)


transforma :: Booleano -> Bool
transforma V = True
transforma F = False
