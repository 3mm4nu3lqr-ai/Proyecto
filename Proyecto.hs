-- Primero, definimos el tipo de dato que será nuestro arbol general.
data Arbol a = Void | Node a [Arbol a]
    deriving(Show, Eq)

-- Ahora, definimos el tipo de dato Prop para las formulas de lógica proposicional (Créditos a Eduardo).
data Prop = Var String 
            | Cons Bool 
            | Not Prop
            | And Prop Prop 
            | Or Prop Prop
            | Impl Prop Prop 
            | Syss Prop Prop
            deriving (Eq)

type Estado = [String]

-- Fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Luego, para que se vea bien en la términal: (Créditos a Eduardo, que él se rifó este código jeje)
-- Nota: Cambié los símbolos porque mi términal no los imprimía xd. Atte. Fernando.
instance Show Prop where 
                    show (Cons True) = "Verdadero"
                    show (Cons False) = "Falso"
                    show (Var p) = p
                    show (Not p) = "~" ++ show p
                    show (Or p q) = "(" ++ show p ++ " | " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " & " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"

-- Función que muestra el árbol de sintaxis abstracta de una fórmula de lógica proposicional.
arbolSintaxis :: Prop -> Arbol Prop

arbolSintaxis (Var x) = Node (Var x) []
arbolSintaxis (Cons b) = Node (Cons b) []
arbolSintaxis (Not p) = Node (Not p) [arbolSintaxis p]
arbolSintaxis (And p q) = Node (And p q) [arbolSintaxis p, arbolSintaxis q]
arbolSintaxis (Or p q) = Node (Or p q) [arbolSintaxis p, arbolSintaxis q]
arbolSintaxis (Impl p q) = Node (Impl p q) [arbolSintaxis p, arbolSintaxis q]
arbolSintaxis (Syss p q) = Node (Syss p q) [arbolSintaxis p, arbolSintaxis q]

-- Función que muestra la fórmula de lógica proposicional de un árbol de sintaxis dado (lo mismo que la anterior pero al revés).
formulaArbol :: Arbol Prop -> Prop

formulaArbol (Node (Var x) []) = Var x
formulaArbol (Node (Cons b) []) = Cons b
formulaArbol (Node (Not _) [a]) = Not (formulaArbol a)
formulaArbol (Node (And _ _) [a, b]) = And (formulaArbol a) (formulaArbol b)
formulaArbol (Node (Or _ _) [a, b]) = Or (formulaArbol a) (formulaArbol b)
formulaArbol (Node (Impl _ _) [a, b]) = Impl (formulaArbol a) (formulaArbol b)
formulaArbol (Node (Syss _ _) [a, b]) = Syss (formulaArbol a) (formulaArbol b)

-- Necesitamos una función auxiliar para la función de evaluación de un árbol, es un estado de ejemplo.
estado :: String -> Bool

estado "p" = True
estado "q" = True
estado "k" = False
estado "l" = True
estado _ = False

-- Función que evalúa si un árbol de sintaxis dado es verdadero o falso dependiendo de un estado dado.
evalArbol :: Arbol Prop -> (String -> Bool) -> Bool

evalArbol (Node (Var p) []) est = est p
evalArbol (Node (Cons b) []) est = b
evalArbol (Node (Not _) [a]) est = not (evalArbol a est)
evalArbol (Node (And _ _) [a, b]) est = (evalArbol a est) && (evalArbol b est)
evalArbol (Node (Or _ _) [a, b]) est = (evalArbol a est) || (evalArbol b est)
evalArbol (Node (Impl _ _) [a, b]) est = not (evalArbol a est) || (evalArbol b est)
evalArbol (Node (Syss _ _) [a, b]) est = evalArbol a est == evalArbol b est

-- Hagamos un árbol de prueba para facilitarnos la vida.
arbolPrueba :: Arbol Prop
arbolPrueba =
    Node (And (Var "p") (Or (Var "q") (Not (Var "r"))))
        [ Node (Var "p") []
        , Node (And (Var "q") (Not (Var "l")))
            [ Node (Var "q") []
            , Node (Not (Var "l"))
                [ Node (Var "l") [] ]
            ]
        ]

-- Función que recibe un árbol y regresa la cantidad de elementos que hay en el árbol.
cantidadElementos :: Arbol a -> Int
cantidadElementos Void = 0
cantidadElementos (Node _ h) = 1 + sumaList (mapCanti h)

-- Función que recibe un árbol y un elemento. El resultado es un booleano que indica si el elemento se encuentra o no en el árbol.
busca :: Eq a => Arbol a -> a -> Bool
busca Void _ = False
busca (Node x h) y = (x == y) || orList (mapBusqueda h y)
    
-- Función que recibe un árbol y devuelve la suma de todos sus elementos.
sumaElementos :: Arbol Prop -> Int
sumaElementos Void = 0
sumaElementos (Node _ h) =
    1 + sumaList2 (mapSuma h)

-- Función preorden
preorden :: Arbol a -> [a]
preorden Void = []
preorden (Node x h) = x : concatList (mapPre h)

-- Función postorden
postorden :: Arbol a -> [a]
postorden Void = []
postorden (Node x h) = concatList (mapPost h)  ++ [x]

-- Función que recibe un árbol y devuelve la altura del árbol.
altura :: Arbol a -> Int
altura Void = 0
altura (Node _ h) = 1 + maxList (mapAltura h)

-- Función que recibe un árbol y devuelve el espejo de dicho árbol.
espejo :: Arbol a -> Arbol a
espejo Void = Void
espejo (Node x h) = Node x (revList (mapEsp h))

-- Función podar
podar :: Arbol a -> Int -> Arbol a
podar Void _ = Void
podar w 0 = Void
podar (Node x h) n = 
    Node x (mapPodar h (n-1))

-- Función elementosProfundidad
elementosProfundidad :: Arbol a -> Int -> [a]
elementosProfundidad Void _ = []
elementosProfundidad (Node x h) 0 = [x]
elementosProfundidad (Node _ h) n = 
    concatList (mapProfundidad h (n-1))

-- Funciones auxiliares
mapCanti :: [Arbol a] -> [Int] -- Aplica la función cantidadElementos a cada subárbol de la lista
mapCanti [] = []
mapCanti (x:xs) = cantidadElementos x : mapCanti xs

sumaList :: [Int] -> Int -- Suma todos los enteros de una lista usando recursión
sumaList [] = 0
sumaList (x:xs) = x + sumaList xs

mapBusqueda :: Eq a => [Arbol a] -> a -> [Bool] -- Aplica la función busca a cada subárbol de la lista, buscando el mismo elemento
mapBusqueda [] _ = []
mapBusqueda (w:ws) y = busca w y : mapBusqueda ws y

orList :: [Bool] -> Bool -- Calcula el OR lógico de todos los valores en una lista de booleanos
orList [] = False
orList (b:bs) = b || orList bs

mapSuma ::[Arbol Prop] -> [Int] -- Aplica la función sumaElementos a cada subárbol
mapSuma [] = []
mapSuma (w:ws) = sumaElementos w : mapSuma ws

sumaList2 :: Num a => [a] -> a -- Suma general para listas numéricas
sumaList2 [] = 0
sumaList2 (x:xs) = x + sumaList2 xs

mapPre :: [Arbol a] -> [[a]] -- Aplica preorden a cada subárbol, generando una lista de listas
mapPre [] = []
mapPre (w:ws) = preorden w : mapPre ws

concatList :: [[a]] -> [a] -- Concatena una lista de listas en una sola lista
concatList [] = []
concatList (x : xs) = x ++ concatList xs

mapPost :: [Arbol a] -> [[a]] -- Aplica postorden a cada subárbol, generando una lista de listas
mapPost [] = []
mapPost (w:ws) = postorden w : mapPost ws

mapAltura :: [Arbol a] -> [Int] -- Aplica altura a cada subárbol
mapAltura [] = []
mapAltura (w:ws) = altura w : mapAltura ws

maxList :: [Int] -> Int -- Devuelve el máximo elemento de una lista de enteros
maxList [] = 0
maxList [x] = x
maxList (x:xs) =
    let m = maxList xs
    in if x > m then x else m

mapEsp :: [Arbol a] -> [Arbol a] -- Aplica espejo a cada subárbol
mapEsp [] = []
mapEsp (w:ws) = espejo w : mapEsp ws

revList :: [a] -> [a] -- Invierte una lista de manera recursiva
revList [] = []
revList (x:xs) = revList xs ++ [x]

mapPodar :: [Arbol a] -> Int -> [Arbol a] -- Aplica podar a cada subárbol, disminuyendo la profundidad permitida
mapPodar [] _ = []
mapPodar (w:ws) n = podar w n : mapPodar ws n

mapProfundidad :: [Arbol a] -> Int -> [[a]] -- Aplica elementosProfundidad a cada subárbol, para una profundidad dada
mapProfundidad [] _ = []
mapProfundidad (w:ws) n = elementosProfundidad w n : mapProfundidad ws n
