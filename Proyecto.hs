-- Primero, definimos el tipo de dato que será nuestro arbol general.
data Arbol a = Void | Node a [Arbol a]
    deriving(Show, Eq)

-- Ahora, definimos el tipo de dato Prop para las formulas de lógica proposicional (Créditos a Eduardo).
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

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
arbolPrueba =
    Node (And (Var "p") (Or (Var "q") (Not (Var "r"))))
        [ Node (Var "p") []
        , Node (And (Var "q") (Not (Var "l")))
            [ Node (Var "q") []
            , Node (Not (Var "l"))
                [ Node (Var "l") [] ]
            ]
        ]