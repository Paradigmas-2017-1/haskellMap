import System.IO  
import System.Directory  
import Data.List  

-- Definições para os atributos do grafo
type Vertice = Int
type Aresta = (Vertice, Vertice)
type Grafo = [Aresta]

-- Função grafo retorna uma array de aresta
grafo :: [Aresta]
grafo = [
    (1,2), (2,3), (1,4), (1,5) -- Definindo ligações dos nós do grafo
    ]

-- Função adjacentes recebe o grafo e um vértive e retorna a lista de ligações
adjacentes :: Grafo -> Vertice -> [Vertice]
adjacentes [] _ = [] -- se entrada vier vazia, printa nada
adjacentes ((a,b):c) v
    | (a == v) = b:(adjacentes c v)
    | (b == v) = a:(adjacentes c v)
    | otherwise = adjacentes c v
  
main = do  
    handle <- openFile "cidades.txt" ReadMode  
    tempdir <- getTemporaryDirectory  
    (tempName, tempHandle) <- openTempFile tempdir "temp"  
    conteudo <- hGetContents handle  
    let qtdCidades = lines conteudo  
        cidadesNumeradas = zipWith (\n line -> show n ++ " - " ++ line) [1..] qtdCidades  
    putStrLn "Essas são as cidades:"  
    putStr $ unlines cidadesNumeradas  
    putStrLn "Qual você deseja olhar as ligações?"  
    escolhaCidade <- getLine
    let escolha = read escolhaCidade  
        ligacoes  = adjacentes grafo escolha
        --z = zipWith (\n line -> show n ++ " - " ++ line) [1..] ligacoes
    --putStrLn "Essas são as cidades:"  
    --putStr $ unlines z  
    hClose handle
    hClose tempHandle
