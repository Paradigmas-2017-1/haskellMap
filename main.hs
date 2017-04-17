-- Cardapio
-- opcao, qtdPessoas, desconto

import Control.Monad
import System.IO

mostraConteudoArq :: String -> String -> IO()
mostraConteudoArq stringMensagem stringArq = do
    handle <- openFile stringArq ReadMode
    conteudoOpcoes <- hGetContents handle
    let qtdOpcoes = lines conteudoOpcoes
        opcoesEnum = zipWith (\n line -> show n ++ " - " ++ line) [1..] qtdOpcoes
    putStrLn stringMensagem
    putStr $ unlines opcoesEnum
    hClose handle

pedidos :: Int -> String -> IO ()
pedidos qtdPessoas fileName = do
    putStrLn "Escolha a opcao do pedido:"
    opcaoCardJant <- getLine
    let optionToInt = read opcaoCardJant :: Int
    handle <- openFile fileName ReadMode
    conteudoOpcoes <- hGetContents handle
    let arrayOfContents = lines conteudoOpcoes
        lineContent = arrayOfContents !! (optionToInt - 1)
    appendFile "pedidos.txt" (lineContent ++ "\n")
    putStrLn "Mais um pedido (s - sim/ n -nao)?"
    resposta <- getChar
    if (resposta /= 'n') then pedidos qtdPessoas fileName
    else print "Pedido encerrado."

showDefaultMenu :: Int -> IO()
showDefaultMenu numOfPeople = do
    mostraConteudoArq "Aqui estao as opcoes:" "opcoes.txt"

    putStrLn "Escolha a opcao de lanche:"
    escolhaOpcao <- getLine
    let escolhaOpcaoInt = read escolhaOpcao
    if (escolhaOpcaoInt == 1) then do
        mostraConteudoArq "Opcoes de cafe:" "cafe.txt"
        pedidos numOfPeople "cafe.txt"
        showDefaultMenu numOfPeople
    else if (escolhaOpcaoInt == 2) then do
        mostraConteudoArq "Opcoes de almoco:" "almoco.txt"
        pedidos numOfPeople "almoco.txt"
        showDefaultMenu numOfPeople
    else if (escolhaOpcaoInt == 3) then do
        mostraConteudoArq "Opcoes de cafe da manha:" "jantar.txt"
        pedidos numOfPeople "jantar.txt"
        showDefaultMenu numOfPeople
    else do
        print "Pedido encerrado"

mostraPedidos :: IO()
mostraPedidos = do
    handle<- openFile "pedidos.txt" ReadMode
    contents <- hGetContents handle
    putStrLn contents
--    let thisWords = words contents
  --  let b = length thisWords
   -- let a = [x*2 | x <- [1..b]]

    hClose handle
--    thisWords !! a

main = do
    putStrLn "Bem Vindo ao Restaurante do Haskell!!!"
    putStrLn "Digite a quantidade de pessoas na mesa:"
    qtdPessoasMesa <- getLine
    let qtdPessoasMesaInt = read qtdPessoasMesa
    if (qtdPessoasMesaInt > 8) then print "Ultrapassou o limite da mesa!"
    else do
        print "Sinta-se em casa"
        showDefaultMenu qtdPessoasMesaInt
        mostraPedidos
