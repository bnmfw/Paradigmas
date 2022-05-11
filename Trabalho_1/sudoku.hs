type Item = (Int, Int)
type Tabuleiro = [[Item]]
type Posicao = [Int]

meuTabuleiro :: Tabuleiro
meuTabuleiro = [[(0,0), (0,0), (0,9), (1,0), (1,0), (1,5), (2,6), (2,0), (2,8)], 
                [(0,7), (0,3), (0,0), (1,0), (1,2), (1,0), (2,0), (2,0), (2,0)], 
                [(0,5), (0,2), (0,6), (1,8), (1,0), (1,0), (2,0), (2,4), (2,7)], 
                [(3,0), (3,0), (3,0), (4,0), (4,4), (4,0), (5,8), (5,9), (5,0)], 
                [(3,0), (3,0), (3,9), (4,6), (4,0), (4,3), (5,5), (5,0), (5,0)], 
                [(3,0), (3,6), (3,2), (4,0), (4,8), (4,0), (5,0), (5,0), (5,0)], 
                [(6,2), (6,6), (6,0), (7,0), (7,0), (7,7), (8,9), (8,5), (8,1)], 
                [(6,0), (6,0), (6,0), (7,0), (7,1), (7,0), (8,0), (8,6), (8,4)], 
                [(6,1), (6,0), (6,8), (7,6), (7,0), (7,0), (8,2), (8,0), (8,0)]]

outroTabuleiro :: Tabuleiro
outroTabuleiro = [[(0,0) , (0,1)], 
                  [(1,0), (1,1)]]


-- Retorna o valor de um elemento do tabuleiro
getValor :: (Int, Int) -> Int
getValor (_, valor) = valor

-- Retorna a regiao de um elemento do tabuleiro
getRegiao :: (Int, Int) -> Int
getRegiao (regiao, _) = regiao

-- Alterna o valor de um elemento do tabuleiro e retorna o elemento
setValor :: (Int, Int) -> Int -> (Int, Int)
setValor (regiao, antigo) novo = (regiao, novo)

-- Retorna o valor de um elemento do tabuleiro dada sua posicao
get :: Tabuleiro -> Posicao -> Int
get tab pos = getValor((tab!!(pos!!0))!!(pos!!1))

-- Altera o valor de um elemento numa lista, retornando a lista
setLista :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
setLista (a:b) 0 valor = [setValor a valor] ++ b
setLista (a:b) pos valor =  [a] ++ setLista b (pos-1) valor 

-- Altera o valor de um elemento no tabueleiro, retornando o tabuleiro modificado
set :: Tabuleiro -> Posicao -> Int -> Tabuleiro
set (a:b) [0, coluna] valor = (setLista a coluna valor) : b
set (a:b) [linha, coluna] valor = [a] ++ set b [linha-1, coluna] valor

main = do
    print (set outroTabuleiro [1,1] 4)