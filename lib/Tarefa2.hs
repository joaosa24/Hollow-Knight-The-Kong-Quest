{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : João Pedro Ribeiro de Sá <a104612@alunos.uminho.pt>
              Hélder Tiago Peixoto da Cruz <a104174@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Data.List 
 
{- | A função 'valida' recebe um __Jogo__ e determina se este é válido ou inválido.
Esta recorre às funções auxiliares 'hasFloor', 'hasRessaltaProp', 'checkColidePositions', 'hasMinimalEnemies','checkGhostLives' e 'validPosition'  para desta forma determinar o valor logico desta função. 

@
valida :: Jogo -> Bool
valida jogo = hasFloor (mapa jogo) && hasRessaltaProp jogo &&
              checkColidePositions jogo && hasMinimalEnemies (inimigos jogo) &&
              checkGhostLives (inimigos jogo) && validPosition jogo &&
              checkTrapdoorLength (jogador jogo) (mapa jogo) &&
              checkStairs (mapa jogo) && inBounds (inimigos jogo) (jogador jogo) (mapa jogo) &&
              checkFloor (mapa jogo)
@

== Propriedades:
prop> Se valida = True, o Jogo é válido. 
prop> Se valida = False, o Jogo é inválido.
prop> Basta que uma das funções auxiliares verificarem o valor __False__ para a verificar que o Jogo é inválido. 
-}

valida :: Jogo -> Bool
valida jogo = hasFloor (mapa jogo) && hasRessaltaProp jogo &&
              checkColidePositions jogo && hasMinimalEnemies (inimigos jogo) &&
              checkGhostLives (inimigos jogo) && validPosition jogo &&
              checkTrapdoorLength (jogador jogo) (mapa jogo) &&
              checkStairs (mapa jogo) && inBounds (inimigos jogo) (jogador jogo) (mapa jogo)

-- true se jogo verdadeiro


-- *Funções Auxiliares

-- ** Função 'hasFloor' 
{- | A função 'hasFloor' recebe um __Mapa__ e dá como resultado um __boolean__.
Esta função servirá para determinar o valor lógico de um determinado __Mapa__ com base na existência (ou não) de uma fila apenas composta por blocos "Plataforma":

@
hasFloor :: Mapa -> Bool
hasFloor (Mapa _ _ blocos) =  all (== Plataforma) chao 
  where chao = last blocos 
@

== Exemplo de utilização:
>>> hasFloor (Mapa ((0.0, 0.0), Este) (10.0, 10.0) [[Vazio,Vazio,Escada],[Escada,Escada,Escada],[Plataforma,Plataforma,Plataforma]]) = True
>>> hasFloor (Mapa ((0.0, 0.0), Este) (10.0, 10.0) [[Vazio,Vazio,Escada],[Escada,Escada,Escada],[Vazio,Vazio,Escada]]) = False
-}

hasFloor :: Mapa -> Bool
hasFloor (Mapa _ _ blocos) =  all (== Plataforma) chao 
  where chao = last blocos 

-- ** Função 'hasRessaltaProp' 
{- | A função 'hasRessaltaProp' recebe um __Jogo__ e dá como resultado um __boolean__.
Esta função servirá para determinar o valor lógico de um determinado __Jogo__ analisando o estado da propriedade "ressalta" nas diferentes entidades do Jogo.

@
hasRessaltaProp :: Jogo -> Bool
hasRessaltaProp jogo = hasRessaltaPropEnemy (inimigos jogo) && hasRessaltaPropPlayer (jogador jogo)
@

== Propriedades:
prop> Se hasRessaltaProp jogo = True     Isto significa que todas as entidades têm o estado da propriedade "ressalta" correto
prop> Se hasRessaltaProp jogo = False    Isto significa que pelo menos uma das entidades, não está com o estado da propriedade "ressalta" correto
-}

hasRessaltaProp :: Jogo -> Bool
hasRessaltaProp jogo = hasRessaltaPropEnemy (inimigos jogo) && hasRessaltaPropPlayer (jogador jogo)

-- ** Funções 'hasRessaltaPropEnemy' e 'hasRessaltaPropPlayer'
{- | As funções 'hasRessaltaPropEnemy' e 'hasRessaltaPropPlayer' recebem uma __Lista de Personagens__ e um __Personagem__ , respetivamente, e dão como resultado um __boolean__.
Estas funções servirão para determinar se o estado da propriedade "ressalta" está correto para todas as entidades.

@
hasRessaltaPropEnemy :: [Personagem] -> Bool
hasRessaltaPropEnemy inimigos = all ressalta inimigos

hasRessaltaPropPlayer :: Personagem -> Bool
hasRessaltaPropPlayer jogador = (ressalta jogador == False)
@

-}
hasRessaltaPropEnemy :: [Personagem] -> Bool
hasRessaltaPropEnemy inimigos = all ressalta inimigos

hasRessaltaPropPlayer :: Personagem -> Bool
hasRessaltaPropPlayer jogador = (ressalta jogador == False)

-- ** Função 'checkColidePositions' 
{- | A função 'checkColidePositions' recebe um __Jogo__ e dá como resultado um __boolean__.
Esta função servirá para determinar o valor lógico de um determinado __Jogo__ analisando as posições das entidades do jogo.

@
checkColidePositions :: Jogo -> Bool
checkColidePositions jogo = not $ any (positionsColide posicao_jogador) (map posicao (inimigos jogo)) 
                                where posicao_jogador = posicao (jogador jogo)
@

== Propriedades:
prop> Se checkColidePositions jogo = True     Isto significa que a posição inicial do jogador, não colide com a de nenhum inimigo
prop> Se checkColidePositions jogo = False    Isto significa que a posição inicial do jogodar, colide com a posição inicial, de pelo menos um inimigo
-}

checkColidePositions :: Jogo -> Bool
checkColidePositions jogo = not $ any (positionsColide posicao_jogador) (map posicao (inimigos jogo)) 
                                where posicao_jogador = posicao (jogador jogo)

positionsColide :: Posicao -> Posicao -> Bool
positionsColide (x1, y1) (x2, y2) = floor(x1) == floor(x2) && floor(y1) == floor(y2)

-- ** Função 'hasMinimalEnemies' 
{- | A função 'hasMinimalEnemies' recebe uma __Lista de Personagens__ e dá como resultado um __boolean__.
Esta função servirá para determinar se existe o número mínimo de inimigos.

@
hasMinimalEnemies :: [Personagem] -> Bool 
hasMinimalEnemies inimigos = length inimigos >= 2
@

== Propriedades:
prop> hasMinimalEnemies [inimigos] = True     Isto significa que o número de inimigos está de acordo com os requisitos
prop> hasMinimalEnemies [inimigos] = False    Isto significa que o número de inimigos não está de acordo com os requisitos
-}

hasMinimalEnemies :: [Personagem] -> Bool 
hasMinimalEnemies inimigos = length inimigos >= 2

-- ** Função 'checkGhostLives' 
{- | A função 'checkGhostLives' recebe uma __Lista de Personagens__ e dá como resultado um __boolean__.
Esta função servirá para determinar se todos os inimigos do tipo "Fantasma" têm 1 de vida.

@
checkGhostLives :: [Personagem] -> Bool 
checkGhostLives inimigos = all ghostHasOneLife ghosts
                where ghosts = filter (isGhost) inimigos
@

== Propriedades:
prop> checkGhostLives [inimigos] = True     Isto significa que todos os inimigos do tipo fantasma, têm 1 de vida
prop> checkGhostLives [inimigos] = False    Isto significa que existe algum inimigo do tipo fantasma, que não tem 1 de vida
-}

checkGhostLives :: [Personagem] -> Bool 
checkGhostLives inimigos = all ghostHasOneLife ghosts
                where ghosts = filter (isGhost) inimigos

-- ** Funções 'isGhost' e 'ghostHasOneLife'
{- | As funções 'isGhost' e 'ghostHasOneLife' recebem um __Personagem__ e dão como resultado um __boolean__.
Estas funções servirão para determinar se um inimigo é do tipo fantasma e se o fantasma tem 1 de vida, respetivamente.

@
isGhost :: Personagem -> Bool
isGhost p = tipo p == Fantasma

ghostHasOneLife :: Personagem -> Bool 
ghostHasOneLife ghost = vida ghost == 1
@

== Propriedades:
prop> A função isGhost verifica se o inimigo é um fantasma
prop> A função ghostHasOneLife recebe, necessariamente, um inimigo do tipo fantasma e verifica se o mesmo tem 1 de vida
-}

isGhost :: Personagem -> Bool
isGhost p = tipo p == Fantasma

ghostHasOneLife :: Personagem -> Bool 
ghostHasOneLife ghost = vida ghost == 1

-- ** Função 'checkStairs' 
{- | A função 'checkStairs' recebe um __Mapa__ e dá como resultado um __boolean__.
Esta função servirá para determinar se as escadas existentes no mapa estão construídas de uma maneira válida.
Para isso irá recorrer a duas funções auxiliares: 'trapDoorCheck' e 'checkStairEnding'
Note que a função principal passa como argumento os blocos correspondentes a uma coluna, de modo a facilitar a análise

@
checkStairs :: Mapa -> Bool 
checkStairs (Mapa _ _ blocos) = trapDoorCheck blocos_por_coluna && checkStairEnding blocos_por_coluna
                              where blocos_por_coluna = transpose blocos
@

== Propriedades: checkStairs
prop> checkStairs mapa = True     Isto significa que todas as escadas do mapa estão de acordo com os requisitos
prop> checkStairs mapa = False    Isto significa que pelo menos uma escada não está de acordo com os requisitos
-}
checkStairs :: Mapa -> Bool 
checkStairs (Mapa _ _ blocos) = trapDoorCheck blocos_por_coluna && checkStairEnding blocos_por_coluna
                              where blocos_por_coluna = transpose blocos

-- *** Função 'trapDoorCheck' , 'trapDoorCheckAux' , 'hasTrapDoorOnTop' , 'hasTrapDoorOnBottom'
{- | A função 'trapDoorCheck' recebe uma __Matriz de Blocos__ e dá como resultado um __boolean__.
Esta função determina se existe algum bloco do tipo 'Alcapao' nas extremidades de um segmento de blocos do tipo 'Escada'

@
trapDoorCheck :: [[Bloco]] -> Bool 
trapDoorCheck matriz = all (trapDoorCheckAux) matriz

trapDoorCheckAux :: [Bloco] -> Bool
trapDoorCheckAux [] = True  
trapDoorCheckAux blocos = not (hasTrapDoorOnTop blocos || hasTrapDoorOnBottom blocos)

hasTrapDoorOnTop :: [Bloco] -> Bool
hasTrapDoorOnTop (Escada : rest) = case rest of
  Alcapao : _ -> True
  _           -> hasTrapDoorOnTop rest
hasTrapDoorOnTop (_ : rest) = hasTrapDoorOnTop rest
hasTrapDoorOnTop [] = False

hasTrapDoorOnBottom :: [Bloco] -> Bool
hasTrapDoorOnBottom = hasTrapDoorOnTop . reverse
@

== Propriedades: trapDoorCheck
prop> trapDoorCheck blocos = True     Isto significa que não existe nenhuma bloco do tipo 'Alcapao' nas extremidades dos segmentos de escada do mapa
prop> trapDoorCheck blocos = False    Isto significa que existe um bloco do tipo 'Alcapao' em pelo menos uma das extremidades dos segmentos de escada do mapa
-}
trapDoorCheck :: [[Bloco]] -> Bool 
trapDoorCheck matriz = all (trapDoorCheckAux) matriz

trapDoorCheckAux :: [Bloco] -> Bool
trapDoorCheckAux [] = True  
trapDoorCheckAux blocos = not (hasTrapDoorOnTop blocos || hasTrapDoorOnBottom blocos)

hasTrapDoorOnTop :: [Bloco] -> Bool
hasTrapDoorOnTop (Escada : rest) = case rest of
  Alcapao : _ -> True
  _           -> hasTrapDoorOnTop rest
hasTrapDoorOnTop (_ : rest) = hasTrapDoorOnTop rest
hasTrapDoorOnTop [] = False

hasTrapDoorOnBottom :: [Bloco] -> Bool
hasTrapDoorOnBottom = hasTrapDoorOnTop . reverse

-- *** Função 'checkStairEnding' , 'checkDown' , 'checkDownAux' , 'checkDownAux2' , 'checkUp' e 'checkUpAux'
{- | A função 'checkStairEnding' verifica se todas os segmentos de Escada têm, em pelo menos uma das suas extremidades, um bloco do tipo 'Plataforma'
A funcção utiliza duas funções auxiliares, de modo a facilitar a análise

@
checkStairEnding :: [[Bloco]] -> Bool
checkStairEnding blocos = checkDown blocos || checkUp blocos

checkDown :: [[Bloco]] -> Bool 
checkDown matriz = all  (checkDownAux) matriz

checkDownAux :: [Bloco] -> Bool
checkDownAux blocos | Escada `elem` blocos = checkDownAux2 blocos 
                    | otherwise = True 

checkDownAux2 :: [Bloco] -> Bool
checkDownAux2 [] = True  
checkDownAux2 (Escada:Plataforma:t) = checkDownAux2 t 
checkDownAux2 (Escada:Escada:t) = checkDownAux2 (Escada:t)
checkDownAux2 (bloco:xs) | bloco `elem` [Vazio,Plataforma,Alcapao] = checkDownAux2 xs
checkDownAux2 _ = False

checkUp :: [[Bloco]] -> Bool 
checkUp matriz = all (checkUpAux) matriz 

checkUpAux :: [Bloco] -> Bool 
checkUpAux blocos = checkDownAux coluna_invertida
                  where coluna_invertida = reverse blocos
@
-}

checkStairEnding :: [[Bloco]] -> Bool
checkStairEnding blocos = checkDown blocos || checkUp blocos

checkDown :: [[Bloco]] -> Bool 
checkDown matriz = all  (checkDownAux) matriz

checkDownAux :: [Bloco] -> Bool
checkDownAux blocos | Escada `elem` blocos = checkDownAux2 blocos 
                    | otherwise = True 

checkDownAux2 :: [Bloco] -> Bool
checkDownAux2 [] = True  
checkDownAux2 (Escada:Plataforma:t) = checkDownAux2 t 
checkDownAux2 (Escada:Escada:t) = checkDownAux2 (Escada:t)
checkDownAux2 (bloco:xs) | bloco `elem` [Vazio,Plataforma,Alcapao] = checkDownAux2 xs
checkDownAux2 _ = False

checkUp :: [[Bloco]] -> Bool 
checkUp matriz = all (checkUpAux) matriz 

checkUpAux :: [Bloco] -> Bool 
checkUpAux blocos = checkDownAux coluna_invertida
                  where coluna_invertida = reverse blocos

-- ** Função 'checkTrapdoorLength' 
{- | A função 'checkTrapdoorLength' recebe um __Personagem__ , um __Mapa__ e dá como resultado um __boolean__.
Esta função verifica se os blocos do tipo 'Alcapao' são maiores ou iguais ao tamanho do Jogador

@
checkTrapdoorLength :: Personagem -> Mapa -> Bool
checkTrapdoorLength jogador (Mapa _ _ blocos) = all (>= fst (tamanho jogador)) (blocks)
                                            where blocks = map fromIntegral (trapdoorLength blocos)
@

== Propriedades: checkStairs
prop> checkTrapdoorLength jogador mapa = True     Isto significa que todas os alcapões do mapa estão de acordo com os requisitos
prop> checkTrapdoorLength jogador mapa = False    Isto significa que pelo menos um alcapão não está de acordo com os requisitos
-}

checkTrapdoorLength :: Personagem -> Mapa -> Bool
checkTrapdoorLength jogador (Mapa _ _ blocos) = all (>= fst (tamanho jogador)) (blocks)
                                            where blocks = map fromIntegral (trapdoorLength blocos)
                                             
-- *** Função 'trapdoorLength' , 'trapdoorLengthAux' e 'groupTrapdoor'
{- | A função trapdoorLength usa outras duas funções para fazer a verificação do mapa
@
trapdoorLength :: [[Bloco]] -> [Int]
trapdoorLength [] = []
trapdoorLength (x:xs) = trapdoorLengthAux x ++ trapdoorLength xs

trapdoorLengthAux :: [Bloco] -> [Int]
trapdoorLengthAux blocos = map length (groupTrapdoor blocos)

groupTrapdoor :: [Bloco] -> [[Bloco]]
groupTrapdoor blocos = filter (isTrapdoor) (group blocos) 
                      where isTrapdoor :: [Bloco] -> Bool 
                            isTrapdoor l = head l == Alcapao

@
-}

trapdoorLength :: [[Bloco]] -> [Int]
trapdoorLength [] = []
trapdoorLength (x:xs) = trapdoorLengthAux x ++ trapdoorLength xs

trapdoorLengthAux :: [Bloco] -> [Int]
trapdoorLengthAux blocos = map length (groupTrapdoor blocos)

groupTrapdoor :: [Bloco] -> [[Bloco]]
groupTrapdoor blocos = filter (isTrapdoor) (group blocos) 
                      where isTrapdoor :: [Bloco] -> Bool 
                            isTrapdoor l = head l == Alcapao


-- ** Função 'validPosition' 
{- | A função 'validPosition' recebe um __Jogo__ e dá como resultado um __boolean__.
Esta função servirá para determinar se todos as entidades e colecionáveis estão num bloco do tipo "Vazio".

@
validPosition :: Jogo -> Bool 
validPosition jogo = playerValidPosition (jogador jogo) (mapa jogo) && enemiesValidPosition (inimigos jogo) (mapa jogo) && collectiblesValidPosition (colecionaveis jogo) (mapa jogo) 
@

== Propriedades:
prop> validPosition jogo = True     Isto significa que todos as entidades e colecionáveis estão num bloco do tipo "Vazio"
prop> validPosition jogo = False    Isto significa que existe, pelo menos, uma entidade / colecionável num bloco sem ser do tipo "Vazio"
-}

validPosition :: Jogo -> Bool 
validPosition jogo = (playerValidPosition (jogador jogo) (mapa jogo) && enemiesValidPosition (inimigos jogo) (mapa jogo) && collectiblesValidPosition (colecionaveis jogo) (mapa jogo)) 
                   where enemies = inimigos jogo
                         player = jogador jogo
                         map = mapa jogo

-- ** Funções 'playerValidPosition' , 'enemiesValidPosition' e 'collectiblesValidPosition'
{- | As funções 'playerValidPosition' ,'enemiesValidPosition' e 'collectiblesValidPosition' recebem um __Personagem__ , uma __Lista de Personagens__ e uma __Lista de Tuplos de Colecionáveis__ e dão como resultado um __boolean__.
Estas funções servirão para determinar se todas as entidades e colecionáveis estão na posição correta.

@
playerValidPosition :: Personagem -> Mapa -> Bool
playerValidPosition jogador (Mapa _ _ blocos) = blockInPosition (posicao jogador) blocos == Vazio

enemiesValidPosition :: [Personagem] -> Mapa -> Bool
enemiesValidPosition [] _ = True 
enemiesValidPosition (e1:enemies) mapa@(Mapa _ _ blocos) = (blockInPosition (posicao e1) blocos == Vazio) && enemiesValidPosition enemies mapa


collectiblesValidPosition :: [(Colecionavel,Posicao)] -> Mapa -> Bool
collectiblesValidPosition [] _ = True
collectiblesValidPosition ((_,pos):t) mapa@(Mapa _ _ blocos) = (blockInPosition pos blocos == Vazio) && collectiblesValidPosition t mapa
@

== Propriedades:
prop> A função 'playerValidPosition' verifica se o jogoador está num bloco do tipo "Vazio"
prop> A função 'enemiesValidPosition' verifica se todos os inimigos estão num bloco do tipo "Vazio"
prop> A função 'collectiblesValidPosition' verifica se todos os colecionáveis estão num bloco do tipo "Vazio"
-}

playerValidPosition :: Personagem -> Mapa -> Bool
playerValidPosition jogador (Mapa _ _ blocos) = blockInPosition (posicao jogador) blocos == Vazio 

-- remover recursividade usando a função all
enemiesValidPosition :: [Personagem] -> Mapa -> Bool
enemiesValidPosition [] _ = True 
enemiesValidPosition (e1:enemies) mapa@(Mapa _ _ blocos) = (elem (blockInPosition (posicao e1) blocos) [Vazio, Escada]) && enemiesValidPosition enemies mapa

collectiblesValidPosition :: [(Colecionavel,Posicao)] -> Mapa -> Bool
collectiblesValidPosition [] _ = True
collectiblesValidPosition ((_,pos):t) mapa@(Mapa _ _ blocos) = (blockInPosition pos blocos == Vazio) && collectiblesValidPosition t mapa

blockInPosition :: Posicao -> [[Bloco]] -> Bloco
blockInPosition (x,y) blocos = linha !! floor x 
                              where linha = blocos !! floor y


-- ** Funções 'inBounds' , 'inBoundsEnemy' e 'inBoundsPlayer'
{- | As funções 'inBounds' (recorrendo às duas funções auxiliares,'inBoundsEnemy' e 'inBoundsPlayer') é responsável por determinar se todos os personagens estão dentro do Mapa

@
inBounds :: [Personagem] -> Personagem -> Mapa -> Bool
inBounds inimigos jogador (Mapa _ _ blocos) = inBoundsEnemy inimigos blocos && inBoundsPlayer jogador blocos 

inBoundsEnemy :: [Personagem] -> [[Bloco]] -> Bool
inBoundsEnemy [] _ = True 
inBoundsEnemy (h:t) blocos = (fst(posicao h) >= 0.0 && fst(posicao h) < largura) && (snd(posicao h) >= 0.0 && snd(posicao h) < altura) && inBoundsEnemy t blocos
                              where altura = fromIntegral(length blocos)
                                    largura = fromIntegral(length (head blocos))

inBoundsPlayer :: Personagem -> [[Bloco]] -> Bool
inBoundsPlayer jogador blocos = (x_jogador >= 0.0 && x_jogador < largura) && (y_jogador >= 0.0 && y_jogador < altura)
                                 where altura = fromIntegral(length blocos)
                                       largura = fromIntegral(length (head blocos))
                                       x_jogador = fst(posicao jogador)
                                       y_jogador = snd(posicao jogador)
@

== Propriedades:
prop> A função 'inBounds' é a disjunção das duas funções auxiliares (i.e apenas será verdade quando ambas forem verdadeiras)
prop> A função 'inBoundsEnemy' verifica se todos os inimigos estão dentro do mapa.
prop> A função 'inBoundsPlayer' verifica se o jogador está dentro do mapa
-}

inBounds :: [Personagem] -> Personagem -> Mapa -> Bool
inBounds inimigos jogador (Mapa _ _ blocos) = inBoundsEnemy inimigos blocos && inBoundsPlayer jogador blocos 

inBoundsEnemy :: [Personagem] -> [[Bloco]] -> Bool
inBoundsEnemy [] _ = True 
inBoundsEnemy (h:t) blocos = (fst(posicao h) >= 0.0 && fst(posicao h) < largura) && (snd(posicao h) >= 0.0 && snd(posicao h) < altura) && inBoundsEnemy t blocos
                              where altura = fromIntegral(length blocos)
                                    largura = fromIntegral(length (head blocos))

inBoundsPlayer :: Personagem -> [[Bloco]] -> Bool
inBoundsPlayer jogador blocos = (x_jogador >= 0.0 && x_jogador < largura) && (y_jogador >= 0.0 && y_jogador < altura)
                                 where altura = fromIntegral(length blocos)
                                       largura = fromIntegral(length (head blocos))
                                       x_jogador = fst(posicao jogador)
                                       y_jogador = snd(posicao jogador)
                                    