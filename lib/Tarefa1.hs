{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : João Pedro Ribeiro de Sá <a104612@alunos.uminho.pt>
              Hélder Tiago Peixoto da Cruz <a104174@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324
    
{- | A função 'colisoesParede' analisa se um Personagem está a colidir (ou não) com um bloco do tipo "Plataforma"

@
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa _ _ blocos) jogador = hitBoxColideBlocos (hitbox_jogador) (hitBoxPlataforma blocos 0) || not(hitBoxDentroLimites hitbox_jogador blocos)
  where
    hitbox_jogador = hitBoxPersonagem jogador
@

== Propriedades:
prop> Se colisoesParede = True, existe colisão. 
prop> Se colisoesParede = False, não existe colisão.
prop> Basta que uma das funções auxiliares verificarem o valor __True__ (note que existe a negação da segunda função!) para existir uma colisão. 
-}

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa _ _ blocos) jogador = hitBoxColideBlocos (hitbox_jogador) (hitBoxPlataforma blocos 0) || not(hitBoxDentroLimites hitbox_jogador blocos)
  where
    hitbox_jogador = hitBoxPersonagem jogador

{- | A função 'colisoesPersonagens' analisa se um Personagem está a colidir (ou não) com outro

@
colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens personagem1 personagem2 =  hitBoxColide (hitbox_personagem1) (hitbox_personagem2)
                                        where hitbox_personagem1 = hitBoxPersonagem (personagem1)
                                              hitbox_personagem2 = hitBoxPersonagem (personagem2)
@

== Propriedades:
prop> Se colisoesPersonagens = True, existe colisão. 
prop> Se colisoesPersonagens = False, não existe colisão.
-}


colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens personagem1 personagem2 =  hitBoxColide (hitbox_personagem1) (hitbox_personagem2)
                                        where hitbox_personagem1 = hitBoxPersonagem (personagem1)
                                              hitbox_personagem2 = hitBoxPersonagem (personagem2)



-- *Funções Auxiliares

-- ** Função 'hitBoxDentroLimites' 
{- | A função 'hitBoxDentroLimites' recebe uma hitbox e a matriz de blocos e analisa se a hitbox está ou não dentro dos limites do mapa.
É utilizada a função 'fromIntegral' pois a coordenadas da hitbox são do tipo Double, logo é necessário fazer uma conversão!

@
hitBoxDentroLimites :: Hitbox -> [[Bloco]] -> Bool
hitBoxDentroLimites ((x1,y1),(x2,y2)) blocos = x1 > 0 && y2 > 0 && x2 < larg && y2 < alt
  where larg = fromIntegral(length(head blocos))
        alt = fromIntegral(length blocos)
@
-}

hitBoxDentroLimites :: Hitbox -> [[Bloco]] -> Bool
hitBoxDentroLimites ((x1,y1),(x2,y2)) blocos = x1 > 0 && y2 > 0 && x2 < larg && y2 < alt
  where larg = fromIntegral(length(head blocos))
        alt = fromIntegral(length blocos)

-- ** Função 'hitBoxPersonagem' 
{- | A função 'hitBoxPersonagem' recebe um Personagem e calcula a respetiva Hitbox

@
hitBoxPersonagem :: Personagem -> Hitbox 
hitBoxPersonagem personagem = (((x_personagem - x_hitbox),(y_personagem + y_hitbox)), (x_personagem + x_hitbox, y_personagem - y_hitbox))
  where 
    x_personagem = fst (posicao personagem)
    y_personagem = snd (posicao personagem)
    x_hitbox = fst (tamanho personagem) / 2
    y_hitbox = snd (tamanho personagem) / 2
@
-}


hitBoxPersonagem :: Personagem -> Hitbox 
hitBoxPersonagem personagem = (((x_personagem - x_hitbox),(y_personagem + y_hitbox)), (x_personagem + x_hitbox, y_personagem - y_hitbox))
  where 
    x_personagem = fst (posicao personagem)
    y_personagem = snd (posicao personagem)
    x_hitbox = fst (tamanho personagem) / 2
    y_hitbox = snd (tamanho personagem) / 2

-- ** Função 'hitBoxColide' 
{- | A função 'hitBoxColide' recebe duas hitboxes e verifica se existe colisão (ou não) através de uma comparação das coordenadas que caracaterizam cada uma das hitboxes

@
hitBoxColide :: Hitbox -> Hitbox -> Bool
hitBoxColide ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  not (x2 < x3 || x4 < x1 || y2 > y3 || y4 > y1)
@
-}

hitBoxColide :: Hitbox -> Hitbox -> Bool
hitBoxColide ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  not (x2 < x3 || x4 < x1 || y2 > y3 || y4 > y1)

-- ** Função 'hitBoxColideBlocos' 
{- | A função 'hitBoxColideBlocos' recebe uma Hitbox e uma lista do tipo Hitbox e retorna um __Bool__
Utiliza a função de ordem superior para verificar se existe alguma colisão!

@
hitBoxColideBlocos :: Hitbox -> [Hitbox] -> Bool
hitBoxColideBlocos h1 h_list = any (hitBoxColide h1) h_list
@
-}

hitBoxColideBlocos :: Hitbox -> [Hitbox] -> Bool
hitBoxColideBlocos h1 h_list = any (hitBoxColide h1) h_list

-- ** Função 'hitBoxPlataforma' 
{- | A função 'hitBoxPlataforma' recebe uma matriz de Blocos, um Double (correspondente à linha em análise) e retorna um __Bool__

@
hitBoxPlataforma :: [[Bloco]] -> Double -> [Hitbox]
hitBoxPlataforma [] _ = []
hitBoxPlataforma (h:t) linha = hitBoxPlataformaAux 0 linha h ++ hitBoxPlataforma t (linha+1) 
@
-}

hitBoxPlataforma :: [[Bloco]] -> Double -> [Hitbox]
hitBoxPlataforma [] _ = []
hitBoxPlataforma (h:t) linha = hitBoxPlataformaAux 0 linha h ++ hitBoxPlataforma t (linha+1) 

-- ** Função 'hitBoxPlataformaAux' 
{- | A função 'hitBoxPlataformaAux' recebe dois Doubles (correspondentes às coordenadas), um lista de Blocos (uma linha da matriz) e retorna uma lista contendo as hitboxes dos blocos do tipo Plataforma daquela linha

@
hitBoxPlataformaAux :: Double -> Double -> [Bloco] -> [Hitbox]
hitBoxPlataformaAux _ _ [] = []
hitBoxPlataformaAux x y (h:t) = if h == Plataforma then ((x,y+1),(x+1,y)) : hitBoxPlataformaAux (x+1) y t else hitBoxPlataformaAux (x+1) y t
@
-}

hitBoxPlataformaAux :: Double -> Double -> [Bloco] -> [Hitbox]
hitBoxPlataformaAux _ _ [] = []
hitBoxPlataformaAux x y (h:t) = if h == Plataforma then ((x,y+1),(x+1,y)) : hitBoxPlataformaAux (x+1) y t else hitBoxPlataformaAux (x+1) y t