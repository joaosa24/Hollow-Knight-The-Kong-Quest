{-| 
Module      : Utils
Description : O objetivo deste Módulo é definir vários funções para auxiliar a construção da parte gŕafica do projeto
Copyright   : João Pedro Ribeiro de Sá <a104612@alunos.uminho.pt>
              Hélder Tiago Peixoto da Cruz <a104174@alunos.uminho.pt>
-}

module Utils where 

import LI12324
import Niveis

------------------------------------------------------------------------------------------------------------

-- | Função que recebe uma matriz de blocos e devolve a altura do Mapa correspondente
getAlturaMapa :: [[Bloco]] -> Double 
getAlturaMapa blocos = fromIntegral(length blocos)

-- | Função que recebe uma matriz de blocos e devolve a largura do Mapa correspondente
getLarguraMapa :: [[Bloco]] -> Double 
getLarguraMapa (h:t) = fromIntegral(length h)

-- | Função que recebe um estado e devolve a Matriz de Blocos correspondente
getMatrix :: Estado -> [[Bloco]]
getMatrix (Estado (Jogo (Mapa _ _ blocos) _ _ _)) = blocos

-- | Função que recebe um estado e devolve o Mapa correspondente
getMapa :: Estado -> Mapa
getMapa (Estado jogo) = (mapa jogo)

-- | Função que verifica se o Jogador está na posição da Estrela
hasStar :: Posicao -> Posicao -> Bool 
hasStar (x_jogador,y_jogador) (x_estrela,y_estrela) = (x_jogador == x_estrela && y_jogador == y_estrela)

-- | Função que recebe um Estado e devolve o Jogador
getJogador :: Estado -> Personagem
getJogador (Estado (Jogo _ _ _ j)) = j 

-- | Função que verifica se um inimigo é do tipo "Fantasma"
isFantasma :: Personagem -> Bool
isFantasma enemy = tipo enemy == Fantasma

-- | Função que recebe um estado e devolve a lista de inimigos do tipo "Fantasma"
getFantasma :: Estado -> [Personagem]
getFantasma (Estado (Jogo _ inimigos _ _)) = filter isFantasma inimigos

-- | Função que verifica se um inimigo é do tipo MacacoMalvado
isMacaco :: Personagem -> Bool
isMacaco enemy = tipo enemy == MacacoMalvado

-- | Função que recebe um estado e devolve o inimigo do tipo "MacacoMalvado"
getMacaco :: Estado -> Personagem
getMacaco (Estado (Jogo _ inimigos _ _)) = head (filter isMacaco inimigos)

-- | Função que determina se o jogador está a colidir com algum personagem
bateNoJogador :: Personagem -> [Personagem] -> Bool
bateNoJogador jogador [] = False 
bateNoJogador jogador (h:t) = (posicao jogador == posicao h ) || bateNoJogador jogador t