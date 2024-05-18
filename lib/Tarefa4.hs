{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : João Pedro Ribeiro de Sá <a104612@alunos.uminho.pt>
              Hélder Tiago Peixoto da Cruz <a104174@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324


{-
A função principal /atualiza/ lida com a atualização das velocidades e direções dos personagens, tanto inimigos quanto do jogador, com base nas ações fornecidas.
Para a realização desta função foram criadas duas grandes funções auxiliares, __aplicaAcaoInimigo__ e __aplicarAcaoJogador__, que como o próprio nome diz, aplicam a ação às personagens específicas.

@
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesInimigos acaoJogador jogo =
    let
        inimigosAtualizados = zipWith aplicarAcaoInimigo (repeat acaoJogador) (inimigos jogo)
        jogadorAtualizado = aplicarAcaoJogador acaoJogador jogo (jogador jogo)
    in
        jogo { inimigos = inimigosAtualizados, jogador = jogadorAtualizado }
@

-}

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesInimigos acaoJogador jogo =
    let
        -- Esta linha tem como objetivo atualizar os inimigos, tendo em conta o seu movimento, com a ajuda da função auxiliar /aplicarAcaoInimigo/. O movimento dos inimigos é idêntico ao do personagem, pois se o jogador andar para a esquerda, o inimigo fará o mesmo, e vice versa.
        inimigosAtualizados = zipWith aplicarAcaoInimigo (repeat acaoJogador) (inimigos jogo)
        -- Do mesmo modo esta linha atualizará o jogador, com a ajuda da função auxiliar /aplicarAcaoJogador/, que mais para a frente será explicada
        jogadorAtualizado = aplicarAcaoJogador acaoJogador jogo (jogador jogo)
    in
        jogo { inimigos = inimigosAtualizados, jogador = jogadorAtualizado }

{- 
A função auxiliar /aplicarAcaoInimigo/ como o próprio nome diz vai aplicar uma ação ao Inimigo, sendo que a ação escolhida para os mesmos foi movimentarem-se tanto para a esquerda como para a direita.
Esta função vai verificar se foi passada uma ação para o inimigo.
Caso não tenha passado nenhuma ação (__Nothing__), o inimigo permanecerá inalterado. 
Caso contrário (__Just acao__), ele chama a função /moverInimigo/ para atualizar a posição do inimigo com base na ação especificada.

@
aplicarAcaoInimigo :: Maybe Acao -> Personagem -> Personagem
aplicarAcaoInimigo maybeAcao inimigo =
    case maybeAcao of
        Just acao -> moverInimigo acao inimigo
        Nothing   -> inimigo
@

/moverInimigo/ considera apenas movimentos laterais, atualizando o movimento ou para a esquerda, ou para a direita.
@
moverInimigo :: Acao -> Personagem -> Personagem
moverInimigo acao inimigo =
    case acao of
        AndarDireita -> inimigo { posicao = atualizarPosicao (posicao inimigo) 1 }
        AndarEsquerda -> inimigo { posicao = atualizarPosicao (posicao inimigo) (-1) }
        _ -> inimigo

A /atualizarPosicao/ é uma função auxiliar simples, que tem como objetivo modificar o valor x (negativo ou positivo tendo em conta o dx recebido na /moverInimigo/).
@
atualizarPosicao :: Posicao -> Int -> Posicao
atualizarPosicao (x, y) dx = (x + fromIntegral dx, y)
@

-}

-- Aplica a ação ao inimigo, atualizando sua posição se necessário.
aplicarAcaoInimigo :: Maybe Acao -> Personagem -> Personagem
aplicarAcaoInimigo maybeAcao inimigo =
    case maybeAcao of
        Just acao -> moverInimigo acao inimigo  -- Se uma ação foi fornecida, move o inimigo de acordo.
        Nothing   -> inimigo  -- Se nenhuma ação foi fornecida, mantém o inimigo como está.

-- Determina o movimento do inimigo com base na ação fornecida.
moverInimigo :: Acao -> Personagem -> Personagem
moverInimigo acao inimigo =
    case acao of
        AndarDireita -> inimigo { posicao = atualizarPosicao (posicao inimigo) 1 }  -- Move o inimigo para a direita.
        AndarEsquerda -> inimigo { posicao = atualizarPosicao (posicao inimigo) (-1) }  -- Move o inimigo para a esquerda.
        _ -> inimigo  -- Para outras ações, não altera a posição do inimigo.

-- Atualiza a posição x do inimigo com base no deslocamento dado.
atualizarPosicao :: Posicao -> Int -> Posicao
atualizarPosicao (x, y) dx = (x + fromIntegral dx, y)  -- Adiciona o deslocamento ao x e mantém y inalterado.



{- 
Como dito anteriormente, a função /atualiza/ foi dividida em duas funções auxiliares, senda esta a /aplicarAcaoJogador/ que,novamente, como o nome diz, vai atribuir um movimento ao personagem principal. 
Esta função vai receber uma ação, um mapa e uma personagem, e devolverá essa mesma personagem atualizada com a devida ação desejada pelo jogador.

@
aplicarAcaoJogador :: Maybe Acao -> Jogo -> Personagem -> Personagem
aplicarAcaoJogador maybeAcao jogo jogador =
  case maybeAcao of
    Just Subir -> if estaNaEscada jogador (mapa jogo) then subir jogador else saltar jogador
    Just Descer -> if estaNaEscada jogador (mapa jogo) then descer jogador else parar jogador
    Just AndarDireita -> andarDireita jogador
    Just AndarEsquerda -> andarEsquerda jogador
    Just Saltar -> saltar jogador
    Just Parar -> parar jogador
    Nothing -> jogador 
  where
    subir jogador = jogador { velocidade = (0, -1), emEscada = True }  
    descer jogador = jogador { velocidade = (0, 1), emEscada = True }
    andarDireita jogador = jogador { velocidade = (1, 0), direcao = Este, emEscada = False }  
    andarEsquerda jogador = jogador { velocidade = (-1, 0), direcao = Oeste, emEscada = False }
    saltar jogador = 
      if not (emEscada jogador) 
        then jogador { velocidade = (vx, vy + 2), emEscada = False } 
        else subir jogador
    parar jogador = jogador { velocidade = (0, 0), emEscada = False }

    (vx, vy) = velocidade jogador
@

-}

-- Aplica a ação ao jogador, atualizando a sua velocidade e direção
aplicarAcaoJogador :: Maybe Acao -> Jogo -> Personagem -> Personagem
aplicarAcaoJogador maybeAcao jogo jogador =
  case maybeAcao of
    Just Subir -> if estaNaEscada jogador (mapa jogo) then subir jogador else saltar jogador
    Just Descer -> if estaNaEscada jogador (mapa jogo) then descer jogador else parar jogador
    Just AndarDireita -> andarDireita jogador
    Just AndarEsquerda -> andarEsquerda jogador
    Just Saltar -> saltar jogador
    Just Parar -> parar jogador
    Nothing -> jogador  -- Mantém o movimento atual (inércia)
  where
    -- Funções para processar cada ação
    subir jogador = jogador { velocidade = (0, -1), emEscada = True }  
    descer jogador = jogador { velocidade = (0, 1), emEscada = True }
    andarDireita jogador = jogador { velocidade = (1, 0), direcao = Este, emEscada = False }  
    andarEsquerda jogador = jogador { velocidade = (-1, 0), direcao = Oeste, emEscada = False }
    saltar jogador = 
      if not (emEscada jogador) 
        then jogador { velocidade = (vx, vy + 2), emEscada = False } 
        else subir jogador
    parar jogador = jogador { velocidade = (0, 0), emEscada = False }

    (vx, vy) = velocidade jogador

{- 
A função auxiliar /estaNaEscada/ tem como objetivo verificar se o personagem principal está ou não perante uma escada.
Com a ajuda desta função, é facilitada a definição na função /aplicarAcaoJogador/ no caso de subir o personagem, pois este apenas subirá caso esta função auxiliar devolva um True.

@
estaNaEscada :: Personagem -> Mapa -> Bool
estaNaEscada personagem (Mapa _ _ blocos) =
  let
    (x, y) = posicao personagem
    (indiceX, indiceY) = posicaoParaIndice (x, y)

    blocoNaPosicao = if indiceY >= 0 && indiceY < length blocos && indiceX >= 0 && indiceX < length (head blocos)
                     then (blocos !! indiceY) !! indiceX
                     else Vazio
  in
    blocoNaPosicao == Escada
@

Foi também criada a função /posicaoParaIndice/ que tal como o próprio nome diz, tem como objetivo tornar a posição do personagem (x,y) em índices de matriz correspondentes, pois o mapa é representado internamente por uma matriz.

@
posicaoParaIndice :: Posicao -> (Int, Int)
posicaoParaIndice (x, y) = (floor x, floor y)
@

-}

estaNaEscada :: Personagem -> Mapa -> Bool
estaNaEscada personagem (Mapa _ _ blocos) =
  let
    (x, y) = posicao personagem
    (indiceX, indiceY) = posicaoParaIndice (x, y) -- O mapa é representado internamente por uma matriz, logo é necessário converter as coordenadas (x, y) para índices de matriz correspondentes.

    -- Obtém o bloco na posição do personagem, para posteriomente poder verificar se é uma escada ou não.
    -- Primeiro verifica se a personagem se encontra dentro do mapa, verificando que a coordenada Y não deve ser negativa (indiceY >= 0) e deve ser menor que o número total de linhas no mapa (length blocos). O mesmo acontece para o indiceX.
    -- Se essa condição se verificar, então obtém-se o bloco onde o personagem efetivamente está.
    blocoNaPosicao = if indiceY >= 0 && indiceY < length blocos && indiceX >= 0 && indiceX < length (head blocos) 
                     then (blocos !! indiceY) !! indiceX
                     else Vazio  -- Fora do mapa é considerado como vazio
  in
    blocoNaPosicao == Escada

-- Converte uma posição no mapa para índices de matriz
posicaoParaIndice :: Posicao -> (Int, Int)
posicaoParaIndice (x, y) = (floor x, floor y)



