{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : João Pedro Ribeiro de Sá <a104612@alunos.uminho.pt>
              Hélder Tiago Peixoto da Cruz <a104174@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324

import System.Random (mkStdGen, randomR)

{-
A função movimenta é a principal da Tarefa 3 e é responsável por atualizar o estado do jogo a cada quadro, levando em conta vários aspetos como gravidade, movimento dos inimigos, ações do jogador, vida dos inimigos e colecionáveis.
Para a definição desta função foram criadas várias funções auxiliares (umas mais complexas do que outras), de modo a organizar de melhor forma o código, como para facilitar a deteção de erros e etc.
As funções que agora referimos serão todas explicadas ao longo do código.

@
movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta seed time jogo =
  let
    jogoAplicadoGravidade = jogoComGravidade time jogo
    
    inimigosAtualizados = map (movimentaInimigo seed time (mapa jogoAplicadoGravidade)) (inimigos jogoAplicadoGravidade)
    jogoComMovimentoInimigos = jogoAplicadoGravidade { inimigos = inimigosAtualizados }
    
    (jogadorAtualizado, mapaAtualizado) = movimentaJogador time (mapa jogoComMovimentoInimigos) (jogador jogoComMovimentoInimigos)
    jogoComMovimentoJogador = jogoComMovimentoInimigos { jogador = jogadorAtualizado, mapa = mapaAtualizado }
    
    jogoComVidas = atualizaVidas jogoComMovimentoJogador
    
    jogoAtualizado = atualizaColecionaveis time jogoComVidas
  in
    jogoAtualizado
@
-}
movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta seed time jogo =
  let
    -- Aplica a gravidade a todos os personagens no jogo (jogador e inimigos).
    jogoAplicadoGravidade = jogoComGravidade time jogo
    
    -- Atualiza os inimigos com base na lógica de movimento, incluindo aleatoriedade.
    inimigosAtualizados = map (movimentaInimigo seed time (mapa jogoAplicadoGravidade)) (inimigos jogoAplicadoGravidade)
    jogoComMovimentoInimigos = jogoAplicadoGravidade { inimigos = inimigosAtualizados }
    
    -- Atualiza o jogador com base nos controlos do utilizador e outras lógicas, incluindo a verificação de alçapões.
    (jogadorAtualizado, mapaAtualizado) = movimentaJogador time (mapa jogoComMovimentoInimigos) (jogador jogoComMovimentoInimigos)
    jogoComMovimentoJogador = jogoComMovimentoInimigos { jogador = jogadorAtualizado, mapa = mapaAtualizado }
    
    -- Aplica as regras de vida para cada inimigo.
    jogoComVidas = atualizaVidas jogoComMovimentoJogador
    
    -- Atualiza os colecionáveis com base na lógica de coleta.
    jogoAtualizado = atualizaColecionaveis time jogoComVidas
  in
    jogoAtualizado


{-
A primeira função auxiliar que decidimos abordar verifica se o personagem que o jogador controla está sobre uma plataforma ou não.
A função "estaSobrePlataforma" vai ser bastante útil para quando tivermos de aplicar a função de gravidade, pois caso esta função devolva um False, sabemos que o personagem deverá cair.

@
estaSobrePlataforma :: Personagem -> Mapa -> Bool
estaSobrePlataforma personagem (Mapa _ _ blocos) =
  let (x, y) = posicao personagem
      (largura, altura) = tamanho personagem
      hitboxInferior = (x, y - altura / 2) 

      (indiceX, indiceY) = posicaoParaIndice hitboxInferior

      blocoAbaixo = (blocos !! indiceY) !! indiceX
  in blocoAbaixo == Plataforma
@

-}
-- Verifica se o personagem está sobre uma plataforma
estaSobrePlataforma :: Personagem -> Mapa -> Bool
estaSobrePlataforma personagem (Mapa _ _ blocos) =
  let (x, y) = posicao personagem
      (largura, altura) = tamanho personagem
      hitboxInferior = (x, y - altura / 2)  -- Parte inferior da hitbox do personagem, para verificar se o personagem está em contato com uma plataforma.

      -- Converte a posição do personagem para índices na matriz de blocos
      (indiceX, indiceY) = posicaoParaIndice hitboxInferior

      -- Verifica se a posição abaixo do personagem contém um bloco de plataforma
      blocoAbaixo = (blocos !! indiceY) !! indiceX
  in blocoAbaixo == Plataforma

-- Converte uma posição no mapa para índices de matriz, utilizando o "floor" para arredondar os valores de x e y para os numeros inteiros mais próximos em direção a zero.
posicaoParaIndice :: Posicao -> (Int, Int)
posicaoParaIndice (x, y) = (floor x, floor y)

{-

Como falado anteriormente, é agora necessário aplicar as funções que darão gravidade ao nosso personagem.
Entre elas temos :
- a função /aplicaGravidadePersonagem/ (que como o próprio nome diz, aplica gravidade a um único personagem).
- a função /jogoComGravidade/ (que itera a função anterior por todos os personagens do mapa "jogador ou inimigos" para que todos sejam afetados pela gravidade.)

@
aplicaGravidadePersonagem :: Tempo -> Mapa -> Personagem -> Personagem
aplicaGravidadePersonagem time mapa personagem =
  if not (estaSobrePlataforma personagem mapa)
    then let (vx, vy) = velocidade personagem
             (gx, gy) = gravidade
         in personagem { posicao = (x, y - gy * time), velocidade = (vx, vy - gy * time) }
    else personagem
  where (x, y) = posicao personagem
@

@
jogoComGravidade :: Tempo -> Jogo -> Jogo
jogoComGravidade time jogo =
  let
    jogadorComGravidade = aplicaGravidadePersonagem time (mapa jogo) (jogador jogo)
    
    inimigosComGravidade = map (aplicaGravidadePersonagem time (mapa jogo)) (inimigos jogo)
  in
    jogo { 
      jogador = jogadorComGravidade, 
      inimigos = inimigosComGravidade 
    }
@


-}
-- Aplica a gravidade a um único personagem
aplicaGravidadePersonagem :: Tempo -> Mapa -> Personagem -> Personagem
aplicaGravidadePersonagem time mapa personagem =
  if not (estaSobrePlataforma personagem mapa)
    then let (vx, vy) = velocidade personagem
             (gx, gy) = gravidade
         in personagem { posicao = (x, y - gy * time), velocidade = (vx, vy - gy * time) }
    else personagem
  where (x, y) = posicao personagem

-- Aplica a gravidade a todos os personagens no jogo
jogoComGravidade :: Tempo -> Jogo -> Jogo
jogoComGravidade time jogo =
  let
    -- Aplica a gravidade ao jogador
    jogadorComGravidade = aplicaGravidadePersonagem time (mapa jogo) (jogador jogo)
    
    -- Aplica a gravidade a cada inimigo
    inimigosComGravidade = map (aplicaGravidadePersonagem time (mapa jogo)) (inimigos jogo)
  in
    jogo { 
      jogador = jogadorComGravidade, 
      inimigos = inimigosComGravidade 
    }

{-
Outra função bastante importante para a implementação da função principal __movimenta__, é a função auxiliar /movimentaInimigo/.
Esta função é responsável por atualizar a posição e a velocidade de um inimigo no jogo, considerando o movimento aleatório e as regras de colisão. 

@
movimentaInimigo :: Semente -> Tempo -> Mapa -> Personagem -> Personagem
movimentaInimigo seed time mapa inimigo =
  case tipo inimigo of
    MacacoMalvado -> inimigo
    _ -> 
      let
        gen = mkStdGen seed
        (deltaX, _) = randomR (-1, 1) gen 
        novaVelocidade = if deveRessaltar inimigo mapa then (-vx) else vx + deltaX
      in
        if deveRessaltar inimigo mapa
          then inimigo { velocidade = (novaVelocidade, vy) }  -- Inverte a direção
          else inimigo { posicao = (x + novaVelocidade * time, y), velocidade = (novaVelocidade, vy) }
  where
    (x, y) = posicao inimigo
    (vx, vy) = velocidade inimigo
@
-}
-- A função 'movimentaInimigo' deve considerar a semente para aleatoriedade no movimento dos inimigos
movimentaInimigo :: Semente -> Tempo -> Mapa -> Personagem -> Personagem
movimentaInimigo seed time mapa inimigo =
  case tipo inimigo of
    MacacoMalvado -> inimigo  -- O Macaco Malvado não se move
    _ -> 
      let
        gen = mkStdGen seed
        (deltaX, _) = randomR (-1, 1) gen  -- Gera um delta para o movimento em x
        novaVelocidade = if deveRessaltar inimigo mapa then (-vx) else vx + deltaX
      in
        if deveRessaltar inimigo mapa
          then inimigo { velocidade = (novaVelocidade, vy) }  -- Inverte a direção
          else inimigo { posicao = (x + novaVelocidade * time, y), velocidade = (novaVelocidade, vy) }
  where
    (x, y) = posicao inimigo
    (vx, vy) = velocidade inimigo

{- 
A função /deveRessaltar/ é crucial para o movimento do inimigo pois se este se encontrar no limite do mapa ou no limite de uma plataforma, este deve invertir o seu movimento no sentido oposto.
Esta função permite que os inimigos nunca "caim".

@
    deveRessaltar :: Personagem -> Mapa -> Bool
    deveRessaltar inimigo (Mapa _ _ blocos) =
     let
      (x, y) = posicao inimigo
      (largura, altura) = tamanho inimigo
      (vx, vy) = velocidade inimigo

      proximaPosicao = (x + vx, y + vy)

      (indiceX, indiceY) = posicaoParaIndice proximaPosicao

      dentroDoMapa = indiceX >= 0 && indiceX < length (head blocos) && indiceY >= 0 && indiceY < length blocos

      choqueComParedeOuFimPlataforma
        | dentroDoMapa = let bloco = (blocos !! indiceY) !! indiceX in bloco == Plataforma || bloco == Vazio
        | otherwise = True
     in
      choqueComParedeOuFimPlataforma
@
-}

    -- Verifica se o inimigo deve ressaltar (inverter o movimento) devido ao limite do mapa ou da plataforma
    deveRessaltar :: Personagem -> Mapa -> Bool
    deveRessaltar inimigo (Mapa _ _ blocos) =
     let
      (x, y) = posicao inimigo
      (largura, altura) = tamanho inimigo
      (vx, vy) = velocidade inimigo

      -- Calcula a próxima posição do inimigo
      proximaPosicao = (x + vx, y + vy)

      -- Converte a próxima posição para índices na matriz de blocos
      (indiceX, indiceY) = posicaoParaIndice proximaPosicao

      -- Verifica se a próxima posição está dentro dos limites do mapa
      dentroDoMapa = indiceX >= 0 && indiceX < length (head blocos) && indiceY >= 0 && indiceY < length blocos

      -- Verifica se a próxima posição é uma parede ou o fim de uma plataforma
      choqueComParedeOuFimPlataforma
        | dentroDoMapa = let bloco = (blocos !! indiceY) !! indiceX in bloco == Plataforma || bloco == Vazio -- Se for um destes blocos, o inimigo ou está perante uma parede ou vai cair.
        | otherwise = True
     in
      choqueComParedeOuFimPlataforma
{-
Uma das funções principais para a definição de movimento no jogo é a /movimentaJogador/.
Esta vai ter em conta todos os tipos de movimentos que o personagem principal pode tomar, bem como as suas excessões/alterações que possam ocorrer durante o jogo.
Alterações estas que tanto podem influenciar o mapa (alçapões) como ao próprio personagem (colisão com paredes).

@
movimentaJogador :: Tempo -> Mapa -> Personagem -> (Personagem, Mapa)
movimentaJogador time mapa jogador =
  let
    (x, y) = posicao jogador
    (vx, vy) = velocidade jogador

    proximaPosicao = (x + vx * time, y + vy * time)

    colideComParede = colisaoComParede proximaPosicao mapa

    (novoJogador, novoMapa) =
      if colideComParede
        then (jogador, mapa)
        else let jogadorAtualizado = jogador { posicao = proximaPosicao }
                 mapaAtualizado = atualizaAlcapao jogadorAtualizado mapa
             in (jogadorAtualizado, mapaAtualizado)

  in
    (novoJogador, novoMapa)
@


-}

-- Atualiza a posição do jogador e outras propriedades, incluindo a verificação de alçapões
movimentaJogador :: Tempo -> Mapa -> Personagem -> (Personagem, Mapa)
movimentaJogador time mapa jogador =
  let
    (x, y) = posicao jogador
    (vx, vy) = velocidade jogador

    -- Calcula a próxima posição do jogador
    proximaPosicao = (x + vx * time, y + vy * time)

    -- Verifica se a próxima posição colide com uma parede
    colideComParede = colisaoComParede proximaPosicao mapa

    -- Atualiza a posição do jogador e o mapa se não houver colisão
    (novoJogador, novoMapa) =
      if colideComParede
        then (jogador, mapa)  -- Mantém a posição atual e o mapa se houver colisão
        else let jogadorAtualizado = jogador { posicao = proximaPosicao }
                 mapaAtualizado = atualizaAlcapao jogadorAtualizado mapa
             in (jogadorAtualizado, mapaAtualizado)

  in
    (novoJogador, novoMapa)

{-
A função colisaoComParede é utilizada para determinar se uma posição específica no jogo está imediatamente ao lado de uma parede (plataforma). 

@
colisaoComParede :: Posicao -> Mapa -> Bool
colisaoComParede (x, y) (Mapa _ _ blocos) =
  let
    indiceX = floor x
    indiceY = floor y
    blocoEsquerda = if indiceX > 0 then (blocos !! indiceY) !! (indiceX - 1) else Vazio
    blocoDireita = if indiceX < (length (head blocos) - 1) then (blocos !! indiceY) !! (indiceX + 1) else Vazio
  in
    blocoEsquerda == Plataforma || blocoDireita == Plataforma
@

-}
-- Verifica se há colisão com uma parede
colisaoComParede :: Posicao -> Mapa -> Bool
colisaoComParede (x, y) (Mapa _ _ blocos) =
  let
    indiceX = floor x
    indiceY = floor y

    -- Obtém os blocos à esquerda e à direita do jogador
    blocoEsquerda = if indiceX > 0 then (blocos !! indiceY) !! (indiceX - 1) else Vazio
    -- Se o índice "indiceX" for maior que 0 (o que significa que não estamos no limite esquerdo do mapa), verifica-se o bloco à esquerda (indiceX - 1). Se estivermos no limite esquerdo, considera-se que não há bloco (Vazio).
    blocoDireita = if indiceX < (length (head blocos) - 1) then (blocos !! indiceY) !! (indiceX + 1) else Vazio
    -- Se o índice "indiceX" for menor que o número de colunas no mapa menos 1 (length (head blocos) - 1), verifica-se o bloco à direita (indiceX + 1). Se estivermos no limite direito, considera-se que não há bloco (Vazio).

  in
    blocoEsquerda == Plataforma || blocoDireita == Plataforma -- Caso seja uma Plataforma, a função retorna True.

{-
A função /atualizaAlcapao/ é responsável por atualizar o mapa do jogo quando um jogador passa por cima de um alçapão.

@
atualizaAlcapao :: Personagem -> Mapa -> Mapa
atualizaAlcapao jogador (Mapa inicioPosicao estrela blocos) =
  let
    (x, y) = posicao jogador
    (indiceX, indiceY) = posicaoParaIndice (x, y)
    blocoAtual = (blocos !! indiceY) !! indiceX
  in
    if blocoAtual == Alcapao
      then let
             novosBlocos = atualizaMatrizBlocos indiceX indiceY Vazio blocos
           in Mapa inicioPosicao estrela novosBlocos
      else Mapa inicioPosicao estrela blocos
@

-}
-- Verifica e atualiza alçapões no mapa
atualizaAlcapao :: Personagem -> Mapa -> Mapa
atualizaAlcapao jogador (Mapa inicioPosicao estrela blocos) =
  let
    (x, y) = posicao jogador
    (indiceX, indiceY) = posicaoParaIndice (x, y)
    blocoAtual = (blocos !! indiceY) !! indiceX -- Identifica o tipo de bloco onde o jogador está atualmente.
  in
    if blocoAtual == Alcapao
      then let
             novosBlocos = atualizaMatrizBlocos indiceX indiceY Vazio blocos -- Uso da função auxiliar "atualizaMatrizBlocos" para trocar Alçapão por Vazio
           in Mapa inicioPosicao estrela novosBlocos
      else Mapa inicioPosicao estrela blocos


-- Atualiza a matriz de blocos no mapa, substituindo um bloco específico
atualizaMatrizBlocos :: Int -> Int -> Bloco -> [[Bloco]] -> [[Bloco]]
atualizaMatrizBlocos x y novoBloco blocos =
  let
    (linhasAntes, linhaAlvo:linhasDepois) = splitAt y blocos
    -- "linhaAlvo" é a linha onde o bloco será substituído, enquanto "linhasAntes" e "linhasDepois" representam as partes da matriz acima e abaixo da linha alvo, respectivamente.
    (blocosAntes, _:blocosDepois) = splitAt x linhaAlvo
    -- Divide a "linhaAlvo" na coluna x, separando os blocos à esquerda ("blocosAntes") do bloco alvo e os blocos à direita ("blocosDepois"). O bloco alvo é representado por "_", indicando que este será substituído.
    novaLinha = blocosAntes ++ [novoBloco] ++ blocosDepois
    -- Cria uma nova linha substituindo o bloco alvo pelo "novoBloco" (no caso será o "Vazio").
  in
    linhasAntes ++ [novaLinha] ++ linhasDepois
    -- A nova matriz de blocos é formada pela concatenação das "linhasAntes", da "novaLinha" (com o bloco atualizado) e das "linhasDepois".

{-

/atualizaColecionaveis/ tem como objetivo atualizar a lista de colecionáveis no jogo depois de verificar quais é que foram efetivamente coletados pelo jogador.
Dentro desta função, são ainda incorporadas duas outras (/processarColecionavel/ e /jogadorColetou/), que de seguida serão explicadas.

@
atualizaColecionaveis :: Tempo -> Jogo -> Jogo
atualizaColecionaveis time jogo =
  let
    jogadorAtual = jogador jogo
    colecionaveisAtualizados = filter (not . jogadorColetou jogadorAtual) (colecionaveis jogo)
    jogadorComColecionaveis = foldl processarColecionavel jogadorAtual (colecionaveis jogo)
  in
    jogo { colecionaveis = colecionaveisAtualizados, jogador = jogadorComColecionaveis }
@

-}

-- Atualiza o estado dos colecionáveis no jogo
atualizaColecionaveis :: Tempo -> Jogo -> Jogo
atualizaColecionaveis time jogo =
  let
    jogadorAtual = jogador jogo
    colecionaveisAtualizados = filter (not . jogadorColetou jogadorAtual) (colecionaveis jogo)
    -- Esta linha cria uma nova lista de colecionáveis que ainda estão no jogo
    -- "filter" para criar uma nova lista de colecionáveis, removendo aqueles que o jogador já coletou.
    -- "not . jogadorColetou jogadorAtual" significa que estamos a manter na lista apenas os colecionáveis que o jogador ainda não coletou."
    jogadorComColecionaveis = foldl processarColecionavel jogadorAtual (colecionaveis jogo)
    -- "foldl" para aplicar a função "processarColecionavel" a cada colecionável na lista, atualizando o jogador conforme necessário. 
  in
    jogo { colecionaveis = colecionaveisAtualizados, jogador = jogadorComColecionaveis }

{-
A função /processarColecionavel/ tem como objetivo atualizar o estado do jogador com base no tipo de colecionável coletado.
Se se verificar que um jogador coletou um colecionavel (através da /jogadorColetou/), dependendo de qual deles é (martelo ou moeda), terá o seu efeito específico (no caso dano ou aumento de pontuação).

@
processarColecionavel :: Personagem -> (Colecionavel, Posicao) -> Personagem
processarColecionavel jogador colec@(colecionavel, _) =
  if jogadorColetou jogador colec
    then case colecionavel of
           Martelo -> jogador { aplicaDano = (True, 10) }  
           Moeda -> jogador { pontos = pontos jogador + 1 }  
    else jogador
@

-}

-- Processa a coleta de um colecionável pelo jogador
processarColecionavel :: Personagem -> (Colecionavel, Posicao) -> Personagem
processarColecionavel jogador colec@(colecionavel, _) = -- colec passa a representar (colecionavel, _)
  if jogadorColetou jogador colec
    then case colecionavel of
           Martelo -> jogador { aplicaDano = (True, 10) }  -- Arma o jogador por 10 segundos
           Moeda -> jogador { pontos = pontos jogador + 1 }  -- Incrementa a pontuação
    else jogador

{-
A função /jogadorColetou/ simplesmente verifica, devolvendo um True ou False, a posição do jogador e a posição do colecional, verificando se são efetivamente iguais ou não.

@
jogadorColetou :: Personagem -> (Colecionavel, Posicao) -> Bool
jogadorColetou jogador (_, posicaoColecionavel) =
  posicao jogador == posicaoColecionavel
@

-}
-- Verifica se o jogador coletou um colecionável
jogadorColetou :: Personagem -> (Colecionavel, Posicao) -> Bool
jogadorColetou jogador (_, posicaoColecionavel) =
  posicao jogador == posicaoColecionavel

{-
A função /estaNaHitbox/ é usada para determinar se um inimigo está dentro da área de ataque (hitbox) do jogador. 
Ela é fundamental para a mecânica de combate do jogo, ajudando a decidir quando um inimigo é atingido pelo jogador.

@
estaNaHitbox :: Personagem -> Personagem -> Bool
estaNaHitbox jogador inimigo =
  let (jogadorX, jogadorY) = posicao jogador
      (dirX, _) = direcaoParaVetor (direcao jogador)
      (tamanhoJogadorX, tamanhoJogadorY) = tamanho jogador
      (inimigoX, inimigoY) = posicao inimigo
      hitboxX = jogadorX + dirX * tamanhoJogadorX 
  in (inimigoX >= hitboxX && inimigoX <= hitboxX + tamanhoJogadorX) &&
     (inimigoY >= jogadorY && inimigoY <= jogadorY + tamanhoJogadorY)
@

-}

-- Considerando que temos a posição do jogador e a direção para saber onde a hitbox deve estar.
-- A hitbox é uma área à frente do jogador baseada no seu tamanho.
estaNaHitbox :: Personagem -> Personagem -> Bool
estaNaHitbox jogador inimigo =
  let (jogadorX, jogadorY) = posicao jogador  -- Coordenadas atuais do jogador no jogo.
      (dirX, _) = direcaoParaVetor (direcao jogador) -- É o vetor direcional da direção do jogador, obtido pela função "direcaoParaVetor". Este vetor é usado para determinar a orientação da hitbox.
      (tamanhoJogadorX, tamanhoJogadorY) = tamanho jogador -- São as dimensões do jogador, utilizadas para calcular o tamanho da hitbox.
      (inimigoX, inimigoY) = posicao inimigo -- São as coordenadas atuais do inimigo no jogo.
      hitboxX = jogadorX + dirX * tamanhoJogadorX  -- O centro da hitbox move-se para a direção do jogador.
      -- Se o jogador estiver virado para a direita (Este), dirX será 1, e hitboxX será jogadorX + tamanhoJogadorX, colocando a hitbox à direita do jogador.
      -- Se o jogador estiver virado para a esquerda (Oeste), dirX será -1, e hitboxX será jogadorX - tamanhoJogadorX, colocando a hitbox à esquerda do jogador.

  in (inimigoX >= hitboxX && inimigoX <= hitboxX + tamanhoJogadorX) &&
     (inimigoY >= jogadorY && inimigoY <= jogadorY + tamanhoJogadorY)

{-
A função /direcaoParaVetor/ tem como objetivo, como o nome indica, transformar uma dada direção (Norte, Sul, Este ou Oeste) para vetor.
Esses vetores são úteis para calcular a posição da "hitbox" do personagem em relação à sua direção atual. 

@
direcaoParaVetor :: Direcao -> (Double, Double)
direcaoParaVetor Oeste = (-1, 0)
direcaoParaVetor Este  = (1, 0)
direcaoParaVetor Norte = (0, 1)
direcaoParaVetor Sul   = (0, -1)
@

-}

-- Converte a direção do jogador num vetor para uso na posição da hitbox.
direcaoParaVetor :: Direcao -> (Double, Double)
direcaoParaVetor Oeste = (-1, 0)
direcaoParaVetor Este  = (1, 0)
direcaoParaVetor Norte = (0, 1)
direcaoParaVetor Sul   = (0, -1)



{-

/vidaFuncao/ é uma função de alto nível que itera sobre todos os inimigos e aplica a função aplicarDano a cada um deles. 
Isso é feito para verificar se algum inimigo foi atingido pela área de ataque do jogador.

@
vidaFuncao :: Personagem -> [Personagem] -> [Personagem]
vidaFuncao jogador inimigos = map (aplicarDano jogador) inimigos
@

-}

vidaFuncao :: Personagem -> [Personagem] -> [Personagem]
vidaFuncao jogador inimigos = map (aplicarDano jogador) inimigos

{-
/aplicarDano/ verifica individualmente para cada inimigo se ele está na área de ataque do jogador (estaNaHitbox).
Se o inimigo estiver na área de ataque e o jogador estiver num estado onde pode aplicar dano (fst (aplicaDano jogador)), então a função atualiza a vida do inimigo subtraindo 1. 
Se o inimigo não estiver na área de ataque do jogador ou o jogador não estiver num estado de ataque, então o inimigo é retornado sem mudanças.

@
aplicarDano :: Personagem -> Personagem -> Personagem
aplicarDano jogador inimigo =
  if estaNaHitbox jogador inimigo && fst (aplicaDano jogador)
    then inimigo { vida = vida inimigo - 1 }
    else inimigo 
@

-}

aplicarDano :: Personagem -> Personagem -> Personagem
aplicarDano jogador inimigo =
  if estaNaHitbox jogador inimigo && fst (aplicaDano jogador)
    then inimigo { vida = vida inimigo - 1 }
    else inimigo 

{- 
/atualizaVidas/ pega no estado atual do jogo e usa a função /vidaFuncao/ para atualizar a lista de inimigos com as suas vidas possivelmente diminuídas devido ao ataque do jogador. 
Ela substitui a lista antiga de inimigos com a nova lista atualizada no estado do jogo.

@
atualizaVidas :: Jogo -> Jogo
atualizaVidas jogo = jogo { inimigos = vidaFuncao (jogador jogo) (inimigos jogo) }
@

-}
atualizaVidas :: Jogo -> Jogo
atualizaVidas jogo = jogo { inimigos = vidaFuncao (jogador jogo) (inimigos jogo) }


