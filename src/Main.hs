{-| 
Module      : Main
Description : O objetivo desta tarefa é __implementar todas as tarefas anteriormente realizadas__ de modo a __construir uma aplicação gráfica__ que permita ao utilizador jogar. Isto usando a biblioteca _Gloss_.
Copyright   : João Pedro Ribeiro de Sá <a104612@alunos.uminho.pt>
              Hélder Tiago Peixoto da Cruz <a104174@alunos.uminho.pt>

Nesta tarefa, implementámos um estilo visual baseado num jogo chamado Knight Kong, com inspiração no jogo "Hollow Knight". 
Ao abrir o jogo, a tela apresenta-nos o Menu Principal dividido em três opções: Jogar | Editar | Sair . 

Ao escolher a opção Jogar, o utilizador é levado para o ecrã do jogo.
A opção de Editar permite ao utilizador escolher o mapa que deseja jogar.

Bibliografia:

-- ** Guião Gloss feito pelo CeSIUM - https://www.youtube.com/watch?v=VDVGO3BTGYk&list=PLadvWyx_6w6XiJ95A4MqSfmIaRVbXWFGS
-- ** Documentação do Gloss - https://hackage.haskell.org/package/gloss
-- ** Fórum Stack Overflow
-- ** Material disponibilizado pelos docentes da UC
-}

module Main where 

import LI12324
import Graphics.Gloss 
import Graphics.Gloss.Interface.Pure.Game
import Niveis
import Utils
import Data.List 
import Data.Maybe

---------------------------------------------------------------------------------
-- Novos tipos de dados

-- Tipo de dados que contém os backgrounds, os titulos / textos , os botões e as ilustrações do personagem principal
data TexturasDiversas = BackgroundMenu
                      | BackgroundJogo
                      | BackgroundMorte
                      | BackgroundVitoria
                      | Titulo 
                      | TextoMorte
                      | TextoVitoria
                      | TituloJogo
                      | Mapa1_Text 
                      | Mapa2_Text
                      | TextoEdit
                      | JOGAR 
                      | EDIT
                      | SAIR 
                      | JOGAR_PRESSED 
                      | EDIT_PRESSED
                      | SAIR_PRESSED 
                      | BEGIN_PERSONAGEM
                      | JOGAR_PERSONAGEM 
                      | EDIT_PERSONAGEM
                      | SAIR_PERSONAGEM 
                      | MORTE_PERSONAGEM
                      | VITORIA_PERSONAGEM
                      | Estrela
                      | Mapa1
                      | Mapa1_pressed
                      | Mapa2
                      | Mapa2_pressed
                      deriving (Show,Eq)

-- Tipo de dados que contém os mapas para servir de auxilio ao desenho no menu editar
data Mapas = Mapa_um | Mapa_dois deriving (Show,Eq)

-- Opcões do Menu Inicial
data MenuInicialOpcoes = Jogar | Edit | Sair deriving (Show, Eq)

-- Estado do Jogo
data Modo = Menu | Editar | EmJogo | MenuInicial MenuInicialOpcoes | Morte | Vitoria | MenuEditar Mapas deriving (Show, Eq)

-- Texturas dos Blocos
type TexturesBlocos = [(Bloco,Picture)]

-- Skins dos Inimigos
type Skins = [(Entidade,Picture)]

-- Skins do Jogador
type SkinsJogador = [(Jogador_orientado,Picture)]

-- Tipo de dados que representam a orientação do Jogador
data Jogador_orientado = JogadorN | JogadorS | JogadorO | JogadorE deriving (Show,Eq)

-- Texturas Diversas para vários usos (maioritariamente para desenho de Menus)
type TexturesDiversas = [(TexturasDiversas,Picture)]

-- Texturas dos Colecionáveis
type TexturesCollectibles = [(Colecionavel,Picture)]


---------------------------------------------------------------------------------

-- | O estado do programa.

type EstadoGloss = (Modo,Estado,TexturesBlocos,Skins,TexturesDiversas,TexturesCollectibles,SkinsJogador)

---------------------------------------------------------------------------------

-- | Uma constante que define a taxa de atualização do programa - quantas vezes a função 'reageTempoGloss' é chamada por segundo.
fr :: Int 
fr = 1

-- | Altura do Mapa 
altura :: Float 
altura = 280

-- | Comprimento do Mapa
comprimento :: Float 
comprimento = (-510)

-- | Define a dimensão da janela (neste caso FullScreen).
janela :: Display
janela = FullScreen 


-- | Distância entre duas Pictures no mapa
l :: Float
l = 22.5

---------------------------------------------------------------------------------

-- | Função que define o estado inicial do programa
estadoGlossInicial :: Modo -> TexturesBlocos -> Skins -> TexturesDiversas -> TexturesCollectibles -> SkinsJogador -> EstadoGloss
estadoGlossInicial modo textures skins menu collectibles_textures skins_jogador = (modo, estadoInicial, textures, skins,menu,collectibles_textures,skins_jogador)

-- | Função que reage a um evento, por parte do utilizador, através do teclado.
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
-- Navegação nos mapas
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Menu,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (MenuInicial Jogar,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Menu,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (MenuInicial Edit,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Jogar,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (MenuInicial Edit,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Edit,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (MenuInicial Sair,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Sair,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (MenuInicial Jogar,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Sair,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (MenuInicial Edit,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Edit,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (MenuInicial Jogar,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Jogar,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (EmJogo,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Edit,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (Editar,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuEditar Mapa_um,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (MenuEditar Mapa_dois,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)  
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuEditar Mapa_dois,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (MenuEditar Mapa_um,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)  
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (Editar,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (MenuEditar Mapa_um,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
-- Selecionar Mapa no Menu Editar
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuEditar Mapa_um,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (Menu,estado_updated,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)  
  where estado_updated = estado {jogo = estado_mapa_update1}
        estado_mapa_update1 = (jogo estado) {mapa = nivel1}
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (Editar,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (MenuEditar Mapa_dois,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuEditar Mapa_dois,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (Menu,estado_updated,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
  where estado_updated = estado {jogo = estado_mapa_update2}
        estado_mapa_update2 = (jogo estado) {mapa = nivel2}
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Sair,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = error "Até à próxima!"
-- CheatCode que remove vida ao Jogador
reageEventoGloss (EventKey (Char '-') Down _ _) (EmJogo,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (EmJogo,(tiraVida estado),texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
-- Voltar ao Menu
reageEventoGloss (EventKey (Char 'm') Down _ _) (EmJogo,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (Menu,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador)
-- Movimentos do Jogador
reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) (EmJogo,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (EmJogo,(movePlayer estado 0),texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) 
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (EmJogo,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (EmJogo,(movePlayer estado 1),texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) 
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (EmJogo,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (EmJogo,(movePlayer estado 2),texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) 
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (EmJogo,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (EmJogo,(movePlayer estado 3),texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) 
reageEventoGloss (EventKey (SpecialKey KeyDelete) Down _ _) (EmJogo,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = (Morte,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) 
-- Voltar ao Menu Inicial após completar o Jogo
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Morte, estado, texturas, skins, texturas_diversas,collectibles_textures,skins_jogador) = (Menu, estadoInicial, texturas, skins, texturas_diversas,collectibles_textures,skins_jogador) 
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Vitoria, estado, texturas, skins, texturas_diversas,collectibles_textures,skins_jogador) = (Menu, estadoInicial, texturas, skins, texturas_diversas,collectibles_textures,skins_jogador) 
reageEventoGloss (EventKey (Char 'q') Down _ _) _ = error "BYE"
reageEventoGloss _ s = s

-- | Função que remove uma vida ao Jogador
tiraVida :: Estado -> Estado
tiraVida estado = estado { jogo = updatedJogo }
  where
    jogadorAtualizado = (jogador (jogo estado)) { vida = vida (jogador (jogo estado)) - 1 }
    jogoAtualizado = (jogo estado) { jogador = jogadorAtualizado }
    updatedJogo = (jogo estado) { jogador = jogadorAtualizado }

-- | Função que atualiza as Coordenadas e a Direção do Jogador
movePlayer :: Estado -> Int -> Estado
movePlayer estado flag
  | canMove x_updated y_updated estado = updatedEstado
  | otherwise = estado
  where
    coordenadas = posicao (jogador (jogo estado))
    x_player = fst coordenadas
    y_player = snd coordenadas
    (xs, ys) = case flag of
      0 -> (0.0, -1.0)  -- flag 0 corresponde a um movimento para cima
      1 -> (0.0, 1.0)   -- flag 1 corresponde a um movimento para baixo
      2 -> (-1.0, 0.0)  -- flag 2 corresponde a um movimento para a esquerda
      3 -> (1.0, 0.0)   -- flag 3 corresponde a um movimento para a direita
    x_updated = x_player + xs
    y_updated = y_player + ys
    direcao_atualizada = case flag of
      0 -> Norte
      1 -> Sul
      2 -> Oeste
      3 -> Este
    jogadorAtualizado =
      (jogador (jogo estado))
        { posicao = (x_updated, y_updated),
          direcao = direcao_atualizada
        }
    jogoAtualizado = (jogo estado) { jogador = jogadorAtualizado }
    updatedEstado = if (bateNoJogador (jogador (jogo estado)) (inimigos (jogo estado))) then tiraVida (estado { jogo = jogoAtualizado }) else (estado { jogo = jogoAtualizado }) 

-- | Função que verifica se um jogador pode ou não executar um movimento
canMove :: Double -> Double -> Estado -> Bool
canMove x y estado = (x>= 0.0) && (x <= largura_mapa - 3.0) && (y >= 0) && (y <= altura_mapa - 3.0) && not (isPlataforma (floor x + 1) (floor y + 1) mapa) && not (isTrapdoor (floor x + 1) (floor y + 1) mapa)
  where largura_mapa = getLarguraMapa (getMatrix(estado))
        altura_mapa = getAlturaMapa (getMatrix(estado))
        mapa = getMatrix (estado)

{-
Por alguma razão, parece me ser devido ao resize das imagens, tanto a altura como a largura do mapa estão após o mapa desenhado
Por isso, subtraí 3 unidades
-}

-- | Função que verifica se um bloco na Matriz é do tipo Plataforma
isPlataforma :: Int -> Int -> [[Bloco]] -> Bool
isPlataforma x y mapa = case (mapa !! y) !! x of
                          Plataforma -> True
                          _          -> False

isTrapdoor :: Int -> Int -> [[Bloco]] -> Bool
isTrapdoor x y mapa = case (mapa !! y) !! x of
                          Alcapao    -> True
                          _          -> False
---------------------------------------------------------------------------------

-- | Função que reage à passagem do tempo.
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss time (EmJogo, estado, texturas, skins, texturas_diversas, collectibles_textures,skins_jogador)
  | vida_jogador <= 0 = (Morte, estado, texturas, skins, texturas_diversas, collectibles_textures,skins_jogador)
  | hasStar (posicao (jogador (jogo estado))) (getPosicaoEstrelaMapa (getMapa estado)) = (Vitoria, estadoInicial, texturas, skins, texturas_diversas, collectibles_textures,skins_jogador)
  | otherwise = (EmJogo, estado, texturas, skins, texturas_diversas, collectibles_textures,skins_jogador)
  where
    vida_jogador = vida (jogador (jogo estado))
reageTempoGloss _ estado = estado


---------------------------------------------------------------------------------

-- | Função que desenha um bloco
desenhaBloco :: Float -> Float -> Bloco -> TexturesBlocos -> Picture
desenhaBloco x y bloco texturas = Translate x y $ Scale escalaX escalaY textura
  where
    textura = (fromJust . lookup bloco) texturas
    (escalaX, escalaY) =
      case bloco of
        Plataforma -> (0.054, 0.054)
        _ -> (0.075, 0.075)  


-- | Função que desenha uma lista de blocos
desenhaLinha :: Float -> Float -> [Bloco] -> TexturesBlocos -> [Picture]
desenhaLinha x y (h:t) texturas = bloco : resto
  where bloco = desenhaBloco x y h texturas
        resto = desenhaLinha (x+l) y t texturas
desenhaLinha _ _ _ _ = []

-- | Função que desenha um Mapa de Blocos
desenhaMapa :: Float -> Float -> [[Bloco]] -> TexturesBlocos -> [Picture]
desenhaMapa x y (h:t) texturas = linha ++ resto 
  where linha = desenhaLinha x y h texturas
        resto = desenhaMapa x (y-l) t texturas
desenhaMapa _ _ _ _ = []

-- | Função que desenha o Jogador
desenhaJogador :: Personagem -> SkinsJogador -> Picture
desenhaJogador (Personagem {vida = 0}) _ = Blank
desenhaJogador jogador skins = Translate (realX x_jogador) (realY y_jogador) $ Scale escalaX escalaY imagem
  where
    x_jogador = fst (posicao jogador)
    y_jogador = snd (posicao jogador) - 0.1
    (escalaX, escalaY) = case direcao jogador of
      Norte -> (0.06, 0.06) -- 
      Sul -> (0.06, 0.06) -- 
      Este -> (0.25, 0.25) 
      Oeste -> (0.25, 0.25) 
    imagem = case direcao jogador of
      Norte -> (fromJust . lookup JogadorN) skins
      Sul -> (fromJust . lookup JogadorS)  skins
      Este -> (fromJust . lookup JogadorE)  skins
      Oeste -> (fromJust . lookup JogadorO)  skins


-- | Função que desenha uma lista de inimigos do tipo "Fantasma"
desenhaFantasmas :: [Personagem] -> Skins -> [Picture]
desenhaFantasmas ((Personagem {vida = 0}):t) skins = desenhaFantasmas t skins 
desenhaFantasmas (h:t) skins = (desenhaFantasma h skins) : (desenhaFantasmas t skins)  
desenhaFantasmas _ _ = []

-- | Função que desenha um inimigo do tipo "Fantasma"
desenhaFantasma :: Personagem -> Skins -> Picture
desenhaFantasma fantasma skins = Translate (realX x_personagem) (realY y_personagem) imagem
  where x_personagem = fst (posicao fantasma)
        y_personagem = snd (posicao fantasma)
        imagem = (fromJust . lookup Fantasma) skins

-- | Função que desenha um inimigo do tipo "MacacoMalvado"
desenhaMacacoMalvado :: Personagem -> Skins -> Picture
desenhaMacacoMalvado (Personagem {vida = 0}) skins = Blank 
desenhaMacacoMalvado macaco skins = Translate (realX x_macaco) (realY y_macaco) $ Scale 0.1 0.1 imagem
  where x_macaco = fst (posicao macaco)  
        y_macaco = snd (posicao macaco) - 0.3 -- para o macaco ficar um pouco mais acima do bloco
        imagem = (fromJust.lookup MacacoMalvado) skins

-- | Função que desenha a Estrela 
desenhaEstrela :: Posicao -> TexturesDiversas -> Picture
desenhaEstrela posicao texturas_diversas = Translate (realX x_estrela) (realY y_estrela) $ Scale 0.08 0.08 imagem 
  where x_estrela = fst posicao
        y_estrela = snd posicao 
        imagem    = (fromJust . lookup Estrela) texturas_diversas

-- | Função que desenha um colecionável do tipo "Martelo"
desenhaMartelo :: Posicao -> TexturesCollectibles -> Picture
desenhaMartelo posicao texturas_collectibles = 
  Translate (realX x_martelo) (realY y_martelo) $ Scale 0.04 0.04 imagem 
  where 
    x_martelo = fst posicao
    y_martelo = snd posicao
    imagem = (fromJust . lookup Martelo) texturas_collectibles

-- | Função que desenha um colecionável do tipo "Moeda"
desenhaMoeda :: Posicao -> TexturesCollectibles -> Picture
desenhaMoeda posicao texturas_collectibles = 
  Translate (realX x_moeda) (realY y_moeda) $ Scale 0.05 0.05 imagem 
  where 
    x_moeda = fst posicao
    y_moeda = snd posicao
    imagem = (fromJust . lookup Moeda) texturas_collectibles

-- | Função que desenha uma lista de colecionáveis, recorrendo às duas funções anteriores
desenhaCollectibles :: [(Colecionavel, Posicao)] -> TexturesCollectibles -> [Picture]
desenhaCollectibles [] _ = []
desenhaCollectibles ((coletavel, pos):t) textures_collectibles =
      case coletavel of
        Moeda -> desenhaMoeda pos textures_collectibles : desenhaCollectibles t textures_collectibles
        Martelo -> desenhaMartelo pos textures_collectibles : desenhaCollectibles t textures_collectibles

-- | Função que desenha o Menu Inicial (NADA SELECIONADO)
desenhaMenu :: TexturesDiversas -> Picture
desenhaMenu texturas_diversas =
  Pictures
    [ 
      bg,
      Translate (-10) 150 titulo,
      Translate (-450) (-100) $ Scale 0.4 0.4 personagem,
      Translate (-10) 0 $ Scale 0.15 0.15 botao_jogar,
      Translate (-10) (-100) $ Scale 0.15 0.15 botao_edit,
      Translate (-10) (-200) $ Scale 0.15 0.15 botao_sair
    ]
    where bg = (fromJust . lookup BackgroundMenu) texturas_diversas
          botao_jogar = (fromJust . lookup JOGAR) texturas_diversas
          botao_edit = (fromJust . lookup EDIT) texturas_diversas
          botao_sair = (fromJust . lookup SAIR) texturas_diversas
          titulo = (fromJust . lookup Titulo) texturas_diversas
          personagem = (fromJust. lookup BEGIN_PERSONAGEM) texturas_diversas

-- | Função que desenha o Menu Inicial (JOGAR SELECIONADO)
desenhaMenuJogar :: TexturesDiversas -> Picture
desenhaMenuJogar texturas_diversas =
  Pictures
    [ 
      bg,
      Translate (-10) 150 titulo,
      Translate (-450) (-100) $ Scale 0.9 0.9 personagem,
      Translate (-10) 0 $ Scale 0.15 0.15 botao_jogar_pressed,
      Translate (-10) (-100) $ Scale 0.15 0.15 botao_edit,
      Translate (-10) (-200) $ Scale 0.15 0.15 botao_sair
    ]
    where bg = (fromJust . lookup BackgroundMenu) texturas_diversas
          botao_jogar_pressed = (fromJust . lookup JOGAR_PRESSED) texturas_diversas
          botao_edit = (fromJust . lookup EDIT) texturas_diversas
          botao_sair = (fromJust . lookup SAIR) texturas_diversas
          titulo = (fromJust . lookup Titulo) texturas_diversas
          personagem = (fromJust. lookup JOGAR_PERSONAGEM) texturas_diversas

-- | Função que desenha o Menu Inicial (EDIT SELECIONADO)
desenhaMenuEdit :: TexturesDiversas -> Picture 
desenhaMenuEdit texturas_diversas =
  Pictures
    [ 
      bg,
      Translate (-10) 150 titulo,
      Translate (-450) (-100) $ Scale 0.6 0.6 personagem,
      Translate (-10) 0 $ Scale 0.15 0.15 botao_jogar,
      Translate (-10) (-100) $ Scale 0.15 0.15 botao_edit_pressed,
      Translate (-10) (-200) $ Scale 0.15 0.15 botao_sair
    ]
    where bg = (fromJust . lookup BackgroundMenu) texturas_diversas
          botao_jogar = (fromJust . lookup JOGAR) texturas_diversas
          botao_edit_pressed = (fromJust . lookup EDIT_PRESSED) texturas_diversas
          botao_sair = (fromJust . lookup SAIR) texturas_diversas
          titulo = (fromJust . lookup Titulo) texturas_diversas
          personagem = (fromJust. lookup EDIT_PERSONAGEM) texturas_diversas

-- | Função que desenha o Menu Inicial (SAIR SELECIONADO)
desenhaMenuSair :: TexturesDiversas -> Picture
desenhaMenuSair texturas_diversas =
  Pictures
    [ 
      bg,
      Translate (-10) 150 titulo,
      Translate (-400) (-100) personagem,
      Translate (-10) 0 $ Scale 0.15 0.15 botao_jogar,
      Translate (-10) (-100) $ Scale 0.15 0.15 botao_edit,
      Translate (-10) (-200) $ Scale 0.15 0.15 botao_sair_pressed
    ]
    where bg = (fromJust . lookup BackgroundMenu) texturas_diversas
          botao_jogar = (fromJust . lookup JOGAR) texturas_diversas
          botao_edit = (fromJust . lookup EDIT) texturas_diversas
          botao_sair_pressed = (fromJust . lookup SAIR_PRESSED) texturas_diversas
          titulo = (fromJust . lookup Titulo) texturas_diversas
          personagem = (fromJust. lookup SAIR_PERSONAGEM) texturas_diversas

-- | Função que desenha o Menu Editar (nada selecionado)
desenhaEditMenu :: TexturesDiversas -> Picture
desenhaEditMenu texturas_diversas =
  Pictures
    [ 
      bg,
      Translate (-350) 0 $ Scale 0.5 0.5 mapa1,
      Translate (330) 0  $ Scale 0.5 0.5 mapa2,
      Translate (-350) 185 $ Scale 0.6 0.6 mapa1_text,
      Translate (330) 185 $ Scale 0.6 0.6 mapa2_text,
      Translate 0 (-300) $ Scale 0.7 0.7 edit_text
    ]
    where bg = (fromJust . lookup BackgroundMenu) texturas_diversas
          mapa1 = (fromJust . lookup Mapa1) texturas_diversas
          mapa2 = (fromJust. lookup Mapa2) texturas_diversas
          mapa1_text = (fromJust . lookup Mapa1_Text) texturas_diversas
          mapa2_text = (fromJust . lookup Mapa2_Text) texturas_diversas
          edit_text = (fromJust . lookup TextoEdit) texturas_diversas

-- | Função que desenha o Menu Editar (MAPA1 SELECIONADO)
desenhaEditMenu_Mapa1 :: TexturesDiversas -> Picture
desenhaEditMenu_Mapa1 texturas_diversas =
  Pictures
    [ 
      bg,
      Translate (-350) 0 $ Scale 0.5 0.5 mapa1_selec,
      Translate (330) 0  $ Scale 0.5 0.5 mapa2,
      Translate (-350) 185 $ Scale 0.6 0.6 mapa1_text,
      Translate (330) 185 $ Scale 0.6 0.6 mapa2_text,
      Translate 0 (-300) $ Scale 0.7 0.7 edit_text
    ]
    where bg = (fromJust . lookup BackgroundMenu) texturas_diversas
          mapa1_selec = (fromJust . lookup Mapa1_pressed) texturas_diversas
          mapa2 = (fromJust. lookup Mapa2) texturas_diversas
          mapa1_text = (fromJust . lookup Mapa1_Text) texturas_diversas
          mapa2_text = (fromJust . lookup Mapa2_Text) texturas_diversas
          edit_text = (fromJust . lookup TextoEdit) texturas_diversas

-- | Função que desenha o Menu Editar (MAPA2 SELECIONADO)
desenhaEditMenu_Mapa2 :: TexturesDiversas -> Picture
desenhaEditMenu_Mapa2 texturas_diversas =
  Pictures
    [ 
      bg,
      Translate (-350) 0 $ Scale 0.5 0.5 mapa1,
      Translate (330) 0  $ Scale 0.5 0.5 mapa2_selec,
      Translate (-350) 185 $ Scale 0.6 0.6 mapa1_text,
      Translate (330) 185 $ Scale 0.6 0.6 mapa2_text,
      Translate 0 (-300) $ Scale 0.7 0.7 edit_text
    ]
    where bg = (fromJust . lookup BackgroundMenu) texturas_diversas
          mapa1 = (fromJust . lookup Mapa1) texturas_diversas
          mapa2_selec = (fromJust. lookup Mapa2_pressed) texturas_diversas
          mapa1_text = (fromJust . lookup Mapa1_Text) texturas_diversas
          mapa2_text = (fromJust . lookup Mapa2_Text) texturas_diversas
          edit_text = (fromJust . lookup TextoEdit) texturas_diversas

-- | Função que desenha o Ecrã de Morte
desenhaMenuMorte :: TexturesDiversas -> Picture
desenhaMenuMorte texturas_diversas =
  Pictures
    [ 
      bg,
      Translate (-380) (0) personagem,
      Translate (200) 0  texto
    ]
    where bg = (fromJust . lookup BackgroundMorte) texturas_diversas
          personagem = (fromJust. lookup MORTE_PERSONAGEM) texturas_diversas
          texto = (fromJust. lookup TextoMorte) texturas_diversas

-- | Função que desenha o Ecrã de Vitória
desenhaMenuVitoria :: TexturesDiversas -> Picture
desenhaMenuVitoria texturas_diversas =
  Pictures
    [ 
      bg,
      Translate (-380) (0) $ Scale 0.5 0.5 personagem,
      Translate (200) 0  texto
    ]
    where bg = (fromJust . lookup BackgroundVitoria) texturas_diversas
          personagem = (fromJust. lookup VITORIA_PERSONAGEM) texturas_diversas
          texto = (fromJust. lookup TextoVitoria) texturas_diversas


-- | Função auxiliar para converter as coordenadas à escala do mapa.
realX :: Double -> Float
realX = (+ comprimento) . (*l) . realToFrac . succ 

-- | Função auxiliar para converter as coordenadas à escala do mapa.
realY :: Double -> Float
realY = (+ altura) . (*(-l)) . realToFrac . succ 

-- | Desenha o estado do programa, consoante algumas variáveis - transforma um estado numa 'Picture'.
desenhaEstadoGloss :: EstadoGloss -> Picture 
desenhaEstadoGloss (Menu, _, _, _,texturas_diversas,_,_) = desenhaMenu texturas_diversas
desenhaEstadoGloss (Editar, _, _, _,texturas_diversas,_,_) = desenhaEditMenu texturas_diversas
desenhaEstadoGloss (MenuEditar Mapa_um, _, _, _,texturas_diversas,_,_) = desenhaEditMenu_Mapa1 texturas_diversas
desenhaEstadoGloss (MenuEditar Mapa_dois, _, _, _,texturas_diversas,_,_) = desenhaEditMenu_Mapa2 texturas_diversas
desenhaEstadoGloss (MenuInicial Jogar, _, _, _,texturas_diversas,_,_) = desenhaMenuJogar texturas_diversas
desenhaEstadoGloss (MenuInicial Edit,_,_,_,texturas_diversas,_,_) = desenhaMenuEdit texturas_diversas
desenhaEstadoGloss (MenuInicial Sair,_,_,_,texturas_diversas,_,_) = desenhaMenuSair texturas_diversas
desenhaEstadoGloss (EmJogo,estado,texturas,skins,texturas_diversas,collectibles_textures,skins_jogador) = Pictures desenho
                where desenho = desenhoFundo ++ desenhoMapa ++ desenhoSubTitulo ++ desenhoFantasma ++ [desenhoMacacoMalvado] ++ [desenhoEstrela] ++ desenhoCollectibles ++ [desenhoJogador] 
                      desenhoFundo = [Translate 0 0 fundo]
                      fundo = (fromJust . lookup BackgroundJogo) texturas_diversas
                      desenhoSubTitulo = [Translate 0 (-310) texto]
                      texto = (fromJust . lookup TituloJogo) texturas_diversas
                      desenhoMapa = desenhaMapa comprimento altura blocos texturas
                      blocos  = getMatrix estado
                      desenhoJogador = desenhaJogador player skins_jogador
                      player = getJogador estado
                      desenhoFantasma = desenhaFantasmas fantasma skins
                      fantasma = getFantasma estado
                      desenhoMacacoMalvado = desenhaMacacoMalvado macaco skins
                      macaco = getMacaco estado
                      desenhoEstrela = desenhaEstrela pos_estrela texturas_diversas
                      pos_estrela = getPosicaoEstrelaMapa (getMapa estado)
                      desenhoCollectibles = desenhaCollectibles (colecionaveis (jogo estado)) collectibles_textures
desenhaEstadoGloss (Morte, _, _, _, texturas_diversas,_,_) = desenhaMenuMorte texturas_diversas
desenhaEstadoGloss (Vitoria, _, _, _, texturas_diversas,_,_) = desenhaMenuVitoria texturas_diversas

---------------------------------------------------------------------------------

-- | Função principal
main :: IO ()
main = do 
  escada <- loadBMP "../imgs/escada.bmp"
  plataforma <- loadBMP "../imgs/bloco.bmp"
  alcapao <- loadBMP "../imgs/alcapao.bmp"
  vazio <- loadBMP "../imgs/vazio.bmp"
  fantasma <- loadBMP "../imgs/fantasma.bmp"
  macaco <- loadBMP "../imgs/macaco.bmp"
  backgroundMenu <- loadBMP "../imgs/bgMenu.bmp"
  backgroundJogo <- loadBMP "../imgs/bgJogo.bmp"
  backgroundMorte <- loadBMP "../imgs/bgMorte.bmp" 
  backgroundWIN <- loadBMP "../imgs/bgWin.bmp"
  playbutton <- loadBMP "../imgs/play.bmp"
  editbutton <- loadBMP "../imgs/edit.bmp"
  leavebutton <- loadBMP "../imgs/sair.bmp"
  playbutton_pressed <- loadBMP "../imgs/play_pressed.bmp"
  editbutton_pressed <- loadBMP "../imgs/edit_pressed.bmp"
  leavebutton_pressed <- loadBMP "../imgs/sair_pressed.bmp"
  titulo <- loadBMP "../imgs/titulo.bmp"
  knight <- loadBMP "../imgs/knight.bmp"
  knight_esquerda <- loadBMP "../imgs/knight_esq.bmp"
  knight_direita <- loadBMP "../imgs/knight_dir.bmp"
  knight_tras <- loadBMP "../imgs/knight_tras.bmp"
  textomorte <- loadBMP "../imgs/textomorte.bmp"
  textojogo <- loadBMP "../imgs/titulo_jogo.bmp"
  textovitoria <- loadBMP "../imgs/textovitoria.bmp"
  personagem_jogar <- loadBMP "../imgs/main_carac.bmp"
  personagem_unselect <- loadBMP "../imgs/normal.bmp"
  personagem_edit <- loadBMP "../imgs/main_edit.bmp"
  personagem_sair <- loadBMP "../imgs/main_sair.bmp"
  personagem_morte <- loadBMP "../imgs/main_morte.bmp"
  personagem_win <- loadBMP "../imgs/main_win.bmp"
  estrela <- loadBMP "../imgs/estrela.bmp"
  machado <- loadBMP "../imgs/axe.bmp"
  moeda <- loadBMP "../imgs/coin.bmp"
  mapa1 <- loadBMP "../imgs/mapa1.bmp"
  mapa1_pressed <- loadBMP "../imgs/mapa1_pressed.bmp" 
  mapa2 <- loadBMP "../imgs/mapa2.bmp"
  mapa2_pressed <- loadBMP "../imgs/mapa2_pressed.bmp" 
  mapa1_text <- loadBMP "../imgs/Mapa-1.bmp"
  mapa2_text <- loadBMP "../imgs/Mapa-2.bmp"
  texto_edit <- loadBMP "../imgs/textoEdit.bmp"
  play janela
       white
       fr
       (estadoGlossInicial Menu 
          [
          (Escada,escada),
          (Plataforma,plataforma),
          (Alcapao,alcapao),
          (Vazio,vazio)]
          [(Fantasma,fantasma),
          (MacacoMalvado,macaco)]
          [(BackgroundMenu,backgroundMenu),
          (BackgroundJogo,backgroundJogo),
          (BackgroundMorte,backgroundMorte),
          (BackgroundVitoria,backgroundWIN),
          (JOGAR,playbutton),
          (EDIT, editbutton),
          (SAIR,leavebutton),
          (JOGAR_PRESSED,playbutton_pressed),
          (EDIT_PRESSED,editbutton_pressed),
          (SAIR_PRESSED,leavebutton_pressed),
          (Titulo,titulo),
          (TituloJogo,textojogo),
          (TextoMorte,textomorte),
          (TextoVitoria,textovitoria),
          (TextoEdit,texto_edit),
          (Mapa1_Text,mapa1_text),
          (Mapa2_Text,mapa2_text),
          (JOGAR_PERSONAGEM,personagem_jogar),
          (BEGIN_PERSONAGEM,personagem_unselect),
          (EDIT_PERSONAGEM,personagem_edit),
          (SAIR_PERSONAGEM,personagem_sair),
          (MORTE_PERSONAGEM,personagem_morte),
          (VITORIA_PERSONAGEM,personagem_win),
          (Estrela,estrela),
          (Mapa1,mapa1),
          (Mapa1_pressed,mapa1_pressed),
          (Mapa2,mapa2),
          (Mapa2_pressed,mapa2_pressed)]
          [(Martelo,machado),
          (Moeda,moeda)]
          [(JogadorS,knight),
          (JogadorE,knight_direita),
          (JogadorO,knight_esquerda),
          (JogadorN,knight_tras)])
       desenhaEstadoGloss
       reageEventoGloss
       reageTempoGloss