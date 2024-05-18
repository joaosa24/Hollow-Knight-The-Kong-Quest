{-| 
Module      : Niveis
Description : O objetivo deste Módulo é definir o Estado e consequentemente, define o Jogo e tudo anexado ao mesmo
Copyright   : João Pedro Ribeiro de Sá <a104612@alunos.uminho.pt>
              Hélder Tiago Peixoto da Cruz <a104174@alunos.uminho.pt>
-}

module Niveis where 

import LI12324

------------------------------------------------------------------------------------------------------------

-- | Definição dos Inimigos 
inimigo :: Personagem
inimigo =
  Personagem
    { velocidade = (1.0, 0.0)
    , tipo = Fantasma
    , posicao = (10.0, 7.0)
    , direcao = Este
    , tamanho = (2.0, 2.0)
    , emEscada = False
    , ressalta = True
    , vida = 1
    , pontos = 10
    , aplicaDano = (False, 0.0)
    }

inimigo2 :: Personagem
inimigo2 =
  Personagem
    { velocidade = (1.0, 0.0)
    , tipo = Fantasma
    , posicao = (11.0, 12.0)
    , direcao = Este
    , tamanho = (2.0, 2.0)
    , emEscada = False
    , ressalta = True
    , vida = 1
    , pontos = 10
    , aplicaDano = (False, 0.0)
    }
inimigo3 :: Personagem
inimigo3 =
  Personagem
    { velocidade = (1.0, 0.0)
    , tipo = Fantasma
    , posicao = (6.0, 18.0)
    , direcao = Este
    , tamanho = (2.0, 2.0)
    , emEscada = False
    , ressalta = True
    , vida = 1
    , pontos = 10
    , aplicaDano = (False, 0.0)
    }

inimigo4 :: Personagem
inimigo4 =
  Personagem
    { velocidade = (1.0, 0.0)
    , tipo = MacacoMalvado
    , posicao = (24.0, 6.0)
    , direcao = Este
    , tamanho = (2.0, 2.0)
    , emEscada = False
    , ressalta = True
    , vida = 1
    , pontos = 10
    , aplicaDano = (False, 0.0)
    }

-- | Definição do Jogador
jogadorInicial :: Personagem
jogadorInicial = Personagem
        { velocidade = (1.0, 0.0)
        , tipo = Jogador
        , posicao = getPosicaoJogadorMapa (getMapafromJogo(jogo estadoInicial))
        , direcao = getDirecaoJogadorMapa (getMapafromJogo(jogo estadoInicial))
        , tamanho = (1.0, 1.0)
        , emEscada = False
        , ressalta = False
        , vida = 3
        , pontos = 0
        , aplicaDano = (False, 0.0)
        }

------------------------------------------------------------------------------------------------------------

-- | Definição do Estado (irá ser utilizado na parte gráfica)
data Estado = Estado
    { 
      jogo :: Jogo
    }
    deriving (Read, Show, Eq)

------------------------------------------------------------------------------------------------------------   

-- | Definição do Estado Inicial
estadoInicial :: Estado 
estadoInicial = Estado
    { 
      jogo = jogoInicial
    }

-- | Definição do Jogo Inicial, caracterizando assim o Estado Inicial
jogoInicial :: Jogo
jogoInicial  = Jogo 
  {
    mapa = (Mapa ((1.0, 22.0), Sul) (43.0, 9.0) (convertToMap mapa3)),
    inimigos = [inimigo,inimigo2,inimigo3,inimigo4],
    colecionaveis = [(Moeda,(41.0,22.0)),(Moeda,(43.0,14.0)),(Moeda,(31.0,14.0)),(Moeda,(18.0,2.0)),(Moeda,(31.0,1.0)),(Moeda,(21.0,6.0)),(Moeda,(26.0,6.0)),(Moeda,(6.0,22.0)),(Martelo,(17.0,12.0)),(Martelo,(15.0,22.0))],
    jogador = jogadorInicial

  } 

------------------------------------------------------------------------------------------------------------

-- | Função que devolve um Mapa a partir de um Jogo
getMapafromJogo :: Jogo -> Mapa 
getMapafromJogo jogo = (mapa jogo)

-- | Função que devolve a Posição do Jogador a partir de um Mapa
getPosicaoJogadorMapa :: Mapa -> Posicao
getPosicaoJogadorMapa (Mapa (posicao,_) _ _) = posicao

-- | Função que devolve a Direção do Jogador a partir de um Mapa
getDirecaoJogadorMapa :: Mapa -> Direcao 
getDirecaoJogadorMapa (Mapa (_,direcao) _ _) = direcao

-- | Função que devolve a Posição da Estrela a partir de um Mapa 
getPosicaoEstrelaMapa :: Mapa -> Posicao
getPosicaoEstrelaMapa (Mapa _ posicao_estrela _) = posicao_estrela

-- | Função que converte uma lista de Strings em uma matriz de Blocos (auxilia a edição dos mapas)
convertToMap :: [String] -> [[Bloco]]
convertToMap m = map linha m 
  where 
    linha :: String -> [Bloco]
    linha "" = []
    linha (h:t)
      | h == 'p' = Plataforma : linha t
      | h == 'H' = Escada : linha t
      | h == '~' = Alcapao : linha t
      | h == ' ' = Vazio : linha t

------------------------------------------------------------------------------------------------------------

-- | Definição dos Níveis do Jogo
nivel1 :: Mapa
nivel1 = (Mapa ((1.0, 22.0), Norte) (43.0, 9.0) (convertToMap mapa3))

nivel2 :: Mapa 
nivel2 = (Mapa ((1.0, 22.0), Norte) (43.0, 9.0) (convertToMap mapa1))

-- | Definição dos Mapas (Em Strings) 
mapa1 :: [String]
mapa1 = ["                                              ",
         "                                              ",
         "                                              ",
         " pppHpppppH                   H ppppp         ",
         "    H      pppppppppppppppppppH     p         ",
         "    H                         H     p         ",
         " pppp~~                       H     pppp   ppp",
         "                              H     p        p",
         " H           Hppppppppppppppppppp   p        p",
         " Hpppppppppppp                      p        p",
         " H                                  p        p",
         " H                                  pppppppppp",
         " H                                            ",
         " H                      H                     ",
         " pppp~~~~~ppppppppppppppH                     ",
         "                        H                     ",
         "                     pppppppppppp~~~pppppHppp ",
         "    H                                    H    ",
         "    H                                    H    ",
         "    H                                    H    ",
         " Hpppppp~~~pppppppppHpppppppp~~pppppppppppppH ",
         " H                  H                       H ",
         " H                  H                       H ",
         " H                  H                       H ",
         "pppppppppppppppppppppppppppppppppppppppppppppp"]

mapa2 :: [String]
mapa2 = ["pppppppppppppppppppppppppppppppppppppppppppppp",
         "p      H    H                                p",
         "ppp~pppHp~~pHppppppppppppppp          pppppppp",
         "p      H                                     p",
         "pppppppHppp       ppppppppp  pppppp          p",
         "pppppppHpppppppppppp                         p",
         "p      H    H      p                         p",
         "ppp~pppHp~~pHppppppp                         p",
         "p      H           p                         p",
         "p      H    H      p                         p",
         "ppp~pppHp~~pHppppppp                         p",
         "ppp~pppHp~~pHppppppp                         p",
         "p      H           p                         p",
         "p      H    H      p                         p",
         "p      H           p                         p",
         "p      H    H      p                         p",
         "p      H    H      p                         p",
         "ppp~pppHp~~pHppppppp~~~~~~~~pppppppHHppppppppp",
         "p      H           p        pppp   HH        p",
         "p      H    H      p               HH        p",
         "ppp~pppHp~~pHppppppp               HH        p",
         "p      H           p               HH        p",
         "p      H    H      p               HH        p",
         "p      H           p               HH        p",
         "pppppppHpppppppppppppppppppppppppppppppppppppp"]

mapa3 :: [String]
mapa3 = ["pppppppppppppppppppppppppppppppppppppppppppppp",
         "p                                            p",
         "p                                            p",
         "p           H          ppppppppppppppppp     p",
         "p           Hpppppppppp                H     p",
         "p           H                          H     p",
         "p           H                          H     p",
         "p           H                          H     p",
         "pH          H       ppppppppppp              p",
         "pHp~~~~~~~pppp                               p",
         "pH                                           p",
         "pH                                  pppppppppp",
         "pH                                           p",
         "pH                                           p",
         "pH        pppp~~ppp                          p",
         "pH                                 H         p",
         "pH                         ppppppppHpppppppppp",
         "pH                                 H         p",
         "pH                                 H         p",
         "pH        H                        H         p",
         "ppppppppppH                        H         p",
         "p         H                        H         p",
         "p         H                        H         p",
         "p         H                        H         p",
         "pppppppppppppppppppppppppppppppppppppppppppppp"]