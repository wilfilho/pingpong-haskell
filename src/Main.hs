module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- Define a estrutura fundamental dos dados no jogo.
data PingPongGame = Game {
    posicaoBola    :: (Float, Float),
    velocidade     :: (Float, Float),
    player1        :: Float,
    player2        :: Float,
    finalJogo      :: Float
} deriving Show

-- Define a velocidade utilizada em todo o jogo.
velocidadePadrao = (146, 50)

-- Define a janela do jogo.
window :: Display
window = InWindow "Ping Pong" (600, 600) (0, 0)

-- Define a cor de fundo utilizada na janela do jogo.
background :: Color
background = black

-- Define fps da animação
fps = 30

-- Define desenho/estado inicial do jogo, ou seja, as posições iniciais dos
-- dos jogadores/raquetes, bem como da bola.
mundoInicial = Game {
    posicaoBola = (0, 0),
    velocidade = velocidadePadrao,
    player1 = 0,
    player2 = 0,
    finalJogo = (-600)
}

corParedes = light (light (light black))

padding = 20

-- DESENHO DAS PAREDES --
desenhaParedeVertical x y  = translate x y $ color corParedes $ rectangleSolid 20 200
desenhaParedeHorizontal x y = translate x y $ color corParedes $ rectangleSolid 600 20
-- Desenha parede equesrda inferior
paredeEsquerdaInferior = desenhaParedeVertical (-300) (-200)
-- Desenha parede esquerda superior
paredeEsquerdaSuperior = desenhaParedeVertical (-300) (200)
-- Desenha parede direita inferior
paredeDireitaInferior = desenhaParedeVertical (300) (-200)
-- Desenha parede direita superior
paredeDireitaSuperior = desenhaParedeVertical (300) (200)
-- Desenha parede topo
paredeTopo = desenhaParedeHorizontal 0 300
-- Desenha parede rodapé
paredeRodape = desenhaParedeHorizontal 0 (-300)


-- DESENHO DAS RAQUETES --
desenharRaquete x y cor = translate x y $ color cor $ rectangleSolid 10 70
-- Desenha raquete do jogador1
raqueteJogador1 mundo = desenharRaquete 290 novaPosicao green
 where novaPosicao = player1 mundo
raqueteJogador2 mundo = desenharRaquete (-290) novaPosicao blue
 where novaPosicao = player2 mundo


-- DESENHO DA BOLA --
posicaoHorizontal (x, y) = x
posicaoVertical (x, y) = y

desenharBola mundo = translate localizacaoHorizontalBola localizacaoVerticalBola $ color white $ circleSolid 10
 where
    localizacaoVerticalBola = posicaoVertical $ posicaoBola mundo
    localizacaoHorizontalBola = posicaoHorizontal $ posicaoBola mundo

-- DESENHA O MUNDO --
-- Define desenho do mundo que aparecerá no jogo.
renderizar mundo = pictures [
        paredeEsquerdaInferior,
        paredeEsquerdaSuperior,
        paredeDireitaInferior,
        paredeDireitaSuperior,
        paredeTopo,
        paredeRodape,
        raqueteJogador2 mundo,
        raqueteJogador1 mundo,
        desenharBola mundo,
        translate (finalJogo mundo) 0 $ color green $ rectangleSolid 200 100
    ]

-- ANIMAÇÃO BOLA --
colisaoLateral :: (Float, Float) -> Bool 
colisaoLateral (x, _) = leftCollision || rightCollision
    where
        leftCollision    = x - padding <= -fromIntegral 600 / 2 
        rightCollision   = x + padding >=  fromIntegral 600 / 2

colisaoVertical :: (Float, Float) -> Bool 
colisaoVertical (_, y) = topCollision || bottomCollision
    where
        topCollision      = y - padding <= -fromIntegral 600 / 2 
        bottomCollision   = y + padding >=  fromIntegral 600 / 2

movimentarBola seconds game = game { posicaoBola = (novaPosicaoHorizontal, novaPosicaoVertical) }
    where
        (xb, yb) = posicaoBola game
        (vx, vy) = velocidade game
        novaPosicaoVertical = yb + vy * seconds * 2.5
        novaPosicaoHorizontal = xb + vx * seconds * 2.5

foiGol (bolaX, bolaY) = colisaoLateral (bolaX, bolaY) && bolaY >= (-100) && bolaY <= 100

bateuRaquete (bolaX, bolaY) raqueteJogador = colisaoLateral (bolaX, bolaY) && bolaY <= raqueteJogador + 35 && bolaY >= raqueteJogador - 35

bateuRaqueteEsquerda game = bateuRaquete (posicaoBola game) (player2 game)
bateuRaqueteDireita  game = bateuRaquete (posicaoBola game) (player1 game)

analiseGol game = foiGol (posicaoBola game) && not (bateuRaqueteEsquerda game) && not (bateuRaqueteDireita game)

definirDirecao game = novoMundo
    where
        (atualX, atualY) = velocidade game
        novoY 
            | colisaoVertical (posicaoBola game) = -atualY
            | otherwise = atualY
        novoX
            | colisaoLateral (posicaoBola game) = -atualX
            | otherwise = atualX
        novoMundo
            | analiseGol game = game {
                    velocidade = (0, 0),
                    posicaoBola = (0, 0),
                    finalJogo = 0
                }
            | otherwise = game { velocidade = (novoX, novoY) }


-- CAPTURA EVENTOS DO TELCADO --
novaPosicaoCimaRaquete posicaoAtual
    | posicaoAtual < 80 = posicaoAtual + 20
    | otherwise = posicaoAtual

novaPosicaoBaixoRaquete posicaoAtual
    | posicaoAtual > -80 = posicaoAtual - 20
    | otherwise = posicaoAtual

eventosTeclado (EventKey (Char char) _ _ _) game
    | char == 'w' = renderizarPlayer1 $ novaPosicaoCimaRaquete $ player1 game
    | char == 's' = renderizarPlayer1 $ novaPosicaoBaixoRaquete $ player1 game
    | char == 'o' = renderizarPlayer2 $ novaPosicaoCimaRaquete $ player2 game
    | char == 'l' = renderizarPlayer2 $ novaPosicaoBaixoRaquete $ player2 game
    where
        renderizarPlayer1 posicao = game { player1 = posicao}
        renderizarPlayer2 posicao = game { player2 = posicao}
eventosTeclado _ game = game

update seconds game = definirDirecao $ movimentarBola seconds game

main :: IO ()
main = play window background fps mundoInicial renderizar eventosTeclado update