{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Apecs
import Apecs.Gloss
import Control.Monad
import Data.Traversable
import Graphics.Gloss.Data.Bitmap
import Linear
import System.Exit
import System.Random

newtype Position = Position (V2 Float) deriving (Show)

instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving (Show)

instance Component Velocity where type Storage Velocity = Map Velocity

newtype Target = Target Int deriving (Show)

instance Component Target where type Storage Target = Map Target

data Bullet = Bullet deriving (Show)

instance Component Bullet where type Storage Bullet = Map Bullet

newtype Particle = Particle Float deriving (Show)

instance Component Particle where type Storage Particle = Map Particle

data Player = Player deriving (Show)

instance Component Player where type Storage Player = Unique Player

newtype Score = Score Int deriving (Show, Num)

instance Semigroup Score where (<>) = (+)

instance Monoid Score where mempty = 0

instance Component Score where type Storage Score = Global Score

newtype Time = Time Float deriving (Show, Num)

instance Semigroup Time where (<>) = (+)

instance Monoid Time where mempty = 0

instance Component Time where type Storage Time = Global Time

makeWorld
  "World"
  [ ''Position,
    ''Velocity,
    ''Player,
    ''Target,
    ''Bullet,
    ''Score,
    ''Time,
    ''Particle,
    ''Camera
    ]

type System' a = System World a

type Kinetic = (Position, Velocity)

worldWidth, worldHeight :: Int
worldWidth = 600

worldHeight = 800

alienCount :: Int
alienCount = 5

playerSpeed, bulletSpeed, enemySpeed, xmin, xmax, ymin, ymax :: Float
playerSpeed = 170

bulletSpeed = 500

enemySpeed = 80

xmin = - fromIntegral worldWidth / 2

xmax = fromIntegral worldWidth / 2

ymin = - fromIntegral worldHeight / 2

ymax = fromIntegral worldHeight / 2

hitBonus, missPenalty :: Int
hitBonus = 100

missPenalty = 40

playerStartPos, scorePos :: V2 Float
playerStartPos = V2 0 (ymin * 0.6)

scorePos = V2 (xmin + 10) (ymin + 10)

initialize :: System' ()
initialize = do
  _player <- newEntity (Player, Position playerStartPos, Velocity 0)
  pure ()

stepPosition :: Float -> System' ()
stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

clampPlayer :: System' ()
clampPlayer = cmap
  $ \(Player, Position (V2 x y)) -> Position (V2 (min xmax . max xmin $ x) y)

incrTime :: Float -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t + dT)

clearTargets :: System' ()
clearTargets =
  cmap $ \allEntities@(Target {}, Position (V2 x _), Velocity _) ->
    if x < xmin - 50 || x > xmax + 50
      then Nothing
      else Just allEntities

stepParticles :: Float -> System' ()
stepParticles dT =
  cmap $ \(Particle t) ->
    if t < 0
      then Right $ Not @(Particle, Kinetic)
      else Left $ Particle (t - dT)

clearBullets :: System' ()
clearBullets =
  cmap $ \(Bullet, Position (V2 _ y), Score s) ->
    if y > ymax
      then Right (Not @(Bullet, Kinetic), Score (s - missPenalty))
      else Left ()

handleCollisions :: SystemT World IO ()
handleCollisions =
  cmapM_ $ \(Target {}, Position posT, etyT) ->
    cmapM_ $ \(Bullet, Position posB, etyB) ->
      when (norm (posT - posB) < 30) $ do
        destroy etyT (Proxy @(Target, Kinetic))
        destroy etyB (Proxy @(Bullet, Kinetic))
        spawnParticles 15 (Position posB) (-500, 500) (200, -50)
        modify global $ \(Score x) -> Score (x + hitBonus)

triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t' = t + phase
      trigger = floor (t' / period) /= (floor ((t' + dT) / period) :: Int)
  when trigger $ void sys

spawnParticles
  :: Int -> Position -> (Float, Float) -> (Float, Float) -> System' ()
spawnParticles n pos dvx dvy = replicateM_ n $ do
  vx <- liftIO $ randomRIO dvx
  vy <- liftIO $ randomRIO dvy
  t <- liftIO $ randomRIO (0.02, 0.3)
  newEntity (Particle t, pos, Velocity (V2 vx vy))

step :: Float -> System' ()
step dT = do
  incrTime dT
  stepPosition dT
  clampPlayer
  clearTargets
  clearBullets
  stepParticles dT
  handleCollisions
  triggerEvery dT 1 0
    $ do
      n <- pickAlien
      newEntity
        (Target n, Position (V2 (xmin - 50) 100), Velocity (V2 enemySpeed 0))
  triggerEvery dT 1 0.5
    $ do
      n <- pickAlien
      newEntity
        (Target n, Position (V2 (xmax + 50) 170), Velocity (- V2 enemySpeed 0))
  where
    pickAlien = liftIO $ randomRIO (0, alienCount - 1)

handleEvent :: Event -> System' ()
handleEvent = \case
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x - playerSpeed) 0)
  EventKey (SpecialKey KeyLeft) Up _ _ ->
    cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x + playerSpeed) 0)
  EventKey (SpecialKey KeyRight) Down _ _ ->
    cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x + playerSpeed) 0)
  EventKey (SpecialKey KeyRight) Up _ _ ->
    cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x - playerSpeed) 0)
  EventKey (SpecialKey KeySpace) Down _ _ ->
    cmapM_ $ \(Player, pos :: Position) -> do
      _bullet <- newEntity (Bullet, pos, Velocity (V2 0 bulletSpeed))
      pure ()
  EventKey (SpecialKey KeyEsc) Down _ _ -> liftIO exitSuccess
  _ -> pure ()

translatePos :: Position -> Picture -> Picture
translatePos (Position (V2 x y)) = translate x y

data Assets = Assets {aliens :: [Picture], fire :: Picture, ship :: BitmapData}

draw :: Assets -> System' Picture
draw Assets {aliens, fire, ship} = do
  player <-
    foldDraw
      $ \(Player, pos) ->
        let (_, shipHeight) = bitmapSize ship
         in translatePos pos
              $ translate 0 (- fromIntegral shipHeight / 2)
              $ bitmap ship
  targets <- foldDraw $ \(Target n, pos) -> translatePos pos $ aliens !! n
  bullets <- foldDraw $ \(Bullet, pos) -> translatePos pos fire
  particles <-
    foldDraw
      $ \(Particle _, Velocity (V2 vx vy), pos) ->
        translatePos pos . color orange $ Line [(0, 0), (vx / 10, vy / 10)]
  Score s <- get global
  let score =
        color white
          $ translatePos (Position scorePos)
          $ scale 0.1 0.1
          $ Text ("Score: " ++ show s)
  pure $ player <> targets <> bullets <> score <> particles

main :: IO ()
main = do
  assets <- loadAssets
  w <- initWorld
  runWith w $ do
    initialize
    play
      (InWindow "Shmup" (worldWidth, worldHeight) (10, 10))
      black
      60
      (draw assets)
      handleEvent
      step

loadAssets :: IO Assets
loadAssets = do
  aliens <- for alienShips $ fmap (scale 0.5 0.5) . loadBMP
  fire <- loadBMP "images/fire.bmp"
  Bitmap ship <- loadBMP "images/ship.bmp"
  pure Assets {aliens, fire, ship}
  where
    alienShips =
      [ "images/shipBeige_manned.bmp",
        "images/shipBlue_manned.bmp",
        "images/shipGreen_manned.bmp",
        "images/shipPink_manned.bmp",
        "images/shipYellow_manned.bmp"
        ]
