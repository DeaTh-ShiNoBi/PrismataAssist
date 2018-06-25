{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language LambdaCase #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
-- import System.IO
import Graphics.Vty (emptyImage)
import qualified Graphics.Vty as Vty

-------------------------------------------------------------------------------
-- Main - set up network and handle input
-------------------------------------------------------------------------------

main :: IO ()
main = do
  vty <- Vty.mkVty =<< Vty.standardIOConfig

  -- Create inputs to network
  -- 1. Key presses
  --
  (charAddHandler, fireChar) :: (AddHandler Char, Char -> IO ()) <-
    newAddHandler

  network :: EventNetwork <-
    compile $ mdo
      eInput :: Event Char <-
        fromAddHandler charAddHandler


      -- eNextTurn' :: Event () <- mapEventIO pure eNextTurn


      {-
                          | Game  | Game' |
      | d |               | Enter | ()    |
      -}

      bGameState :: Behavior GameState <-
        prismataAssist eInput


      -- apply :: Behavior (a -> b) -> Event a -> Event b
      --
      -- have :: Behavior Gold
      -- want :: Behavior (String -> Gold)
      --
      -- changes :: Behavior a -> MomentIO (Event (Future a))
      eFutureGameState :: Event (Future GameState) <-
        changes bGameState
      -- imposeChanges :: Behavior a -> Event () -> Behavior a
      --
      --
      -- have :: Gold
      -- want :: String -> Gold
      -- f gold = \string -> gold
      -- let
      --   eGold :: Event Gold
      --   eGold = apply ((\gold _ -> gold) <$> bGold) eInput

      -- Outputs go down here
      -- valueB :: MonadMoment m => Behavior a -> m a
      state0 :: GameState <- valueB bGameState
      liftIO ((Vty.update vty . Vty.picForImage . renderGame) state0)

      -- reactimate' :: Event (Future (IO ())) -> MomentIO ()
      reactimate'
        ((fmap . fmap)
          (Vty.update vty . Vty.picForImage . renderGame)
          eFutureGameState)
      -- we have: Event [Char]
      -- we want: Event (IO ())
      -- reactimate (print <$> eGold)
      -- reactimate :: Event (IO ()) -> MomentIO ()
      -- reactimate (Vty.update vty . Vty.picForImage . renderGame <$> bGameState <@ eNextTurn')
      --
      -- fmap :: (a -> b)     -> (f a     -> f b)
      --
      -- fmap :: (f a -> f b) -> (g (f a) -> g (f a))

      -- fmap.fmap.fmap :: (a -> b) -> (f (g (h a


  actuate network

  -- hSetBuffering stdin NoBuffering
  -- hSetEcho stdin False

  let loop :: IO ()
      loop = do
        e <- Vty.nextEvent vty
        case e of
          Vty.EvKey (Vty.KChar c) _ ->
            case c of
              'q' -> pure ()
              _ -> do
                fireChar c
                loop
          Vty.EvKey Vty.KEnter _ -> do
            fireChar '\n'
            loop
          _ -> loop

  loop
  Vty.shutdown vty

  -- Define network
  -- fromAddHandler :: AddHandler a -> MomentIO (Event a)
  -- compile :: MomentIO () -> IO EventNetwork
  --
  -- reactimate :: Event (IO ()) -> MomentIO ()
  -- actuate :: EventNetwork -> IO ()

  -- Actuate network

  -- Feed inputs into network
  -- (Listen for key presses)

-------------------------------------------------------------------------------
-- Game logic - data types and pure functions
-------------------------------------------------------------------------------

newtype Gold
  = Gold { unGold :: Int }
  deriving (Show, Ord, Eq, Num)

newtype Green
  = Green { unGreen :: Int }
  deriving (Show, Ord, Eq, Num)

newtype Red
  = Red { unRed :: Int }
  deriving (Show, Ord, Eq, Num)

newtype Blue
  = Blue { unBlue :: Int }
  deriving (Show, Ord, Eq, Num)

newtype Energy
  = Energy { unEnergy :: Int }
  deriving (Show, Ord, Eq, Num)

data GameState = GameState
  { gameTurn :: Int
  , gameGold :: Gold
  , gameGreen :: Green
  , gameGreenRate :: Green
  , gameBlue :: Blue
  , gameBlueRate :: Blue
  , gameRed :: Red
  , gameRedRate :: Red
  , gameEnergy :: Energy
  , gameNumDrones :: Int
  , gameNumEngineers :: Int
  , gameNumConduits :: Int
  , gameNumBlastforges :: Int
  , gameNumAnimus :: Int
  , gameNumForcefields :: Int
  , gameNumGaussCannons :: Int
  , gameNumWalls :: Int
  , gameNumSteelSplitters :: Int
  , gameNumTarsiers :: Int
  , gameNumRhinos :: Int
  , gameDronesBuilding :: Int
  , gameEngineersBuilding :: Int
  , gameConduitsBuilding :: Int
  , gameBlastforgesBuilding :: Int
  , gameAnimusBuilding :: Int
  , gameForcefieldsBuilding :: Int
  , gameGaussCannonsBuilding :: Int
  , gameWallsBuilding :: Int
  , gameSteelSplittersBuilding :: Int
  , gameTarsiersBuilding :: Int
  , gameRhinosBuilding :: Int
  } deriving Show

data Unit
  = Animus
  | Blastforge
  | Conduit
  | Drone
  | Engineer
  | Forcefield
  | GaussCannon
  | Rhino
  | SteelSplitter
  | Tarsier
  | Wall
  deriving (Show, Eq)

unitGoldCost :: Unit -> Gold
unitGoldCost = \case
  Animus -> 6
  Blastforge -> 5
  Conduit -> 4
  Drone -> 3
  Engineer -> 2
  Forcefield -> 1
  GaussCannon -> 6
  Rhino -> 5
  SteelSplitter -> 6
  Tarsier -> 4
  Wall -> 5

unitGreenCost :: Unit -> Green
unitGreenCost = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 0
  Drone -> 0
  Engineer -> 0
  Forcefield -> 1
  GaussCannon -> 1
  Rhino -> 0
  SteelSplitter -> 0
  Tarsier -> 0
  Wall -> 0

unitBlueCost :: Unit -> Blue
unitBlueCost  = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 0
  Drone -> 0
  Engineer -> 0
  Forcefield -> 0
  GaussCannon -> 0
  Rhino -> 0
  SteelSplitter -> 1
  Tarsier -> 0
  Wall -> 1

unitRedCost :: Unit -> Red
unitRedCost = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 0
  Drone -> 0
  Engineer -> 0
  Forcefield -> 0
  GaussCannon -> 0
  Rhino -> 1
  SteelSplitter -> 0
  Tarsier -> 1
  Wall -> 0

unitEnergyCost :: Unit -> Energy
unitEnergyCost = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 0
  Drone -> 1
  Engineer -> 0
  Forcefield -> 0
  GaussCannon -> 0
  Rhino -> 0
  SteelSplitter -> 0
  Tarsier -> 0
  Wall -> 0

unitDroneCost :: Unit -> Int
unitDroneCost = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 0
  Drone -> 0
  Engineer -> 0
  Forcefield -> 1
  GaussCannon -> 0
  Rhino -> 0
  SteelSplitter -> 0
  Tarsier -> 0
  Wall -> 0

canBuyUnit :: Unit -> Gold -> Green -> Blue -> Red -> Energy -> Int -> Bool
canBuyUnit x g gr b r e d = and
  [ g >= unitGoldCost x
  , gr >= unitGreenCost x
  , b >= unitBlueCost x
  , r >= unitRedCost x
  , e >= unitEnergyCost x
  , d >= unitDroneCost x
  ]

unitGoldRate :: Unit -> Int
unitGoldRate = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 0
  Drone -> 1
  Engineer -> 0
  Forcefield -> 0
  GaussCannon -> 0
  Rhino -> 0
  SteelSplitter -> 0
  Tarsier -> 0
  Wall -> 0

unitGreenRate :: Unit -> Green
unitGreenRate = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 1
  Drone -> 0
  Engineer -> 0
  Forcefield -> 0
  GaussCannon -> 0
  Rhino -> 0
  SteelSplitter -> 0
  Tarsier -> 0
  Wall -> 0

unitBlueRate :: Unit -> Blue
unitBlueRate = \case
  Animus -> 0
  Blastforge -> 1
  Conduit -> 0
  Drone -> 0
  Engineer -> 0
  Forcefield -> 0
  GaussCannon -> 0
  Rhino -> 0
  SteelSplitter -> 0
  Tarsier -> 0
  Wall -> 0

unitRedRate :: Unit -> Red
unitRedRate = \case
  Animus -> 1
  Blastforge -> 0
  Conduit -> 0
  Drone -> 0
  Engineer -> 0
  Forcefield -> 0
  GaussCannon -> 0
  Rhino -> 0
  SteelSplitter -> 0
  Tarsier -> 0
  Wall -> 0

tail1 :: [a] -> [a]
tail1 [] = []
tail1 [x] = [x]
tail1 (_:xs) = xs

-------------------------------------------------------------------------------
-- Game logic - network
-------------------------------------------------------------------------------

prismataAssist :: Event Char -> MomentIO (Behavior GameState)
prismataAssist eInput = mdo
  -- Logic goes here
  -- filterE :: (a -> Bool) -> Event a -> Event a
  (eNextTurn2 :: Event (), fireNextTurn2 :: () -> IO ()) <-
    newEvent

  let
    eNextTurn :: Event ()
    eNextTurn = () <$ filterE (== '\n') eInput

  let
    eUndoTurn :: Event ()
    eUndoTurn = () <$ filterE (== 'u') eInput

  let
    eBuyUnit :: Event Unit
    eBuyUnit = filterApply bCanBuy (filterJust (charToUnit <$> eInput))
      where
        charToUnit :: Char -> Maybe Unit
        charToUnit = \case
          'a' -> Just Animus
          'b' -> Just Blastforge
          'c' -> Just Conduit
          'd' -> Just Drone
          'e' -> Just Engineer
          'f' -> Just Forcefield
          'g' -> Just GaussCannon
          'r' -> Just Rhino
          's' -> Just SteelSplitter
          't' -> Just Tarsier
          'w' -> Just Wall
          _ -> Nothing

  let
    eBuyDrone :: Event Unit
    eBuyDrone = filterE (== Drone) eBuyUnit

    eBuyEngineer :: Event Unit
    eBuyEngineer = filterE (== Engineer) eBuyUnit

    eBuyConduit :: Event Unit
    eBuyConduit = filterE (== Conduit) eBuyUnit

    eBuyBlastforge :: Event Unit
    eBuyBlastforge = filterE (== Blastforge) eBuyUnit

    eBuyAnimus :: Event Unit
    eBuyAnimus = filterE (== Animus) eBuyUnit

    eBuyForcefield :: Event Unit
    eBuyForcefield = filterE (== Forcefield) eBuyUnit

    eBuyGaussCannon :: Event Unit
    eBuyGaussCannon = filterE (== GaussCannon) eBuyUnit

    eBuyWall :: Event Unit
    eBuyWall = filterE (== Wall) eBuyUnit

    eBuySteelSplitter :: Event Unit
    eBuySteelSplitter = filterE (== SteelSplitter) eBuyUnit

    eBuyTarsier :: Event Unit
    eBuyTarsier = filterE (== Tarsier) eBuyUnit

    eBuyRhino :: Event Unit
    eBuyRhino = filterE (== Rhino) eBuyUnit

  -----------------------------------------------------------------------------
  -- Behaviors
  -----------------------------------------------------------------------------

  let
    bCanBuy :: Behavior (Unit -> Bool)
    bCanBuy =
      pure (\gold green blue red energy drones unit ->
        canBuyUnit unit gold green blue red energy drones)
      <*> bGold
      <*> bGreen
      <*> bBlue
      <*> bRed
      <*> bEnergy
      <*> bNumDrones


    -- (<$) ::          a -> Event b -> Event a
    -- (<@) :: Behavior a -> Event b -> Event a

  let
    undo :: (GameState -> a) -> Event (b -> a)
    undo f = (\(gs:_) _ -> f gs) <$> bHistory <@ eUndoTurn

  bTurn :: Behavior Int <-
    accumB 1
      (unions
        [ (+1) <$ eNextTurn
        , undo gameTurn
        ])

  -- accumB :: Gold -> Event (Gold -> Gold) -> Moment (Behavior Gold)
  bGold :: Behavior Gold <-
    accumB 6
      (unions
        [ (\x y z -> Gold (x + y) + z) <$> bNumDrones <*> bDronesBuilding
                                <@ eNextTurn
        , (\u g -> g - unitGoldCost u) <$> eBuyUnit
        , undo gameGold
        ])

  bGreenRate :: Behavior Green <-
    accumB 0
      (unions
        [ (\u gr -> gr + unitGreenRate u) <$> eBuyUnit
        , undo gameGreenRate
        ])

  bGreen :: Behavior Green <-
    accumB 0
      (unions
        [ (+) <$> bGreenRate <@ eNextTurn
        , (\u gr -> gr - unitGreenCost u) <$> eBuyUnit
        , undo gameGreen
        ])

  bBlueRate :: Behavior Blue <-
    accumB 0
      (unions
        [ (\u b -> b + unitBlueRate u) <$> eBuyUnit
        , undo gameBlueRate
        ])

  bBlue :: Behavior Blue <-
    accumB 0
      (unions
        [ (\n _ -> n) <$> bBlueRate <@ eNextTurn
        , (\u b -> b - unitBlueCost u) <$> eBuyUnit
        , undo gameBlue
        ])

  bRedRate :: Behavior Red <-
    accumB 0
      (unions
        [ (\u r -> r + unitRedRate u) <$> eBuyUnit
        , undo gameRedRate
        ])

  bRed :: Behavior Red <-
    accumB 0
      (unions
        [ (\n _ -> n * 2) <$> bRedRate <@ eNextTurn
        , (\u r -> r - unitRedCost u) <$> eBuyUnit
        , undo gameRed
        ])

  let
    bEnergyRate :: Behavior Energy
    bEnergyRate = Energy <$> bNumEngineers

  bEnergy :: Behavior Energy <-
    accumB 2
      (unions
        [ (\n _ -> n) <$> bEnergyRate <@ eNextTurn
        , (\u e -> e - unitEnergyCost u) <$> eBuyUnit
        , undo gameEnergy
        ])

  bNumDrones :: Behavior Int <-
    accumB 6
      (unions
        [ (+) <$> bDronesBuilding <@ eNextTurn
        , (\u d -> d - unitDroneCost u) <$> eBuyUnit
        , undo gameNumDrones
        ])

  bDronesBuilding :: Behavior Int <-
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuyDrone
        , undo gameDronesBuilding
        ])

  bNumEngineers :: Behavior Int <-
    accumB 2
      (unions
        [ (+) <$> bEngineersBuilding <@ eNextTurn
        , undo gameNumEngineers
        ])

  bEngineersBuilding :: Behavior Int <-
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuyEngineer
        , undo gameEngineersBuilding
        ])

  bNumConduits :: Behavior Int <-
    accumB 0
      (unions
        [ (+) <$> bConduitsBuilding <@ eNextTurn
        , undo gameNumConduits
        ])

  bConduitsBuilding :: Behavior Int <-
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuyConduit
        , undo gameConduitsBuilding
        ])

  bNumBlastforges :: Behavior Int <-
    accumB 0
      (unions
        [ (+) <$> bBlastforgesBuilding <@ eNextTurn
        , undo gameNumBlastforges
        ])

  bBlastforgesBuilding :: Behavior Int <-
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuyBlastforge
        , undo gameBlastforgesBuilding
        ])

  bNumAnimus :: Behavior Int <-
    accumB 0
      (unions
        [ (+) <$> bAnimusBuilding <@ eNextTurn
        , undo gameNumAnimus
        ])

  bAnimusBuilding :: Behavior Int <-
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuyAnimus
        , undo gameAnimusBuilding
        ])

  bNumForcefields :: Behavior Int <-
    accumB 0
      (unions
        [ (+) <$> bForcefieldsBuilding <@ eNextTurn
        , undo gameNumForcefields
        ])

  bForcefieldsBuilding :: Behavior Int <-
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuyForcefield
        , undo gameForcefieldsBuilding
        ])

  bNumGaussCannons :: Behavior Int <-
    accumB 0
      (unions
        [ (+) <$> bGaussCannonsBuilding <@ eNextTurn
        , undo gameNumGaussCannons
        ])

  bGaussCannonsBuilding :: Behavior Int <-
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuyGaussCannon
        , undo gameGaussCannonsBuilding
        ])

  bNumWalls :: Behavior Int <-
    accumB 0
      (unions
        [ (+) <$> bWallsBuilding <@ eNextTurn
        , undo gameNumWalls
        ])

  bWallsBuilding :: Behavior Int <-
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuyWall
        , undo gameWallsBuilding
        ])

  bNumSteelSplitters :: Behavior Int <-
    accumB 0
      (unions
        [ (+) <$> bSteelSplittersBuilding <@ eNextTurn
        , undo gameNumSteelSplitters
        ])

  bSteelSplittersBuilding :: Behavior Int <-
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuySteelSplitter
        , undo gameSteelSplittersBuilding
        ])

  bNumTarsiers :: Behavior Int <-
    accumB 0
      (unions
        [ (+) <$> bTarsiersBuilding <@ eNextTurn
        , undo gameNumTarsiers
        ])

  bTarsiersBuilding :: Behavior Int <-
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuyTarsier
        , undo gameTarsiersBuilding
        ])

  bNumRhinos :: Behavior Int <-
    accumB 0
      (unions
        [ (+) <$> bRhinosBuilding <@ eNextTurn
        , undo gameNumRhinos
        ])

  bRhinosBuilding :: Behavior Int <-
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuyRhino
        , undo gameRhinosBuilding
        ])

  -- Time-varying entire game state
  bHistory :: Behavior [GameState] <- do
    gs0 <- valueBLater bGameState
    accumB [gs0]
      (unions
        [ (:) <$> bGameState <@ eNextTurn2
        , tail1 <$ eUndoTurn
        ])

  let
    bGameState :: Behavior GameState
    bGameState = pure GameState
                  <*> bTurn
                  <*> bGold
                  <*> bGreen
                  <*> bGreenRate
                  <*> bBlue
                  <*> bBlueRate
                  <*> bRed
                  <*> bRedRate
                  <*> bEnergy
                  <*> bNumDrones
                  <*> bNumEngineers
                  <*> bNumConduits
                  <*> bNumBlastforges
                  <*> bNumAnimus
                  <*> bNumForcefields
                  <*> bNumGaussCannons
                  <*> bNumWalls
                  <*> bNumSteelSplitters
                  <*> bNumTarsiers
                  <*> bNumRhinos
                  <*> bDronesBuilding
                  <*> bEngineersBuilding
                  <*> bConduitsBuilding
                  <*> bBlastforgesBuilding
                  <*> bAnimusBuilding
                  <*> bForcefieldsBuilding
                  <*> bGaussCannonsBuilding
                  <*> bWallsBuilding
                  <*> bSteelSplittersBuilding
                  <*> bTarsiersBuilding
                  <*> bRhinosBuilding

  reactimate (fireNextTurn2 <$> eNextTurn)

  {-
  bGameState :: Behavior GameState <-
    switchB bGameState0 ((bLastGameState <$ eUndoTurn))
  -}
  pure bGameState







-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

renderGame :: GameState -> Vty.Image
renderGame GameState{..} =
  str ("[[ Turn " ++ show gameTurn ++ " ]]")
  +-+
  str ""
  +-+
  str "Gold: " +|+ bstr (show (unGold gameGold))
  +-+
  str "Green: " +|+ bstr (show (unGreen gameGreen))
  +-+
  str "Blue: " +|+ bstr (show (unBlue gameBlue))
  +-+
  str "Red: " +|+ bstr (show (unRed gameRed))
  +-+
  str "Energy: " +|+ bstr (show (unEnergy gameEnergy))
  +-+
  str ""
  +-+
  unitImage "Drones: " gameNumDrones gameDronesBuilding
  +-+
  unitImage "Engineers: " gameNumEngineers gameEngineersBuilding
  +-+
  unitImage' "Conduits: " gameNumConduits gameConduitsBuilding
  +-+
  unitImage' "Blastforges: " gameNumBlastforges gameBlastforgesBuilding
  +-+
  unitImage' "Animus: " gameNumAnimus gameAnimusBuilding
  +-+
  unitImage' "Forcefield: " gameNumForcefields gameForcefieldsBuilding
  +-+
  unitImage' "GaussCannon: " gameNumGaussCannons gameGaussCannonsBuilding
  +-+
  unitImage' "Wall: " gameNumWalls gameWallsBuilding
  +-+
  unitImage' "SteelSplitter: " gameNumSteelSplitters gameSteelSplittersBuilding
  +-+
  unitImage' "Tarsier: " gameNumTarsiers gameTarsiersBuilding
  +-+
  unitImage' "Rhino: " gameNumRhinos gameRhinosBuilding
 where
  str :: String -> Vty.Image
  str = Vty.string Vty.defAttr

  bstr :: String -> Vty.Image
  bstr = Vty.string (Vty.withStyle Vty.defAttr Vty.bold)

  numBuilding :: Int -> Vty.Image
  numBuilding 0 = emptyImage
  numBuilding n = str (" (+" ++ show n ++ ")")

  unitImage :: [Char] -> Int -> Int -> Vty.Image
  unitImage u n b = str u +|+ bstr (show n) +|+ numBuilding b

  unitImage' :: [Char] -> Int -> Int -> Vty.Image
  unitImage' u n b =
    if n > 0 || b > 0
      then unitImage u n b
      else emptyImage

(+-+) :: Vty.Image -> Vty.Image -> Vty.Image
(+-+) = (Vty.<->)
infixr 4 +-+

(+|+) :: Vty.Image -> Vty.Image -> Vty.Image
(+|+) = (Vty.<|>)
infixr 5 +|+



