{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language RecursiveDo #-}
{-# language LambdaCase #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
-- import System.IO
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

newtype Gold = Gold Int
  deriving (Show, Ord, Eq, Num)

newtype Green = Green Int
  deriving (Show, Ord, Eq, Num)

newtype Red = Red Int
  deriving (Show, Ord, Eq, Num)

newtype Blue = Blue Int
  deriving (Show, Ord, Eq, Num)

newtype Energy = Energy Int
  deriving (Show, Ord, Eq, Num)


data GameState = GameState
  { gameGold :: Gold
  , gameNumDrones :: Int
  , gameGreen :: Green
  , gameBlue :: Blue
  , gameRed :: Red
  , gameEnergy :: Energy
  , gameDronesBuilding :: Int
  } deriving Show

data Unit
  = Animus
  | Blastforge
  | Conduit
  | Drone
  | Engineer
  deriving (Show, Eq)

unitGoldCost :: Unit -> Gold
unitGoldCost = \case
  Animus -> 6
  Blastforge -> 5
  Conduit -> 4
  Drone -> 3
  Engineer -> 2

unitGreenCost :: Unit -> Green
unitGreenCost = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 0
  Drone -> 0
  Engineer -> 0

unitBlueCost :: Unit -> Blue
unitBlueCost  = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 0
  Drone -> 0
  Engineer -> 0

unitRedCost :: Unit -> Red
unitRedCost = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 0
  Drone -> 0
  Engineer -> 0

unitEnergyCost :: Unit -> Energy
unitEnergyCost = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 0
  Drone -> 1
  Engineer -> 0

canBuyUnit :: Unit -> Gold -> Green -> Blue -> Red -> Energy -> Bool
canBuyUnit x g gr b r e = and
  [ g >= unitGoldCost x
  , gr >= unitGreenCost x
  , b >= unitBlueCost x
  , r >= unitRedCost x
  , e >= unitEnergyCost x
  ]

unitGoldRate :: Unit -> Int
unitGoldRate = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 0
  Drone -> 1
  Engineer -> 0

unitGreenRate :: Unit -> Int
unitGreenRate = \case
  Animus -> 0
  Blastforge -> 0
  Conduit -> 1
  Drone -> 0
  Engineer -> 0

unitBlueRate :: Unit -> Int
unitBlueRate = \case
  Animus -> 0
  Blastforge -> 1
  Conduit -> 0
  Drone -> 0
  Engineer -> 0

unitRedRate :: Unit -> Int
unitRedRate = \case
  Animus -> 1
  Blastforge -> 0
  Conduit -> 0
  Drone -> 0
  Engineer -> 0

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

    eUndoTurn :: Event ()
    eUndoTurn = () <$ filterE (== 'u') eInput

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
          _ -> Nothing

    bCanBuy :: Behavior (Unit -> Bool)
    bCanBuy =
      pure (\gold green blue red energy unit -> 
        canBuyUnit unit gold green blue red energy)
      <*> bGold
      <*> bGreen
      <*> bBlue
      <*> bRed
      <*> bEnergy

    -- (<$) ::          a -> Event b -> Event a
    -- (<@) :: Behavior a -> Event b -> Event a

  let 
    undo :: (GameState -> a) -> Event (b -> a)
    undo f = (\gs _ -> f gs) <$> bLastGameState <@ eUndoTurn

  bNumDrones :: Behavior Int <-
    accumB 6 
      (unions
        [ (+) <$> bDronesBuilding <@ eNextTurn
        , undo gameNumDrones
        ])

  bDronesBuilding :: Behavior Int <- 
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (\u g -> g + unitGoldRate u) <$> eBuyUnit
        , undo gameDronesBuilding
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

  bGreen :: Behavior Green <- do
    bGreenRate :: Behavior Int <-
      accumB 0 
        (unions
          [ (\u gr -> gr + unitGreenRate u) <$> eBuyUnit
          ])

    accumB 0
      (unions
        [ (\x y -> Green x + y) <$> bGreenRate <@ eNextTurn
        , (\u gr -> gr - unitGreenCost u) <$> eBuyUnit
        , undo gameGreen
        ])

  bBlue :: Behavior Blue <- do
    bBlueRate :: Behavior Int <-
      accumB 0 
        (unions
          [ (\u b -> b + unitBlueRate u) <$> eBuyUnit
          ])
        
    accumB 0
      (unions
        [ (\n _ -> Blue n) <$> bBlueRate <@ eNextTurn
        , (\u b -> b - unitBlueCost u) <$> eBuyUnit
        , undo gameBlue
        ])

  bRed :: Behavior Red <- do
    bRedRate :: Behavior Int <-
      accumB 0 
        (unions
          [ (\u r -> r + unitRedRate u) <$> eBuyUnit
          ])
        
    accumB 0
      (unions
        [ (\n _ -> Red n * 2) <$> bRedRate <@ eNextTurn
        , (\u r -> r - unitRedCost u) <$> eBuyUnit
        , undo gameRed
        ])

  bEnergy :: Behavior Energy <- do
    bNumEngineers :: Behavior Int <- 
      accumB 2
        (unions
          [ (+1) <$ filterE (==Engineer) eBuyUnit
          ])

    accumB 2 
      (unions
        [ (\n _ -> Energy n) <$> bNumEngineers <@ eNextTurn
        , (\u e -> e - unitEnergyCost u) <$> eBuyUnit
        , undo gameEnergy
        ])

  {-
  eFoo
  bBar
  bBaz
       f                            :: Bar -> Baz -> Foo -> Whatever
  pure f                            :: Behavior (Bar -> Baz -> Foo -> Whatever)
  pure f <*> bBar                   :: Behavior (Baz -> Foo -> Whatever)
  pure f <*> bBar <*> bBaz          :: Behavior (Foo -> Whatever)
  pure f <*> bBar <*> bBaz <@> eFoo :: Event Whatever
  -}


  --                        *----------
  --
  --
  --            ------------*
  --
  --
  -- -----------*
  --
  --                        ^               ^
  --                        ^

  -- Time-varying entire game state
  bLastGameState :: Behavior GameState <- do
    gs <- valueBLater bGameState
    stepper gs (bGameState <@ eNextTurn2)
    -- (valueB bGameState0) >>= (flip stepper (bGameState <@ eNextTurn))

  let
    bGameState :: Behavior GameState
    bGameState = pure GameState 
                   <*> bGold 
                   <*> bNumDrones 
                   <*> bGreen
                   <*> bBlue
                   <*> bRed
                   <*> bEnergy
                   <*> bDronesBuilding

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
renderGame gs = 
  str ("Drones: " ++ show (gameNumDrones gs)
                  ++ if gameDronesBuilding gs > 0
                    then
                      " (+"
                      ++ show (gameDronesBuilding gs)
                      ++ ")"
                    else "")
  +-+ 
  str ("Gold: " ++ show (gameGold gs))
  +-+ 
  str ("Green: " ++ show (gameGreen gs))
  +-+ 
  str ("Blue: " ++ show (gameBlue gs))
  +-+ 
  str ("Red: " ++ show (gameRed gs))
  +-+ 
  str ("Energy: " ++ show (gameEnergy gs))
 where
  str = Vty.string Vty.defAttr

(+-+) :: Vty.Image -> Vty.Image -> Vty.Image
(+-+) = (Vty.<->)
infixr 4 +-+

(+|+) :: Vty.Image -> Vty.Image -> Vty.Image
(+|+) = (Vty.<|>)
infixr 5 +|+






