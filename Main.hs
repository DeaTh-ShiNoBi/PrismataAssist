{-# language ScopedTypeVariables #-}
{-# language RecursiveDo #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
-- import System.IO
import qualified Graphics.Vty as Vty

type Gold = Int

data GameState = GameState
  { gameGold :: Gold
  , gameNumDrones :: Int
  , gameGreen :: Int
  , gameBlue :: Int
  , gameRed :: Int
  , gameEnergy :: Int
  , gameDronesBuilding :: Int
  } deriving Show

data GameUnit
  = Drone
  | Engineer
  | Conduit
  | Blastforge
  | Animus
  deriving Show

{-
unitGoldCost :: GameUnit -> Gold
unitGreenCost :: GameUnit -> Int
unitEnergyCost :: GameUnit -> Int
-}

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

prismataAssist :: MonadMoment m => Event Char -> m (Behavior GameState)
prismataAssist eInput = mdo
  -- Logic goes here
  -- filterE :: (a -> Bool) -> Event a -> Event a 
  let
    eNextTurn :: Event ()
    eNextTurn = () <$ filterE (== '\n') eInput

    eUndoTurn :: Event ()
    eUndoTurn = () <$ filterE (== 'u') eInput

    eBuyDrone :: Event ()
    eBuyDrone = 
      whenE bCanBuyDrone (() <$ filterE (== 'd') eInput)
     where
      bCanBuyDrone :: Behavior Bool
      bCanBuyDrone = pure (\g e -> g >= 3 && e >= 1)
                        <*> bGold
                        <*> bEnergy

    -- (<$) ::          a -> Event b -> Event a
    -- (<@) :: Behavior a -> Event b -> Event a

  bNumDrones :: Behavior Int <-
    accumB 6 
      (unions
        [ (+) <$> bDronesBuilding <@ eNextTurn
        ])

  bDronesBuilding :: Behavior Int <- 
    accumB 0
      (unions
        [ (\_ -> 0) <$ eNextTurn
        , (+1) <$ eBuyDrone
        ])

  -- accumB :: Gold -> Event (Gold -> Gold) -> Moment (Behavior Gold)
  bGold :: Behavior Gold <-
    accumB 6 
      (unions
        [ (\x y z -> x + y + z) <$> bNumDrones <*> bDronesBuilding
                                <@ eNextTurn
        , subtract 3 <$ eBuyDrone
        , subtract 2 <$ eBuyEngineer
        , subtract 4 <$ eBuyConduit
        , subtract 5 <$ eBuyBlastforge
        , subtract 6 <$ eBuyAnimus
        ])

  let 
    eBuyConduit :: Event ()
    eBuyConduit = 
      whenE bCanBuyConduit (() <$ filterE (== 'c') eInput)
     where
      bCanBuyConduit :: Behavior Bool
      bCanBuyConduit = (>= 4) <$> bGold

  bGreen :: Behavior Int <- do
    bNumConduit :: Behavior Int <-
      accumB 0 
        (unions
          [ (+1) <$ eBuyConduit
          ])

    accumB 0
      (unions
        [ (+) <$> bNumConduit <@ eNextTurn
        ])

  let 
    eBuyBlastforge :: Event ()
    eBuyBlastforge = 
      whenE bCanBuyBlastforge (() <$ filterE (== 'b') eInput)
     where
      bCanBuyBlastforge :: Behavior Bool
      bCanBuyBlastforge = (>= 5) <$> bGold

  bBlue :: Behavior Int <- do
    bNumBlastforge :: Behavior Int <-
      accumB 0 
        (unions
          [ (+1) <$ eBuyBlastforge
          ])
        
    accumB 0
      (unions
        [ (\n _ -> n) <$> bNumBlastforge <@ eNextTurn
        ])

  let 
    eBuyAnimus :: Event ()
    eBuyAnimus = 
      whenE bCanBuyAnimus (() <$ filterE (== 'a') eInput)
     where
      bCanBuyAnimus :: Behavior Bool
      bCanBuyAnimus = (>= 6) <$> bGold

  bRed :: Behavior Int <- do
    bNumAnimus :: Behavior Int <-
      accumB 0 
        (unions
          [ (+1) <$ eBuyAnimus
          ])
        
    accumB 0
      (unions
        [ (\n _ -> n * 2) <$> bNumAnimus <@ eNextTurn
        ])

  let 
    eBuyEngineer :: Event ()
    eBuyEngineer = 
      whenE bCanBuyEngineer (() <$ filterE (== 'e') eInput)
     where
      bCanBuyEngineer :: Behavior Bool
      bCanBuyEngineer = (>= 2) <$> bGold

  bEnergy :: Behavior Int <- do
    bNumEngineers :: Behavior Int <- 
      accumB 2
        (unions
          [ ((+1) <$ eBuyEngineer)
          ])

    accumB 2 
      (unions
        [ (\n _ -> n) <$> bNumEngineers <@ eNextTurn
        , subtract 1 <$ eBuyDrone
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

  -- Time-varying entire game state
  bLastGameState :: Behavior GameState <- do
    gs <- valueBLater bGameState0
    stepper gs (bGameState <@ eNextTurn)
    -- (valueB bGameState0) >>= (flip stepper (bGameState <@ eNextTurn))

  let
    bGameState0 :: Behavior GameState
    bGameState0 = pure GameState 
                   <*> bGold 
                   <*> bNumDrones 
                   <*> bGreen
                   <*> bBlue
                   <*> bRed
                   <*> bEnergy
                   <*> bDronesBuilding

  bGameState :: Behavior GameState <-
    switchB bGameState0 ((bLastGameState <$ eUndoTurn))
  pure bGameState
