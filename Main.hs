{-# language ScopedTypeVariables #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO

type Gold = Int

data GameState = GameState
  { gameGold :: Gold
  , gameNumDrones :: Int
  } deriving Show


main :: IO ()
main = do
  -- Create inputs to network
  -- 1. Key presses
  -- 
  (charAddHandler, fireChar) :: (AddHandler Char, Char -> IO ()) <- 
    newAddHandler 

  network :: EventNetwork <- 
    compile $ do
      eInput :: Event Char <- 
        fromAddHandler charAddHandler

      -- Logic goes here
      -- filterE :: (a -> Bool) -> Event a -> Event a 
      let
        eNextTurn :: Event ()
        eNextTurn = () <$ filterE (== '\n') eInput

        eBuyDrone :: Event ()
        eBuyDrone = () <$ filterE (== 'd') eInput

        -- (<$) :: a -> Event ??? -> Event a

      bNumDrones :: Behavior Int <-
        accumB 6 ((+1) <$ eBuyDrone)

      -- accumB :: Gold -> Event (Gold -> Gold) -> Moment (Behavior Gold)
      bGold :: Behavior Gold <-
        accumB 6 (apply ((\d () g -> g + d) <$> bNumDrones) eNextTurn)


      -- Time-varying entire game state
      let
        bGameState :: Behavior GameState
        bGameState = pure GameState <*> bGold <*> bNumDrones
      
      -- apply :: Behavior (a -> b) -> Event a -> Event b
      --
      -- have :: Behavior Gold
      -- want :: Behavior (String -> Gold)
      --
      -- changes :: Behavior a -> MomentIO (Event (Future a)) 
      eFutureGameState :: Event (Future GameState) <- 
        changes (imposeChanges bGameState eNextTurn)
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
      liftIO (print state0)
      -- reactimate' :: Event (Future (IO ())) -> MomentIO ()
      reactimate' ((fmap.fmap) print eFutureGameState)
      -- we have: Event [Char]
      -- we want: Event (IO ())
      -- reactimate (print <$> eGold)
      -- reactimate :: Event (IO ()) -> MomentIO ()
      --
      -- fmap :: (a -> b)     -> (f a     -> f b)
      --
      -- fmap :: (f a -> f b) -> (g (f a) -> g (f a))

      -- fmap.fmap.fmap :: (a -> b) -> (f (g (h a 


  actuate network

  hSetBuffering stdin NoBuffering
  -- hSetEcho stdin False

  let loop :: IO ()
      loop = do
        c <- getChar
        case c of
          'q' -> pure ()
          _ -> do 
            fireChar c
            loop

  loop
 
  -- Define network
  -- fromAddHandler :: AddHandler a -> MomentIO (Event a) 
  -- compile :: MomentIO () -> IO EventNetwork
  --
  -- reactimate :: Event (IO ()) -> MomentIO ()
  -- actuate :: EventNetwork -> IO ()

  -- Actuate network
 
  -- Feed inputs into network
  -- (Listen for key presses)
