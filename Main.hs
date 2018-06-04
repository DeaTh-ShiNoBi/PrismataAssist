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

      -- Logic goes here
      -- filterE :: (a -> Bool) -> Event a -> Event a 
      let
        eNextTurn :: Event ()
        eNextTurn = () <$ filterE (== '\n') eInput

        eBuyDrone :: Event ()
        eBuyDrone = 
          whenE bCanBuyDrone (() <$ filterE (== 'd') eInput)
         where
          bCanBuyDrone :: Behavior Bool
          bCanBuyDrone = pure (\g e -> g >= 3 && e >= 1)
                            <*> bGold
                            <*> bEnergy

        -- (<$) :: a -> Event ??? -> Event a

      bNumDrones :: Behavior Int <-
        accumB 6 
          (unions
            [ (+1) <$ eBuyDrone
            ])

      -- accumB :: Gold -> Event (Gold -> Gold) -> Moment (Behavior Gold)
      bGold :: Behavior Gold <-
        accumB 6 
          (unions
            [ (+) <$> bNumDrones <@ eNextTurn
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
      let
        bGameState :: Behavior GameState
        bGameState = pure GameState 
                       <*> bGold 
                       <*> bNumDrones 
                       <*> bGreen
                       <*> bBlue
                       <*> bRed
                       <*> bEnergy

      eNextTurn' :: Event () <- mapEventIO pure eNextTurn
      
      
      {-
                          | Game  | Game' |
      | d |               | Enter | ()    |
      -}


      -- apply :: Behavior (a -> b) -> Event a -> Event b
      --
      -- have :: Behavior Gold
      -- want :: Behavior (String -> Gold)
      --
      -- changes :: Behavior a -> MomentIO (Event (Future a)) 
      -- eFutureGameState :: Event (Future GameState) <- 
      --   changes (imposeChanges bGameState eNextTurn)
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
      -- reactimate' ((fmap.fmap) print eFutureGameState)
      -- we have: Event [Char]
      -- we want: Event (IO ())
      -- reactimate (print <$> eGold)
      -- reactimate :: Event (IO ()) -> MomentIO ()
      reactimate (Vty.update vty . Vty.picForImage . renderGame <$> bGameState <@ eNextTurn')
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
renderGame gs = Vty.string Vty.defAttr (show gs)
