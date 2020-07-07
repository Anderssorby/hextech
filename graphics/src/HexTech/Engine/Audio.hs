module HexTech.Engine.Audio where

import qualified SDL.Exception                 as SDL
import qualified SDL.Mixer                     as Mixer
import           Control.Monad.Reader
import           Control.Exception.Safe         ( MonadThrow
                                                , MonadCatch
                                                , catch
                                                )

import           HexTech.Resource               ( Resources(..) )
import           HexTech.Config                 ( Config(..) )
import           HexTech.Engine.Types           ( Logger(..) )

class (Monad m, MonadIO m) => Audio m where
  playGameMusic :: m ()
  stopGameMusic :: m ()
  --playJumpSfx :: m ()
  --playDuckSfx :: m ()
  --playPointSfx :: m ()
  --playBirdSfx :: m ()
  --playHurtSfx :: m ()
  --playLavaSfx :: m ()
  --playRockSfx :: m ()
  --playQuakeSfx :: m ()
  --playDeathSfx :: m ()
  --playRecoverSfx :: m ()
  --playStockSfx :: m ()
  lowerGameMusic :: m ()
  raiseGameMusic :: m ()

playGameMusic' :: (MonadReader Config m, MonadIO m) => m ()
playGameMusic' =
  asks (rGameMusic . cResources) >>= Mixer.playMusic Mixer.Forever

{-| Pauses or unpauses the music, but only if it is changing the state
-}
toggleMusic :: (MonadIO m, Logger m) => Bool -> m ()
toggleMusic shallPlay = do
  paused <- pausedMusic
  when (paused && shallPlay)         resumeMusic
  when (not paused && not shallPlay) pauseMusic


resumeMusic :: MonadIO m => m ()
resumeMusic = Mixer.resumeMusic

pauseMusic :: MonadIO m => m ()
pauseMusic = Mixer.pauseMusic

playingMusic :: MonadIO m => m Bool
playingMusic = Mixer.playingMusic

pausedMusic :: MonadIO m => m Bool
pausedMusic = Mixer.pausedMusic

stopGameMusic' :: MonadIO m => m ()
stopGameMusic' = Mixer.haltMusic

lowerGameMusic' :: MonadIO m => m ()
lowerGameMusic' = Mixer.setMusicVolume 16

raiseGameMusic' :: MonadIO m => m ()
raiseGameMusic' = Mixer.setMusicVolume 128

playChunk
  :: (MonadReader Config m, MonadIO m, MonadThrow m, MonadCatch m)
  => (Resources -> Mixer.Chunk)
  -> m ()
playChunk sfx = flip catch ignore $ asks (sfx . cResources) >>= Mixer.play
 where
  ignore :: Monad m => SDL.SDLException -> m ()
  ignore _ = return ()

--playJumpSfx', playDuckSfx', playPointSfx', playBirdSfx', playHurtSfx', playLavaSfx', playRockSfx', playQuakeSfx', playDeathSfx', playRecoverSfx', playStockSfx'
--  :: (MonadReader Config m, MonadIO m, MonadThrow m, MonadCatch m) => m ()
--playJumpSfx' = playChunk rJumpSfx
--playDuckSfx' = playChunk rDuckSfx
--playPointSfx' = playChunk rPointSfx
--playBirdSfx' = playChunk rBirdSfx
--playHurtSfx' = playChunk rHurtSfx
--playLavaSfx' = playChunk rLavaSfx
--playRockSfx' = playChunk rRockSfx
--playQuakeSfx' = playChunk rQuakeSfx
--playDeathSfx' = playChunk rDeathSfx
--playRecoverSfx' = playChunk rRecoverSfx
--playStockSfx' = playChunk rStockSfx
