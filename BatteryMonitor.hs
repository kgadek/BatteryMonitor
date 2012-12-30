module BatteryMonitor (batteryMonitorRun) where

import Control.Monad (void)
import qualified System.Libnotify as Notify
import System.Cmd
import Control.Concurrent (threadDelay)
import Control.Monad.Trans (liftIO)
import Text.Printf (printf)


showBatStatus :: Float -> IO ()
showBatStatus ratio = void $ Notify.oneShot "Battery status" (makeMsg ratio) "dialog-question" Nothing
  where makeMsg = printf "Procent: %.0f%%" . (*) 100

batteryAction :: [Float] -> IO ()
batteryAction (ratio:recent:_) | ratio <= 0.20 && recent > 0.20 = showBatStatus ratio
                               | ratio <= 0.15 && recent > 0.15 = showBatStatus ratio
                               | ratio <= 0.10 && recent > 0.10 = showBatStatus ratio
                               | ratio <= 0.05                  = void $ rawSystem "/home/konrad/bin/gsuspend" []
                               | otherwise                      = return ()
batteryAction (ratio:htail) = batteryAction (ratio:100.0:htail)

batteryMonitor :: Int -> Float -> [Float] -> IO ()
batteryMonitor timeout full history = do
  charge <- readFile "/sys/class/power_supply/BAT1/charge_now"
  nhist <- case reads charge of
            [(now,_)] -> let nhist = appendHist now in batteryAction nhist >> return nhist
            _         -> return history
  liftIO $ threadDelay timeout
  batteryMonitor timeout full nhist
    where appendHist x = take 5 $ (fromIntegral x / full):history

batteryMonitorRun :: Int -> IO ()
batteryMonitorRun timeout = do
  liftIO $ threadDelay timeout
  charge_full <- readFile "/sys/class/power_supply/BAT1/charge_full"
  case reads charge_full of
    [(full, _)] -> batteryMonitor timeout full []
    _           -> batteryMonitorRun timeout