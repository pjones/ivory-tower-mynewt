{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

{-

This file is part of the package ivory-tower-mynewt. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at:

  git://git.devalot.com/ivory-tower-mynewt

No part of this package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Main
  ( main
  ) where

--------------------------------------------------------------------------------
import Ivory.Language
import Ivory.Tower
-- import Ivory.Tower.Config
import Ivory.OS.Mynewt.Tower

--------------------------------------------------------------------------------
blinky :: Tower e ()       
blinky = do
  (cin, cout) <- channel
  everySec <- period (Milliseconds 1000)
  
  monitor "stateMgmt" $ do
    ledlit <- stateInit "ledlit" (ival false)
  
    handler everySec "flipflop" $ do
      e <- emitter cin 1
  
      callback $ \_ -> do
        toggled <- iNot <$> deref ledlit
        store ledlit toggled
        emitV e toggled
        
  monitor "ledMgmt" $ do
    handler systemInit "initLED" $
      callback $ \_ ->
        undefined -- FIXME: mkGPIOPort
  
    handler cout "updateLED" $
      callback $ \incomming -> do
        requestedState <- deref incomming
        gpioWrite ledPin requestedState
  
--------------------------------------------------------------------------------
main :: IO ()
main = compileTowerMynewt id p blinky
  where p _ = undefined

--------------------------------------------------------------------------------
-- FIXME: Not yet implemented:
gpioWrite :: pin -> IBool -> Ivory eff ()
gpioWrite = undefined -- Should we mirror ivory-bsp-stm32/src/Ivory/BSP/STM32/Peripheral/GPIOF4/Peripheral.hs?
ledPin = undefined
