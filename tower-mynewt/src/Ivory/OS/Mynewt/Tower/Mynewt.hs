{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
module Ivory.OS.Mynewt.Tower.Mynewt
  ( compileTowerMynewt
  ) where

--------------------------------------------------------------------------------
import Control.Monad (forM_)

--------------------------------------------------------------------------------
import Ivory.Language
import qualified Ivory.Stdlib as Ivory
import Ivory.Tower (Tower, showUnique)
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Backend
import Ivory.Tower.Options
import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.ThreadCode

data EmitterImpl = EmitterImpl
  { emitterInit    :: forall eff. Ivory eff ()
  , emitterDeliver :: forall eff. Ivory eff ()
  , emitterUser    :: ModuleDef
  , emitterGen     :: ModuleDef
  }

--------------------------------------------------------------------------------
data MynewtBackend = MynewtBackend

--------------------------------------------------------------------------------
instance TowerBackend MynewtBackend where

  ------------------------------------------------------------------------------
  -- | Represent a callback procedure which, when supplied with a
  -- thread, gets encoded as an Ivory procedure inside a Tower
  -- handler.
  newtype TowerBackendCallback MynewtBackend a =
    MynewtCallback (forall s.
                     AST.Thread ->
                     (Def ('[ConstRef s a] ':-> ()), ModuleDef))

  ------------------------------------------------------------------------------
  -- | A function that can deliver emitted messages to handlers.
  newtype TowerBackendEmitter MynewtBackend =
    MynewtEmitter (Maybe (AST.Monitor -> AST.Thread -> EmitterImpl))

  ------------------------------------------------------------------------------
  -- | A type that can eventually be used to generate a monitor method
  -- (handler procedure).
  data TowerBackendHandler MynewtBackend a =
    MynewtHandler AST.Handler
                  (forall s. AST.Monitor ->
                             AST.Thread  ->
                             (Def ('[ConstRef s a] ':-> ()), ThreadCode))

  ------------------------------------------------------------------------------
  -- | Convert a Tower callback body into a 'MynewtCallback'.
  -- Callbacks should not call @return@ so we strip off that effect,
  -- making it a compile-time error.
  callbackImpl _ sym f = MynewtCallback $ \t ->
    let p = voidProc n (\r -> body $ noReturn (f r))
        n = showUnique sym ++ "_" ++ AST.threadName t
    in (p, incl p)

  ------------------------------------------------------------------------------
  -- | Create a procedure to deliver messages to handlers listening on
  -- an emitter, along with the code necessary to initialize and
  -- support an emitter.
  emitterImpl _ _   []       = (Emitter $ const (return ()), MynewtEmitter Nothing)
  emitterImpl _ ast handlers = (emitterCall, wrapper) where
    emitterCall = Emitter $ call_ (emitterProc_ ast)
    wrapper     = MynewtEmitter (Just $ mkEmmitterImpl ast handlers)

  ------------------------------------------------------------------------------
  handlerImpl _ ast emitters callbacks = undefined

  ------------------------------------------------------------------------------
  monitorImpl _ ast handlers mod = undefined

  ------------------------------------------------------------------------------
  towerImpl _ ast monitors = undefined

--------------------------------------------------------------------------------
-- | Create an emitter procedure with the given body.
emitterProc :: IvoryArea a
            => AST.Emitter
            -> (forall eff. ConstRef s a -> Ivory eff ())
            -> Def ('[ConstRef s a] ':-> ())
emitterProc ast f = voidProc (emitterProcName ast) (\r -> body (f r))

--------------------------------------------------------------------------------
emitterProcName :: AST.Emitter -> String
emitterProcName = showUnique . AST.emitter_name

--------------------------------------------------------------------------------
-- | Create an emitter procedure with a stubbed body.
emitterProc_ :: IvoryArea a => AST.Emitter -> Def ('[ConstRef s a] ':-> ())
emitterProc_ ast = emitterProc ast (const $ return ())

--------------------------------------------------------------------------------
-- | Create procedures and Ivory pieces for an emitter.
mkEmmitterImpl :: forall a. (IvoryArea a, IvoryZero a)
               => AST.Emitter
               -> [TowerBackendHandler MynewtBackend a]
               -> AST.Monitor
               -> AST.Thread
               -> EmitterImpl
mkEmmitterImpl ast hs m t = EmitterImpl initialize deliver user gen
  where
    -- Ivory procedure to initialize this emitter:
    initialize :: forall eff. Ivory eff ()
    initialize = do
      comment "Reset message count."
      store (addrOf messageCount) 0

    -- Ivory procedure to deliver messages to all listening handlers:
    deliver :: forall eff. Ivory eff ()
    deliver = do
      comment "Send each message to all of the listening handlers."
      mc <- deref (addrOf messageCount)
      forM_ (zip messages [0 ..]) $ \(m, index) ->
        Ivory.when (fromInteger index <? mc) $
          mapM_ (\p -> call_ p (constRef (addrOf m))) sinks

    -- Insert the call to the procedure that dispatches messages:
    user :: ModuleDef
    user = private (incl $ emitterProc ast (call_ dispatch))

    -- Insert the generated code and memory areas
    gen :: ModuleDef
    gen = do
      incl dispatch
      private $ mapM_ defMemArea messages
      private $ defMemArea messageCount

    -- The handlers that are listening to this emitter:
    sinks :: forall s. [Def ('[ConstRef s a] ':-> ())]
    sinks = [ fst $ h m t | MynewtHandler _ h <- hs]

    -- Maximum number of messages this emitter supports:
    maxMessages :: Integer
    maxMessages = AST.emitter_bound ast - 1

    -- A place to store the message count.
    messageCount :: MemArea ('Stored Uint32)
    messageCount = area (emitterID ++ "_message_count") Nothing

    -- A place to store all of the messages to be delivered.
    messages :: [MemArea a]
    messages = [ area (emitterID ++ "_message_" ++ show i) Nothing
               | i <- [0 .. maxMessages]
               ]

    -- A per-thread unique ID for this emitter:
    emitterID :: String
    emitterID = emitterProcName ast ++ "_" ++ AST.threadName t

    -- A procedure that emits a message by storing it in the next
    -- available memory slot (from the @messages@ list of addrs):
    dispatch :: Def ('[ConstRef s a] ':-> ())
    dispatch = voidProc (emitterID ++ "_emit") $ \msg -> body $ do
      mc <- deref (addrOf messageCount)
      Ivory.when (mc <=? fromInteger maxMessages) $ do
        store (addrOf messageCount) (mc + 1)
        msg' <- assign (messageAt mc)
        refCopy msg' msg

    -- Nasty way to find the address of a message memory area give its index.
    messageAt :: Uint32 -> Ref 'Global a
    messageAt idx = foldl aux dflt (zip messages [0..])
      where
      dflt = addrOf (messages !! 0) -- Should be impossible.
      aux basecase (msg, midx) =
        (fromInteger midx ==? idx) ? (addrOf msg, basecase)

--------------------------------------------------------------------------------
compileTowerMynewt :: (e -> fixme) -> (TOpts -> IO e) -> Tower e () -> IO ()
compileTowerMynewt = undefined
