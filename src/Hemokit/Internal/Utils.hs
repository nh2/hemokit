module Hemokit.Internal.Utils
  ( withJustM
  , untilNothing
  , textBase64
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import           Data.Text (Text)
import qualified Data.Text.Encoding as T


-- | If the monad retuns a Just, runs the function on its contents.
-- Returns True if the action was executed.
withJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m Bool
withJustM act f = act >>= maybe (return False) (\x -> f x >> return True)


-- | Runs the monadic action as long as the producer returns Justs.
-- Returns True if the action was ever executed.
untilNothing :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m Bool
untilNothing act f = act `withJustM` (\x -> f x >> again)
  where
    again = untilNothing act f >> return () -- void would be nicer


-- | Base64-encodes a ByteString as Text.
--
-- This cannot fail since Base64 is ASCII.
textBase64 :: BS.ByteString -> Text
textBase64 bs = case T.decodeUtf8' (Base64.encode bs) of
  Left ex -> error $ "textBase64: BUG: base64 encoding cannot be encoded: " ++ show ex -- this cannot happen
  Right t -> t
