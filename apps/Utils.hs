module Utils
  ( withJustM
  , untilNothing
  ) where


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
