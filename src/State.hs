module State (stateMain) where

import Control.Monad

newtype ST s a = ST {apply::s -> (s, a)}

instance Functor (ST s) where
  fmap = liftM

instance Applicative (ST s) where
  pure  = return
  (<*>) = ap

instance Monad (ST s) where
  return x = ST (\s -> (s, x))
  sa >>= f = ST $ \s ->
    let
      (ns, a) = apply sa s
    in
      apply (f a) ns

type Stack = [Int]

pop :: ST Stack Int
pop = ST $ \(x:xs) -> (xs, x)

push :: Int -> ST Stack ()
push x = ST $ \xs -> (x:xs, ())

pop2 :: ST Stack (Int, Int)
pop2 = do
  x <- pop
  y <- pop
  push 42
  return (x, y)

pop2' :: ST Stack (Int, Int)
pop2' = 
  pop >>= \x ->
  pop >>= \y ->
  push 42 >>
  return (x, y)

stateMain :: IO()
stateMain = do
  print (apply pop2 [1, 2, 3])
  print (apply pop2' [4, 5, 6])