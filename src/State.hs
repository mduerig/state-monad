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

push :: Int -> ST Stack Int
push x = ST $ \xs -> (x:xs, x)

peek :: ST Stack Int
peek = ST $ \(x:xs) -> (x:xs, x)

unOp :: (Int -> Int) -> ST Stack Int
unOp f = do
  x <- pop
  push (f x)

binOp :: (Int -> Int -> Int) -> ST Stack Int
binOp f = do
  x <- pop
  y <- pop
  push (f x y)

inc :: ST Stack Int
inc = unOp (+1)

dec :: ST Stack Int
dec = unOp ((-)1)

add :: ST Stack Int
add = binOp (+)

sub :: ST Stack Int
sub = binOp (-)

stateMain :: IO()
stateMain = do
  print (
    apply (
      do sub; add; inc)
        [1, 2, 3, 4, 5])
  print (
    apply (
      sub >> add >> inc)
        [1, 2, 3, 4, 5])
  print (
    apply (
      sub >>= \_ -> add >>= \_ -> inc)
        [1, 2, 3, 4, 5])        