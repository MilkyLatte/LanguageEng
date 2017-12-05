data State s a = State (s -> (s, a))

runState :: State s a -> s -> (s, a)
runState (State prog) = prog

execState :: State s a -> s -> s
execState prog = \s -> fst (runState prog s)

{-
addTwo :: State Int Int
addTwo = do s <- get


main :: IO a
main = do putStrLn "Hello World"
          s <- getLine
          return s

main = putStrLn "Hello World" >>= (\_ -> getLine >>= (\s -> return s))

getInt :: Db -> Maybe Int



(>>=) :: m a -> (a -> m b) -> m b

-}



evalState :: State s a -> s -> a
evalState prog = \s -> snd (runState prog s)

instance Functor (State s) where
  fmap f s = s >>= (return . f)

instance Applicative (State s) where
  pure x = State (\s -> (s, x))
  (State prog) <*> (State p) = State (\s -> (s, let (s', x ) = p s in (snd (prog s')) x ))

instance Monad (State s) where
  return x = State (\s -> (s, x))
  State p >>= f = State (\s -> let (s', x ) = p s in runState (f x) s')



get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put x = State (\_ -> (x ,()))

--1
add :: Int -> Int -> State Int Int
add x y = State (\s -> ((s + x), (x + y)))

--2
operationMlt :: Int -> Int -> State Int Int
operationMlt x y = State (\s -> (s + y, x * y))

--3

runOp :: Int -> Int -> (Int, Int)
runOp x y = runState (operationMlt x y) 0
