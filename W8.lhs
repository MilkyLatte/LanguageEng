
1.2.1
There are 3 monad laws:
1. Left identity: return a >>= f = f a
2. Right identity: m >>= return  = m
3. Associativity: (m >>= f) >>= g = m >>= (\x -> f >>= g)

 data Maybe a = Just a
               | Nothing

>

 instance Monad Maybe where
    return x        = Just x
    Just x >>= f = f x
    Nothing >>= f   = Nothing


> applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
> applyMaybe Nothing f  = Nothing
> applyMaybe (Just a) f = f a

1.2.2

1. Left identity:
Just a:
   return a >>= f
{ def 'return' }
   Just a >>= f
{ def '>>=' }
   f a

2. Right identity
Nothing:
Nothing >>= return = Nothing
 { def '>>=' }
   Nothing
Just a:
   Just a >>= return = Just a
  { def '>>=' }
   return a
  { def 'return' }
   Just a

3. Associativity
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
Nothing:
   (Nothing >>= f) >>= g = Nothing >>= (\x -> f x >>= g)
  { def '>>=' }
    Nothing >>= g
  { def '>>=' }
    Nothing
  Right side:
  { def '>>=' }
    Nothing
Just a:
   (Just a >>= f) >>= g = Just a >>= (\x -> f x >>= g)
  { def '>>=' }
   f a >>= g
  Right Side:
  { def '>>=' }
   f a >>= g

1.2.4

> data Exception e a = Throw e | Continue a
>
> instance Functor (Exception e) where
>   fmap f (Throw x)   = Throw x
>   fmap f (Continue a) = Continue (f a)
>
> instance Applicative (Exception e) where
>   pure a = Continue a
>   _ <*> Throw e = Throw e
>   Continue f <*> Continue y = Continue (f y)
>   Throw e <*> Continue x    = Throw e
>
> instance Monad (Exception e) where
>  return a = Continue a
>  Throw e >>= _     = Throw e
>  Continue a >>= f  = f a

1.2.5
It is incorrect because an instance of a Monad requires kind
(* -> *) and the kind of Exception is (* -> * -> *)

1.3.1

> data Tree a = Leaf a | Fork (Tree a) (Tree a)
>
> instance Functor Tree where
>   fmap f (Leaf x)   = Leaf (f x)
>   fmap f (Fork a b) = Fork (fmap f a) (fmap f b)
>
> instance Applicative Tree where
>   pure a = Leaf a
>   (Leaf x)   <*> (Leaf y)   = Leaf (x y)
>   (Leaf f)   <*> (Fork x y) = f <$> (Fork x y)
>   (Fork x y) <*> (Leaf z)   = Fork (x <*> (Leaf z)) (y <*> (Leaf z))
>   (Fork x y) <*> (Fork a b) = Fork (Fork (x <*> a) (y <*> b)) (Fork (y <*> a) (x <*> b))
>
> instance Monad Tree where
>   return a = Leaf a
>   (Leaf x) >>= f = f x
>   (Fork x y) >>= f = Fork (x >>= f) (y >>= f)


Left identity:
  return a >>= f = f a
  { def 'return' }
    Leaf a >>= f
  { def '>>=' }
    f a

Right identity:
Leaf a:
  Leaf a >>= return  = Leaf a
  { def '>>=' }
    return a
  { def 'return' }
    Leaf a

Fork x y:
  (Fork x y) >>= return = Fork x y
  { def '>>=' }
    Fork (x >>= return) (y >>= return)
   
