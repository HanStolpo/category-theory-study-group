module Main where

class Contravariant f where
    contramap :: (b -> a) -> f a -> f b

newtype Op r a = Op (a -> r)
runOp (Op f) = f

instance Contravariant (Op r) where
    contramap f (Op g) = Op (g . f)


op :: Op Bool Int
op = Op (\x -> x > 0)

f :: String -> Int
f x = read x

alpha :: Op Bool a -> Op String a
alpha (Op f) = Op (const "2")

main = do
  putStrLn $ "(contramap f . alpha) \"\" == 2" ++
    show (runOp((contramap f . alpha) op) "")
  putStrLn $ "(alpha . contramap f) \"\" == 2" ++
    show (runOp((alpha . contramap f ) op) "")
  putStrLn $ "contramap f . alpha  = alpha . contramap f " ++
    show ((runOp((contramap f . alpha) op) "") == (runOp((alpha . contramap f ) op) "") )
