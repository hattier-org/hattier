module ExampleCase where

data Foo = Foo Int String

example :: Foo -> Int
example foo =
  case xs of
    Just 0 name -> error "hello"
    Just 123 diffName -> error "cruel world"

exampleCons' :: [Int] -> Int
exampleCons' xs =
  case xs of
    (x' : x : xs) -> error "pls"
    (x : xs) -> error "work"
