module ExampleCase where

example :: Int -> Int
example n =
  case n of
    1 -> error "hello"
    100 -> error "world"
    _ -> error "!"
