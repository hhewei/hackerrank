{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (replicateM, foldM_)

data Tree = Node Int [Tree]
data Crumb = Crumb Int [Tree] [Tree]
type Zipper = (Tree, [Crumb])

node :: Int -> Tree
node v = Node v []

changeValue :: Int -> Zipper -> IO Zipper
changeValue v (Node _ children, bs) =
  return (Node v children, bs)

printValue :: Zipper -> IO Zipper
printValue z@(Node v _, _) = do
  print v
  return z

visitLeft :: Zipper -> IO Zipper
visitLeft (self, Crumb i (l:ls) rs : bs) =
  return (l, Crumb i ls (self:rs) : bs)

visitRight :: Zipper -> IO Zipper
visitRight (self, Crumb i ls (r:rs) : bs) =
  return (r, Crumb i (self:ls) rs : bs)

visitParent :: Zipper -> IO Zipper
visitParent (self, Crumb i ls rs : bs) =
  return (Node i ((reverse ls) ++ [self] ++ rs), bs)

visitChild :: Int -> Zipper -> IO Zipper
visitChild n (Node i children, bs) =
  let (ls, c:rs) = splitAt (n-1) children in
  return (c, Crumb i (reverse ls) rs : bs)

insertLeft :: Int -> Zipper -> IO Zipper
insertLeft v (c, Crumb i ls rs : bs) =
  return (c, Crumb i (node v : ls) rs : bs)

insertRight :: Int -> Zipper -> IO Zipper
insertRight v (c, Crumb i ls rs : bs) =
  return (c, Crumb i ls (node v : rs) : bs)

insertChild :: Int -> Zipper -> IO Zipper
insertChild v (Node i children, bs) =
  return (Node i (node v : children), bs)

delete :: Zipper -> IO Zipper
delete (_, Crumb i ls rs : bs) =
  return (Node i (reverse ls ++ rs), bs)

main :: IO ()
main = do
  q <- readLn
  monads <- replicateM q $ do
    line <- getLine
    return$ case words line of
      [ "change", v ] ->
        changeValue (read v)
      [ "print" ] ->
        printValue
      [ "visit", "left" ] ->
        visitLeft
      [ "visit", "right" ] ->
        visitRight
      [ "visit", "parent" ] ->
        visitParent
      [ "visit", "child", n ] ->
        visitChild (read n)
      [ "insert", "left", v ] ->
        insertLeft (read v)
      [ "insert", "right", v ] ->
        insertRight (read v)
      [ "insert", "child", v ] ->
        insertChild (read v)
      [ "delete" ] ->
        delete
      a ->
        fail$ "unparsed: " ++ line
  foldM_ (\z a -> a z) (node 0, []) monads
