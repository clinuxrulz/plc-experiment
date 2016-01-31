{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Control.Monad.Codensity
import Control.Monad.State.Strict
import Control.Monad.State.Class
import qualified Data.List as L
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import Data.Foldable

data CircuitState = CircuitState {
  csNextIdent :: Int,
  csNodeMap :: IM.IntMap CircuitNode,
  csWireMap :: IM.IntMap Int
}

initCircuitState :: CircuitState
initCircuitState = CircuitState {
  csNextIdent = 0,
  csNodeMap = IM.empty,
  csWireMap = IM.empty
}

data CircuitNode = CircuitNode {
  cnIdent :: Int,
  cnType :: CircuitNodeType,
  cnName :: String
}

data CircuitNodeType
  = CntInput
  | CntOutput
  | CntFunction

newtype CircuitM a = CircuitM (Codensity (State CircuitState) a)

unCircuitM :: CircuitM a -> Codensity (State CircuitState) a
unCircuitM (CircuitM a) = a

instance Functor CircuitM where
  fmap f (CircuitM ma) = CircuitM $ fmap f ma

instance Applicative CircuitM where
  pure = CircuitM . pure
  (CircuitM f) <*> (CircuitM a) = CircuitM $ f <*> a

instance Monad CircuitM where
  return a = CircuitM $ return a
  (CircuitM ma) >>= f = CircuitM $ ma >>= (unCircuitM . f)

instance MonadState CircuitState CircuitM where
  get = CircuitM $ lift get
  put = CircuitM . lift . put

newtype DString = DString (String -> String)

instance Monoid DString where
  mempty = DString id
  mappend (DString f) (DString g) = DString (f . g)

makeDString :: String -> DString
makeDString str = DString (str ++)

runDString :: DString -> String
runDString (DString f) = f ""

allocIdent :: CircuitM Int
allocIdent = do
  cs <- get
  let ident = csNextIdent cs
  put $ cs { csNextIdent = ident + 1 }
  return ident

createInputNodeM :: String -> CircuitM Int
createInputNodeM name = do
  ident <- allocIdent
  let node =
        CircuitNode {
          cnIdent = ident,
          cnType = CntInput,
          cnName = name
        }
  modify (\cs -> cs { csNodeMap = IM.insert ident node (csNodeMap cs) })
  return ident

createFunctionNodeM :: String -> CircuitM Int
createFunctionNodeM name = do
  ident <- allocIdent
  let node =
        CircuitNode {
          cnIdent = ident,
          cnType = CntFunction,
          cnName = name
        }
  modify (\cs -> cs { csNodeMap = IM.insert ident node (csNodeMap cs) })
  return ident

wireNodeToNodeM :: Int -> Int -> CircuitM ()
wireNodeToNodeM node1 node2 = do
  modify (\cs -> cs { csWireMap = IM.insert node1 node2 (csWireMap cs) })

plus :: Int -> Int -> CircuitM Int
plus node1 node2 = do
  plusId <- createFunctionNodeM "+"
  wireNodeToNodeM node1 plusId
  wireNodeToNodeM node2 plusId
  return plusId

test :: CircuitM ()
test = do
  a <- createInputNodeM "a"
  b <- createInputNodeM "b"
  r <- a `plus` b
  return ()

newtype CircuitA a b = CircuitA (Kleisli CircuitM a b)

instance Category CircuitA where
  id = CircuitA id
  (CircuitA bc) . (CircuitA ab) = CircuitA (bc . ab)

instance Arrow CircuitA where
  arr = CircuitA . arr
  first (CircuitA bc) = CircuitA $ first bc
  second (CircuitA bc) = CircuitA $ second bc
  (CircuitA bc) *** (CircuitA de) = CircuitA $ bc *** de
  (CircuitA bc) &&& (CircuitA bd) = CircuitA $ bc &&& bd

instance Functor (CircuitA r) where
  fmap f ca = (arr f) . ca

instance Applicative (CircuitA r) where
  pure = arr . const
  cf <*> ca = (arr (\(f,a) -> f a)) . (cf &&& ca)

type CBool = Int
type CInt32 = Int

makeInputA :: String -> CircuitA () Int
makeInputA name = CircuitA $ Kleisli $ const $ createInputNodeM name

addInt32A :: CircuitA (CInt32,CInt32) CInt32
addInt32A = CircuitA $ Kleisli $
  (\(a,b) -> do
    plus <- createFunctionNodeM "Int32.+"
    wireNodeToNodeM a plus
    wireNodeToNodeM b plus
    return plus
  )

example1 :: CircuitA () CInt32
example1 = proc x -> do
  a <- makeInputA "a" -< x
  b <- makeInputA "b" -< x
  addInt32A -< (a,b)

renderCircuitA :: CircuitA () a -> String
renderCircuitA (CircuitA c) = renderCircuitM $ runKleisli c ()

renderCircuitM :: CircuitM a -> String
renderCircuitM c =
  let finalState = execState ((lowerCodensity . unCircuitM) c) initCircuitState
  in
  runDString $
    (makeDString "digraph G {\n") <>
    -- Nodes
    (
      fold $
        L.intersperse
          (makeDString "\n")
          (
            (\(CircuitNode { cnIdent = x, cnName = y }) ->
              makeDString ("  x" ++ (show x) ++ "[label=\"" ++ y ++ "\"];")
            ) <$> (toList $ csNodeMap finalState)
          )
    ) <> (makeDString "\n") <>
    -- Edges
    (
      fold $
        L.intersperse
          (makeDString "\n")
          (
            toList $
              IM.mapWithKey
                (\node1 node2 -> makeDString $ "  x" ++ (show node1) ++ " -> x" ++ (show node2) ++ ";")
                (csWireMap finalState)
          )
    ) <> (makeDString "\n") <>
    (makeDString "}\n")

main :: IO ()
main = do
  putStrLn $ renderCircuitA example1
  return ()
