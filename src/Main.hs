{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Main where

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

data Circuit a where
  CLitBool :: Bool -> Circuit Bool
  CLitInt :: Int -> Circuit Int
  CVar :: Int -> Circuit a
  COrInt :: Circuit Int -> Circuit Int -> Circuit Int
  CAndInt :: Circuit Int -> Circuit Int -> Circuit Int
  CNotInt :: Circuit Int -> Circuit Int
  COrBool :: Circuit Bool -> Circuit Bool -> Circuit Bool
  CAndBool :: Circuit Bool -> Circuit Bool -> Circuit Bool
  CNotBool :: Circuit Bool -> Circuit Bool

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

type CBool = Circuit Bool
type CInt = Circuit Int

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

createInputNode :: String -> CircuitM Int
createInputNode name = do
  ident <- allocIdent
  let node =
        CircuitNode {
          cnIdent = ident,
          cnType = CntInput,
          cnName = name
        }
  modify (\cs -> cs { csNodeMap = IM.insert ident node (csNodeMap cs) })
  return ident

createFunctionNode :: String -> CircuitM Int
createFunctionNode name = do
  ident <- allocIdent
  let node =
        CircuitNode {
          cnIdent = ident,
          cnType = CntFunction,
          cnName = name
        }
  modify (\cs -> cs { csNodeMap = IM.insert ident node (csNodeMap cs) })
  return ident

wireNodeToNode :: Int -> Int -> CircuitM ()
wireNodeToNode node1 node2 = do
  modify (\cs -> cs { csWireMap = IM.insert node1 node2 (csWireMap cs) })

plus :: Int -> Int -> CircuitM Int
plus node1 node2 = do
  plusId <- createFunctionNode "+"
  wireNodeToNode node1 plusId
  wireNodeToNode node2 plusId
  return plusId

test :: CircuitM ()
test = do
  a <- createInputNode "a"
  b <- createInputNode "b"
  r <- a `plus` b
  return ()

newtype CircuitA a b = CircuitA (Kleisli CircuitM a b)

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
  putStrLn $ renderCircuitM test
  return ()
