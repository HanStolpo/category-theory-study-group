{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Backend.SVG.CmdLine
-- import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.GraphViz
import Diagrams.Path
import Data.Maybe
import Control.Monad

import Data.GraphViz hiding (Path)
import Data.GraphViz.Commands
import qualified Data.Graph.Inductive.Graph as Graph

import qualified Debug.Trace as Debug

main = do
  print [(minBound :: Node)..maxBound]
  theGraph >>= defaultMain
  where
    theGraph :: IO (Diagram B)
    theGraph = drawGraph
        drawNode
        drawEdge
        <$> layedOutGraph
    layedOutGraph = layoutGraph' params Dot graph_
    params :: GraphvizParams Graph.Node Node String () Node
    params = defaultDiaParams

data Node
  = N_a
  | N_b
  | Fa
  | F'a
  | Fb
  | F'b
  | GFa
  | GF'a
  | GFb
  | GF'b
  | G'Fa
  | G'F'a
  | G'Fb
  | G'F'b
  deriving (Eq, Ord, Show, Bounded, Enum)

nodeToText :: Node -> String
nodeToText n =
  case n of
    N_a   -> "a"
    N_b   -> "b"
    Fa    -> "Fa"
    F'a   -> "F'a"
    Fb    -> "Fb"
    F'b   -> "F'b"
    GFa   -> "G(Fa)"
    GF'a  -> "G(F'a)"
    GFb   -> "G(Fb)"
    GF'b  -> "G(F'b)"
    G'Fa  -> "G'(Fa)"
    G'F'a -> "G'(F'a)"
    G'Fb  -> "G'(Fb)"
    G'F'b -> "G'(F'b)"

drawNode :: Node -> P2 Double -> Diagram B
drawNode n = place ((text (nodeToText n) # fontSize 6) `atop` (circle 19)) #
  case n of
    GFa -> lc red
    G'F'a -> lc red
    GFb -> lc blue
    G'F'b -> lc blue
    _ -> id

data Edge
  = E_f
  | F
  | F'
  | G
  | G'
  | E_a
  | E_Ga
  | E_G'a
  | E_b
  | E_Ff
  deriving (Eq, Ord, Show, Bounded, Enum)

-- edgeToText :: Edge -> String
-- edgeToText e =
--   case e of
--     E_f -> "⨍"
--     F   -> "F"
--     F'  -> "F'"
--     G   -> "G"
--     G'  -> "G'"
--     E_a -> "α"
--     E_Ga -> "Gα"
--     E_G'a -> "G'α"
--     E_b -> "β"
--     E_Ff ->  "F⨍"
--
drawEdge :: Node -> P2 Double -> Node -> P2 Double -> String -> Path V2 Double -> Diagram B
drawEdge src spos tgt tpos edg path =
  mconcat
    [ strokeP path
    , arrowBetween lastP bndry
    , place (text edg # fontSize 6) (onContinuousPath 0.5 path .+^ (perp ^* 8))
    ] #
  case (src, tgt) of
    (GFa, G'Fa) -> lc red
    (G'Fa, G'F'a) -> lc red
    (GFa, GF'a) -> lc red
    (GF'a, G'F'a) -> lc red
    ----
    (GFb, G'Fb) -> lc blue
    (G'Fb, G'F'b) -> lc blue
    (GFb, GF'b) -> lc blue
    (GF'b, G'F'b) -> lc blue
    ----
    (G'F'a, G'F'b) -> lc green
    (GFa, GFb) -> lc khaki
    _ -> id
  where
    lastP = last(last (pathPoints path)) -- (onContinuousPath 1 path)
    bndry = tpos .+^ envelopeV (normalize (lastP .-. tpos)) (circle 19 :: Diagram B)
    V2 tx ty = Tangent (mconcat (map unLoc (pathTrails path))) `atParam` 0.5
    perp = normalize (V2 (negate ty) tx)

onContinuousPath :: Double -> Path V2 Double -> Point V2 Double
onContinuousPath t ph =
  let
    trails_ = mconcat (map unLoc (pathTrails ph))
    firstP = fromMaybe (P (V2 0 0)) (join (listToMaybe <$> (listToMaybe (pathPoints ph))))
  in
    firstP .+^ trails_ `atParam` 0.5

graph_ =
  mkGraph
    [(minBound :: Node)..maxBound]
    [ (N_a  , Fa    , "F")
    , (N_a  , F'a   , "F'")
    , (Fa   , F'a   , "α_a")
    , (Fa   , GFa   , "G")
    , (Fa   , G'Fa  , "G'")
    , (F'a  , GF'a  , "G")
    , (F'a  , G'F'a , "G'")
    , (GFa  , G'Fa  , "β_Fa")
    , (GF'a , G'F'a , "β_F'a")
    , (GFa  , GF'a  , "G(α_a)")
    , (G'Fa , G'F'a , "G'(α_a)")
    ----
    , (N_b  , Fb    , "F")
    , (N_b  , F'b   , "F'")
    , (Fb   , F'b   , "α_b")
    , (Fb   , GFb   , "G")
    , (Fb   , G'Fb  , "G'")
    , (F'b  , GF'b  , "G")
    , (F'b  , G'F'b , "G'")
    , (GFb  , G'Fb  , "β_Fb")
    , (GF'b , G'F'b , "β_F'b")
    , (GFb  , GF'b  , "G(α_b)")
    , (G'Fb , G'F'b , "G'(α_b)")
    ---
    , (N_a, N_b, "⨍")
    , (Fa, Fb,"F⨍")
    , (GFa, GFb, "G(F⨍)")
    , (G'Fa, G'Fb, "G'(F⨍)")
    , (G'F'a, G'F'b, "G'(F'⨍)")
    ]
