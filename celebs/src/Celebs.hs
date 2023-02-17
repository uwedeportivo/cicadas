module Celebs (
    Vertex,
    DiGraph,
    knows,
    partition,
    peersAndNonPeers,
    findCelebsThru,
    findCelebs,
    exampleGraph
) where

import Data.Array ( Array, (!), array, bounds )

type Vertex = Int

type DiGraph = Array Vertex [Vertex]

knows :: DiGraph -> Vertex -> Vertex -> Bool
knows gr u v = elem u (gr!v)

partition :: Eq a => (a -> Bool) -> [a] -> ([a], [a])
partition p xs = foldr pf ([], []) xs
    where 
        pf x (ys, zs) = if (p x) then ((x:ys), zs) else (ys, (x:zs))

peersAndNonPeers :: DiGraph -> Vertex -> ([Vertex], [Vertex])
peersAndNonPeers gr u = partition (knows gr u) (gr!u)

findCelebsThru :: Vertex -> DiGraph -> [Vertex]
findCelebsThru u gr = if (null nprs) then (u:prs) else findCelebsThru (head nprs)  gr
    where
        (prs, nprs) = peersAndNonPeers gr u

findCelebs :: DiGraph -> [Vertex]
findCelebs gr = findCelebsThru u gr
   where
    u = (fst . bounds) gr

exampleGraph :: DiGraph
exampleGraph = array (1, 5) [(1, [2, 3, 4, 5]), (2, [1, 3, 4]), (3, [4]), (4, [3]), (5, [3, 4])]
