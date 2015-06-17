# A* / Dijkstra / BFS haskell package
This package contains standard implementation of A*. It is implemented genericaly so structure of the node should be implemented by user. 
Package containes quick generator of graph suitable for algorithm. This algorith is useful as Dijkstra algorithm. For this usage just inser heuristic function which always returns 0. In case you want to use it as BFS then in definition of edges in graph is important to insert 0 for each node's distance

## Installing
For correct usage if this package you must install following dependencies:
 
    git clone https://github.com/Lukx19/Astar-Dijkstra-BFS-Haskell
    cd Astar-Dijkstra-BFS-Haskell
    cabal configure
    cabal build
    cabal install
    cd ..
    

## Custom graph nodes
Algorithm is written genericaly. User needs to implement his own node data type. This data type should derive Ix class and Show class.
Node should also incloud all data needed for calculating heuristic function.

## Heuristics
Based on the usage of this algoritm user needs to implement his own heuristic function. If there is no need for heuristic program offers default function "heuristic" which always returns 0. heuristic function is called from whitin of algorithm with two parameters. First parameter is currently calculated node and secoond is goal node. User should extract all the data he needs from these nodes and calculate numeric value. 

## Graph generation
Graph might be constructed manually or with function newGraph.

    grafDIJ = Graph [0,1,2,3] (listArray (0,3) [[Edge 0 1 2 ,Edge 0 2 6],[Edge 1 2 1, Edge 1 3 5],[Edge 2 1 1, Edge 2 3 1],[]] )
    grafGEN = newGraph [0,1,2,3] [Edge 0 1 2 ,Edge 0 2 6, Edge 1 2 1, Edge 1 3 5,Edge 2 1 1, Edge 2 3 1]

## Algorith usage
Algortim receives from user constructed graph, starting node, goal node and heuristic function. It returns list of all nodes on the shortest path in correct order.
    
    ghci -package astar
    in ghci -> :module AStar

    aStar::Num key
      => Ord key 
      =>(Ix nd_t) 
      => Graph nd_t key         -- receives graph
      -> nd_t                   -- starting node
      -> nd_t                   -- goal node 
      -> (nd_t -> nd_t -> key)  -- heuristic function receiving two nodes and returning numeric value
      -> [nd_t]                 -- return list of nodes on the shortest path
    aStar grafGEN 0 3 heuristic

