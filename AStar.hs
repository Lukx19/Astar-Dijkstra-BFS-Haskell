--
-- Copyright (c) 2015 Lukas Jelinek 
-- MIT license see http://opensource.org/licenses/MIT
--
import System.Environment
import Data.Array
import Data.Ix
import Data.Map
import Data.List (last,head,sort)
import Data.PQueue.Prio.Min as Queue

data Edge a k= Edge a a k deriving (Show)
data Graph a k= Graph [a] (Array a [Edge a k])  deriving(Show)
type PQueue k nd= MinPQueue k nd
type Heuristic k nd =  (nd -> nd -> k)
type Used k nd = Map nd k

-- | 'main' runs the main program
--main :: IO ()
--main = print "hello"

-- receives graph ,start position and goal position. Returns vertexes which lay on the path in the order based on heuristic function.
-- Minimalising cost.
--  graph for task
--  start node
--  goal node
--   heuristic function used for calculating heuristic at each node. heuristic function receives from algorith
--      current node and goal node and it should return numeric value
--  return: list of all nodes on the shortest path
aStar::Num key
      => Ord key
      =>(Ix nd_t) 
      => Graph nd_t key
      -> nd_t 
      -> nd_t 
      -> Heuristic key nd_t
      -> [nd_t]
aStar (Graph nodes edges) (start) (goal) heurist | inRange (rl,rr) goal && inRange (rl,rr) start 
                                                    = aStarCalc (Graph nodes edges) heurist goal queue used
                                                |otherwise = error "unreachable goal or start"
                                where
                                    (rl,rr) = (Data.List.head nodes,(Data.List.last nodes)) 
                                    used = Data.Map.singleton start (heurist start goal)
                                    queue = Queue.singleton (heurist start goal) start

--priority queue of fringe elements sorted by sum of distance and heuristic
--  graph for task
--  heuristic function used for calculating heuristic at each node. heuristic function receives from algorith
--    current node and goal node and it should return numeric value
--  goal node of graph which we want to reach
--  fringe used for algorith calculations
--  Map of already visited nodes with lowest overall cost      
--  return: shortest path in graph 
aStarCalc::Ix nd_t 
        => Num key
        => Ord key 
        => Graph nd_t key
        -> Heuristic key nd_t
        -> nd_t 
        ->PQueue key nd_t 
        -> Used key nd_t 
        -> [nd_t]
aStarCalc (Graph nodes edges) heurist goal fringe used  | (Queue.null fringe) == True = []                                                                                       
                                                        | nd == goal = [goal] 
                                                        | otherwise 
                                                        =nd:(aStarCalc (Graph nodes edges) heurist goal ch_fringe ch_used) 
                                                    where 
                                                        ((key,nd),sh_fringe) = Queue.deleteFindMin fringe   
                                                        neighb = edges Data.Array.! nd
                                                        (ch_fringe,ch_used) = addToFrindge neighb heurist goal sh_fringe used
                                                        
-- Receives list of edges (from to dist). The Nodes, where is edge heading are added to fringe if they have not been evaluated previously.
-- In case that node was already evaluated but with higher cost then new calculated than this node is added to fringe anyway and cost is updated.
--  list of edges with potetial to be added to fringe
--  heuristic fce
--  goal node
--  priority queue handling fringe. Sorted by cost- minimum first.
--  Map of already used nodes with corresponding calculated cost to reach them in previous runs
--  return: tuple (new extended fringe,new updated map of used nodes)    
addToFrindge::Ix nd_t 
            =>Num key
            => Ord key 
            =>[Edge nd_t key]
            ->Heuristic key nd_t 
            ->nd_t
            ->PQueue key nd_t 
            ->Used key nd_t 
            ->(PQueue key nd_t, Used key nd_t)
addToFrindge [] _ _ fringe used = (fringe,used)
addToFrindge ((Edge from to dist):es) heurist goal fringe used | to `member` used  && newCost > oldCost 
                                                                = addToFrindge es heurist goal fringe used
                                                               | otherwise 
                                                                = addToFrindge es heurist goal chFringe (Data.Map.insert to newCost used)
                                                          where 
                                                               (nd,cost) = elemAt (Data.Map.findIndex from used) used 
                                                               newCost =  (cost + dist)
                                                               (_,oldCost) = elemAt (findIndex to used) used
                                                               priority = (newCost + (heurist to goal))
                                                               chFringe = Queue.insert priority to fringe 

-- Sample heuristic function expecting numeric values of node index. heuristic function should calculate how exspensive it is to cross 
-- received node. E.g. Packman problem should use Manhattan distance between calculated node and goal node. 
-- This function is called for each node of graph
--   current node
--   goal node
--   return: Numeric cost to cross the node 
heuristic::Ix nd_t
        => Num nd_t 
        => Num key
        => Ord key 
        => nd_t
        ->nd_t
        ->key
heuristic (first) (goal) = 0

-- Generates graf from list of nodes and list of edges
--  list of nodes
--  list of edges
--  return: created graph for A*/Dijkstra/BFS
newGraph::Ix nd_t
        =>Num key
        =>Ord key 
        =>[nd_t]
        ->[Edge nd_t key]
        ->Graph nd_t key
newGraph nd edges_arr = Graph sorted_nd edges
                          where
                          edges = array (rl,rr) $makeEdgeCouples edges_arr [(i,[])|i<-(Data.Ix.range (rl,rr))]
                          sorted_nd = Data.List.sort nd  
                          (rl,rr) = (Data.List.head sorted_nd,(Data.List.last sorted_nd)) 

-- Cretes special format of list with field (node,array_of_edges). This is used to call Data.array constructor
--  list of edges
--  accumulator for incremental creation of couples
--  return: resulting list of defined fields
makeEdgeCouples::Ix nd_t
               =>Num key
               =>Ord key
               =>[Edge nd_t key]
               ->[(nd_t,[Edge nd_t key])]
               ->[(nd_t,[Edge nd_t key])]
makeEdgeCouples [] acc =acc
makeEdgeCouples (e:es) acc = makeEdgeCouples es (addToAcc acc e)

--Receives edge and adds this edge to corresponding field based on "from" node in the edge
--  accumulator with data fields
--  one edge which is added
--  return: new extended accumulator
addToAcc::Ix nd_t
        =>Num key
        =>Ord key
        =>[(nd_t,[Edge nd_t key])]
        ->Edge nd_t key
        ->[(nd_t,[Edge nd_t key])]
addToAcc [] (Edge from to dist) = [(from,[(Edge from to dist)])]
addToAcc ((nd,edges):acc) (Edge from to dist) | nd == from = ((nd,(Edge from to dist):edges):acc)
                                              | otherwise = (nd,edges):addToAcc acc (Edge from to dist)  


-- examples how to create graphs
grafDIJ = Graph [0,1,2,3] (listArray (0,3) [[Edge 0 1 2 ,Edge 0 2 6],[Edge 1 2 1, Edge 1 3 5],[Edge 2 1 1, Edge 2 3 1],[]] )
grafBFS = Graph [0,1,2,3] (listArray (0,3) [[Edge 0 1 0 ,Edge 0 2 0],[Edge 1 2 0, Edge 1 3 0],[Edge 2 1 0, Edge 2 3 0],[]] )
grafLINE = Graph [0,1,2,3] (listArray (0,3) [[Edge 0 1 0],[Edge 1 2 0],[Edge 2 3 0],[]] )
grafCHAR = Graph ['a','b','c','d'] (listArray ('a','d') [[Edge 'a' 'b' 0],[Edge 'b' 'c' 0],[Edge 'c' 'd' 0],[]] )
grafGEN = newGraph [0,1,2,3] [Edge 0 1 2 ,Edge 0 2 6, Edge 1 2 1, Edge 1 3 5,Edge 2 1 1, Edge 2 3 1]
grafGEN2 = newGraph ['a','b','c','d'] [Edge 'a' 'b' 0,Edge 'b' 'c' 0,Edge 'c' 'd' 0]
grafFOREST = newGraph [0,1,2,3,4,5,6] [Edge 0 1 2,Edge 1 0 2, Edge 2 3 2, Edge 3 2 2,Edge 4 5 2]