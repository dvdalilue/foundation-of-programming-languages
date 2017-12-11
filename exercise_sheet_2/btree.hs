data BTree a = Leaf a | Node (BTree a) (BTree a)
    deriving (Eq,Ord) -- Definición de orden estructural

leaves :: BTree a -> Int
leaves (Leaf x) = 1
leaves (Node bl br) = leaves bl + leaves br

nodes :: BTree a -> Int
nodes (Leaf x) = 0
nodes (Node bl br) = 1 + nodes bl + nodes br

predicate_nodes_leaves :: BTree a -> Bool
predicate_nodes_leaves b = leaves b == (nodes b) + 1 -- ???
