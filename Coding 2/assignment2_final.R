# Problem 8.2.4 - Fold-Fulkerson algorithm (not counted)

library(optrees)

nodes <- 1:4
arcs <- matrix(c(1, 2, 2,
                1, 3, 1,
                2, 3, -2,
                2, 4, 1,
                3, 4, 1),
              ncol = 3,
              byrow = T)

# answer obtained
getShortestPathTree(nodes,
                    arcs,
                    algorithm = "Bellman-Ford",
                    directed = T, 
                    show.data = T, 
                    show.distances = T)

# Problem 8.2.5
nodes <- 1:7
arcs <- matrix(c(1, 2, 33,
                 1, 3, 48,
                 1, 4, 76,
                 1, 5, 98,
                 1, 6, 124,
                 1, 7, 156,
                 2, 3, 33,
                 2, 4, 48,
                 2, 5, 76,
                 2, 6, 98,
                 2, 7, 124,
                 3, 4, 33,
                 3, 5, 48,
                 3, 6, 76,
                 3, 7, 98,
                 4, 5, 33,
                 4, 6, 48,
                 4, 7, 76,
                 5, 6, 33,
                 5, 7, 48,
                 6, 7, 33),
               ncol = 3,
               byrow = T)


# shortest path: 144 thousand dollars
# same as previous calculated
spTreeDijkstra(nodes,
               arcs,
               directed = T, 
               source.node = 1)

# Problem 8.3.6 - Fold-Fulkerson algorithm

nodes <- 1:14 
arcs <- matrix(c(1, 2, 3, # source node to items
                1, 3, 3,
                1, 4, 3,
                1, 5, 3,
                1, 6, 3,
                1, 7, 3,
                1, 8, 3,
                # node 2 to trucks
                2, 9, 1,
                2, 10, 1,
                2, 11, 1,
                2, 12, 1,
                2, 13, 1,
                # node 3 to trucks
                3, 9, 1,
                3, 10, 1,
                3, 11, 1,
                3, 12, 1,
                3, 13, 1,
                # node 4 to trucks
                4, 9, 1,
                4, 10, 1,
                4, 11, 1,
                4, 12, 1,
                4, 13, 1,
                # node 5 to trucks
                5, 9, 1,
                5, 10, 1,
                5, 11, 1,
                5, 12, 1,
                5, 13, 1,
                # node 6 to trucks
                6, 9, 1,
                6, 10, 1,
                6, 11, 1,
                6, 12, 1,
                6, 13, 1,
                # node 7 to trucks
                7, 9, 1,
                7, 10, 1,
                7, 11, 1,
                7, 12, 1,
                7, 13, 1,
                # node 8 to trucks
                8, 9, 1,
                8, 10, 1,
                8, 11, 1,
                8, 12, 1,
                8, 13, 1,
                # trucks to sink node
                9, 14, 6,
                10, 14, 4,
                11, 14, 5,
                12, 14, 4,
                13, 14, 3),
              ncol = 3,
              byrow = T)

maxFlowFordFulkerson(nodes,
                     arcs,
                     directed = T,
                     source.node = 1,
                     sink.node = 14)

# Problem 8.6.1 - Minimal Spanning Tree Algorithm

nodes <- 1:5
# 1 - Gary
# 2 - Fort Wayne
# 3 - Evansville
# 4 - Terre Haute
# 5 - South Bend
arcs <- matrix(c(1, 3, 217,
                 1, 4, 164,
                 1, 5, 58,
                 2, 3, 290, 
                 2, 4, 201,
                 2, 5, 79,
                 3, 4, 113, 
                 4, 5, 196),
               ncol = 3,
               byrow = T)

# MST is Fort Wayne - South Bend - Gary - Terre Haute - Evansville
getMinimumSpanningTree(nodes,
                       arcs,
                       algorithm = "Prim",
                       show.data = T,
                       show.graph = T)

# Problem 9.3.3

library(lpsymphony)
obj <- c(2, 3)
mat <- matrix(c(1, 2,
                3, 4),
              ncol = 2,
              byrow = T)
rhs <- c(10, 25)
dir <- c("<=", "<=")
types <- c("I", "I")

# x1 = 4, x2 = 3, z = 17
lpsymphony_solve_LP(obj,
                    mat,
                    dir,
                    rhs,
                    types = types,
                    first_feasible = F,
                    max = TRUE)

