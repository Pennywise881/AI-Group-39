# A* algorithm to the delivery-man problem

# Functions:

### initializeNodes
Creates a nodeMatrix that contains a list with informaion about each node
fCost,  gCost, parent position, position and  visited T/F.
 
### checkNode
Calculates f  = g, h and t cost of the node.
If the f cost is lower then the nodes current cost(a batter path found):
Then remove it from the open list if it exists in the open list.
Update new node's: f cost, g cost and parent position(because we found a new better path to the node).

### tracePath
When the optimal path to the goalnode is found this recursive function is used to trace the path back to the cars position. Returns the node that the car should go to in next step.

### getNearestPackage
Finds the nearest package from the car.

### getPathToGoal
Looks at the surounding nodes, if a node is not visited it calles the function ***checkNode*** for that node. If a node is the goal-node it calls the ***tracePath*** function. And from that determens which is the next move. Returns a car object with the car$nextMove variable.

### doAStar
Calls function ***initializeNodes***. Sets the position of the car as the node parent position. Sets visited as true. Adds it to the closed list.
If the car has no load: Calls function ***getNearestPackage*** . Sets that as the goal-node.
If the car has load: still has the drop off from the previous nearest package as the goal-node.
If the car reach the goal-node: stop (nextMove = 5).
Calls ***getPathToGoal***. Returns the car object with the updated next move.
