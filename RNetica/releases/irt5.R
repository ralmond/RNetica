## Restore from save

## This simple R script describes how to reattach a Netica network
## after saveing an R session

## Option 1, use a hard coded path on your local machine (this is a
## sample net from the RNetica installation).
irt5 <-
  ReadNetworks("T:/Documents/R/win-library/2.13/RNetica/sampleNets/IRT5.dne")
## Option 2, if you previously read/saved the net from a file, you can
## and have saved the NeticaBN object in a file, you can use it as an
## argument to ReadNetworks and the network will be reread from the
## file. (This won't work unless irt5 is already defined.)
# irt5 <- ReadNetworks(irt5)

## Now, you need to restore connections to the nodes.  There are
## several options for how to do that.

## Option A, find the key nodes by name:
## single node
irt5.theta <- NetworkFindNode(irt5,"Theta")
## list of nodes
irt5.x <- NetworkFindNode(irt5,paste("Item",1:5,sep="_"))

## Option B, retrive a list of all nodes

irt5.nodes <- NetworkAllNodes(irt5)
## You can then access them by name using $
NodeProbs(irt5.nodes$Theta)
## But this method creates objects for all nodes, even internal ones
## you may not need.

## Option C, retrieve nodes by node set
## This assumes you have grouped key nodes into node sets before
## saving the network.  I frequently use a node set called
## "ReportingVars" for variables whose beliefs I want to monitor and
## one call "Observables" for variables whose values I might want to
## set.

irt5.reportingVars <- NetworkNodesInSet(irt5,"ReportingVars")
irt5.obs <- NetworkNodesInSet(irt5,"Observables")


## Before you quit R, make sure to save your work

##WriteNetworks(irt5,"pathname")

## Or to save it back to the same place

## WriteNetworks(irt5)

