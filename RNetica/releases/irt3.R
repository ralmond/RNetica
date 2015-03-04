### A simple example of using RNetica
### Russell Almond, 2015-03-04

## The goal of this exercise is to run though the basic commands to
## build, compile and draw inferences from a Netica network.
## To do this, will we will answer Problem 6.17 from Almond, Mislevy,
## Steinberg, Yan and Williamson (2015), Bayesian Models in
## Educational Assessment, Springer.

## Homework problem 6.17

###########################################################################
## Create Network and variables

###  Create the network and set metadata
irt3 <- CreateNetwork("IRT3")
NetworkTitle(irt3) <- "3-item IRT network, four levels for theta"
NetworkComment(irt3) <- "Almond et al (2015) problem 6.17"

### Create nodes

## Create the latent ability node
theta <- NewDiscreteNode(irt3,"theta",c("VeryLow","Low","High","VeryHigh"))
NodeStateTitles(theta) <- paste(c(-1.5,-0.5,0.5,1.5))
NodeLevels(theta) <- c(-1.5,-0.5,0.5,1.5) # Associate numeric value with level
## This makes the node numeric, so we can later calculate an expected
## value.

## We can create multiple nodes at once, handy when they have a common
## pattern.  Here create Item_1, Item_2, and Item_3
items <- NewDiscreteNode(irt3,paste("Item",1:3,sep="_"),c("Right","Wrong"))

### Add links

## There are three ways we can create links
AddLink(theta,items[[1]])
NodeParents(items[[2]]) <- list(theta)  #directly set the parents:
                                        #Note:  this can also be used
                                        #to change the order of the parents
## Note, we can often loop over item sets
for(i in 3:3) {
  AddLink(theta,items[[i]])
}

### Add Conditional Probability Tables (CPTs)

## Most of the heavy lifting is done by the "[" operator, which when
## used with a NeticaNode, accesses its CPT.  This is actually really
## powerful and has a lot of different options and modes.  For more
## information, see:
help("Extract.NeticaNode")

## Simple node without any parents, just build a triangle distribution.
theta[] <- c(1,3,3,1)/8

## The book gives recommended values for the CPTs for the items.  For
## Item_1 the book gives probabilities .378, .622, .818, .924 for Very
## Low, Low, High & Very High respectively

## Access parent state by name
items[[1]]["VeryLow"]<-.378             #Note that when number of
                                        #elements is one less than
                                        #number of states, the last
                                        #value is calculated to
                                        #complete the normalization
## Access parent state combinations by order
items[[1]][2:4]<- c(0.622,0.818,0.924)



## Secret:  Values in the table are from Rasch model IRT.  Item 1 has
## difficulty -1, Item 2 has difficulty 0 and Item 3 has difficulty
## 1.  We can use the Rasch equations (basically inverse logit
## (theta-difficulty) to calculate the CPT.

invlogit <- function (x) exp(x)/(1+exp(x))
items[[2]][1:4] <- invlogit(NodeLevels(theta)-0)
items[[3]][1:4] <- invlogit(NodeLevels(theta)-1)

## The CPTtools package has more sophisticated tools for building CPTs
## which are set to be inserted into Netica.


## Save the network

## Probably want to save the network in its uncompiled state here.
#WriteNetworks(irt3,"T:/My Documents/BayesNets/irt3.dne")


############################################
## Compilation and inference

## Before doing inference, we need to compile network.
CompileNetwork(irt3)

## Now we can access prior beliefs
NodeBeliefs(theta)
NodeExpectedValue(theta)                #Numeric nodes only
## NodeExpectedValue puts the standard deviation of expected value as
## an attribute, to access it use:
attr(NodeExpectedValue(theta),"std_dev")

### Enter Findings

## We will illustrate the inference functions by verifying that the
## net is monotonic, that is as the student gets more items right, the
## probabilities in theta (and hence the expected value) shift to
## higher values.

## Start by setting all of the observables to wrong
for( i in 1:3) {
  NodeFinding(items[[i]])<-"Wrong"
}

## Now query the key node
NodeBeliefs(theta)
NodeExpectedValue(theta)

## Next, one at a time change the nodes findings to Right and recheck
## the expected value.
for (i in 1:3) {
  NodeFinding(items[[i]])<-"Right"
  print(NodeExpectedValue(theta))
}

### Retract Findings

## If we want to reset everything, we can retract findings
RetractNetFindings(irt3)                #RetractNodeFindings affects
                                        #just one node
NodeBeliefs(theta)


### Save the network

## Saving the network at this point, will also save all findings entered

#WriteNetworks(irt3,"T:/My Documents/BayesNets/irt3.dne")
## Don't need pathname as it is the same as the one above.
