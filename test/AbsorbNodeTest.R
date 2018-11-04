XSession <- NeticaSession()
startSession(XSession)
setwd("~/ownCloud/documents/BNTutorials/RussellsBayesNetClass/Networks/miniACED/")


## Read in network -- Do this every time R is restarted
profModel <- ReadNetworks(file.path(library(help="RNetica")$path,
                                    "sampleNets","miniACEDPnet.dne"),
                          session=XSession)
## If profModels already exists could also use

## Reconnect nodes -- Do this every time R is restarted
allNodes <- NetworkAllNodes(profModel)
sgp <- allNodes$SolveGeometricProblems
profNodes <- NetworkNodesInSet(profModel,"Proficiencies")

### @@@@@@@@@@@@@@@@@@@@@ Simple Scoring Example @@@@@@@@@@@@@@@@@@@@@@@@@@
### Start New Student
## Copy the master proficiency model
## to make student model
Fred.SM <- CopyNetworks(profModel,"Fred")
Fred.SMvars <- NetworkAllNodes(Fred.SM)
CompileNetwork(Fred.SM)

## Setup score history
prior <- NodeBeliefs(Fred.SMvars$SolveGeometricProblems)
Fred.History <- matrix(prior,1,3)
row.names(Fred.History) <- "*Baseline*"
colnames(Fred.History) <- names(prior)
Fred.History

EMnet <- ReadNetworks(file.path(library(help="RNetica")$path,
                                "sampleNets","CommonRatioEasyEM.dne"),
                      session=XSession)
obs <- AdjoinNetwork(Fred.SM,EMnet)
CompileNetwork(Fred.SM)
NodeFinding(obs$isCorrect) <- "Yes"

post <- NodeBeliefs(Fred.SMvars$SolveGeometricProblems)
Fred.History <- rbind(Fred.History,new=post)
Fred.History
DeleteNetwork(EMnet) ## Delete EM
## Getting a segfault at the next line, not sure why.
AbsorbNodes(obs)
