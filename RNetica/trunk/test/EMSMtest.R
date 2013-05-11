emsmdir <- paste(library(help="RNetica")$path, "sampleNets",
                 sep=.Platform$file.sep)
EMSMMotif <- ReadNetworks(paste(emsmdir, "EMSMMotif.dne",
                                sep=.Platform$file.sep))
EMTask1a <- ReadNetworks(paste(emsmdir, "EMTask1a.dne",
                                sep=.Platform$file.sep))
EMTask1b <- ReadNetworks(paste(emsmdir, "EMTask1b.dne",
                                sep=.Platform$file.sep))
EMTask2a <- ReadNetworks(paste(emsmdir, "EMTask2a.dne",
                                sep=.Platform$file.sep))
EMTask2b <- ReadNetworks(paste(emsmdir, "EMTask2b.dne",
                                sep=.Platform$file.sep))
System <- ReadNetworks(paste(emsmdir, "System.dne",
                                sep=.Platform$file.sep))


motifnodes <- NetworkAllNodes(EMSMMotif)
task1anodes <- NetworkAllNodes(EMTask1a)
task1bnodes <- NetworkAllNodes(EMTask1b)
task2anodes <- NetworkAllNodes(EMTask2a)
task2bnodes <- NetworkAllNodes(EMTask2b)
systemnodes <- NetworkAllNodes(System)

## Force Skill1 and Skill2 into a clique
MakeCliqueNode(NetworkFindNode(System,NetworkFootprint(EMTask1a)))


obs <- list(EMTask1a=c(Obs1a1="Right",Obs1a2="Wrong"),
            EMTask1b=c(Obs1b1="Right",Obs1b2="Wrong"),
            EMTask2a=c(Obs2a="Half"),
            EMTask2b=c(Obs2b="Half"))



## First do this net by net.
student <- CopyNetworks(System,"student")
for (i in 1:length(obs)) {
  em <- GetNamedNetworks(names(obs)[i])
  emnodes <- AdjoinNetwork(student,em)
  CompileNetwork(student)
  EnterFindings(student,obs[[i]])
  AbsorbNodes(emnodes)
}
CompileNetwork(student)
emsm.prob <- JointProbability(NetworkNodesInSet(student,"Proficiency"))


## Now for the motif method.
mobs <- obs
names(mobs) <- NULL
mobs <- do.call("c",mobs)

mstudent <- CopyNetworks(EMSMMotif,"mstudent")
CompileNetwork(mstudent)
EnterFindings(mstudent,mobs)
## Need these in same order
motif.prob <- JointProbability(NetworkFindNode(mstudent,names(dimnames(emsm.prob))))

stopifnot(
  sum(abs(emsm.prob-motif.prob)) <.000001
  )
