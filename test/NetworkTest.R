stopifnot(is.IDname(c("aFish","Wanda1","feed me","fish_food","1more","US$",
                      "a123456789012345678901234567890"))
          ==c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE))

##StartNetica()
print(NeticaVersion())

net1 <- CreateNetwork("Untitled")
stopifnot(is(net1,"NeticaBN"))
stopifnot(as.character(net1)=="Untitled")
stopifnot(is.active(net1))
stopifnot(toString(net1)=="<Netica BN: Untitled >")

nets <- CreateNetwork(paste("Untitled",2:3,sep=""))
stopifnot(all(sapply(nets,is,"NeticaBN")))
stopifnot(as.character(nets) == c("Untitled2","Untitled3"))

net2 <- GetNamedNetworks("Untitled2")
stopifnot(is(net2,"NeticaBN"))
stopifnot(as.character(net2)=="Untitled2")
stopifnot((net2 == nets) == c(TRUE,FALSE))

fish <- GetNamedNetworks("fish")
stopifnot(all(sapply(fish,is.null)))

netn <- GetNthNetwork(2)
stopifnot(is(netn,"NeticaBN"))
stopifnot(as.character(net2)=="Untitled2")
stopifnot(net2 == netn)

netnn <- GetNthNetwork(c(1,3))
stopifnot(all(sapply(netnn,is,"NeticaBN")))
stopifnot(as.character(netnn) == c("Untitled","Untitled3"))

netn17 <- GetNthNetwork(17)
stopifnot(all(sapply(netn17,is.null)))

netd <- DeleteNetwork(net2)
stopifnot(!is.active(netd))
stopifnot(!is.active(net2))
stopifnot(as.character(netd)=="Untitled2")
stopifnot(toString(netd) == "<Deleted Netica BN: Untitled2 >")
          
netd2 <- GetNamedNetworks("Untitled2")
stopifnot(all(sapply(netd2,is.null)))

netnd2 <- GetNthNetwork(2)
stopifnot(is(netnd2,"NeticaBN"))
stopifnot(as.character(netnd2)=="Untitled3")

netc <- CopyNetworks(netnn,paste("Copy",c(1,3),sep=""))
stopifnot(all(sapply(netc,is,"NeticaBN")))
stopifnot(as.character(netc) == c("Copy1","Copy3"))

netnall <- GetNthNetwork(1:4)
stopifnot(all(sapply(netnall,is,"NeticaBN")))
stopifnot(as.character(netnall) == c("Untitled","Untitled3","Copy1","Copy3"))

for (net in netnall) {
  DeleteNetwork(net)
}

          
##StopNetica()
