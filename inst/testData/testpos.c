/**
 * Node.c --- This file contains functions for creating,
 * destroying, and modifying states of nodes.
 */

#include <string.h>
#include <stdio.h>
#include <Netica.h>

void main() {

  environ_ns* netica_env = NewNeticaEnviron_ns("",NULL,NULL);
  net_bn* anet = NewNet_bn("Anet",netica_env);
  node_bn* anode = NewNode_bn("anode",2,anet);

  double x,y;
  x=2; y=2;
  SetNodeVisPosition_bn(anode,NULL,x,y);

  double x1,y1;
  GetNodeVisPosition_bn(anode,NULL,&x1,&y1);

  printf("x=%f, y=%f\n",x1,y1);
}
