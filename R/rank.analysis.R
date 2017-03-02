rank_analysis=function(P,method="bfs",names="",print.level=0){
  #' @title Rank Analysis of networks
  #' @description  Performes a complete Rank analysis of a network.
  #' Calculates Expected Rankings, (Mutual) Rank Probabilities and number of possible rankings.
  #' Works best with small networks.
  #'
  #' @param P a partial order as matrix object
  #' @param method string indicating method to get all possible rankings. Default should not be changed
  #' @param names optional argument for names if P does not have row/column names
  #' @param print.level should diagnostics be printed (1) or not (0). Defaults to 0
  #' @details TODO
  #' @return a list containing
  #' \item{n.ext}{Number of Linear Extensions of P}
  #' \item{mse}{Array indicating equivalent nodes}
  #' \item{rank.prob}{Matrix containing rank probabilities \code{rank.prob[i,k]} is the probability that i has rank k}
  #' \item{mutual.rank}{Matrix containing mutual rank probabilities. \code{mutual.rank[i,j]} is the probability that i is ranked lower than j}
  #' \item{expected.rank}{Expected Ranking of nodes in any centrality ranking}
  #' \item{rank.spread}{???}
  #' \item{g.lattice}{igraph object. The lattice of ideals}
  #' @references \insertRef{ddd-elirp-06}{netrankr}
  #' @examples
  #' ###TODO
  #' @export
  if(is.null(rownames(P)) & length(names)!=nrow(P)){
    rownames(P)=1:nrow(P)
  }
  else if(is.null(rownames(P)) & length(names)==nrow(P)){
    rownames(P)=names
  }
  n.full <- nrow(P)
  P.full <- P
  ###############################Equivalence
  MSE=which((P+t(P))==2,arr.ind=T)
  if(length(MSE)>=1){
    MSE<-t(apply(MSE,1,sort))
    MSE<-MSE[!duplicated(MSE),]
    g<-igraph::graph.empty()
    g<-igraph::add.vertices(g,nrow(P))
    g<-igraph::add.edges(g,c(t(MSE)))
    g<-igraph::as.undirected(g)
    MSE<-igraph::clusters(g)$membership
    equi<-which(duplicated(MSE))
    P<-P[-equi,-equi]
  }
  else{
    MSE <- 1:nrow(P)
  }
  names <- rownames(P)
  #number of Elements
  m <- length(names)
  ###############################LATTICEOFIDEALS
  #calculate lattice of ideals
  g.ideals=.lattice_of_ideals(P)
  if(print.level==1){
    print("lattice done")
    print(c(igraph::vcount(g.ideals),igraph::ecount(g.ideals)))
  }
  E(g.ideals)$color="grey"
  #number of ideals
  n=igraph::vcount(g.ideals)

  ###############################TOPDOWN
  visited<<-rep(0,n)
  lef.g<<-rep(0,n)
  e<-.assign_top_down(1,g.ideals)
  lef<-lef.g
  rm(list=c("lef.g","visited"),pos = ".GlobalEnv")
  if(print.level==1){
    print("Top Down done")
    print(c(e))
  }
  ###############################BottomUp
  visited<<-rep(0,n)
  lei<-.assign_bottom_up(g.ideals,m,method)
  rm(visited,pos = ".GlobalEnv")
  if(print.level==1){
    print("Bottom Up done")
  }
  ###############################RankProb
  rp.g<<-matrix(0,m,m)
  visited<<-rep(0,n)
  .compute_rank_prob(g.ideals,1,1,lei,lef,e)#g,v,h,lei,lef,e
  rp=rp.g
  rm(list=c("visited","rp.g"),pos = ".GlobalEnv")
  if(print.level==1){
    print("Rank Prob done")
  }
  ###############################MutualRankProb
  visited<<-rep(0,n)
  visitedElement<<-rep(0,n)
  mrp.g<<-matrix(0,m,m)
  .compute_mutual_rank_prob(g.ideals,1,1,lei,lef,e,m)
  mrp=mrp.g
  rm(list=c("visited","visitedElement","mrp.g"),pos = ".GlobalEnv")
  if(print.level==1){
    print("Mutual Rank done")
  }
  ###############################END
  expected=rp%*%1:m
  rank.spread=rowSums((matrix(rep(1:m,each=m),m,m)-c(expected))^2*rp)
  gc()
  expected=c(expected)
  ###############################
  # Insert equivalent nodes again
  rp.full=matrix(0,n.full,ncol(rp))
  mrp.full=matrix(0,n.full,n.full)
  expected.full=c(0,n.full)
  rank.spread.full=rep(0,n.full)
  for(i in sort(unique(MSE))){
    idx <- which(MSE==i)
    if(length(idx)>1){
      group.head <- i
      rp.full[idx,]  <- do.call(rbind, replicate(length(idx), rp[group.head,], simplify=FALSE))
      mrp.full[idx,] <- do.call(rbind, replicate(length(idx), mrp[group.head,MSE], simplify=FALSE))
      expected.full[idx] <- expected[group.head]
      rank.spread.full[idx] <- rank.spread[group.head]
    }
    else if(length(idx)==1){
      rp.full[idx,] <- rp[i,]

      mrp.full[idx,] <- mrp[i,MSE]
      expected.full[idx] <- expected[i]
      rank.spread.full[idx] <- rank.spread[i]
    }
  }
  ###############################
  return(list(lin.ext=e,
              names=rownames(P.full),
              mse=MSE,
              rank.prob=rp.full,
              mutual.rank.prob=t(mrp.full),
              expected.rank=expected.full,
              rank.spread=sqrt(rank.spread.full),
              g.lattice=g.ideals))
}
#############################################################################################HELPER
.assign_top_down=function(v,g){
  visited[v]<<-1
  e=0
  nv=igraph::neighborhood(g,1,v,"out",1)[[1]]
  for(w in nv){
    if(w==vcount(g)){
      e=1
      lef.g[w]<<-e
    }
    else{
      if(visited[w]==0){
        e=e+.assign_top_down(w,g)
      }
      else{
        e=e+lef.g[w]
      }
    }
  }
  lef.g[v]<<-e
  return(e)
}

.assign_bottom_up=function(g,m,method){
  if(method=="matrix"){
    A=igraph::get.adjacency(g,sparse=F)
    lei=A[1,]
    for(i in 2:m){
      lei=lei+(expm::`%^%`(A,i))[1,]
    }
    lei[1]=1
  }
  else if(method=="bfs"){
    n=igraph::vcount(g)
    lei=rep(0,n)
    lei[1]=1
    n1=igraph::neighborhood(g,1,1,"out",1)[[1]]
    L=list()
    inList=rep(0,n)
    for(w in n1){
      L=.lappend(L,w)
      lei[w]=1
      inList[w]=1
    }
    while(length(L)>=1){
      v=L[[1]]
      L[[1]]<-NULL
      visited[v]<<-1
      nv=igraph::neighborhood(g,1,v,"out",1)[[1]]
      for(w in nv){
        lei[w]=lei[w]+lei[v]

        if(visited[w]==0 & inList[w]==0){
          L=.lappend(L,w)
          inList[w]=1

        }
      }
    }
  }
  return(lei)
}

.compute_rank_prob=function(g,v,h,lei,lef,e){
  visited[v]<<-1
  nv=igraph::neighborhood(g,1,v,"out",1)[[1]]
  for(w in nv){
    idx=igraph::get.edge.ids(g,c(v,w))
    x=igraph::get.edge.attribute(g,"label",idx)
    rp.g[x,h]<<-rp.g[x,h]+(lei[v]*lef[w])/e
    if(w!=vcount(g) & visited[w]==0){
      .compute_rank_prob(g,w,h+1,lei,lef,e)
    }
  }
}

.compute_mutual_rank_prob=function(g,v,h,lei,lef,e,m){
  visited[v]<<-1
  nv=neighborhood(g,1,v,"out",1)[[1]]
  for(w in nv){
    for(y in 1:m){
      if(visitedElement[y]==1){
        idx=igraph::get.edge.ids(g,c(v,w))
        x=igraph::get.edge.attribute(g,"label",idx)
        mrp.g[x,y]<<-mrp.g[x,y]+(lei[v]*lef[w])/e
      }
    }
    if(w!=igraph::vcount(g) & visited[w]==0){
      idx=igraph::get.edge.ids(g,c(v,w))
      x=igraph::get.edge.attribute(g,"label",idx)
      visitedElement[x]<<-1
      .compute_mutual_rank_prob(g,w,h+1,lei,lef,e,m)
      visitedElement[x]<<-0
    }
  }
}

.lattice_of_ideals=function(P){ #P_ij=1 i<j
  n=nrow(P)
  g=igraph::graph.empty()
  g=igraph::add.vertices(g,1,attr=list(name="empty"))
  L=list()
  min.elem=which(colSums(P)==0)
  min.elem=sort(min.elem)
  #append minimum elements to L
  L=sapply(unname(min.elem),function(x).lappend(L,x))
  #add minimum elements to graph
  g=igraph::add.vertices(g,length(min.elem),attr=list(name=as.character(min.elem)))
  #add edges
  add.edges=c(matrix(c(rep("empty",length(min.elem)), min.elem), 2, byrow = T))
  g=igraph::add.edges(g,add.edges,attr=list(label=min.elem)) #added attr
  P.del=P
  rownames(P.del)=1:nrow(P)
  colnames(P.del)=1:nrow(P)
  while(length(L)>=1){
    P.del=P
    rownames(P.del)=1:nrow(P)
    colnames(P.del)=1:nrow(P)
    I=L[[1]]
    L[[1]]<-NULL
    idx.del=which(rownames(P.del)%in%I)
    if(length(idx.del>0)){
      P.del=P.del[-idx.del,-idx.del]
    }
    if(length(P.del)==1){
      min.elem=setdiff(1:n,I)
    }
    else{
      min.elem=as.integer(rownames(P.del)[unname(which(colSums(P.del)==0))])
    }

    for(x in min.elem){
      Iprime=c(I,x)
      Iprime=sort(Iprime)
      inL=Position(function(y) identical(y, Iprime), L, nomatch = 0) > 0
      if(!inL){
        L=.lappend(L,Iprime)
        v.name=paste(Iprime,collapse=" ")
        L.v.name=paste(I,collapse=" ")
        g<-igraph::add.vertices(g,1,attr=list(name=v.name))
        g<-igraph::add.edges(g,c(L.v.name,v.name),attr=list(label=setdiff(Iprime,I))) #added label
      }
      else{
        v.name=paste(Iprime,collapse=" ")
        L.v.name=paste(I,collapse=" ")
        g<-igraph::add.edges(g,c(L.v.name,v.name),attr=list(label=setdiff(Iprime,I))) #added label
      }
    }
  }
  return(g)
}

.lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}
################################################################################
