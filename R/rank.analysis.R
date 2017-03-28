rank_analysis=function(P,names="",only.results=T,verbose=F,force=F){
  #' @title Rank Analysis of networks
  #' @description  Performes a complete Rank analysis of a network.
  #' Calculates Expected Rankings, (Relative) Rank Probabilities and number of possible rankings.
  #' Works best with small networks.
  #' @importFrom Rcpp evalCpp
  #' @useDynLib netrankr
  #' 
  #' @param P matrix representing a partial ranking
  #' @param names optional argument for names if P does not have row/column names
  #' @param only.results logical. wether only results (default) or additionally the ideal tree and lattice should be returned
  #' @param verbose logical. should diagnostics be printed. Defaults to F
  #' @param force logical. If False(default), stops the analysis if the network has more than 50 nodes and less than 0.2 comparable pairs .
  #' Only change if you know what you are doing. 
  #' @details TODO
  #' @return 
  #' \item{n.ext}{Number of possible centrality rankings}
  #' \item{topo.order}{Random Ranking used to build the lattice of ideals}
  #' \item{mse}{Array indicating equivalent nodes}
  #' \item{rank.prob}{Matrix containing rank probabilities: \code{rank.prob[i,k]} is the probability that i has rank k}
  #' \item{mutual.rank}{Matrix containing relative rank probabilities: \code{mutual.rank[i,j]} is the probability that i is ranked lower than j}
  #' \item{expected.rank}{Expected ranks of nodes in any centrality ranking}
  #' \item{rank.spread}{Variance of the ranking probabilities}
  #' \item{tree}{igraph object. The tree of ideals (if only.results=F)}
  #' \item{lattice}{igraph object. The lattice of ideals (if only.results=F)}
  #' @seealso [approx_rank_mutual], [approx_rank_expected]
  #' @examples
  #' P=matrix(c(0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,rep(0,10)),5,5,byrow=TRUE)
  #' res=rank_analysis(P)
  #' @export
  # Check for names ------------------------------------------------
  if(is.null(rownames(P)) & length(names)!=nrow(P)){
    rownames(P)=1:nrow(P)
  }
  else if(is.null(rownames(P)) & length(names)==nrow(P)){
    rownames(P)=names
  }
  n.full <- nrow(P)
  P.full <- P
  # Equivalence classes ------------------------------------------------
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
  nElem <- length(names)
# sanity check if applicable ------------------------------------------------
  if(nrow(P)>50 & comparable_pairs(P)<0.2 & force==F){
    stop("Input data too big. Use approximations or set force=T if you know what you are doing")
  }
#Prepare Data structures---------------------
  n<-nrow(P)
  topo.order<-as.vector(igraph::topological.sort(igraph::graph_from_adjacency_matrix(P,"directed")))
  
  P<-P[topo.order,topo.order]
  ImPred<-igraph::get.adjlist(igraph::graph_from_adjacency_matrix(P,"directed"),"in")
  ImPred=lapply(ImPred,function(x) as.vector(x)-1)
  
  ImSucc<-igraph::get.adjlist(igraph::graph_from_adjacency_matrix(P,"directed"),"out")
  ImSucc=lapply(ImSucc,function(x) as.vector(x)-1)
  # TREEOFIDEALS ----------------------------------------------------  
  if(verbose==T){
    print("building tree of ideals")
  }
  tree<-treeOfIdeals(ImPred)
  nIdeals=length(tree$label)
  if(verbose==T){
    print("tree of ideals built")
  }
  Ek=sapply(0:(nElem-1),function(x){which(tree$label==x)-1})
  # tree$child=lapply(tree$child,function(x) {idx=order(tree$label[x+1],decreasing=T);x[idx]})
  if(verbose==T){
    print("building lattice of Ideals")
  }
  latofI=LatticeOfIdeals(tree$child,tree$parent,Ek,nElem,nIdeals)

  if(verbose==T){
    print("lattice of ideals built")
  }
  ideallist=listingIdeals(ImSucc,nElem,nIdeals)
  # ideallist=lapply(ideallist,sort)
  
  if(verbose==T){
    print("ideals listed")
  }
  # return(ideallist)
  
  if(verbose==T){
    print(paste("No of ideals:",nIdeals))
    print("Calculating Rank Probabilities")
  }

  res=rankprobs(latofI,ideallist,nElem,nIdeals)
  if(verbose==T){
    print(paste("No. of possible Rankings: ",res$linext))
  }
  res$rp=res$rp[order(topo.order),]
  res$mrp=res$mrp[order(topo.order),]
  ###############################END
  expected=res$rp%*%1:nElem
  rank.spread=rowSums((matrix(rep(1:nElem,each=nElem),nElem,nElem)-c(expected))^2*res$rp)
  expected=c(expected)
  ###############################
  # Insert equivalent nodes again
  rp.full=matrix(0,n.full,ncol(res$rp))
  mrp.full=matrix(0,n.full,n.full)
  expected.full=c(0,n.full)
  rank.spread.full=rep(0,n.full)
  for(i in sort(unique(MSE))){
    idx <- which(MSE==i)
    if(length(idx)>1){
      group.head <- i
      rp.full[idx,]  <- do.call(rbind, replicate(length(idx), res$rp[group.head,], simplify=FALSE))
      mrp.full[idx,] <- do.call(rbind, replicate(length(idx), res$mrp[group.head,MSE], simplify=FALSE))
      expected.full[idx] <- expected[group.head]
      rank.spread.full[idx] <- rank.spread[group.head]
    }
    else if(length(idx)==1){
      rp.full[idx,] <- res$rp[i,]
      mrp.full[idx,] <- res$mrp[i,MSE]
      expected.full[idx] <- expected[i]
      rank.spread.full[idx] <- rank.spread[i]
    }
  }

  ###############################
  if(only.results){
    return(list(lin.ext=res$linext,
                topo.order=topo.order,
                names=rownames(P.full),
                mse=MSE,
                rank.prob=rp.full,
                mutual.rank.prob=t(mrp.full),
                expected.rank=expected.full,
                rank.spread=sqrt(rank.spread.full)))
  } else{
    return(list(lin.ext=res$linext,
                topo.order=topo.order,
                names=rownames(P.full),
                mse=MSE,
                rank.prob=rp.full,
                mutual.rank.prob=t(mrp.full),
                expected.rank=expected.full,
                rank.spread=sqrt(rank.spread.full),
                tree=tree,
                lattice=latofI))
  }
  ###############################
}

