approx_rank_expected=function(P,method="lpom"){
  #' @title Approximation of expected rankings
  #' @description  Implements a variety of functions to approximate expected ranks in large partially ordered sets
  #'
  #' @param P a partial order as matrix object
  #' @param method string indicating method to be used. see details
  #' @details The \emph{method} parameter can be set to \emph{lpom}(simplest, most accurate for tiny networks),
  #' \emph{glpom}(most accurate for tiny networks),\emph{loof1}(more accurate for large networks) or
  #' \emph{loof2}(even more accurate for large networks)
  #'
  #' @return a vector containing approximation of expected ranks
  #' @seealso [rank_analysis]
  #' @examples
  #' ###TODO
  #' @export
  g=igraph::graph_from_adjacency_matrix(P,"directed")
  n=nrow(P)
  if(method=="lpom"){
    sx=igraph::degree(g,mode="in")
    ix=n-1-igraph::degree(g,mode="all")
    r.approx=(sx+1)*(n+1)/(n+1-ix)
  }
  else if(method=="glpom"){
    P=P+diag(1,n)
    sx=igraph::degree(g,mode="in")
    r.approx=sx+1
    for(x in 1:n){
      Ix=which(P[x,]==0 & P[,x]==0)
      Sx=setdiff(which(P[,x]==1),x)
      for(y in Ix){
        Iy=which(P[y,]==0 & P[,y]==0)
        r.approx[x]=r.approx[x]+(length(intersect(Sx,Iy))+1)/(length(setdiff(Iy,Ix))+1)
      }
    }
  }
  else if(method=="loof1"){
    P=P+diag(1,n)
    s=igraph::degree(g,mode="in")
    l=igraph::degree(g,mode="out")
    r.approx=s+1
    for(x in 1:n){
      Ix=which(P[x,]==0 & P[,x]==0)
      for(y in Ix){
        r.approx[x]=r.approx[x]+((s[x]+1)*(l[y]+1))/((s[x]+1)*(l[y]+1)+(s[y]+1)*(l[x]+1))
      }
    }
  }
  else if(method=="loof2"){
    P=P+diag(1,n)
    s=igraph::degree(g,mode="in")
    l=igraph::degree(g,mode="out")
    s.approx=s
    l.approx=l
    for(x in 1:n){
      Ix=which(P[x,]==0 & P[,x]==0)
      for(y in Ix){
        s.approx[x]=s.approx[x]+.sl.approx(s[x],s[y],l[x],l[y])
        l.approx[x]=l.approx[x]+.sl.approx(s[y],s[x],l[y],l[x])
      }
    }
    r.approx=s+1
    s=s.approx
    l=l.approx
    for(x in 1:n){
      Ix=which(P[x,]==0 & P[,x]==0)
      for(y in Ix){
        r.approx[x]=r.approx[x]+((s[x]+1)*(l[y]+1))/((s[x]+1)*(l[y]+1)+(s[y]+1)*(l[x]+1))
      }
    }
  }
  return(r.approx)
}

.sl.approx=function(sx,sy,lx,ly){
  ((sx+1)*(ly+1))/((sx+1)*(ly+1)+(sy+1)*(lx+1))
}
#############################
approx_rank_mutual=function(P){
  #' @title Approximation of mutual rank probabilities
  #' @description  Mutual Rank approximation
  #'
  #' @param P a partial order as matrix object
  #' @details TODO
  #'
  #' @return a matrix containing approximation of mutual rank probabilities
  #' @seealso [rank_analysis]
  #' @examples
  #' ###TODO
  #' @export
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
  n=nrow(P)
  g.dom <- igraph::graph_from_adjacency_matrix(P,"directed")
  deg.in <- degree(g.dom,mode="in")
  deg.out <- degree(g.dom,mode="out")
  nom <- outer(deg.in+1,deg.out+1,"*")
  denom <- outer(deg.in+1,deg.out+1,"*")+t(outer(deg.in+1,deg.out+1,"*"))
  mutual.rank <- nom/denom
  mutual.rank <- mutual.rank-diag(diag(mutual.rank))
  # mutual.rank=matrix(0,n,n)
  # for(i in 1:(n-1)){
  #   for(j in (i+1):n){
  #     mutual.rank[i,j]=(deg.in[i]+1)*(deg.out[j]+1)/((deg.in[i]+1)*(deg.out[j]+1)+(deg.in[j]+1)*(deg.out[i]+1))
  #     mutual.rank[j,i] <- 1-mutual.rank[i,j]
  #   }
  # }
  return(mutual.rank)
}
  
