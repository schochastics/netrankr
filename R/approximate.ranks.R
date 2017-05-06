approx_rank_expected <- function(P,method="lpom"){
  #' @title Approximation of expected ranks
  #' @description  Implements a variety of functions to approximate expected ranks in large partially ordered sets.
  #'
  #' @param P a partial order as matrix object.
  #' @param method string indicating method to be used. see Details.
  #' @details The \emph{method} parameter can be set to 
  #' \describe{
  #' \item{lpom}{ simplest, most accurate for tiny networks}
  #' \item{glpom}{ most accurate for tiny networks}
  #' \item{loof1}{ more accurate for large networks}
  #' \item{loof2}{ even more accurate for large networks}
  #' }
  #' @return a vector containing approximations of expected ranks.
  #' @seealso [approx_rank_relative], [rank_analysis], [bradley_terry]
  #' @examples
  #' P=matrix(c(0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,rep(0,10)),5,5,byrow=TRUE)
  #' #Exact result
  #' rank_analysis(P)$expected.rank
  #' #simplest approximation, suited for tiny matrices
  #' approx_rank_expected(P,"lpom")
  #' @export
  g <- igraph::graph_from_adjacency_matrix(P,"directed")
  n <- nrow(P)
  if(method=="lpom"){
    sx <- igraph::degree(g,mode="in")
    ix <- (n-1)-igraph::degree(g,mode="all")
    r.approx=(sx+1)*(n+1)/(n+1-ix)
  }
  else if(method=="glpom"){
    P <- P+diag(1,n)
    sx <- igraph::degree(g,mode="in")
    r.approx=sx+1
    for(x in 1:n){
      Ix <- which(P[x,]==0 & P[,x]==0)
      Sx <- setdiff(which(P[,x]==1),x)
      for(y in Ix){
        Iy <- which(P[y,]==0 & P[,y]==0)
        r.approx[x]=r.approx[x]+(length(intersect(Sx,Iy))+1)/(length(setdiff(Iy,Ix))+1)
      }
    }
  }
  else if(method=="loof1"){
    P <- P+diag(1,n)
    s <- igraph::degree(g,mode="in")
    l <- igraph::degree(g,mode="out")
    r.approx=s+1
    for(x in 1:n){
      Ix <- which(P[x,]==0 & P[,x]==0)
      for(y in Ix){
        r.approx[x] <- r.approx[x]+((s[x]+1)*(l[y]+1))/((s[x]+1)*(l[y]+1)+(s[y]+1)*(l[x]+1))
      }
    }
  }
  else if(method=="loof2"){
    P <- P+diag(1,n)
    s <- igraph::degree(g,mode="in")
    l <- igraph::degree(g,mode="out")
    s.approx <- s
    l.approx <- l
    for(x in 1:n){
      Ix <- which(P[x,]==0 & P[,x]==0)
      for(y in Ix){
        s.approx[x] <- s.approx[x]+.sl.approx(s[x],s[y],l[x],l[y])
        l.approx[x] <- l.approx[x]+.sl.approx(s[y],s[x],l[y],l[x])
      }
    }
    r.approx <- s+1
    s <- s.approx
    l <- l.approx
    for(x in 1:n){
      Ix <- which(P[x,]==0 & P[,x]==0)
      for(y in Ix){
        r.approx[x] <- r.approx[x]+((s[x]+1)*(l[y]+1))/((s[x]+1)*(l[y]+1)+(s[y]+1)*(l[x]+1))
      }
    }
  }
  return(r.approx)
}

.sl.approx <- function(sx,sy,lx,ly){
  ((sx+1)*(ly+1))/((sx+1)*(ly+1)+(sy+1)*(lx+1))
}
#############################
approx_rank_relative <- function(P,iterative=TRUE,num.iter=10){
  #' @title Approximation of relative rank probabilities
  #' @description Approximate relative rank probabilites \eqn{Prob(u<v)}. In a network context, \eqn{Prob(u<v)}
  #' gives the probability that u is less central than v, given the partial ranking P.
  #' @param P a partial order as matrix object.
  #' @param iterative boolean. TRUE (default) if iterative approximation should be used. FALSE if not.
  #' @param num.iter number of iterations to be used. defaults to 10 (see Details).
  #' @details The iterative approach generally gives better approximations than the non iterative, yet only slightly.
  #' More than 10 iterations do not improve the accuracy significantly.
  #'
  #' @return a matrix containing approximation of mutual rank probabilities. 
  #' \code{relative.rank[i,j]} is the probability that i is ranked lower than j
  #' @seealso [rank_analysis]
  #' @examples
  #' P=matrix(c(0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,rep(0,10)),5,5,byrow=TRUE)
  #' P
  #' approx_rank_relative(P,iterative=FALSE) 
  #' approx_rank_relative(P,iterative=TRUE)
  #' @export
  MSE=which((P+t(P))==2,arr.ind=T)
  P.full <- P
  if(length(MSE)>=1){
    MSE <- t(apply(MSE,1,sort))
    MSE <- MSE[!duplicated(MSE),]
    g <- igraph::graph.empty()
    g <- igraph::add.vertices(g,nrow(P))
    g <- igraph::add.edges(g,c(t(MSE)))
    g <- igraph::as.undirected(g)
    MSE <- igraph::clusters(g)$membership
    equi <- which(duplicated(MSE))
    P <- P[-equi,-equi]
  } else{
    MSE<-1:nrow(P)
  }
  n <- nrow(P)
  g.dom <- igraph::graph_from_adjacency_matrix(P,"directed")
  deg.in <- igraph::degree(g.dom,mode="in")
  deg.out <- igraph::degree(g.dom,mode="out")
  nom <- outer(deg.in+1,deg.out+1,"*")
  denom <- outer(deg.in+1,deg.out+1,"*")+t(outer(deg.in+1,deg.out+1,"*"))
  relative.rank <- nom/denom
  relative.rank <- relative.rank-diag(diag(relative.rank))
  relative.rank[t(P)==1] <- 1
  relative.rank[(P)==1] <- 0
  relative.rank[relative.rank==1 & t(relative.rank)==1] <-  0
  if(iterative){
    for(i in 1:(num.iter-1)){
      g.dom <- igraph::graph_from_adjacency_matrix(t(relative.rank),weighted = T)
      deg.in <- igraph::graph.strength(g.dom,mode="in")
      deg.out <- igraph::graph.strength(g.dom,mode="out")
      nom <- outer(deg.in+1,deg.out+1,"*")
      denom <- outer(deg.in+1,deg.out+1,"*")+t(outer(deg.in+1,deg.out+1,"*"))
      relative.rank <- nom/denom
      relative.rank <- relative.rank-diag(diag(relative.rank))
      relative.rank[t(P)==1] <- 1
      relative.rank[(P)==1] <- 0
      relative.rank[relative.rank==1 & t(relative.rank)==1] <-  0
    }
  }
  mrp.full=matrix(0,length(MSE),length(MSE))
  for(i in sort(unique(MSE))){
    idx <- which(MSE==i)
    if(length(idx)>1){
      group.head <- i
      mrp.full[idx,] <- do.call(rbind, replicate(length(idx), relative.rank[group.head,MSE], simplify=FALSE))
    }
    else if(length(idx)==1){
      group.head <- idx
      mrp.full[group.head,] <- relative.rank[i,MSE]
    }
  }
  # mrp.full[t(P.full)==1] <- 1
  # mrp.full[(P.full)==1] <- 0
  # mrp.full[mrp.full==1 & t(mrp.full)==1] <-  0
  diag(mrp.full) <- 0
  return(t(mrp.full))
}

freeman_hierarchy <- function(P){
  #' @title Freemans Hierarchy measure
  #' @description Freeman's hierarchy is based on the singular value decomposition of the skew-symmetric matrix 
  #' \deqn{Z=\frac{P^T-P}{2}}{(P^T-P)/2}.
  #'
  #' @param P a partial order as matrix object.
  #' @details TODO
  #'
  #' @return a matrix with four columns containing Freemans hierarchy.
  #' @seealso [rank_analysis], [approx_rank_expected]
  #' @examples
  #' ###TODO
  #' @export  
  n <- nrow(P)
  Z <- (t(P)-P)*0.5
  svdZ <- svd(Z)
  d <- svdZ$d[1]
  r.all <- sqrt(2*d/n)
  x <- sqrt(d)*svdZ$u[,2]
  y <- sqrt(d)*svdZ$u[,1]
  r.indiv <- sqrt(x^2+y^2)
  height <- atan2(y,x)
  return(list(res=as.data.frame(cbind(x,y,r.indiv,height)),
              r=sqrt(2*d/n),
              var=sum(svdZ$d[1:2])/sum(svdZ$d)))
}
