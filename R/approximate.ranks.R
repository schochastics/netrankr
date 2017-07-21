#' @title Approximation of expected ranks
#' @description  Implements a variety of functions to approximate expected ranks 
#' for partial rankings.
#'
#' @param P a partial order as matrix object.
#' @param method string indicating method to be used. see Details.
#' @details The \emph{method} parameter can be set to 
#' \describe{
#' \item{lpom}{ simplest, most accurate for tiny networks}
#' \item{glpom}{ most accurate for tiny networks}
#' \item{loof1}{ more accurate for larger networks}
#' \item{loof2}{ even more accurate for larger networks}
#' }
#' Which of the above methods performs best depends on the structure of the partial
#' ranking. It is thus advisable to apply more than one for comparrison.
#' @return a vector containing approximations of expected ranks.
#' @author David Schoch
#' @references Brüggemann R., Simon, U., and Mey,S, 2005. Estimation of averaged
#'ranks by extended local partial order models. *MATCH Commun. Math.
#' Comput. Chem.*, 54:489-518.
#' 
#' Brüggemann, R. and Carlsen, L., 2011. An improved estimation of aver-
#' aged ranks of partial orders. *MATCH Commun. Math. Comput. Chem.*,
#' 65(2):383-414.
#' 
#' 
#' @seealso [approx_rank_relative], [exact_rank_prob], [mcmc_rank_prob]
#' @examples
#' P=matrix(c(0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,rep(0,10)),5,5,byrow=TRUE)
#' #Exact result
#' exact_rank_prob(P)$expected.rank
#' approx_rank_expected(P,"lpom")
#' @export
approx_rank_expected <- function(P,method="lpom"){
  g <- igraph::graph_from_adjacency_matrix(P,"directed")
  n <- nrow(P)
  if(method=="lpom"){
    sx <- igraph::degree(g,mode="in")
    ix <- (n-1)-igraph::degree(g,mode="all")
    r.approx=(sx+1)*(n+1)/(n+1-ix)
  }
  else if(method=="glpom"){
    r.approx <- approx_glpom(P)
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
  #' @description Approximate relative rank probabilities \eqn{P(rk(u)<rk(v))}. 
  #' In a network context, \eqn{P(rk(u)<rk(v))} is the probability that u is 
  #' less central than v, given the partial ranking P.
  #' @param P a partial ranking as matrix object.
  #' @param iterative boolean. TRUE (default) if iterative approximation should be used. FALSE if not.
  #' @param num.iter number of iterations to be used. defaults to 10 (see Details).
  #' @details The iterative approach generally gives better approximations than the non iterative, if only slightly.
  #' The default number of iterations is based on the observation, that the approximation does not improve
  #' significantly beyond this value. This observation, however, is based on very small networks such that
  #' increasing it for large network may yield better results.
  #' @author David Schoch
  #' @references De Loof, K. and De Baets, B and De Meyer, H., 2008. Properties of mutual
  #' rank probabilities in partially ordered sets. In *Multicriteria Ordering and
  #' Ranking: Partial Orders, Ambiguities and Applied Issues*, 145-165.
  #' 
  #' @return a matrix containing approximation of mutual rank probabilities. 
  #' \code{relative.rank[i,j]} is the probability that i is ranked lower than j
  #' @seealso [exact_rank_prob] for exact computations
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
  relative.rank <- approx_relative(colSums(P),rowSums(P),P,iterative,num.iter)
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

  diag(mrp.full) <- 0
  return(mrp.full)
}

#buggy as hell
#' freeman_hierarchy <- function(P){
#'   #' @title Freemans Hierarchy measure
#'   #' @description Freeman's hierarchy is based on the singular value decomposition of the skew-symmetric matrix 
#'   #' \deqn{Z=\frac{P^T-P}{2}}{(P^T-P)/2}.
#'   #'
#'   #' @param P a partial ranking as matrix object.
#'   #' @details TODO
#'   #' @author David Schoch
#'   #' @references  Freeman, L.C., 1997. Uncovering Organizational Hierarchies. 
#'   #' *Computational and Mathematical Organization Theory*, 3:5-18.
#'   #' @return a matrix with four columns containing Freemans hierarchy.
#'   #' @seealso [exact_rank_prob], [approx_rank_expected]
#'   #' @examples
#'   #' ###TODO
#'   #' @export  
#'   n <- nrow(P)
#'   Z <- (t(P)-P)*0.5
#'   svdZ <- svd(Z)
#'   d <- svdZ$d[1]
#'   r.all <- sqrt(2*d/n)
#'   x <- round(sqrt(d)*svdZ$u[,1],4)
#'   y <- round(sqrt(d)*svdZ$u[,2],4)
#'   if(any(x<0 & y<0)){ #fixing order by rotating 90 degrees(?)
#'     xy <- cbind(x,y)%*%matrix(c(0,1,-1,0),2,2,byrow=T)
#'     x <- xy[,1]
#'     y <- xy[,2]
#'   }
#'   r.indiv <- sqrt(x^2+y^2)
#'   height <- atan2(y,x)
#'   return(list(res=as.data.frame(cbind(x,y,r.indiv,height)),
#'               r=sqrt(2*d/n),
#'               var=sum(svdZ$d[1:2])/sum(svdZ$d)))
#' }
