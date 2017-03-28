bradley_terry=function(P,sparse.correct=0,max.iter=100,tol=10^-8,print.level=0){
  #' @title Bradley Terry Model
  #' @description  Calculate merrits from dominance relations via BM
  #'
  #' @param P a matrix with partial order information
  #' @param max.iter integer, maximum number of MLE iterations
  #' @param sparse.correct eps to correct for weak connectivity
  #' @param tol double, convergence criterion
  #' @param print.level binary, should diagnostics be printed or not
  #' @return data frame
  #' @details TODO
  #' @examples
  #' ###TODO
  #' @export
  g=igraph::graph_from_adjacency_matrix(t(P),"directed")
  sparse.corrected=F
  P=t(P)
  n=nrow(P)
  if(sparse.correct==0){
    eps=1/n
  }
  else{
    eps=sparse.correct
  }

  # sparse correction
  if(!igraph::is.connected(g,mode = "strong")){
    P=P+matrix(eps,n,n)-diag(eps,n)
    sparse.corrected=T
  }
  #no "games" between players
  N=P+t(P)

  #no of wins for each player
  W=rowSums(P)

  #initialisation
  w_0=rep(1/n,n)
  w_old=rep(n,n)
  iter=0
  while(iter<=max.iter & sqrt(sum((w_old-w_0)^2))>tol){
    iter=iter+1
    w_old=w_0
    w_0=W*rowSums(N/outer(w_0,w_0,"+"))^(-1)
    w_0=w_0/sum(w_0)
    if(print.level==1){
      print(sqrt(sum((w_old-w_0)^2)))
    }
  }
  df.players=data.frame(name=names(w_0),
                        merit=unname(w_0),
                        dominating=unname(igraph::degree(g,mode="out")),
                        dominated=unname(igraph::degree(g,mode="in")),
                        comparable=unname(igraph::degree(g,mode="all")))
  return(list(res=df.players,
              iter=iter,
              sparse.corrected=sparse.corrected
  ))

}
