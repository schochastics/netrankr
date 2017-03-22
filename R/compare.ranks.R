compare_ranks=function(x,y){
  #' @title Count occurences of pairs in rankings
  #' @description  Counts the number of Concordant, Discordant, Tied and Unknown between two rankings
  #'
  #' @param x a ranking vector
  #' @param y a ranking vector with the same length as \emph{x}
  #' @return a list containing
  #' \item{c}{number of concordant pairs}
  #' \item{d}{number of discordant pairs}
  #' \item{t}{number of tied pairs}
  #' \item{u}{number of left and right ties}
  #' @details This function allows
  #' @examples
  #' ###TODO
  #' @export
  if(length(x)!=length(y)){
    stop("x and y must have the same length")
  }
  res=checkPairs(x,y)

  return(list(c=res$concordant,d=res$discordant,t=res$tied,u=res$undef))
}

