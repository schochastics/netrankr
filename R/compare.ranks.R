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
  Con=0
  Dis=0
  Tie=0
  Undef=0
  n=length(x)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      if((x[i]>x[j] & y[i]>y[j]) | (x[i]<x[j] & y[i]<y[j])){
        Con=Con+1
      }
      else if((x[i]>x[j] & y[i]<y[j]) | (x[i]<x[j] & y[i]>y[j])){
        Dis=Dis+1
      }
      else if(x[i]==x[j] & y[i]==y[j]){
        Tie=Tie+1
      }
      else{
        Undef=Undef+1
      }

    }
  }
  return(list(c=Con,d=Dis,t=Tie,u=Undef))
}

