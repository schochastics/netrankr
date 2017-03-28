positional_dominance=function(A,map=F,benefit=T){
  #' @title Generalized Dominance in Graphs
  #' @description generalized dominance relations. More to come
  #'
  #' @param A matrix containing attributes
  #' @param map boolean if rows can be sorted or not(default)
  #' @param benefit boolean if higher values(default) or lower values are better
  #' @return dominance relations
  #' @examples
  #' ###TODO
  #' @export
  
  D=matdom(A,map,benefit)
  # if(map){
  #   A=t(apply(A,1,function(x)sort(x,decreasing=T)))
  # }
  # c=1
  # if(!benefit){
  #   c=-1
  # }
  # n=nrow(A)
  # D=matrix(0,n,n)
  # for(i in 1:n){
  #   for(j in 1:n){
  #     if(i!=j){
  #       if(!map){
  #         if(all((c*(A[i,-c(i,j)]-A[j,-c(i,j)]))<=0)){
  #           D[i,j]=1
  #         }
  #       }
  #       else{
  #         if(all((c*(A[i,]-A[j,]))<=0)){
  #           D[i,j]=1
  #         }
  #       }
  #     }
  #   }
  # }
  return(D)
}
