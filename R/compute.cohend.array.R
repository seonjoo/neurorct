#' Title
#' @title Computing Cohen's d from MRI images
#' @param imgs1 4d image of group 1
#' @param imgs2 4d image of group 2
#' @param figure (default=TRUE) draw image
#' @param col (only if figure=TRUE, default=bluered(200))
#' @param breaks (only if figure=TRUE, default=c(-100:100)/100*7)
#' @return cohen.dmap: image array of Cohen's d.
#' @export
#'
#' @examples
#' library(neurorct)
#' img.dim=c(10,12,15)
#' n=10
#' treatment=array(rnorm(img.dim[1]*img.dim[2]*img.dim[3]*n),dim=c(img.dim,n))
#' treatment[3:5,3:5,3:5,]  <- 10+treatment[3:5,3:5,3:5,]
#' control=array(rnorm(img.dim[1]*img.dim[2]*img.dim[3]*n),dim=c(img.dim,n))
#' control[3:5,3:5,3:5,]  <- 12+control[3:5,3:5,3:5,]
#' z=compute.cohend.array(imgs1=treatment,imgs2=control)
#' @import gplots
#' @importFrom graphics image par
#' @importFrom stats var
#'
compute.cohend.array <- function(imgs1,imgs2,figure=TRUE,col=bluered(200),breaks=c(-100:100)/100*7){
  n1=dim(imgs1)[4]
  n2=dim(imgs2)[4]
  m1=apply(imgs1,c(1,2,3),mean)
  m2=apply(imgs2,c(1,2,3),mean)
  v1=apply(imgs1,c(1,2,3),var)
  v2=apply(imgs2,c(1,2,3),var)
  d=(m1-m2)/((v1*(n1-1) + v2*(n2-1))/(n1+n2-2))
  if(figure==TRUE){
    par(mfrow=c(3,4),mar=rep(0,4))
      for (j in 1:12){
        image(d[,,j],xaxt='n',yaxt='n',col=col,breaks=breaks)
      }
    }
  return(d)
}
