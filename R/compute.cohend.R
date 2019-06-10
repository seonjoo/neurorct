#' Cohen's d computation for neuroimaging data
#'
#' This function computes simple Cohen's d maps from neuroimaging data
#' for group level analysis. The formula we use is
#' \deqn{d=\frac{\bar{X}_1- \bar{X}_2}{s_{pooled}},}
#' \eqn{s_{pooled}} is the pooled
#' standard deviation in two groups.
#'
#' @title Computing Cohen's d from MRI images
#' @param imgs1 4d image of group 1
#' @param imgs2 4d image of group 2
#' @param mask Mask file (default=array(1,dim=dim(imgs1)[1:3]))
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
#' mask=array(0,img.dim);mask[2:8,2:11,2:14]<-1
#' z=compute.cohend(imgs1=treatment,imgs2=control,mask=mask)
#' @import gplots
#' @importFrom graphics image par
#' @importFrom stats var
compute.cohend <- function(imgs1,imgs2,
                           mask=array(1,dim=dim(imgs1)[1:3]),
                           figure=TRUE,
                           col=bluered(200),
                           breaks=c(-100:100)/100*7){

  dim1=dim(imgs1)
  dim2=dim(imgs2)
  n1=dim1[4]
  n2=dim2[4]
  mask.vec=as.vector(mask)
  img1=array(imgs1,dim=c(dim1[1]*dim1[2]*dim1[3],dim1[4]))
  img2=array(imgs2,dim=c(dim2[1]*dim2[2]*dim2[3],dim2[4]))
  m1=apply(img1[mask.vec==1,],1,mean)
  m2=apply(img2[mask.vec==1,],1,mean)
  v1=apply(img1[mask.vec==1,],1,var)
  v2=apply(img2[mask.vec==1,],1,var)
  d=(m1-m2)/sqrt((v1*(n1-1) + v2*(n2-1))/(n1+n2-2))
  d.img=array(0,dim=dim1[1:3])
  d.img[mask==1]<-d
  if(figure==TRUE){
    par(mfrow=c(3,5),mar=rep(0,4))
      for (j in 1:15){
        image(d.img[,,j],xaxt='n',yaxt='n',col=col,breaks=breaks)
      }
    }
  return(d.img)
}
