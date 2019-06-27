#' Pearson's r computation for neuroimaging data
#'
#' This function computes voxel-wise pearson's correlation coefficients between
#' an independent variable and neuroimaging data.
#'
#' @title Computing Cohen's d from MRI images
#' @param imgs 4d images
#' @param x independent variables
#' @param mask Mask file (default=array(1,dim=dim(imgs1)[1:3]))
#' @param col (only if figure=TRUE, default=bluered(200))
#' @param breaks (only if figure=TRUE, default=c(-100:100)/100*7)
#' @param method (default='pearson') 'spearman', 'tau' can be used.
#' @param adjust (default=FALSE) compute adjusted r ()
#' @return rmap: image array of pearson's coefficients.
#' @export
#'
#' @examples
#' library(neurorct)
#' img.dim=c(10,12,15)
#' n=100
#' x=rnorm(n)
#' img=array(rnorm(img.dim[1]*img.dim[2]*img.dim[3]*n),dim=c(img.dim,n))
#' img[3:5,3:5,3:5,]  <- array(rep(x,each=27),c(3,3,3,n)) +img[3:5,3:5,3:5,]
#' mask=array(0,img.dim);mask[2:8,2:11,2:14]<-1
#' z=compute_r(imgs=img,x=x,mask=mask)
#' @import gplots
#' @importFrom graphics image par
#' @importFrom stats var
compute_r <- function(imgs,
                      x,
                      mask=array(1,dim=dim(imgs)[1:3]),
                      col=bluered(200),
                      breaks=c(-100:100)/100,
                      method='pearson',
                      adjust=FALSE){

  dim=dim(imgs)
  mask.vec=as.vector(mask)
  img=array(imgs,dim=c(dim[1]*dim[2]*dim[3],dim[4]))
  if(adjust==FALSE){
    rs=apply(img[mask.vec==1,], 1, function(y)cor(y,x,method=method))
  }else{ rs=sqrt(rs^2 - (1-rs^2)*(1/(n-2)))}
  r.img=array(0,dim=dim[1:3])
  r.img[mask==1]<-rs
  return(r.img)
}
