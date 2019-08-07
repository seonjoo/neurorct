#' Generate multiple imputated datasets using MICE.
#' @param hddat Data list has to follow the following format (e.g. list(cov=age, img=list(bl, fu)))
#' n (number of subjects) x  ( p (number of variables) x timepoint + covariates) matrix.
#' In current implementation, cov should be complete.
#' @param mice.method (default='pmm') mice method for multiple impuation
#' @param hd.method (default='voxelwise') For voxelwise multiple imputation, hd.method='voxelwise',
#' for principal component analysis based method hd.method='pca'
#' @return complete (a list of imputed dataset)
#' @export
#' @import mice parallel
#' @examples m.dim=c(6,6,10)
#' npergroup=30
#' y.base=y.fu=array(0,c(m.dim,npergroup*2))
#' eff.d=1 ## effect size
#' perturb=1
#' set.seed(1234)
#' for (j in 1:(npergroup*2)){
#' y.base[,,,j]<-array(rnorm(m.dim[1]*m.dim[2]*m.dim[3]),dim=m.dim)
#' tmp2<-perturb*array(rnorm(m.dim[1]*m.dim[2]*m.dim[3]),dim=m.dim)
#' if (j<=npergroup){tmp2[3:4,3:4,5:6]<-tmp2[3:4,3:4,5:6]+eff.d}
#' y.fu[,,,j]<-tmp2
#' }
#' ## impose missing
#' attrition=0.2
#' missing.indx=sort(sample(1:(2*npergroup),attrition*npergroup*2))
#' y.fu[,,,missing.indx]<-NA
#' ## In the function, for the missing image, we impose missing values
#' y.base.mat=array(y.base,dim=c(m.dim[1]*m.dim[2]*m.dim[3],npergroup*2))
#' dim(y.base.mat)
#' y.fu.mat=array(y.fu,dim=c(m.dim[1]*m.dim[2]*m.dim[3],npergroup*2))
#' dim(y.fu.mat)
#' group=rep(c('txt','con'),each=npergroup)
#' age=floor(runif(2*npergroup)*20)+20
#' hd.dat=list(cov=age, img=list(y.base.mat, y.fu.mat))
#' system.time(voxel.mi<-hd_mi(hd.dat, hd.method='voxelwise', mice.method='pmm'))
#' system.time(pca.mi<-hd_mi(hd.dat, hd.method='pca', mice.method='pmm'))


hd_mi<-function(hddat,
                hd.method=c('voxelwise', 'pca'),
                mice.method='pmm',
                seed=1,
                mice.maxit=5,
                mc.cores=4,
                pca.threshold=0.8){

  num.voxel=nrow(hddat$img[[1]])
  print(paste('Imaging measure dimension:',num.voxel))
  ncov=ifelse(is.null(hddat$cov),0,ncol(as.matrix(hddat$cov)))
  nimg=length(hddat$img)
  if (ncov>0){if(any(is.na(hddat$cov))==TRUE){stop('Covariates include missing values.')}}
  if (hd.method=='voxelwise'){
    z<-mclapply(1:num.voxel, function(j){mice(cbind(hd.dat$cov, do.call(cbind,lapply(hd.dat$img, function(x)x[j,]))),
            method=mice.method,maxit=mice.maxit,m=m,seed=seed+j)}, mc.cores=mc.cores)
    complete<-lapply(1:m,function(k){
      tmp=do.call(cbind,lapply(z, function(x){complete(x,k)}))
      if(ncov==0){re=list(cov=NULL, img=lapply(1:nimg,function(idx){tmp[,1:num.voxel + (idx-1)*num.voxel]}))}
      if(ncov>0){re=list(cov=tmp[,1:ncov], img=lapply(1:nimg,function(idx){tmp[,1:num.voxel + (idx-1)*num.voxel + ncov]}))}
      return(re)})
  }
  if (hd.method=='pca'){
    ## pca using available data

    allimgdat = do.call(cbind, hddat$img)
    miss=which(apply(allimgdat,2,function(x)any(is.na(x)==TRUE)))
    nomiss=which(apply(allimgdat,2,function(x)any(is.na(x)==TRUE)) ==FALSE)
    dat.m=t(scale(t(allimgdat[,nomiss]),center=TRUE,scale=FALSE))
    #varmat=svd(dat.m)
    varmat= t(dat.m) %*% dat.m#/(length(nomiss)-1)
    varmat.eigen=eigen(varmat)
    ncomp=min(which(cumsum(varmat.eigen$values)/sum(varmat.eigen$values) >pca.threshold))#(ncol(dat.m)-1)
    varmat.score=varmat.eigen$vectors[,1:ncomp] %*% diag(sqrt(varmat.eigen$values[1:ncomp]))
    varmat.loading=dat.m %*% (varmat.eigen$vectors[,1:ncomp]) %*%
      diag(1/sqrt(varmat.eigen$values[1:ncomp]))
    #image(t(varmat.loading) %*% varmat.loading)
    varmat.score.miss=matrix(NA,ncol(allimgdat),ncol(varmat.score))
    varmat.score.miss[nomiss,]<-varmat.score
    system.time(z<-mclapply(1:m, function(j){mice(cbind(hddat$cov,varmat.score.miss),
            method=mice.method,maxit=mice.maxit,m=1,seed=seed+j)},mc.cores=mc.cores))
    complete<-lapply(z,complete)
  }

  return(complete)

}
