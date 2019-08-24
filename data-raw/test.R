# test.R


library(neurorct)
library(tidyverse)

y=matrix(rnorm(100*1000),1000,100)
x=cbind(a =rnorm(100), b = rnorm(100))
y_list = list(y,y,y)
y = y_list

# test fast_lm
library(foreach)
a2_list <- fast_lm(x=x,y=y_list)
a2<-fast_lm(x=x,y=y)
a2_list[[1]]$betamap[,2]
a2_list[[1]]$stderrmat %>% colnames()

y=matrix(rnorm(100*1000),1000,100)
x=cbind(a =rnorm(100), b = rnorm(100))
bl = matrix(rnorm(100*1000),1000,100)
# test direct output
bl_adjust = fast_lm(x = x, y = y, bl = bl)
# test aggregate = TRUE
bl_adjust2 = fast_lm(x = x, y = y, bl = bl, aggregate = TRUE)



# test aggregate
stderrmat=matrix(rnorm(30)^2,10,3)
betamap=matrix(rnorm(30),10,3)
aggre_mi(betamap = betamap, stderrmat = stderrmat)

# test hd_mi
library(parallel)
m.dim=c(6,6,10)
npergroup=30
y.base=y.fu=array(0,c(m.dim,npergroup*2))
eff.d=1 ## effect size
perturb=1
set.seed(1234)
for (j in 1:(npergroup*2)){
  y.base[,,,j]<-array(rnorm(m.dim[1]*m.dim[2]*m.dim[3]),dim=m.dim)
  tmp2<-perturb*array(rnorm(m.dim[1]*m.dim[2]*m.dim[3]),dim=m.dim)
  if (j<=npergroup){tmp2[3:4,3:4,5:6]<-tmp2[3:4,3:4,5:6]+eff.d}
  y.fu[,,,j]<-tmp2
}
## impose missing
attrition=0.2
missing.indx=sort(sample(1:(2*npergroup),attrition*npergroup*2))
y.fu[,,,missing.indx]<-NA
   ## In the function, for the missing image, we impose missing values
y.base.mat=array(y.base,dim=c(m.dim[1]*m.dim[2]*m.dim[3],npergroup*2))
dim(y.base.mat)
y.fu.mat=array(y.fu,dim=c(m.dim[1]*m.dim[2]*m.dim[3],npergroup*2))
dim(y.fu.mat)
group=rep(c(1,0),each=npergroup)
age=floor(runif(2*npergroup)*20)+20
hd.dat=list(cov=age, img=list(y.base.mat, y.fu.mat))
voxel.mi<-hd_mi(hddat = hd.dat, hd.method='voxelwise', mice.method='pmm')
voxel.mi[[1]]$img[[2]]

# test feeding the output of hd_mi to fast_lm
lm_mi_result = fast_lm(x = cbind(group,age), hdmi_output = voxel.mi)
dim(lm_mi_result)
colnames(lm_mi_result)



# a1 = mice::complete(z[[1]], 1)
# a2 = mice::complete(z[[1]], 2)
# a3 = mice::complete(z[[1]], 3)
# a1 == a2
# a2 == a3

