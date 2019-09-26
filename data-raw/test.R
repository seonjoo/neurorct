# test.R

library(neurorct)
library(tidyverse)
library(foreach)

y = matrix(rnorm(100*1000),1000,100)
bl = matrix(rnorm(100*1000),1000,100)
x = cbind(cov1 = rnorm(100), cov2 = rnorm(100))

# test fast_lm
a1 <- fast_lm(x=x, y=y)
a1_aggre = fast_lm(x=x, y=y, aggregate = TRUE)
system.time(
  a2 <- fast_lm(x=x,y=y, bl = bl)
)

a2_aggre = fast_lm(x=x, y=y,bl = bl, aggregate = TRUE)

system.time(
  re1 <-
    lapply(1:1000,
           function(idx){
             lm(y ~ cov1 + cov2, data = as_tibble(cbind(y = y[idx,], x))) %>% broom::tidy() %>% .[2,]
           }) %>%
    do.call(rbind, .)
)


dist(rbind(a1$maps$betamap, re1$estimate))
dist(rbind(a1$maps$tmap, re1$statistic))
dist(rbind(a1$maps$pmap, re1$p.value))
dist(rbind(a1_aggre$stderrmat[,2], re1$std.error))

a1_aggre$stderrmat[,2] - re1$std.error
a1_aggre$stderrmat[,2][1]
re1$std.error[1]
# sqrt(a1_aggre$stderrmat[,2][1])
re1$std.error[1]
a1_aggre$stderrmat[,2]

re2 =
  mclapply(1:1000,
         function(i){
           lm(y ~ ., data = as_tibble(cbind(y = y[i,], x, bl = bl[i,]))) %>% broom::tidy() %>% .[2,]
         },
         mc.cores = 5) %>%
  do.call(rbind, .)

dist(rbind(a2$maps$betamap, re2$estimate))

a2$maps$betamap - re2$estimate
dist(rbind(a2_aggre$stderrmat, re2$std.error))

betahat[,2] - re2$estimate


library(neurorct)
y=matrix(rnorm(100*1000),1000,100)
x=cbind(rnorm(100),rnorm(100))
system.time(a<-t(apply(y, 1, function(zz)summary(lm(zz~x))$coefficients[,3])))
system.time(a2<-fast_lm(x=x,y=y))
sum(abs(a-a2$tmap))


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
voxel.mi<-hd_mi(hddat = hd.dat, hd.method='voxelwise', mice.method='pmm', mc.cores = 1)
voxel.mi[[1]]$img[[2]]

# test feeding the output of hd_mi to fast_lm
lm_mi_result = fast_lm(x = cbind(group,age), hdmi_output = voxel.mi)
dim(lm_mi_result)
colnames(lm_mi_result)



