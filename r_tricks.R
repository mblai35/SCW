### r_tricks.R - R Code Only 04/15/17 ###

#  R Console 3.0.2
#  currently specifying F: # 

op = options()							# save default options # 
								# load installed packages # 	
pckgs = c("rmarkdown","xlsx","maps","car","MCMCpack",
   "MASS","e1071","rpart","randomForest","verification",
   "jpeg","aplpack","rgl","png","beepr","audio")		
								# suppress warnings, messages # 
library.l = function(package) suppressWarnings(suppressMessages(library(package,character.only=TRUE)))
load.it = lapply(pckgs,library.l)				# ** lapply ** # 
									

# http://stackoverflow.com/questions/2458013/what-ways-are-there-to-edit-a-function-in-r #
# b2, b2(0), fix(b2) # 						# call code, output, fix # 
								# ** fix ** # 
### 1. Environment ###

# (a) Online Help #

a1 = function(o) {
# Search "R Tricks #

# rseek.org #							# search engine for R # 

# http://stackoverflow.com/questions/1295955/what-is-the-most-useful-r-trick
# http://adv-r.had.co.nz/ #
# https://www.quora.com/What-are-some-good-hacks-at-using-R # 
}

# (b) Reproducible Research # 

b1 = function(o) {
# https://yihui.name/knitr/demo/minimal/
# http://www.stat.cmu.edu/~cshalizi/rmarkdown/#math-in-r-markdown # 
# https://github.com/rstudio/rmarkdown/issues/103

# library(sweave)
# library(knitr)
# library(rmarkdown)						# html_document,pdf_document # 
# rmarkdown::render("F:/R/r_tricks2.R",output_format="word_document")
}

# (c) Source #

c1 = function(o) {
# source("F:/R/r_tricks.R")					# read trick functions # 
}

# (d) Flexbile Variable Assignments #

d1 = function(o) { 
# http://stackoverflow.com/questions/2271575/whats-the-difference-between-and-in-r #

x <- 5; w = y = 6; 2 -> z;
w+x+y+z
}

# (e) loops # 

e1 = function(o) {
u = 0; start = proc.time()
for (i in 1:10000) {
  u = u+1								# ** cat ** #
  if (u/1000==floor(u/1000)) { cat(i,sep="\n"); u = 0; } }
proc.time() - start						# ** proc.time ** #
}

### (2) Data Functions ### 

# (a) scan # 

a2 = function(o){
# scan with tab delimiter ? # 
# http://stats.idre.ucla.edu/r/faq/how-to-input-data-into-r/ #
# http://grokbase.com/t/r/r-help/14as7n1x0p/r-r-markdown-and-scan #

data.frame(scan(text = "
12 bobby
24 kate
35 david
20 michael",what=list(age=0,name="")))
}

# (b) read other data file formats #

b2 = function(o,out=1) {
# http://www.statmethods.net/input/importingdata.html #
# library(xlsx)

file <- system.file("tests","test_import.xlsx",package="xlsx")
res <- read.xlsx(file,1)  					# read first sheet #
if (out==1) dat = head(res[,-8])				# ** head ** #
if (out==2) dat = res
dat }

# (c) apply #												

c2 = function(o,dat=b2(0,2)) {
# require (2)(b) #
n.dat = dat[,-1]
sum.o = cbind(means=apply(n.dat,2,mean),medians=apply(n.dat,2,median),sds=apply(n.dat,2,sd))
options(scipen=99)						# ** scipen ** # 
print(sum.o,digits=4)						# ** print ** #
}

# (d) by #

d2 = function(o,dat=b2(0,2)) {
# require (2)(b) #
# library(maps) #

map("state",fill=TRUE,col=palette())
west = dat[,1] %in% c("California","Oregon","Washington","Idaho","Nevada","Arizona","Utah","Montana","Wyoming","Colorado","New Mexico")
apply(dat[,-1],2,function(x) by(x,as.factor(west),mean))
}

# (e) paste #

e2 = function(o) {
qux = c("awesome","R","is","!","totally")
print(qux)
qux = qux[c(2,3,5,1,4)]				
print(paste(qux,collapse=""))					# ** print ** # 
print(qux,collapse="***")
print(qux,collapse=" ")
print(noquote(paste(qux,collapse=" ")))			# ** noquote ** # 
}

### (3) MATH Functions ### 

# (a) %x% #

a3 = function(o) {
# https://www.zoology.ubc.ca/~schluter/R/data/ #
# http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/ # 
A = rep(1,4)%x%c(1,2,3)
B = rep(1,3)%x%c(1,2)%x%c(1,1)
dat = cbind(A,B)
A = as.factor(A)							# ** as.factor ** # 
levels(A)[levels(A)==c(1,2,3)] = c("low","med","high")
dat = list(f1=A,f2=B)
dat = data.frame(f1=A,f2=B)
dat
}

# (b) all.equal #

b3 = function(o) {
# identical, round # 
d45 <- pi*(1/4 + 1:10)
test <- rep(1,10)
print(cbind(tan(d45),test))
print(all.equal(tan(d45),test))          			# TRUE #
print(all(tan(d45) == test))         			# FALSE #
print(all.equal(tan(d45),test,tol=0))  			# Difference #
}

# (c) array #

c3 = function(o) {
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
column.names <- c("COL1","COL2","COL3")			# ** column.names ** #
row.names <- c("ROW1","ROW2","ROW3")			# ** row.names ** # 
matrix.names <- c("Matrix1","Matrix2")			# ** matrix.names ** # 
array(c(vector1,vector2),dim=c(3,3,2),dimnames=list(row.names,column.names,matrix.names))
}

# (d) Integration #

d3 = function(o) {
fxy = function(x,y) exp(-y)*(x<y)*(x>0)
lower = function(x) x; upper = function(x) 1+x; upper2 = Inf; 
fx = function(x) {sapply(x,function(x) {integrate(function(y) fxy(x,y),lower(x),upper(x))$value }) }				
print(integrate(fx,0,upper2)$value)		

r = seq(0,1,length=50); z = outer(r,r,fxy)		# ** persp ** # 
persp(r,r,z,xlab="x",ylab="y",zlab="fxy(x,y)",theta=-20,phi=40)
}									 
	
# (e) flexible operations/operators #

e3 = function(o) {

# tr = function(A) sum(diag(A))
'+' = function(A) sum(diag(A))				# rmarkdown cannot recognize these quotes #

A = matrix(c(3,4,3,5),2,2)
# tr(A)
print('+'(A))

"%*~%" <- function(x,y){
  n <- nrow(x)
  out <- matrix(0,n,ncol(x)*ncol(y))
  for(i in 1:n) out[i, ] <- c(y[i, ] %o% x[i, ])
  out }

x <- matrix(1:4,2,2,TRUE)
y <- matrix(5:8,2,2,TRUE)
print(x %*~% y)
}

### (4) STAT functions ###

# (a) Shut the Stars Up # 

a4 = function(o) {
# library(car) 
options(show.signif.stars=FALSE)

mod <- lm(conformity ~ fcategory*partner.status, data=Moore, 
  contrasts=list(fcategory=contr.sum, partner.status=contr.sum))

Anova(mod)								# ** Anova ** #
}

# (b) model selection # 

b4 = function(o) {
# library(MASS)							# birthwt data # 
# library(verification)						# roc.area # 	
data = birthwt							# [HL, 1980, p 92, 125] # 	
head(data)								
data$race = as.numeric(data$race==1)			# white vs non-white # 
fit.f = glm(low~., binomial,data[,-10])			# full model # 
X = model.matrix(fit.f)[1:nrow(data),]			# ** model.matrix ** #
p = ncol(X)-1; num_model = 2^(ncol(X)-1);								
d.g  = as.data.frame(t(matrix(1:0,p,2,byrow=T)))
X.i  = cbind(matrix(1,num_model),expand.grid(d.g)); 	# ** expand.grid ** # 
var_names = c("intercept",names(data[-c(1,10)])); 
colnames(X.i) = var_names
print(c(n=nrow(data),p=p,models=num_model))		# 189*10 # 

m.out = matrix(NA,num_model,5)				# output for criteria # 
for (select in 1:num_model) { 				# loop through models # 
									# select model # 	
X.m = as.matrix(X[,X.i[select,]==1]); cand.names = var_names[X.i[select,]==1]; colnames(X.m) = cand.names; 
model = glm(low~X.m,binomial,data)				# model fit # 
phat = model$fitted.values; param = ncol(X.m); 
									
AIC = AIC(model,k=2); 						# criteria # 
BIC = AIC(model,k=log(nrow(data)))
AREA = -roc.area(data$low,phat)$A
m.out[select,] = cbind(select,param,AIC,BIC,AREA)	# store criteria # 	
}									# end of loop through models # 

rank.m = apply(m.out[,3:5],2,rank)				# ** apply ** # 
score = apply(rank.m,1,mean)		
mod.out = cbind(m.out[,2],X.i[,-1],rank.m,score)[order(score),] 
colnames(mod.out)[c(1,10,11,12)] = c("# parms","AIC.rank","BIC.rank","AREA.rank")
top.mod = head(mod.out,10)
print(top.mod,digits=1)
}

# (c) optim #

c4 = function(o) {
set.seed(23); n = 5; sigma = 1; 
xx = rnorm(5,0,sigma); xbar = mean(xx)
cat("mome is",c(xbar,sigma/sqrt(n))," ",fill=TRUE)
									# log likelihood # 
llike = function(theta,x) sum(log(dnorm(x,theta,sigma)))	
mle = optim(0,llike,method="BFGS",hessian=TRUE,x=xx,control=list(fnscale=-1))
mle.out = c(mle$par,sqrt(-1/mle$hessian))			# ** cat ** # 
cat("mle is",t(mle.out)," ")					
}

# (d) Bayesian Statistics # 

# library(R2WinBugs)
# library(rjags)
# library(stan)

d4 = function(o) {
# library(MCMCpack) 
mu = 1; tau = 1000
set.seed(23); n = 5; sigma = 1; xx = rnorm(5,0,sigma);
lpost = function(theta,x) sum(log(dnorm(x,theta,sigma))) + log(dnorm(theta,mu,tau))  
post.samp = MCMCmetrop1R(lpost,theta.init=0,x=xx,thin=1,mcmc=100000,burnin=1,logfun=TRUE)
summary(post.samp)						# summaries from MCMC samples # 
}

# (e) Machine Learning # 

e4 = function(o) {
# library(MASS)							# USCereal, lda, qda # 	
# library(e1071)							# svm # 
# library(rpart)							# rpart # 
# library(randomForest)						# randomForest # 

data = UScereal; gg = factor(data$mfr)	
kk = (gg=="G")+(gg=="K")					# select Manufacturer and subset # 	
Y.s = subset(data[,-c(1,9,11)],(rep(1,nrow(data))==kk),drop=T)	 
gg.s = factor(subset(gg,(rep(1,nrow(data))==kk),drop=T))
data.s = data.frame(gg.s,Y.s)
print(colnames(data.s))
print(table(gg.s))

p.da = p.cart = p.svm = p.rf = NULL
for(i in 1:nrow(data.s)) {					# n fold cv # 
  data.s.i = data.s[i,]; data.s.xi = data.s[-i,];
  f.da   = qda(gg.s~.,data=data.s.xi,CV=F)
  f.cart = rpart(gg.s~.,data=data.s.xi,method="class")
  f.svm  = svm(gg.s~.,data=data.s.xi,kernel="radial",type="C-classification",probability=T)
  f.rf   = randomForest(gg.s~.,data=data.s.xi,method="class",sampsize=nrow(data.s.xi))

  p.da   = c(p.da,as.character(predict(f.da,data.s.i,type="class")[[1]]))
  p.cart = c(p.cart,as.character(predict(f.cart,data.s.i,type="class")[[1]]))
  p.svm  = c(p.svm,as.character(predict(f.svm,data.s.i,type="class")[[1]]))
  p.rf   = c(p.rf,as.character(predict(f.rf,data.s.i,type="class")[[1]]))
}

# Discriminant Analysis (DA) #
print(table(gg.s,p.da))
print(1-sum(diag(prop.table(table(gg.s,p.da)))))

# Classification and Regression Tree (CART) # 
print(table(gg.s,p.cart))
print(1-sum(diag(prop.table(table(gg.s,p.cart)))))

# Support Vector Machine (SVM) # 
print(table(gg.s,p.svm))
print(1-sum(diag(prop.table(table(gg.s,p.svm)))))

# Random Forest (RF) # 
print(table(gg.s,p.rf))
print(1-sum(diag(prop.table(table(gg.s,p.rf)))))
}

### (5) Graphics ###

# (a) Images #

a5 = function(o) {
# library(jpeg)
image = readJPEG("F:/R/muir.jpg")
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(image,0,0,1,1) 
}

# (b) Faces #

# Data with 7 highly correlated economical variables, observed yearly from 1947 to 1962. #

b5 = function(o) {
# library(aplpack)
# cor(longley)
faces(longley,face.type=1,print.info=T)
}

# (c) 3D Plots #

c5 = function(o,opt=1) {
# library(rgl)
# library(png)

x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x,y)

if (opt==1) plot3d(x,y,z,col=rainbow(1000),axes=T,aspect=T)
# rgl.snapshot("F:/R/plot3d.png",fmt="png",top=TRUE)
if (opt==2) { image = readPNG("F:/R/plot3d.png",native=T)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(image,0,0,1,1) }
}

# (d) Plot Functions #

d5 = function(o) {
x <- c(32,64,96,118,126,144,152.5,158)  
y <- c(99.5,104.8,108.5,100,86,64,35.3,15)

plot.f = function(s=16,ssize=1.5,formx=poly(x,3),color='red',line=1,ww=2) {
  plot(x,y,pch=s,cex=ssize)
  pred = predict(lm(y~formx))
  lines(x,pred,col=color,lty=line,lwd=ww) }
plot.f()
}

# (e) Sounds #
# http://stackoverflow.com/questions/3365657/is-there-a-way-to-make-r-beep-play-a-sound-at-the-end-of-a-script #

e5 = function(o) {
# library(beepr)
options(error = function() for (i in 1:3) wait(beep(7),2),warn=2) 
log(-1000)
options(op) 
options(error = function() {})
}

