###########################################
# Header
oldpar <- par(no.readonly = TRUE)
oldwd <- getwd()
this.dir <- dirname(parent.frame(2)$ofile)
nombre.R <-  sys.frame(1)$ofile
require(tools)
nombre <- print(file_path_sans_ext(nombre.R))
pdf(paste0(nombre,".pdf"))
setwd(this.dir)
#setwd("~/gaming/materias/inferencia_bayesiana/trabajoFinal/imagenes")
#####################################

par(mar=c(3.75,3.75,0.25,0.25))

beta <- 25/6
#caso Equiop muy superior
mu <- c(25,30,25,25)
sigma <- c(8,1,1,1)
mu1_grilla <- seq(mu[1]-40,mu[1]+40,by=0.1)
posterior_ganador <- function(s_1_f,s_1_p,mu= c(25,20,20,20),sigma=c(4,1,1,1),beta=25/6){
  return(dnorm(s_1_f,mu[1],sigma[1])*pnorm(s_1_p,sum(mu[3:4])-(sum(mu[1:2])-mu[1]),sqrt(sum(sigma^2+beta^2)-sigma[1]^2)))
  
}
m <- outer(mu1_grilla,mu1_grilla,posterior_ganador)
levels <- seq(min(m),max(m),length.out = 11)
image(mu1_grilla,mu1_grilla,m,col=rev(gray.colors(10,start=0.2,end=0.95)),breaks = levels,useRaster=T,
      ylab="",xlab="",axes=F)
contour(mu1_grilla,mu1_grilla,m,drawlabels=F,levels = levels,add = T,col=rev(gray.colors(11,start=0,end=0.6)),lwd=1.1)
mtext(text=expression(N(s[1]~";"~mu[1],sigma[1]^2 )) ,side =1,line=2.75,cex=1.75)
mtext(text =expression(Phi(s[1]~";"~sum(mu[i],i %in% A[b])-mu[2],sum(beta^2+ sigma[i]^2,i %in% A[a]~"u"~A[b]) -sigma[1]^2 )) ,side =2,line=0.15,cex=1.75)
abline(v=mu[1],lty=3)
abline(h=mu[1]-(sum(mu[1:2]-mu[3:4])),lty=3)
abline(c(0,1))
#lines(c(0,1)*(s_2+s_2-500),c(1,0)*(s_2+s_2-500))
axis(side=2, labels=NA,cex.axis=0.6,tck=0.015)
axis(side=1, labels=NA,cex.axis=0.6,tck=0.015)
posterior <- apply(diag(dim(m)[1],dim(m)[1])*m,2,sum)
max_post <- mu1_grilla[which.max(posterior)]
points(max_post,max_post,pch=19,cex=0.8)
abline(v=max_post,lty=2)
axis(lwd=0,side=1, at=max_post,labels="max" ,las=0,cex.axis=1,line=-0.9)


beta <- 25/6

posterior_ganador <- function(s_1,mu,sigma,beta=25/6){
  return(dnorm(s_1,mu[1],sigma[1])*pnorm(s_1,sum(mu[3:4])-(sum(mu[1:2])-mu[1]),sqrt(sum(sigma^2+beta^2)-sigma[1]^2)))
}
prior <- function(s_1,mu,sigma,beta=25/6){
  return(dnorm(s_1,mu[1],sigma[1]))
}
sorpresa_de_ganar <- function(s_1,mu,sigma,beta=25/6){
  return(pnorm(s_1,mu[1]-(sum(mu[1:2])-sum(mu[3:4])),sqrt(sum(sigma^2+beta^2)-sigma[1]^2)))
}



plot(mu1_grilla, sorpresa_de_ganar(mu1_grilla,mu,sigma),type="l",axes = F,ann = F,lty=1)
lines(mu1_grilla, prior(mu1_grilla,mu,sigma)/max(prior(mu1_grilla,mu,sigma)),lty=2)
lines(mu1_grilla, sorpresa_de_ganar(mu1_grilla,mu,sigma)*prior(mu1_grilla,mu,sigma)/max(prior(mu1_grilla,mu,sigma)),lty=1,lwd=2)

axis(side=2, labels=NA,cex.axis=0.6,tck=0.015)
axis(side=1, labels=NA,cex.axis=0.6,tck=0.015)
axis(lwd=0,side=1, at=0, labels=0,cex.axis=1.25,line=-0.3)
axis(lwd=0,side=1, at=mu[1], labels=expression(mu[i]),cex.axis=1.5,line=-0.5,tck=0.015)
abline(v=mu[1],lty=3)
points(mu[1],sorpresa_de_ganar(mu[1],mu,sigma),pch=19,cex=0.75)
mid = mu[1]-(sum(mu[1:2])-sum(mu[3:4]))
#abline(v=mid,lty=3)
abline(h=0.5,lty=3)

#y <- 0.1
#segments(mid,y,mu[1],y,lwd=2)
#segments(mid,y+.01,mid,y-0.01,lwd=2)
#segments(mu[1],y+.01,mu[1],y-.01,lwd=2)
#text(mid + (mu[1]-mid)/2, y+0.02,expression(delta) ,cex=1.33)


yy <- c(sorpresa_de_ganar(mu1_grilla,mu,sigma),rep(1,length(mu1_grilla)))
xx <- c(mu1_grilla,rev(mu1_grilla))      
polygon(xx,yy,col=rgb(0,0,0,0.3),border=F)

text(mu1_grilla[length(mu1_grilla)%/%5],0.6, "Surprise",srt=0, cex=1.75)

mtext(text= expression(s[1]),side =1,line=2,cex=2)
mtext(text ="Density" ,side =2,line=1,cex=2)

legend(mu1_grilla[3*length(mu1_grilla)%/%5],0.8,lty = c(1,2,1),lwd=c(1,1,2),
       legend = c("Likelihood","Prior","Posterior"),bty = "n",cex = 1.75)




#######################################
# end 
dev.off()
setwd(oldwd)
par(oldpar, new=F)
#########################################