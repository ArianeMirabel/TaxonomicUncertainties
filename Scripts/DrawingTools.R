Draw_Uncertainty<-function(Gradients){
  invisible(lapply(1:3,function(id){
    Mat_index<-Gradients[[id]]
    plot(colnames(Mat_index),Mat_index[1,],type="l",ylim=c(0,max(Mat_index)),xaxt="n",xlab="",ylab="")
    axis(1,at=colnames(Mat_index),colnames(Mat_index))
    quant95<-apply(Mat_index,2,function(x){quantile(x,probs=c(0.025,0.975))})
    moy<-apply(Mat_index,2,mean)
    polygon(c(colnames(Mat_index),rev(colnames(Mat_index))),c(pmax(quant95[1,]),pmin(rev(quant95[2,]))),col="gray75")
    lines(colnames(Mat_index), quant95[2,], col = "red",lty = 2)
    lines(colnames(Mat_index), quant95[1,], col = "red",lty = 2)
    points(colnames(Mat_index), moy, col = "black",pch = 3)
    abline(h=moy[1],col="red",lty=2)
  }))
  }


Draw_SamplingEffort<-function(Gradients){
  invisible(lapply(1:3,function(ind){
    Gradient<-Gradients[[ind]]
    par(mar=c(0, 2, 2, 1.5))
    plot(colnames(Gradient),Gradient[1,],type="l",ylim=c(0,max(Gradient)),xaxt="n",xlab="",ylab="")
    quant95<-apply(Gradient,2,function(x){quantile(x,probs=c(0.025,0.975))})
    polygon(c(colnames(Gradient),rev(colnames(Gradient))),c(pmax(quant95[1,]),pmin(rev(quant95[2,]))),col="gray75")
    lines(colnames(Gradient), quant95[2,], col = "red",lty = 2)
    lines(colnames(Gradient), quant95[1,], col = "red",lty = 2)
    points(colnames(Gradient), apply(Gradient,2,median), col = "black",pch = 3)
    abline(h=mean(Gradient[,ncol(Gradient)]),col="red",lty=2)
    if(ind==1){mtext("Equivalent Diversity",side=2,cex=0.9,line=2.5)}
    
    par(mar=c(3.1, 2.1, 0, 1.6))
    Gradient_d<-(Gradient-mean(Gradient[,ncol(Gradient)]))/mean(Gradient[,ncol(Gradient)])*100
    plot(colnames(Gradient),Gradient_d[1,],type="l",xaxt="n",xlab="",
         ylim=c(min(Gradient_d),max(Gradient_d)),ylab="")
    quant95_d<-apply(Gradient_d,2,function(x){quantile(x,probs=c(0.025,0.975))})
    polygon(c(colnames(Gradient),rev(colnames(Gradient))),c(pmax(quant95_d[1,]),pmin(rev(quant95_d[2,]))),col="cornsilk2")
    lines(colnames(Gradient), quant95_d[2,], col = "red",lty = 2)
    lines(colnames(Gradient), quant95_d[1,], col = "red",lty = 2)
    points(colnames(Gradient), apply(Gradient_d,2,median), col = "black",pch = 3)
    abline(h=0,col="black",lty=2)
    abline(v=3000,col="black",lty=1)
    axis(1,at=colnames(Gradient_d),labels=colnames(Gradient_d))
    if(ind==1){mtext("Bias (%)",side=2,cex=0.9,line=2.5)}
    if(ind==3){mtext("Sampling effort",side=1,adj=1,line=2.5)}
  }))
  
}




