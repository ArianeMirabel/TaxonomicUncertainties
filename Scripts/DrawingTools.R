Draw_Uncertainty<-function(Gradients){
  invisible(lapply(1:3,function(id){
    Mat_index<-Gradients[[id]]
    plot(colnames(Mat_index),Mat_index[1,],type="l",ylim=c(0,max(Mat_index)),xaxt="n",xlab="",ylab="")
    axis(1,at=colnames(Mat_index),colnames(Mat_index))
    if(id==3){mtext("Uncertainty (%)",1,adj=1,line=2,cex=0.6)}
    quant95<-apply(Mat_index,2,function(x){quantile(x,probs=c(0.025,0.975))})
    moy<-apply(Mat_index,2,mean)
    polygon(c(colnames(Mat_index),rev(colnames(Mat_index))),c(pmax(quant95[1,]),pmin(rev(quant95[2,]))),col="gray75")
    lines(colnames(Mat_index), quant95[2,], col = "red",lty = 2)
    lines(colnames(Mat_index), quant95[1,], col = "red",lty = 2)
    points(colnames(Mat_index), moy, col = "black",pch = 3)
    abline(h=moy[1],col="red",lty=2)
  }))
  }
