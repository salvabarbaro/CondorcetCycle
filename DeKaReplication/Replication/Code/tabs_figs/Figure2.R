################### FIGURE 2: Party level test p-values ######################

pdf("Fig2.pdf",width=13.5,height=7.25)

par(mfrow=c(1,2),mar=c(5,6,4,1))

plot(data$CorepvalT3,data$FCorepvalT3,axes=F,ann=FALSE)
grid(nx = NULL, ny = NULL,
     lty = 2, col = "gray")
par(new = TRUE)
plot(data$CorepvalT3,data$FCorepvalT3,xlab=expression('p'['C,j']),ylab=expression('p'['NC,j']),
    cex.lab=1.4,xlim = c(0,1),ylim=c(0,1),axes=F,pch = 19, col = 4,cex=1.3)
axis(1,pos=0)
axis(2,pos=0)
axis(1, pos=0, at = c(0.15),labels = c(expression(alpha)),cex.axis=1.5)
axis(2, pos=0, at = c(0.15),labels = c(expression(alpha)),cex.axis=1.5)
lines(x=c(0,1),y=c(0.15,0.15),col = "red",lty = 2, lwd = 2)
lines(x=c(0.15,0.15),y=c(0,1),col = "red",lty = 2, lwd = 2)
text(0.6,0.07,"Strong evidence party is core",cex=1.25)
text(0.07,0.5,"Strong evidence party is not core",cex=1.25,srt=90)
text(0.075,0.08,"Contradictory",cex=0.7)
text(0.075,0.05,"evidence",cex=0.7)
text(0.45,0.45,"Inconclusive evidence",srt=320,cex=1.25)

plot(data$CwinpvalT3,data$FCwinpvalT3,axes=F,ann=FALSE)
grid(nx = NULL, ny = NULL,
     lty = 2, col = "gray")
par(new = TRUE)
plot(data$CwinpvalT3,data$FCwinpvalT3,xlab=expression('p'['CW,j']),ylab=expression('p'['NCW,j']),
    cex.lab=1.4,xlim = c(0,1),ylim=c(0,1),axes=F,pch = 19, col = 4,cex=1.3)
axis(1,pos=0)
axis(2,pos=0)
axis(1, at = c(0.15),labels = c(expression(alpha)),cex.axis=1.5)
axis(2, at = c(0.15),labels = c(expression(alpha)),cex.axis=1.5)
lines(x=c(0,1),y=c(0.15,0.15),col = "red",lty = 2, lwd = 2)
lines(x=c(0.15,0.15),y=c(0,1),col = "red",lty = 2, lwd = 2)
text(0.6,0.07,"Strong evidence party is CW",cex=1.25)
text(0.07,0.5,"Strong evidence party is not CW",cex=1.25,srt=90)
text(0.075,0.08,"Contradictory",cex=0.7)
text(0.075,0.05,"evidence",cex=0.7)
text(0.45,0.45,"Inconclusive evidence",srt=320,cex=1.25)

dev.off()
