# График Парето 

defect.counts = c(12,29,18,3,34,4)
names(defect.counts) = c("Weather","Overslept","Alarm Failure","Time Change","Traffic","Other")

df.defects <- data.frame(defect.counts)
library(qcc)
pareto.chart(defect.counts)
pareto.chart(defect.counts,main="My Pareto Chart",xlab="Reasons",ylab="Frequency",cex.names=0.6,las=1,col=topo.colors(6))
abline(h=(sum(defect.counts)*.8),col="red",lwd=4)

# A/b testing 

site1 = c(.40, 500) # pink
site2 = c(.30, 550) # black

abtestfunc <- function(ad1, ad2){
      sterror1 = sqrt( ad1[1] * (1-ad1[1]) / ad1[2] )
      sterror2 = sqrt( ad2[1] * (1-ad2[1]) / ad2[2] )
      minmax1 = c((ad1[1] - 1.28*sterror1) * 100, (ad1[1] + 1.28*sterror1) * 100)
      minmax2 = c((ad2[1] - 1.28*sterror2) * 100, (ad2[1] + 1.28*sterror2) * 100)
      print( round(minmax1,2) )
      print( round(minmax2,2) )
}

abtestfunc(site1, site2)
