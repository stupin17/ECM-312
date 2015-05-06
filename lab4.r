setwd("/home/stupin17/ECM-312/")
mytable <- read.table("turk.csv", header=TRUE, sep=",")
years <- c(mytable[1:16,1], "sum")
chars <- c("Терроризм","Террорист","Окупация","Наркотики","Насилие","Демократия","Развитие")
countries <- c("Казахстан", "Индия", "Армения", "Китай", "Азербайджан", "Туркменистан", "Сомали")
alltable <- array(0,c(17,7,7),list(years,chars,countries))
attach(mytable)
vars <- c("Kazakhstan.csv", "Индия", "Армения", "China.csv", "Azerbaijan.csv", "turk.csv", "Сомали")
for(s in c(1,4,5,6)){
  mytable <- read.table(vars[s], header=TRUE, sep=",")
  ymax <- max(mytable[][2:8])
  for(y in 1:16){
    for(v in 1:7){
      alltable[y,v,s] <- mytable[y,v+1]
    }
  }
  for(v in 1:7){
    alltable[y+1,v,s] <-sum(mytable[,v+1])
  }
}
alltable[,,1]

suppermax <- max(alltable)
sums <- alltable[17,,]
colsv <- rainbow(8)
for(s in 1:7){
  if(sums[1,s]>0){
    cairo_pdf(filename=paste0(countries[s],"_сум.pdf"),width=10,height=6,onefile=TRUE,pointsize=12,family="serif")
    barplot(sums[,s],xlab="Характеристика", ylab="Количество упоминаний", col=colsv, main=paste0("Частота упоминаний характеристик для страны ", countries[s]))
    dev.off()
  }
}

for(v in 1:7){
  cairo_pdf(filename=paste0(chars[v],".pdf"),width=10,height=6,onefile=TRUE,pointsize=12,family="serif")
  par(mai=c(1.3,.8,.5,.1))
  barplot(sums[v,],cex.sub=.5, las=2, main=paste0("Частота упоминаний характеристики«", chars[v],"»для ряда стран"),col=colsv)
  dev.off()
}