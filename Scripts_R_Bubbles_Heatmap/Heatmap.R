library("openxlsx")
data <- read.xlsx("C:/Users/masse/Downloads/7_BDD_Charbon_DEF.xlsx",sheet="Tonio")
colnames(data)
data$CES
data$SIZE # Pour la pondération
CES_cum<-as.vector(by(data$CES*data$SIZE,data$Country,sum))
SIZE_cum <- as.vector(by(data$SIZE,data$Country,sum))
CES_pond<- CES_cum/SIZE_cum
TF  <- as.vector(by(data$TF,data$Country,sum))/SIZE_cum
BSF <- as.vector(by(data$BSF*data$SIZE,data$Country,sum))/SIZE_cum/TF
LF <- as.vector(by(data$LF*data$SIZE/data$TF,data$Country,sum))/SIZE_cum/TF
UF <- as.vector(by(data$UF*data$SIZE/data$TF,data$Country,sum))/SIZE_cum/TF

TF  <- as.vector(by(data$TF,data$Country,sum))/SIZE_cum
BSF <- as.vector(by(data$BSF*data$SIZE,data$Country,sum))/as.vector(by(data$TF*data$SIZE,data$Country,sum))
LF <- as.vector(by(data$LF*data$SIZE,data$Country,sum))/as.vector(by(data$TF*data$SIZE,data$Country,sum))
UF <- as.vector(by(data$UF*data$SIZE,data$Country,sum))/as.vector(by(data$TF*data$SIZE,data$Country,sum))
BSF+LF+UF


library("KefiR")
synth <- data.frame(CES_pond,BSF,LF,UF)
parco(synth,"CES_pond")

plot(TF,CES_pond,col="white")
text(TF,CES_pond,substring(unique(data$Country),1,2))
rownames(synth)<-substring(unique(data$Country),1,9)
heatmap(as.matrix(synth))


palette <- colorRampPalette(c("yellow","pink","#9E1B5D"))(20)
analyses_scaled <- as.matrix(scale(synth))
# Heatmap par défaut
heatmap(analyses_scaled,Colv=T, scale='none', col=palette,cexCol=0.6)
# Heatmap en imposant la fonction hclust pour faire le cluster et sa method "ward.D2"
heatmap(analyses_scaled, Colv=T,hclustfun=function(x) hclust(x, method="ward.D2"), scale='none',cexCol=0.6)
