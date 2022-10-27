library("openxlsx")
data <- read.xlsx(file.choose())
fonctions <- read.xlsx(file.choose(),sheet="Tonio")
library("ggplot2") ; library("ggrepel")
for (i in 1:nrow(fonctions)){
	xy <- cbind(by(data[,which(colnames(data)%in%fonctions[i,1])],data$Country,mean,na.rm=T),
		by(data[,which(colnames(data)%in%fonctions[i,2])],data$Country,mean,na.rm=T))
	xy <- data.frame(xy)
	mes_tailles <- as.vector(unlist(by(data$CES,data$Country,mean,na.rm=T)))
	mes_couleurs <- by(data$Region,data$Country,unique)
	mes_couleurs <- as.vector(unlist(mes_couleurs))
	xy <- data.frame(xy,mes_couleurs,mes_tailles)
	colnames(xy) <- c(fonctions[i,1],fonctions[i,2],"Region","CES")
	xy <- data.frame(xy)
	.labs <- rownames(xy)
	title <- c(paste0(colnames(xy)[1],"+",colnames(xy)[2],".png"))
	png(title,height=800,width=800)
	myplot <- ggplot(xy, aes(xy[[colnames(xy)[1]]],xy[[colnames(xy)[2]]]))+ geom_point(aes(color = Region, size = CES), alpha = 0.5) +
		geom_label_repel(aes(label = .labs,  color = Region), size = 3) +
  		scale_size(range = c(0.5, 24)) +labs(x =colnames(xy)[1], y = colnames(xy)[2])
	print(myplot)
	dev.off()
}
getwd()

