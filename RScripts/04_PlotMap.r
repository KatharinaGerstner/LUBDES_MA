###########
### create map
library(rworldmap)
newmap <- getMap(resolution = "low")
pdf("Output/map_of_studies.pdf",pointsize=8)
plot(newmap, main="Distribution of LUBDES studies included in the meta analysis")
points(data$lon, data$lat, col = "blue", cex = .6, pch=2)
dev.off()
