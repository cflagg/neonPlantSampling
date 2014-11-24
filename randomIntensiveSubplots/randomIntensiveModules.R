###	Generate random Intensive Modules within Tower Plots 40m x 40m, 60m x 60m, and 80m x 80m. There will be 1, 2, and 4 Intensive modules for each of these plot sizes, respectively.

#	Assume 20 plots per site maximum
plot.num = seq(1:20)
plotID = paste("plot", plot.num, sep="_")


#	Choose one random Intensive Module for 40m x 40m plots going into Ops in 2013
Int.random40 = sample(seq(1:4), size=20, replace=T)
mat40 = as.matrix(cbind(plotID, Int.random40))
write.csv(mat40, file="Random_Intensive_D01_HARV_40.csv")

Int.random40 = sample(seq(1:4), size=20, replace=T)
mat40 = as.matrix(cbind(plotID, Int.random40))
write.csv(mat40, file="Random_Intensive_D03_OSBS_40.csv")

Int.random40 = sample(seq(1:4), size=20, replace=T)
mat40 = as.matrix(cbind(plotID, Int.random40))
write.csv(mat40, file="Random_Intensive_D10_RMNP_40.csv")


#	Choose two random Intensive Modules for 60m x 60m plots
mat60 = matrix(data=NA, nrow=length(plot.num), ncol=2)
rownames(mat60) = plotID
colnames(mat60) = c("Intensive 1","Intensive 2")

for (i in 1:length(plot.num)){
rmods = sort(sample(seq(1:9), size=2, replace=F))
mat60[i,] = rmods
}

write.csv(mat60, file="Random_Intensive_60.csv")


#	Choose 4 random Intensive Modules for 80m x 80m plots
mat80 = matrix(data=NA, nrow=length(plot.num), ncol=4)
rownames(mat80) = plotID
colnames(mat80) = c("Intensive 1","Intensive 2","Intensive 3","Intensive 4")

for (i in 1:length(plot.num)){
rmods = sort(sample(seq(1:16), size=4, replace=F))
mat80[i,] = rmods
}

write.csv(mat80, file="Random_Intensive_80.csv")
