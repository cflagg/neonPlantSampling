###	The function here defines randomized clip-harvest locations for 20m x 20m plots/subplots, based on 3m x 0.5m grid cells that contain clip-strips. Grid cells are placed with a 1m buffer from any plot boundary, and those grid cells that overlap 10m2 nested subplots used for Veg Structure measurements are omitted; for 40 x 40m and larger Tower plots, cells that overlap additional 1m2 nested subplots (surrounded by a 1m buffer, 9m2 total) associated with a 20 x 20m "Distributed plot" superimposed on the plot centroid are also omitted.



###	The function assumes that the starting working directory contains .csv files defining the location of nested subplots within a given plot/subplot.


###	The function assumes there is a subfolder within the working directory called "Accepted_plots" in which:
#	There are .csv files providing a list of accepted plots by site, named according to the convention: "Accepted_plots_XXXX.csv", where XXXX is the 4-letter unique site code.
#	There are "plotID" and "plotType" columns in these files, and plotType = "Distributed" or "Tower" (case sensitive).


###	The following variables are required as inputs:
	#	plotSize = one-sided plot size in meters
	#	plotType = Distributed or Tower; must enter with "", e.g. "Tower"
	#	siteCode = 4-letter unique NEON site code; must enter with "", e.g. "HARV"


clipList = function(plotSize, plotType, siteCode){

#	Load 'TeachingDemos' package so that clipIDs may be turned into unique numeric seeds for grid cell randomization
require(TeachingDemos)

#	Input one-sided subplot size, buffer width, grid cell dimensions, and clip-strip dimensions, all in meters.
subplotSize = 20
bufferWidth = 1
yDistance = subplotSize - 2*bufferWidth
xDistance = subplotSize - 2*bufferWidth
cellHeight = 3
cellWidth = 0.5
clipWidth = 0.1
clipHeight = 2

#	Obtain list of accepted plotIDs from "Accepted_plots" folder, and return to current working directory
acceptedFile = paste("Accepted_plots_",siteCode,".csv", sep="")
setwd(paste(getwd(),"/Accepted_plots",sep=""))
plotAccepted = read.csv(acceptedFile, header=T)
setwd("../")

# For Distributed plots, filter out plotIDs with nlcdClass = DeciduousForest,EvergreenForest,MixedForest
if (plotType=="Distributed"){
  clipDistPlots = plotAccepted[which(plotAccepted$plotType==plotType & !plotAccepted$nlcdClass %in% c("DeciduousForest","EvergreenForest","MixedForest")),]
  plotID = as.character(clipDistPlots$plotID)
  
} else {plotID = as.character(plotAccepted$plotID[plotAccepted$plotType==plotType])}

#	Read in table that specifies x and y ranges for 10m2 nested subplot borders; location of nested subplots must be specified in table referenced below
nestedFile = paste("Nested_coords",plotSize,"plot.csv", sep="_")
nestedCoords = read.csv(nestedFile, header=T)

#	Create subfolder in current working directory to hold .csv output from function, and set as working directory
folderName = paste(siteCode, plotType, "clipList", sep="_")
dir.create(folderName)
setwd(paste(getwd(), "/", folderName, sep=""))



##	Define number of subplots per plotID, define subplotIDs for subplots, calculate number of grid cells per subplot, and the starting number for gridIDs within each subplot (gridIDs are assigned so that the range of gridIDs used per subplot do not overlap within a given plotID).
subplotID = unique(nestedCoords$subplotID)
gridCol = xDistance/cellWidth
gridRow = yDistance/cellHeight
totalCell = gridCol*gridRow


#	Define starting point for gridID series within each subplot
gridStart = c()
gridInit = 0

for (m in 1:length(subplotID)){
gridStart = append(gridStart, gridInit)
gridInit = gridInit + 250
}



##	Use a "for" loop to generate random clip-harvest locations for each plot listed in "plotID"
for (i in 1:length(plotID)){



##	Use a nested "for" loop to generate random clip-harvest locations for each subplot within a given plotID. Leading if/else accounts for the fact that this loop should run only once for 20m x 20m plots.

if (plotSize==20){subplotNum = 1
	} else {subplotNum = length(subplotID)}

for (j in 1:subplotNum){

##	Create a 4-column dataframe to hold clipID, subplotID, and the x and y coords of the SW corner of the clip-strip
	
#	Create the dataframe
grid.df = data.frame(matrix(data=NA, nrow=totalCell, ncol=8))
cNames = c("clipID","subplotID","offsetEasting","offsetNorthing","x1Cell","x2Cell","y1Cell","y2Cell")
colnames(grid.df) = cNames
grid.df$clipID = as.character(grid.df$clipID)
grid.df$subplotID = as.integer(grid.df$subplotID)
grid.df$offsetEasting = as.numeric(grid.df$offsetEasting)
grid.df$offsetNorthing = as.numeric(grid.df$offsetNorthing)
grid.df$x1Cell = as.numeric(grid.df$x1Cell)
grid.df$x2Cell = as.numeric(grid.df$x2Cell)
grid.df$y1Cell = as.numeric(grid.df$y1Cell)
grid.df$y2Cell = as.numeric(grid.df$y2Cell)

#	Create clipIDs and add clipID and subplotID to the matrix
gridID = seq(from=1, to=totalCell, by=1) + gridStart[j]
grid.df[,"clipID"] = paste(plotID[i],formatC(gridID, width=3, format="d", flag="0"), sep="_")

if (plotSize==20){grid.df[,"subplotID"] = NA
	} else {grid.df[,"subplotID"] = subplotID[j]}



##	Create x-axis and y-axis coordinates for the SW corner of each clip-harvest strip within each grid cell and add to the matrix. The clip strip itself is 0.1m W x 2m H oriented North/South, and is centered within the 0.5m x 3m grid cell such that there is a 0.2m buffer on the West/East sides of the strip, and a 0.5m buffer on the North/South sides of the strip.

#	Define X and Y coords of SW corner of grid cells in each row of cells and each column of cells within the plot
xTemp = seq(from=bufferWidth, to=xDistance+cellWidth, by=cellWidth)
yTemp = seq(from=bufferWidth, to=(yDistance-1), by=cellHeight)

#	Create offset for column and row X and Y coords so that position corresponds to clip-strip within grid cell
xOffset = xTemp + (cellWidth-clipWidth)/2
yOffset = yTemp + (cellHeight-clipHeight)/2

#	Calculate SW corner of each clip-strip within each grid cell, as well as grid cell corner positions, and add to the data frame created above. Grid cell corner positions are needed to determine whether individual cells overlap %cover nested subplots.
grid.df[,"offsetEasting"] = rep(xOffset, times=gridRow)
grid.df[,"offsetNorthing"] = rep(yOffset, each=gridCol)
grid.df[,"x1Cell"] = rep(xTemp, times=gridRow)
grid.df[,"x2Cell"] = grid.df$x1Cell + cellWidth
grid.df[,"y1Cell"] = rep(yTemp, each=gridCol)
grid.df[,"y2Cell"] = grid.df$y1Cell + cellHeight



##	Remove rows of "grid.df" that overlap nested subplots used for Veg Structure measurements and %cover estimates; ignore the 16cm of the 10m2 subplot that overlaps the 50 cm grid cell buffer, as technicians will position themselves on the West/East side of clip-strips for harvesting.

#	Create an empty vector to hold list of gridIDs that overlap nested subplots
gridOmit = c()

#	Using location of nested subplots specified in "nestedCoords" table for each subplot, identify overlapping grid cells for subplot in question with a "for" loop, store associated gridIDs in "gridOmit"; then remove elements of "gridOmit" from "grid.df"

if (plotSize==20){nestedTemp = nestedCoords
	} else {nestedTemp = nestedCoords[nestedCoords$subplotID==subplotID[j],]}

for (k in 1:nrow(nestedTemp)){
tempGrid = grid.df[which(grid.df$x1Cell < nestedTemp$x2[k] & nestedTemp$x1[k] < grid.df$x2Cell & grid.df$y1Cell < nestedTemp$y2[k] & nestedTemp$y1[k] < grid.df$y2Cell),]
gridOmit = append(gridOmit, tempGrid$clipID)
#	end of "for" loop
}

gridOmit = sort(gridOmit)
clip.df = grid.df[!(grid.df$clipID %in% gridOmit),]



##	Randomize rows of clip.df and use set.seed to ensure results are repeatable on a per plot/subplot basis. Use if/else to set seed per subplot if plotSize > 20.

if (plotSize==20){randomSeed = plotID[i]
	} else {randomSeed = paste(plotID[i],subplotID[j], sep="_")}

char2seed(randomSeed, set=TRUE); clip.df = clip.df[sample(nrow(clip.df)),]



##	Write randomized data frame of clip-strip locations to a .csv file

#	Remove "cell" columns from data frame so that coordinates are only provided for the clip-strip itself; add columns for 'status' and 'date'
clip.df = clip.df[,1:4]
clip.df$status = ""
clip.df$date = ""

#	Define file name for .csv, using if/else to specify name types for plotSize > 20m x 20m
if (plotSize==20){outputName = paste(plotID[i], "clipList.csv", sep="_")
	} else {outputName = paste(plotID[i], formatC(subplotID[j], width=2, format="d", flag="0"), "clipList.csv", sep="_")}

#	Write output to .csv
write.csv(clip.df, file=outputName, row.names=FALSE)


##	End of subplotID "for" loop
}

##	End of plotID "for" loop
}

setwd("../")

##	End of function
}



