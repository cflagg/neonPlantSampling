###	[Version 2 update]	Need to add "set.seed" code so that results for each plot_ID are reproducible. Can use char2seed function in TeachingDemos package so that plot_ID itself is used as the seed.

#	Example code using char2seed. As long as "char2seed" is called BEFORE "sample" calls the random number generator, the results from "sample" will be reproducible. The "char2seed" function must be called befor EVERY "sample" call, however, or subsequent calls to "sample" will return different results - that is, "char2seed" only sets the seed for ONE call to "sample".

temp2 = rnorm(100)
char2seed("HARV", set=TRUE); sample(temp2, size=5)
[1] -1.0902813  0.1646833 -0.2014609 -0.1311346 -0.2781221

###	Randomized clip-harvest locations for 20m x 20m plots/modules, based on 3m x 0.5m grid cells that contain clip-strips. Grid cells are placed with a 1m buffer from any plot boundary, and those grid cells that overlap %cover subplots are omitted.

###	Generate clip-harvest locations for a given set of Distributed plots

##	Inputs required for assigning IDs and coordinates to clip-strip locations
#	Obtain list of accepted plotIDs
plot_accepted = read.csv("Accepted_plots_OSBS.csv", header=T)
plot_ID = as.character(plot_accepted$Plot_ID[plot_accepted$Type=="Distributed"])

#	Read in table that specifies x and y ranges for 10m2 subplot borders; location of subplots must be specified in table referenced below
subplots = read.csv("Subplot_coords_20m_plot.csv", header=T)

#	Define 1-sided plot/subplot size in meters in which grid-cell layout will exist, buffer size in meters, and define size of grid cells in meters.
edge.length = 20
buffer.width = 1
sample.height = edge.length - 2*buffer.width
sample.width = edge.length - 2*buffer.width
cell.height = 3
cell.width = 0.5

#	Define clip-strip dimensions within each grid cell
clip.width = 0.1
clip.height = 2

#	Define dimensions of grid-cell layout, using the plot dimensions above.
grid.col = sample.width/cell.width
grid.row = sample.height/cell.height
total.cell = grid.col*grid.row



##	Use a "for" loop to generate random clip-harvest locations for each plot listed in "plot_accepted"
for (i in 1:length(plot_ID)){


##	Create an 8-column matrix to hold clip_ID, grid_ID, x and y coordinates associated with the range of each grid cell, and the x and y coords of the SW corner of the clip-strip
	
#	Create the matrix
grid.mat = matrix(data=NA, nrow=total.cell, ncol=8)
c.names = c("clip_ID","grid_ID","x_coord","y_coord","x1_cell","x2_cell","y1_cell","y2_cell")
colnames(grid.mat) = c.names

#	Create grid_IDs and add to the matrix
grid_ID = seq(from=1, to=total.cell, by=1)
grid.mat[,"grid_ID"] = grid_ID



##	Create x-axis and y-axis coordinates for the SW corner of each clip-harvest strip within each grid cell and add to the matrix. The clip strip itself is 0.1m W x 2m H oriented North/South, and is centered within the 0.5m x 3m grid cell such that there is a 0.2m buffer on the West/East sides of the strip, and a 0.5m buffer on the North/South sides of the strip.

#	Define X and Y coords of SW corner of grid cells in each row of cells and each column of cells within the plot
temp.x = seq(from=buffer.width, to=sample.width+cell.width, by=cell.width)
temp.y = seq(from=buffer.width, to=(sample.height-1), by=cell.height)

#	Create offset for column and row X and Y coords so that position corresponds to clip-strip within grid cell
offset.x = temp.x + (cell.width-clip.width)/2
offset.y = temp.y + (cell.height-clip.height)/2

#	Calculate start and end X and Y coordinates for each grid cell, SW corner of each clip-strip within each grid cell, and add to the matrix created above
cell.x1 = rep(temp.x, times=grid.row)
cell.x2 = cell.x1 + cell.width
cell.y1 = rep(temp.y, each=grid.col)
cell.y2 = cell.y1 + cell.height
clip.x = rep(offset.x, times=grid.row)
clip.y = rep(offset.y, each=grid.col)
grid.mat[,"x1_cell"] = cell.x1
grid.mat[,"x2_cell"] = cell.x2
grid.mat[,"y1_cell"] = cell.y1
grid.mat[,"y2_cell"] = cell.y2
grid.mat[,"x_coord"] = clip.x
grid.mat[,"y_coord"] = clip.y



##	Remove rows of "grid.mat" that overlap 10m2 subplots used for %cover estimates; ignore the 16cm of the 10m2 subplot that overlaps the 50 cm grid cell buffer, as technicians will position themselves on the West/East side of clip-strips for harvesting.

#	Transform grid.mat into a dataframe
grid.df = data.frame(grid.mat)

#	Create an empty vector to hold list of grid_IDs that overlap 10m2 subplots
grid.omit = c()

#	Using location of subplots specified in "subplots", identify overlapping grid cells with a "for" loop, store associated grid_IDs in "grid.omit"; then remove elements of "grid.omit" from "grid.df" and only keep columns associated with clip-strip locations

for (k in 1:nrow(subplots)){
temp.grid = grid.df[which(grid.df$x1_cell < subplots$x2[k] & subplots$x1[k] < grid.df$x2_cell & grid.df$y1_cell < subplots$y2[k] & subplots$y1[k] < grid.df$y2_cell),]
grid.omit = append(grid.omit, temp.grid$grid_ID)
#	end of "for" loop
}

grid.omit = sort(grid.omit)
clip.df = grid.df[!(grid.df$grid_ID %in% grid.omit), c("clip_ID","grid_ID","x_coord","y_coord")]



##	Randomize rows of clip.df and create unique clip_ID that field technicians will assign to biomass bags originating from a given clip-strip.

#	Create unique clip_ID by combining plot_ID with grid_ID, and add to clip.df
clip_ID = paste(plot_ID[i], formatC(clip.df$grid_ID, width=3, format="d", flag="0"), sep="_")
clip.df[,"clip_ID"] = clip_ID

#	Shuffle clip.df by row
random.df = clip.df[sample(nrow(clip.df)),]



##	Write randomized data frame of clip-strip locations to a .csv file

#	Define file name for .csv
output.name = paste(plot_ID[i], "clip_coords.csv", sep="_")

#	Write output to .csv
write.csv(random.df, file=output.name, row.names=FALSE)



##	End of "for" loop
}



