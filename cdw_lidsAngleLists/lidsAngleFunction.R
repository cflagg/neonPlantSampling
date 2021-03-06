### This function assigns random orientations to LIDS transects on a per plot basis for CWD tally sampling. The first azimuth is randomly chosen (to the nearest 5 deg), and subsequent azimuths are spaced 120˚ from the first. The plotID is used as a set-seed so that the output can be updated while maintaining consistent output.

### Assumptions made by the function:
##  There is a subfolder within the working directory called "Accepted_plots" in which:
#	There are .csv files providing a list of accepted plots by site, named according to the convention: "Accepted_plots_XXXX.csv", where XXXX is the 4-letter unique site code.
#	There is a "plotID" column in these files.

##	The following variables are required as inputs:
#	siteCode = 4-letter unique NEON site code; must enter with "", e.g. "HARV"


lidsList = function(siteCode, plotType){

#  Load 'TeachingDemos' package so that plotIDs may be turned into unique numeric seeds for angle randomization
require(TeachingDemos)

#  Obtain list of accepted plotIDs from "Accepted_plots" folder, and return to current working directory
acceptedFile = paste("Accepted_plots_",siteCode,".csv", sep="")
setwd(paste(getwd(),"/Accepted_plots",sep=""))
plotAccepted = read.csv(acceptedFile, header=T)
plotID = as.character(plotAccepted$plotID[plotAccepted$plotType==plotType])
setwd("../")

# Create a dataframe to hold function output, and insert list of desired plotIDs
lids.df = data.frame(matrix(data=NA, nrow=length(plotID), ncol=4))
cNames = c("plotID","lidsAzimuth1","lidsAzimuth2","lidsAzimuth3")
colnames(lids.df) = cNames
lids.df$plotID = as.character(plotID)

# Create a vector of angles from which to randomly sample; angles are in 10 deg increments
theAngles = seq(from=0, to=350, by=10)


##  Use a "for" loop to generate random LIDS azimuths for each value of plotID
for (i in 1:nrow(lids.df)){
  
  # Use the plotID as a set.seed so that randomly selected azimuths are reproducible
  randomSeed = lids.df$plotID[i]
  char2seed(randomSeed, set=TRUE); angle1 = sample(theAngles, 1)
  
  # Store "angle" in "angle1" field in lids.df
  lids.df$lidsAngle1[i] = angle1
  
# End "for" loop bracket
}


#  Calculate lidsAzimuth2 and lidsAzimuth3 from lidsAzimuth1
angle2 = lids.df$lidsAngle1 + 120
angle2[angle2 >= 360] = angle2[angle2 >= 360] - 360
angle3 = lids.df$lidsAngle1 + 240
angle3[angle3 >= 360] = angle3[angle3 >= 360] - 360

# Add calculated angles to the dataframe
lids.df$lidsAngle2 = angle2
lids.df$lidsAngle3 = angle3

# Add an empty column for remarks (e.g. "no CWD present")
lids.df$remarks = ""

# Write output to a .csv file
outputName = paste(siteCode, plotType, "lidsList.csv", sep="_")
write.csv(lids.df, file=outputName, row.names=FALSE)

# End function bracket
}