### This function randomly chooses two subplots for belowground biomass core sampling from the four possible subplots present in a 40x40m Tower plot. The plotID is used as a set.seed so that the function output is reproducible. Subplots are not randomly chosen for 20x20m Tower plots.

### Assumptions made by the function:
##  There is a subfolder within the working directory called "acceptedPlots" in which:
#  There are .csv files providing a list of accepted plots by site, named according to the convention: "acceptedPlots_XXXX.csv", where XXXX is the 4-letter unique site code.
#	There is a "plotID" column in these files.

##	The following variables are required as inputs:
#	siteCode = 4-letter unique NEON site code; must enter with "", e.g. "HARV"

subplotList = function(siteCode, plotType){
  
  #  Load 'TeachingDemos' package so that plotIDs may be turned into unique numeric seeds for randomly choosing subplots
  require(TeachingDemos)
  
  #  Obtain list of accepted plotIDs from "Accepted_plots" folder, and return to current working directory
  acceptedFile = paste("acceptedPlots_",siteCode,".csv", sep="")
  setwd(paste(getwd(),"/acceptedPlots",sep=""))
  plotAccepted = read.csv(acceptedFile, header=T)
  plotID = as.character(plotAccepted$plotID[plotAccepted$plotType==plotType])
  setwd("../")
  
  # Create a dataframe to hold function output, and insert list of desired plotIDs
  subplots.df = data.frame(matrix(data=NA, nrow=length(plotID), ncol=3))
  cNames = c("plotID","subplotID1","subplotID2")
  colnames(subplots.df) = cNames
  subplots.df$plotID = as.character(plotID)
  
  # Create a vector of subplotIDs from which to randomly sample; subplotIDs are specific to a 40m x 40m plot
  theSubplots = c(21,23,39,41)
  
  ##  Use a "for" loop to generate random LIDS azimuths for each value of plotID
  for (i in 1:nrow(subplots.df)){
    # Use the plotID as a set.seed so that randomly selected subplots are reproducible
    randomSeed = subplots.df$plotID[i]
    char2seed(randomSeed, set=TRUE); theChosen = sample(theSubplots, 2)
    
    # Store randomly chosen subplotIDs in subplots.df
    subplots.df$subplotID1[i] = theChosen[1]
    subplots.df$subplotID2[i] = theChosen[2]
    
  }
  
  # Write output to a .csv file
  outputName = paste(siteCode, "coreSubplotList.csv", sep="_")
  write.csv(subplots.df, file=outputName, row.names=FALSE)

# End function
}