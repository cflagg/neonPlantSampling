Herbaceous clip list README

Clip lists are provided as one .csv file per unique plot or subplot ID, in the following folder structure:

//.../DXX_clipLists/[siteID]_[plotType]_clipList/

where:
XX = domainID
siteID = the 4-letter siteID
plotType = "Distributed" or "Tower"


--For Distributed plots:
* The first 8 characters of the .csv file name correspond to the unique plotID, and there is one .csv file per plot.
* Clip Lists are NOT provided for Distributed plots with NLCDClass = DeciduousForest, EvergreenForest, or MixedForest
* For all other NLCD classes (e.g. WoodyWetlands), technicians must visually assess the plot for % herbaceous cover. If herbaceous cover is ≥ 50%, the plot should be clip harvested; if herbaceous cover is < 50%, do not perform herbaceous clip harvest. Percent herbaceous cover should be assessed from an airborne remote-sensing perspective - i.e. herbaceous cover that exists underneath a woody overstory does not count.


--For Tower plots (20m x 20m):
* Files are named similarly to Distributed plots.


--For Tower plots (40m x 40m and larger):
* The first 8 characters of the .csv file name correspond to the unique plotID, and the two digits that follow the plotID indicate the subplotID. The total number of .csv files per plot is the same as the number of 20m x 20m subplots.
* Herbaceous clip harvest is performed in all Tower plots, regardless of NLCDClass.