library(rCharts)
library(reshape2)
library(plyr)
library(scales)

citiesCrime=read.csv("citiesCrime.csv")

###### On to Cities

# Correlation matrix of variables
corrmatrix<-cor(citiesCrime[c(-1)]) #store corr matrix
corrdata=as.data.frame(corrmatrix)
corrdata$Variable1=names(corrdata)
corrdatamelt=melt(corrdata,id="Variable1")
names(corrdatamelt)=c("Variable1","Variable2","CorrelationCoefficient")
citiescorrmatplot = rPlot(Variable2 ~ Variable1, color = 'CorrelationCoefficient', data = corrdatamelt, type = 'tile', height = 600)
citiescorrmatplot$addParams(height = 450, width=1000)
citiescorrmatplot$guides("{color: {scale: {type: gradient2, lower: 'red',  middle: 'white', upper: 'blue',midpoint: 0}}}")
citiescorrmatplot$guides(y = list(numticks = length(unique(corrdatamelt$Variable1))))
citiescorrmatplot$guides(x = list(numticks = 3))
citiescorrmatplot$addParams(staggerLabels=TRUE)
citiescorrmatplot$save("corrmatplotcities.html",cdn=T)
citiescorrmatplot


# heatmap of variables and Cities
citiesmelt=ddply(melt(citiesCrime),.(variable),transform,rescale=rescale(value))
names(citiesmelt)=c("City","Crime","value","rescale")
hmapcity <- rPlot(City ~ Crime, color = 'rescale', data = citiesmelt, type = 'tile')
hmapcity$addParams(height = 600, width=1000)
hmapcity$guides(reduceXTicks = FALSE)
hmapcity$guides("{color: {scale: {type: gradient, lower: white, upper: red}}}")
hmapcity$guides(y = list(numticks = length(unique(citiesmelt$City))))
hmapcity$guides(x = list(numticks = 3))
hmapcity$save("heatmapcity.html",cdn=T)
hmapcity

################ Clustering (Quick reference: Quick-R, Kabacoff, http://www.statmethods.net/advstats/cluster.html)
set.seed(123)
kmeansdata=kmeans(citiesCrime[c(-1)],5)  # Decided on 5  - interpretation purposes
# get cluster means 
meanvars=aggregate(citiesCrime[c(-1)],by=list(kmeansdata$cluster),FUN=mean)
# append cluster assignment
citiesclust <- data.frame(citiesCrime, kmeansdata$cluster)

# plotting cities by cluster number
citiesgpplot=dPlot(x="City", y="kmeansdata.cluster",groups="kmeansdata.cluster",data=citiesclust,
                   type="bar", height=475,width=700,bounds = list(x=50, y=10, width=600, height=300))
citiesgpplot$yAxis(type="addCategoryAxis")
citiesgpplot$xAxis(type="addCategoryAxis",orderRule="kmeansdata.cluster")
citiesgpplot$save("citiesgpplot.html",cdn=T)
citiesgpplot

############## Parallel Plot#############
names(meanvars)=c("Group","Rape","KidnapAbduct","DowryDeath","AssaultModesty","InsultModesty","CrueltyHusband",
                  "Importation","ImmoralTraffic","DowryProhibit","IndecentRep")
parrcity <- rCharts$new()
parrcity$field('lib', 'parcoords')
parrcity$set(padding = list(top = 25, left = 5, bottom = 10, right = 0), width=1080, height=400)
parrcity$set(data = toJSONArray(meanvars, json = F), 
             colorby = 'Rape', 
             range = range(meanvars$Rape),
             colors = c('red','green')
)
parrcity$setLib("parcoords")
# parrcity$save("parrcity.html",cdn=T)
parrcity
#  Ditto comment - It was tricky to get the parallel coordinates to save. Using the previous command, 
# generate a viz of the plot, copy the source code (view html source) of the page generated, 
# and paste it into a new text editor and save as html file. That'll take care of it. 
#It has some links to the js libs in the parcoords folder... they have to be in place for it to work.  


##########
# 2012 data for shiny app to plot different states/union territories/cities geographically for every crime type and year - App: indiacrimeshiny
##########
# Data for Cities
save(citiesCrime,file="citiesCrime.rda")

################################## Done #######################