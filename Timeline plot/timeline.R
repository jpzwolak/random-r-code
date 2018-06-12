rm(list=ls())

# loading the required packages; if any of the packages is not installed, use install.packages("name")
require(data.table)
require(ggplot2)
require(grid)
library(dplyr)

# set working directory
setwd("/Users/jpzwolak/Research/Projects/R/random-r-code/Timeline plot")

# read in the data file 
data.full <- read.csv("data.csv")

# re-organize the data frame
data <- melt(data.full,  measure.vars=2:3)
data$value <- as.factor(data$value)
data[data$variable=="performance",]$run <- data[data$variable=="performance",]$run+1

within(data, run <- run + 1)


temp.1 <- data.full[,c(1,2,4,5)]
temp.2 <- cbind(group=1+data.full[,1], data.full[,c(3:5)])
colnames(temp.1) <- colnames(temp.2) <- c("group", "event", "start.time", "end.time")
data2 <- rbind(temp.1, temp.2)

# assignign labels to plots
labels.y <- c("A: car", "A: performannce", "B: car", "B: performannce", "C: car", "C: performannce")

# setting up the timeline data
SetTimeline <- data.table(Set=gsub(" ","",data$value),
                          StartTime = as.POSIXct(data$start.time,tz="",format = "%H:%M:%S"), 
                          EndTime = as.POSIXct(data$end.time,tz="",format = "%H:%M:%S"), 
                          run = data$run)

# setting colors for plots
my_cols <- c("#ACCDEA", "#F0B6AE", "#D1D69C", "#B4AFCA", "#F8A31B","#B4AFCA", "#F8A31B")

pdf("my_plot.pdf", width = 8, height = 2)
ggplot(SetTimeline) +
  geom_segment(aes(colour=Set,
                   x=StartTime, xend=EndTime, 
                   y=run, yend=run, 
                   group = run), size=4) +
  scale_colour_manual(values = my_cols) +
  theme(panel.grid.major.x = element_line(size = 0.25, colour = "grey50",linetype = "dashed"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        panel.ontop = TRUE,
        plot.margin=unit(c(0.25,0,0,0.25), "cm"),
        aspect.ratio=0.15,
        axis.ticks.x = element_line(size = 0.25),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour='black', angle = 0, size = 8, hjust = 0.5, vjust = 1.0),
        axis.text.y = element_text(colour='black', angle = 0, size = 8, hjust = 1.0, vjust = 0.5),
        legend.text = element_text(colour='black', size = 8),
        legend.title = element_text(colour='black', size = 0),
        legend.position = 'bottom',
        plot.title = element_text(colour='black', size = 12),
        panel.spacing  = unit(c(0,0,0,0), "in")) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Timeline with marked sets for two tracks: car vs. performance") +
  scale_y_continuous("Groups",
                     expand = c(0.1,0),
                     labels = labels.y,
                     breaks = c(1:length(labels.y))
                     )
dev.off()


## -- thing to do: adding more ticks on the x-axis
# scale_x_discrete("Time",breaksx,labelsx) +#length(data$start.time)))#breaks=breaksx,labels=labelsx) 
#--- to work with 
#breaksx <- seq(1, length(data$start.time), 5)
#labelsx <- substr(as.POSIXct(data$start.time[seq(1, length(data$start.time), 5)],
#                             tz="",format = "%H:%M:%S",origin = "1900-01-01"),15,19)

## -- thing to do: sort the legend
#my.sort <- factor(c("Audi", "BMW", "good", "average", "poor"))
# https://www.rdocumentation.org/packages/ggplot2/versions/1.0.0/topics/scale_x_continuous

# -- thing to do: add automatic control for what is beign plotted, e.g.,
#labels.y <- c("A-performance", "A-car")
#labels.y <- c("A-performance", "B-performance", "C-performance")
#data <- data.full[data.full$group %in% c(1,2),]
