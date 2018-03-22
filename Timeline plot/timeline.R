rm(list=ls())

# loading the required packages; if any of the packages is not installed, use install.packages("name")
require(data.table)
require(ggplot2)
require(grid)

setwd("/Users/jpz1/Research/Projects/PER/Kasia")

data.full <- read.csv("T1.csv")

#breaksx <- seq(1, length(data$start.time), 5)
#labelsx <- substr(as.POSIXct(data$start.time[seq(1, length(data$start.time), 5)],
#                             tz="",format = "%H:%M:%S",origin = "1900-01-01"),15,19)

labelsy <- c("First-S", "First-T", "Second-S", "Second-T", "Third-S", "Third-T")

labelsy <- c("First-S", "Second-S")#,  "Third-T")
data <- data.full[data.full$group %in% c(1,2),]


SetTimeline <- data.table(Set=gsub(" ","",tolower(data$event1)),
                          StartTime = as.POSIXct(data$start.time,tz="",format = "%H:%M:%S",origin = "1900-01-01"), 
                          EndTime = as.POSIXct(data$end.time,tz="",format = "%H:%M:%S",origin = "1900-01-01"), 
                          group = data$group)

my_cols <- c("#ACCDEA", "#F0B6AE", "#D1D69C", "#B4AFCA", "#F8A31B","#B4AFCA", "#F8A31B")

ggplot(SetTimeline) +
  geom_segment(aes(colour=Set, 
                   x=StartTime, xend=EndTime, 
                   y=group, yend=group, 
                   group = group), size=5) +
  scale_colour_manual(values = my_cols) +
  theme(panel.grid.major.x = element_line(size = 0.25, colour = "grey50",linetype = "dashed"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        panel.ontop = TRUE,
        aspect.ratio=0.10,
        axis.ticks.x = element_line(size = 0.25),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour='black', angle = 0, size = 8, hjust = 0.5, vjust = 1),
        axis.text.y = element_text(colour='black', angle = 0, size = 8, hjust = 1, vjust = 0.5, 
                                   margin = margin(0, -0.5, 0, 0, "cm")),
        legend.text = element_text(colour='black', size = 8),
        legend.title = element_text(colour='black', size = 0),
        legend.position = 'bottom',
        plot.title = element_text(colour='black', size = 12),
        panel.spacing  = unit(1, "cm")) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Data timeline with marked sets") +
 # scale_x_discrete("Time",breaksx,labelsx) +#length(data$start.time)))#breaks=breaksx,labels=labelsx) 
  scale_y_continuous("Activity",
                     labels = labelsy,
                     breaks = c(1:length(labelsy)),
                     limits = c(0,7)#length(labelsy)+1)
                     )

ggsave("filename.tiff", plot = last_plot(), device = "tiff", dpi = 1500, width = 9, height = 2, units = "in")








