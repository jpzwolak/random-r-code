rm(list=ls())

# loading the required packages; if any of the packages is not installed, use install.packages("name")
require(data.table) 
library(ggplot2)
library(ggforce)
library(dplyr)

# set working directory
setwd("/Users/jpzwolak/Research/Projects/R/random-r-code/Piechart plot")

# read in the data file 
data.full <- read.csv("data.csv")

# re-organize the data frame
df.data <- melt(data.full,  measure.vars=2:3, value.name = "value")
#df.data$value <- as.factor(df.data$value)

# cleaning the data frame
df.data <- data.table(group = df.data$group,
                      category = df.data$variable,
                      value = gsub(" ","",df.data$value),
                      StartTime = as.POSIXct(df.data$start.time,tz="",format = "%H:%M:%S",origin = "1900-01-01"), 
                      EndTime = as.POSIXct(df.data$end.time,tz="",format = "%H:%M:%S",origin = "1900-01-01"))

df.data$length <-  df.data$EndTime - df.data$StartTime

# modifying the group indecies
df.data$group <- ifelse(df.data$category == "performance", df.data$group+1, df.data$group)

# create the pie values reference list for both categories (i.e., by car and by performance)
pie.val = vector("list", length = 2)
for (i in 1:3){
  temp.1 <- df.data[df.data$group == 2*i-1,]
  temp.2 <- df.data[df.data$group == 2*i,]
  pie.val[[1]][[i]] <- tapply(temp.1$length, temp.1$value, sum, simplify = TRUE)
  pie.val[[2]][[i]] <- tapply(temp.2$length, temp.2$value, sum, simplify = TRUE)
}

# create list of data frames with pie data
df.list <- lapply(pie.val, function(y) bind_rows(lapply(y, function(x) as.data.frame(t(x)))))

# re-format the data frames
df.list <- lapply(df.list, function(x) cbind(transpose(x), 
                                             topics = factor(names(x), levels = names(x))))

df.list <- lapply(df.list, setNames, c('Group 1', 'Group 2', 'Group 3', 'value'))
df.list <- lapply(df.list, melt, id.vars='value')
df.list <- lapply(df.list, function(x) {
                              x[is.na(x)] = 0
                              return(x)})
df.list <- lapply(df.list, setNames, c('value','group','instances'))

#defining the pie charts
dat_pies <- function(x){
  x <- left_join(x, by = "group",
                 x %>%
                   group_by(group) %>%
                   summarize(Cnt_total = sum(instances, na.rm=TRUE))) %>%
    group_by(group) %>%
    mutate(label = ifelse(instances == 0,"",paste(round(instances/Cnt_total*100,1),"%")),
           end_angle = 2*pi*cumsum(instances)/Cnt_total,    # ending angle for each pie slice
           start_angle = lag(end_angle, default = 0),   # starting angle for each pie slice
           mid_angle = 0.5*(start_angle + end_angle),   # middle of each pie slice, for the text label
           hjust = ifelse(mid_angle>pi, 1, 0),
           vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1))
  return(x)
}

rpie = 1 # pie radius
rlabel = 1.05 * rpie # to place labels outside of the pies

#plotting pie charts
my_cols <- c("#0000FF", "#FFFF00", "#FF0000", "#B4AFCA", "#F8A31B","#B4AFCA", "#F8A31B")

pie.plot <- function(x){
  ggplot(x) +
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                     start = start_angle, end = end_angle, fill = value)) +
    geom_text(aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle), label = label,
                  hjust = hjust, vjust = vjust)) +
    scale_fill_manual("Value",
                      breaks = ,
                      values = my_cols) +
    coord_fixed() +
    scale_x_continuous(limits = c(-1.8, 1.8), name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.5, 1.5), name = "", breaks = NULL, labels = NULL) +
    facet_grid(~group)
}

for (i in 1:2){
  p <- pie.plot(dat_pies(df.list[[i]]))
  ggsave(p, filename=paste0("my_plot_",i,".pdf"), width = 8, height = 2, dpi = 300, units = "in")
}
