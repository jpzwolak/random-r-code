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
data <- melt(data.full,  measure.vars=2:3, value.name = "event", value.factor = TRUE)
data$event <- as.factor(data$event)


clean.data <- data.table(Set=gsub(" ","",data$event),
                         group = data$group,
                         type = data$variable)

#
rel.data = clean.data[clean.data$type == tolower()"car",]

pie.val = list()
for (i in 1:3){
  temp <- rel.data[rel.data$group == 2*i-1,]
  pie.val[[i]] <- tapply(temp$length, temp$Set, sum, simplify = TRUE)
}

df <- bind_rows(lapply(pie.val, function(x) as.data.frame(t(x)))) 
df <- cbind(transpose(df), topics = factor(names(df), levels = names(df)))
colnames(df) <- c('Group 1', 'Group 2', 'Group 3', 'topics')
df <- melt(df, id.vars='topics')
df[is.na(df)] <- 0
colnames(df) <- c('topics','group','value')

dat_pies <- left_join(df, by = "group",
                      df %>% 
                        group_by(group) %>%
                        summarize(Cnt_total = sum(value, na.rm=TRUE))) %>%
  group_by(group) %>%
  mutate(label = ifelse(value == 0,"",paste(round(value/Cnt_total*100,1),"%")),
         end_angle = 2*pi*cumsum(value)/Cnt_total,    # ending angle for each pie slice
         start_angle = lag(end_angle, default = 0),   # starting angle for each pie slice
         mid_angle = 0.5*(start_angle + end_angle),   # middle of each pie slice, for the text label
         hjust = ifelse(mid_angle>pi, 1, 0),
         vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1))
dat_pies

rpie = 1 # pie radius
rlabel = 1.05 * rpie # to place labels outside of the pies

ggplot(dat_pies) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = topics)) +
  geom_text(aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle), label = label,
                hjust = hjust, vjust = vjust)) +
  coord_fixed() +
  scale_x_continuous(limits = c(-1.8, 1.8), name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.5, 1.5), name = "", breaks = NULL, labels = NULL) +
  facet_grid(~group)
