library(ggthemes)
library(ggplot2)
library(dplyr)
library(readr)


ec <- read_csv("ec.csv")
ec <- ec %>% mutate(winningPct = first/total,
                    label = paste(winner, "-", year, sep=""),
                    isLandslide = year==2016)
ec$label <- with(ec, reorder(label, winningPct, function(x) -x))
ggplot(ec, aes(x=label, y=winningPct, fill=isLandslide)) + geom_bar(stat="identity") + 
  theme_few() + 
  theme(axis.text.x = element_text(angle=60,hjust=1)) + 
  ggtitle("Electoral College Landslides") + 
  ylab("Winner's Percent of EC") + xlab("Winner - Year")

ggsave(filename = "landslide.png", width = 12, height = 8)
