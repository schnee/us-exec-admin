library(httr)
library(XML)
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)

url <- "https://en.wikipedia.org/wiki/Islam_by_country"

r <- GET(url)

doc <- readHTMLTable(
  doc=content(r, "text"))

protoDF <- doc[[3]]

colnames(protoDF) <- c("Country", "Muslims", "countryPCT", "worldPCT")

write_csv(protoDF, "./protoDF.csv")

# at this point I manually cleaned the CSV, removing wikipedia annotation reference, range
# markers (I always selected the high-side), and other non-numeric elements.
# saved as protoDF-cleaned.csv

newDF <- read_csv("./protoDF-cleaned.csv")
newDF$Muslim <- as.numeric(gsub(",", "", newDF$Muslim))

newDF <- newDF %>% mutate(nonMuslim = floor(Muslim / (countryPCT/100)))

banned <- c("Iraq", "Syria", "Sudan", "Iran", "Somalia", "Libya", "Yemen")
newDF$isAmericaSafeFrom <- newDF$Country %in% banned

orderedLevels <- newDF %>% arrange(desc(countryPCT)) %>% select(Country)

newDF$Country <- factor(newDF$Country, levels = orderedLevels$Country)

wrap = 40

subtitle <- str_wrap("5(b)...prioritize refugee claims made by individuals on the basis of 
                        religious-based persecution, provided that the religion of the individual 
                    is a minority religion in the individual's country of nationality...", 2*wrap)

ggplot(newDF %>% top_n(40,countryPCT) , aes(Country, y=countryPCT, fill=isAmericaSafeFrom)) + 
  geom_col() + ylab("Percent Muslim Pop.") +
  theme_few() + 
  theme(axis.text.x = element_text(angle=60,hjust=1)) + 
  labs(
    title = str_wrap("PROTECTING THE NATION FROM FOREIGN TERRORIST ENTRY INTO THE UNITED STATES",wrap)
  ) + scale_fill_brewer(name="America Is (Temporarily)\nProtected From This\nCountry", palette = "Dark2")

ggsave(filename = "top40-safe.png", width = 12, height = 8, units = "in", dpi = 72)

ggplot(newDF, aes(Country, y=countryPCT, fill=isAmericaSafeFrom)) + 
  geom_col(width = 1) + ylab("Percent Muslim Pop.") +
  theme_few() + 
  theme(axis.text.x = element_blank()) + 
  labs(
    title = str_wrap("PROTECTING THE NATION FROM FOREIGN TERRORIST ENTRY INTO THE UNITED STATES",wrap)
  ) + scale_fill_brewer(name="America Is (Temporarily)\nProtected From This\nCountry", palette = "Dark2")

ggsave(filename = "all-safe.png", width = 12, height=8, units = "in", dpi = 72)

