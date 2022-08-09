library(dplyr)

senegalBase <- read.csv("C:/Users/gusta/Desktop/K-State/Senegal visualizing data app/Test.csv", sep = ";", dec = ",")
senegalBase
names(senegalBase)


place <- subset(senegalBase, select = c(1))


showoptions <- names(subset(senegalBase, select = -c(1,2)))
escolha <- as.name(showoptions[3])

dado <- subset(senegalBase, select = c(escolha))

senegalData <- senegalBase %>%
  group_by(District) %>%
  filter(Year == 2015) %>%
  select(dataUsed = escolha)
  
rm(senegalData)