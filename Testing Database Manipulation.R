library(dplyr)
library(plotly)
library(ggradar)

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
  
rm(un_name)

placeBase <- read.csv("C:/Users/gsantiago/Documents/K-State/Senegal-visualizing-data-app/Data/Full Data.csv")
units <- read.csv("C:/Users/gsantiago/Documents/K-State/Senegal-visualizing-data-app/Data/Units.csv")

place <- toString(names(subset(placeBase, select = c(1))))
period <- toString(names(subset(placeBase, select = c(2))))

showoptions <- names(subset(placeBase, select = -c(1,2)))
escolha <- showoptions[25]
escolha1 <- showoptions[30]
escolha2 <- showoptions[c(20,21)]
escolha3 <- showoptions[50]
escolha4 <- showoptions[15]

escolhas <- c(escolha, escolha1, escolha2, escolha3, escolha4)

unity <- units %>%
          filter(Name %in% escolhas)

unidade <- toString(unity$Unit)

local <- 

nplaceBase <- placeBase %>%
    filter(eval(parse(text = place)) == 'DAKAR',
           eval(parse(text = period)) %in% c(2015,2016,2017)) %>%
    group_by(Years = eval(parse(text = period))) %>%
    summarise_at(vars(escolhas), mean, na.rm=TRUE) %>%
    ungroup() %>%
    mutate_at(vars(-Years), scales::rescale)

nplaceBase[is.na(nplaceBase)] = 0


nplaceBase %>%
  ggradar(
    font.radar = "roboto",
    grid.label.size = 2.5,
    axis.label.size = 3,
    group.point.size = 3
  ) + 
  theme(
    legend.position = "top",
    legend.justification = c(1, 0),
    legend.text = element_text(size = 8, family = "roboto"),
    legend.key = element_rect(fill = NA, color = NA),
    legend.background = element_blank()
  )

