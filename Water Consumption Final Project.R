library(tidyverse)


# QUESTION 1.
# Percentage difference for china in regards to total water consumption, agricultural water use,
# industrial water use, and household water use from 2000-2025.
# What factors play a role in these changes?
# We will answer the same questions for the USA as well.


# QUESTION 2.
# Create a map for Total Consumption and Per Capita water use


# QUESTION 3.
# top 5 and lowest 5 countries with water use per capita in 2000, 2005, 2010, 2020


# QUESTION 4. 
# What is the correlation between water scarcity and rainfall?
# How about the correlation to water scarcity and ground water depletion rate?






# QUESTION 1
install.packages("readxl")
library(readxl)
water_cons <- read_excel("water_cons.xlsx")


Water_Cons_Edited <- water_cons
Water_Cons_Edited <- Water_Cons_Edited |> filter(Country %in% c("USA", "China"))

# China water consumption increase:
(766.41 - 586.04) / 586.04
= 30.8% increase from 2000-2025

# China water use per capita: 
(156.4 - 146.6) / 146.6
= 6.7% increase from 2000-2025

# China agricultural water use %:
(61.3 - 63.8) / 63.8
= -3.9% decrease from 2000-2025

# China industrial water use %
(24.4 - 23.1) / 23.1
= 5.6% increase from 2000-2025

# China household water use %
(14.4 - 13.1) / 13.1
= 9.9% increase from 2000-2025





# USA water consumption increase:
(598.43 - 453.94) / 453.94
= 31.8% increase from 2000-2025

# USA water use per capita:
(482.1 - 489.7) / 489.7
= -1.6% decrease from 2000-2025

# USA agricultural water use %: 
(37.7 - 40.2) / 40.2
= -6.2% decrease from 2000-2025

# USA industrial water use %:
(43.6 - 42.3) / 42.3
= 3.1% increase from 2000-2025

# USA household water use %: 
(18.8 - 17.5) / 17.5
= 7.4% increase from 2000-2025







# QUESTION 2 
install.packages("ggplot2")
install.packages("stringr")
install.packages("maps")
install.packages("dplyr")

library(tidyverse)
library(maps)
library(ggplot2)
library(stringr)



# Mapping Total Water Consumption
Water_Cons_Edited <- water_cons
Water_Cons_Edited <- Water_Cons_Edited |> filter(Year == "2025")
Water_Cons_Edited <- Water_Cons_Edited |> select( `Total Water Consumption (Billion m3)`, "Country")

Map_Total_Cons <- map_data("world")
Merged_Map_And_Water_Cons <- Map_Total_Cons
Merged_Map_And_Water_Cons <- Merged_Map_And_Water_Cons |> left_join(Water_Cons_Edited, by = c("region" = "Country"))
Merged_Map_And_Water_Cons <- Merged_Map_And_Water_Cons |> rename('Water Consumption (Liters)' = `Total Water Consumption (Billion m3)`)

ggplot(Merged_Map_And_Water_Cons) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = `Water Consumption (Liters)`)) + 
    coord_fixed(1.5, xlim = c(-180,180), ylim = c(-60, 85), expand = FALSE) + theme_classic()
ggsave("WaterConsMap4.png")



# Mapping Consumption per Capita

Water_Cons_Edited <- water_cons
Water_Cons_Edited <- Water_Cons_Edited |> filter(Year == "2025")
Water_Cons_Edited <- Water_Cons_Edited |> select("Country", `Per Capita Water Use (L/Day)`)

Merged_Map_And_Water_Cons <- Merged_Map_And_Water_Cons |> left_join(Water_Cons_Edited, by = c("region" = "Country"))

ggplot(Merged_Map_And_Water_Cons) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = `Per Capita Water Use (L/Day)`)) +
    coord_fixed(1.5, xlim = c(-180,180), ylim = c(-60,85), expand = FALSE) + theme_classic()
ggsave("PerCapitaWaterCons.png")





# Question 3

# Highest consumption per capita year 2000:
Water_Cons_Edited <- water_cons
Water_Cons_Edited <- Water_Cons_Edited |> filter(Year == "2000")
Water_Cons_Edited <- Water_Cons_Edited |> arrange(desc(`Per Capita Water Use (L/Day)`))
# 1. USA 
# 2. Canada 
# 3. Central African Republic
# 4. Syria
# 5. Senegal

# Lowest consumption per capita year 2000:
Water_Cons_Edited <- water_cons
Water_Cons_Edited <- Water_Cons_Edited |> filter(Year == "2000")
Water_Cons_Edited <- Water_Cons_Edited |> arrange(`Per Capita Water Use (L/Day)`)
# 1. Nigeria
# 2. Bangladesh
# 3. Ethiopia
# 4. Hong Kong
# 5. South Africa





# Highest consumption per capita year 2005:
Water_Cons_Edited <- water_cons
Water_Cons_Edited <- Water_Cons_Edited |> filter(Year == "2005")
Water_Cons_Edited <- Water_Cons_Edited |> arrange(desc(`Per Capita Water Use (L/Day)`))
# 1. USA 
# 2. Canada 
# 3. Syria
# 4. Malawi
# 5. Senegal

# Lowest consumption per capita year 2005:
Water_Cons_Edited <- water_cons
Water_Cons_Edited <- Water_Cons_Edited |> filter(Year == "2005")
Water_Cons_Edited <- Water_Cons_Edited |> arrange(`Per Capita Water Use (L/Day)`)
# 1. Nigeria
# 2. Bangladesh 
# 3. Ethiopia
# 4. Argentina
# 5. Mozambique





# Highest consumption per capita year 2010:  
Water_Cons_Edited <- water_cons
Water_Cons_Edited <- Water_Cons_Edited |> filter(Year == "2010")
Water_Cons_Edited <- Water_Cons_Edited |> arrange(desc(`Per Capita Water Use (L/Day)`))
# 1. USA 
# 2. Canada 
# 3. Syria
# 4. Turkey
# 5. Israel

# Lowest consumption per capita year 2010:
Water_Cons_Edited <- water_cons
Water_Cons_Edited <- Water_Cons_Edited |> filter(Year == "2010")
Water_Cons_Edited <- Water_Cons_Edited |> arrange(`Per Capita Water Use (L/Day)`)
# 1. Bangladesh 
# 2. Nigeria
# 3. Ethiopia
# 4. Austria
# 5. Mozambique





# Highest consumption per capita year 2020:
Water_Cons_Edited <- water_cons
Water_Cons_Edited <- Water_Cons_Edited |> filter(Year == "2020")
Water_Cons_Edited <- Water_Cons_Edited |> arrange(desc(`Per Capita Water Use (L/Day)`))
# 1. USA 
# 2. Canada 
# 3. Central African Republic
# 4. Jamaica 
# 5. Malawi

# Lowest consumption per capita year 2020: 
Water_Cons_Edited <- water_cons
Water_Cons_Edited <- Water_Cons_Edited |> filter(Year == "2020")
Water_Cons_Edited <- Water_Cons_Edited |> arrange(`Per Capita Water Use (L/Day)`)
# 1. Nigeria
# 2. Ethiopia
# 3. Bangladesh
# 4. Austria
# 5. Mozambique






# Question 4. 

# Water scarcity and Rainfall
water_cons$`Water Scarcity Level` <- as.numeric(factor(water_cons$`Water Scarcity Level`, 
  levels = c("Low", "Moderate", "High", "Critical")))
cor(water_cons$`Rainfall Impact (mm)`, water_cons$`Water Scarcity Level`)
= -0.371
# This indicates a moderately negative relationship; meaning that as rainfall increases, water scarcity decreases.


# Water scarcity and Ground water depletion rate
cor(water_cons$`Water Scarcity Level`, water_cons$`Groundwater Depletion Rate (%)`)
= 0.584
# This indicates a moderately positive correlation with groundwater depletion and water scarcity;
# meaning as water scarcity increases so does the ground water depletion rate.




# Chart for lowest 5 countries total water consumption.
library(tidyverse)
Lowest_5_Scarcity <- water_cons 
Lowest_5_Scarcity <- Lowest_5_Scarcity |> filter(Year == 2025) |> slice_head(n=5)
ggplot(Lowest_5_Scarcity,aes(x = reorder(Country, -`Total Water Consumption (Billion m3)`), 
     y = `Total Water Consumption (Billion m3)`,
        fill = Country)) + 
          geom_bar(stat = "identity") +
             coord_cartesian(ylim = c(0, 400)) + labs(x = "Country", y = "Countries with lowest consumption") + theme_classic()
ggsave("Lowest_Consumption_countries1.png")




# creating scatter plot for correlations from question 4



library(tidyverse)
# rainfall impact and water scarcity plot:
ggplot(Water_Cons_Edited, aes(x =factor(`Water Scarcity Level`,levels = c("Low", "Moderate", "High", "Critical")), y = `Rainfall Impact (mm)`, fill = `Water Scarcity Level`)) +
  theme_classic() + geom_boxplot(outlier.shape = NA) + labs(y = "Rainfall Impact(mm)", x = "Water Scarcity Level")
ggsave("RainfallImpactScarcity.png")


# Groundwater depletion rate and water scarcity plot:
ggplot(Water_Cons_Edited, aes(x = factor(`Water Scarcity Level`,levels = c("Low", "Moderate", "High", "Critical")), y = `Groundwater Depletion Rate (%)`, fill = `Water Scarcity Level`)) + 
  geom_boxplot(outlier.shape = NA) + theme_classic() + labs(x= "Water Scarcity Level", y = "Groundwater Depletion Rate %")
ggsave("Groundwaterdepletionscarcity.png")

































































































































































