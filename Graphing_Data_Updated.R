#load neccessary packages
require(ggplot2)
require(tidyverse)
require(ggpubr)
require(gridExtra)
require(hrbrthemes)
require(grid)

setwd("/Users/ashnapatel12/Como/UK Covid Data")

# _______________________________________________________________________________________________________

# Import the dataset 
London_data <- read.csv("London_data.csv")

# Make date column read as a date by R.  
London_data$Date <- as.Date(London_data$Date, "%Y/%m/%d") #make the date column compatible with R
# London_data$Date <- months(as.Date(London_data$Date))

# Plotting 3 variables on same plot
combined_plot_London <- ggplot(London_data, aes(x=Date)) +
  geom_line(aes(y=newCasesBySpecimenDate, colour = 'purple'), size=0.5) + 
  
  # Transforming data of newAdmissionsHospital & DailyDeaths to fit on second axis
  geom_line(aes(y=newAdmissionsHospital*20, colour = "blue"), size=0.5) +
  geom_line(aes(y=newDailyNsoDeathsByDeathDate*20, colour = "pink"), size=0.5) +
  scale_color_identity(name = "",
                       breaks = c("purple", "blue", "pink"),
                       labels = c("New Cases", "Hospital Admissions", "Daily Deaths"),
                       guide = "legend") +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") + 
  labs(x="") +
  
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "New Cases",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . / 20, name="Hospital Admissions/Daily Deaths")
  ) + 
  
  theme_minimal() +
  
  theme(
    axis.title.y = element_text(color = "purple", size=9),
    axis.title.y.right = element_text(color = "blue", size=9),
    axis.text.x = element_text(color = "black", size = 7, angle = 45),
    
  ) +
  
  ggtitle("London")


# _______________________________________________________________________________________________________

# Import the dataset 
East_data <- read.csv("EastofEngland_data.csv")

# Make date column read as a date by R.  
East_data$Date <- as.Date(East_data$Date, "%Y/%m/%d") #make the date column compatible with R
# London_data$Date <- months(as.Date(London_data$Date))

combined_plot_East <- ggplot(East_data, aes(x=Date)) +
  geom_line(aes(y=newCasesBySpecimenDate, colour = 'purple'), size=0.5) + 
  geom_line(aes(y=newAdmissionsHospital*20, colour = "blue"), size=0.5) +
  geom_line(aes(y=newDailyNsoDeathsByDeathDate*20, colour = "pink"), size=0.5) +
  scale_color_identity(name = "",
                       breaks = c("purple", "blue", "pink"),
                       labels = c("New Cases", "Hospital Admissions", "Daily Deaths"),
                       guide = "legend") +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+ 
  labs(x="") +
  
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "New Cases",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . / 20, name="Hospital Admissions/Daily Deaths")
  ) + 
  
  theme_minimal() +
  
  theme(
    axis.title.y = element_text(color = "purple", size=9),
    axis.title.y.right = element_text(color = "blue", size=9),
    axis.text.x = element_text(color = "black", size = 7, angle = 45),
    
  ) +
  
  ggtitle("East of England")


# _______________________________________________________________________________________________________

# Import the dataset 
NE_data <- read.csv("NorthEast&Yorkshire&Humber_data.csv")

# Make date column read as a date by R.  
NE_data$Date <- as.Date(NE_data$Date, "%Y/%m/%d") #make the date column compatible with R
# London_data$Date <- months(as.Date(London_data$Date))

combined_plot_NE <- ggplot(NE_data, aes(x=Date)) +
  geom_line(aes(y=newCasesBySpecimenDateNorthEast, colour = "purple"), size=0.5) + 
  #  geom_line(aes(y=newAdmissionsHospital*20), size=1, color="blue") +
  geom_line(aes(y=newDailyNsoDeathsByDeathDateNorthEast*20, colour = "pink"), size=0.5, color="pink") +
  scale_color_identity(name = "",
                       breaks = c("purple", "pink"),
                       labels = c("New Cases", "Daily Deaths"),
                       guide = "legend") +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+ 
  labs(x="") +
  
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "New Cases",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . / 20, name="Hospital Admissions/Daily Deaths")
  ) + 
  
  theme_minimal() +
  
  theme(
    axis.title.y = element_text(color = "purple", size=9),
    axis.title.y.right = element_text(color = "blue", size=9),
    axis.text.x = element_text(color = "black", size = 7, angle = 45),
    
  ) +
  
  ggtitle("North-East")

# _______________________________________________________________________________________________________

# Import the dataset 
Yorkshire_data <- read.csv("NorthEast&Yorkshire&Humber_data.csv")

# Make date column read as a date by R.  
Yorkshire_data$Date <- as.Date(Yorkshire_data$Date, "%Y/%m/%d") #make the date column compatible with R
# London_data$Date <- months(as.Date(London_data$Date))

combined_plot_Yorkshire <- ggplot(Yorkshire_data, aes(x=Date)) +
  geom_line(aes(y=newCasesBySpecimenDateYorkshireAndHumber, colour = "purple"), size=0.5) + 
  geom_line(aes(y=newAdmissionsHospitalNorthEastAndYorkshire*20, colour = "blue"), size=0.5) +
  geom_line(aes(y=newDailyNsoDeathsByDeathDateYorkshireAndHumber*20, colour = "pink"), size=0.5) +
  scale_color_identity(name = "",
                       breaks = c("purple", "blue", "pink"),
                       labels = c("New Cases", "Hospital Admissions", "Daily Deaths"),
                       guide = "legend") +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+ 
  labs(x="") +
  
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "New Cases",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . / 20, name="Hospital Admissions/Daily Deaths")
  ) + 
  
  theme_minimal() +
  
  theme(
    axis.title.y = element_text(color = "purple", size=9),
    axis.title.y.right = element_text(color = "blue", size=9),
    axis.text.x = element_text(color = "black", size = 7, angle = 45),
    
  ) +
  
  ggtitle("North-East")

# _______________________________________________________________________________________________________

# Import the dataset 
NW_data <- read.csv("NorthWest_data.csv")

# Make date column read as a date by R.  
NW_data$Date <- as.Date(NW_data$Date, "%Y/%m/%d") #make the date column compatible with R
# London_data$Date <- months(as.Date(London_data$Date))

combined_plot_NW <- ggplot(NW_data, aes(x=Date)) +
  geom_line(aes(y=newCasesBySpecimenDate, colour = 'purple'), size=0.5) + 
  geom_line(aes(y=newAdmissionsHospital*20, colour = "blue"), size=0.5) +
  geom_line(aes(y=newDailyNsoDeathsByDeathDate*20, colour = "pink"), size=0.5) +
  scale_color_identity(name = "",
                       breaks = c("purple", "blue", "pink"),
                       labels = c("New Cases", "Hospital Admissions", "Daily Deaths"),
                       guide = "legend") +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+ 
  labs(x="") +
  
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "New Cases",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . / 20, name="Hospital Admissions/Daily Deaths")
  ) + 
  
  theme_minimal() +
  
  theme(
    axis.title.y = element_text(color = "purple", size=9),
    axis.title.y.right = element_text(color = "blue", size=9),
    axis.text.x = element_text(color = "black", size = 7, angle = 45),
    
  ) +
  
  ggtitle("North-West")


# _______________________________________________________________________________________________________

# Import the dataset 
SE_data <- read.csv("SouthEast_data.csv")

# Make date column read as a date by R.  
SE_data$Date <- as.Date(SE_data$Date, "%Y/%m/%d") #make the date column compatible with R
# London_data$Date <- months(as.Date(London_data$Date))

combined_plot_SE <- ggplot(SE_data, aes(x=Date)) +
  geom_line(aes(y=newCasesBySpecimenDate, colour = 'purple'), size=0.5) + 
  geom_line(aes(y=newAdmissionsHospital*20, colour = "blue"), size=0.5) +
  geom_line(aes(y=newDailyNsoDeathsByDeathDate*20, colour = "pink"), size=0.5) +
  scale_color_identity(name = "",
                       breaks = c("purple", "blue", "pink"),
                       labels = c("New Cases", "Hospital Admissions", "Daily Deaths"),
                       guide = "legend") +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+ 
  labs(x="") +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "New Cases",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . / 20, name="Hospital Admissions/Daily Deaths")
  ) + 
  
  theme_minimal() +
  
  theme(
    axis.title.y = element_text(color = "purple", size=9),
    axis.title.y.right = element_text(color = "blue", size=9),
    axis.text.x = element_text(color = "black", size = 7, angle = 45),
    
  ) +
  
  ggtitle("South-East")


# _______________________________________________________________________________________________________

# Import the dataset 
SW_data <- read.csv("SouthWest_data.csv")

# Make date column read as a date by R.  
SW_data$Date <- as.Date(SW_data$Date, "%Y/%m/%d") #make the date column compatible with R
# London_data$Date <- months(as.Date(London_data$Date))

combined_plot_SW <- ggplot(SW_data, aes(x=Date)) +
  geom_line(aes(y=newCasesBySpecimenDate, colour = 'purple'), size=0.5) + 
  geom_line(aes(y=newAdmissionsHospital*20, colour = "blue"), size=0.5) +
  geom_line(aes(y=newDailyNsoDeathsByDeathDate*20, colour = "pink"), size=0.5) +
  scale_color_identity(name = "",
                       breaks = c("purple", "blue", "pink"),
                       labels = c("New Cases", "Hospital Admissions", "Daily Deaths"),
                       guide = "legend") +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+ 
  labs(x="") +
  
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "New Cases",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . / 20, name="Hospital Admissions/Daily Deaths")
  ) + 
  
  theme_minimal() +
  
  theme(
    axis.title.y = element_text(color = "purple", size=9),
    axis.title.y.right = element_text(color = "blue", size=9),
    axis.text.x = element_text(color = "black", size = 7, angle = 45),
    
  ) +
  
  ggtitle("South-West")


# _______________________________________________________________________________________________________

main <- ggarrange(combined_plot_East, combined_plot_London, combined_plot_NW, combined_plot_SE, combined_plot_SW, combined_plot_NE,
                  labels = c("A", "B", "C", "D", "E", "F", "G"),
                  ncol = 2, nrow = 3)


# annotate_figure(main,
#                 top = text_grob("UK Covid Data", color = "red", face = "bold", size = 14),
#                 bottom = text_grob("Data source: \n https://coronavirus.data.gov.uk", color = "blue",
#                                    hjust = 1, x = 1, face = "italic", size = 10),
#                 fig.lab = "Figure 1", fig.lab.face = "bold"
)

main




