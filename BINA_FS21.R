# --- ----------------------------------------------------------------------
# --- CASE STUDY - Der Aktienmarkt während einer Krise
# --- 
# --- (c) June 2021, N. Brandenberg, F. Breca, E. Memedovic
# ---
# --- Description: Prints various plots to show the eleven secotrs of S&P500 and
# ---              how they change during various crises.
# ---
# --- To-Do before run: Change the working directory!
# ---                   To get the optimal plots, please view them on full screen "Zoom-Mode"
# ---
# --- Libraries: ggplot2, readr, dplyr, ... 
# ---
# --- Data:  S&P500 from the 11 different Sectors and dates and periods of various crises
# ---
# --- Version
# ---    V1 June 2021
# --- ----------------------------------------------------------------------

# Set the correct working directory
# getwd()
# setwd("C:/path")

# Install packages automatically if they are not already installed
if(!"ggplot2" %in% rownames(installed.packages())) install.packages("ggplot2", dependencies = TRUE)
if(!"readr" %in% rownames(installed.packages())) install.packages("readr", dependencies = TRUE)
if(!"dplyr" %in% rownames(installed.packages())) install.packages("dplyr", dependencies = TRUE)
if(!"lubridate" %in% rownames(installed.packages())) install.packages("lubridate", dependencies = TRUE)
if(!"Hmisc" %in% rownames(installed.packages())) install.packages("Hmisc", dependencies = TRUE)
if(!"corrplot" %in% rownames(installed.packages())) install.packages("corrplot", dependencies = TRUE)
if(!"RColorBrewer" %in% rownames(installed.packages())) install.packages("RColorBrewer", dependencies = TRUE)
if(!"directlabels" %in% rownames(installed.packages())) install.packages("directlabels", dependencies = TRUE)
if(!"stringr" %in% rownames(installed.packages())) install.packages("stringr", dependencies = TRUE)
if(!"ggforce" %in% rownames(installed.packages())) install.packages("ggforce", dependencies = TRUE)
if(!"gridExtra" %in% rownames(installed.packages())) install.packages("gridExtra", dependencies = TRUE)


# Load packages
library(ggplot2) # Creates various plots
library(readr) # Reading data (csv-files)
library(dplyr) # Data manipulation
library(lubridate) # Date handling
library(Hmisc) # Correlation calculations
library(corrplot) # Visualizing correlation matrix
library(RColorBrewer) # ColorBrewer palettes
library(directlabels) # To label charts
library(stringr) # To modify strings/numbers
library(ggforce) # Used for marked rectangles
library(gridExtra) # Layout for ggplots 

# --- Input-Files ---

# Stock market files
input_file_stock_monthly <- "data/S&P500/11_Sectors_Mnthly_sep1989_mar_2021.csv"
input_file_stock_daily <- "data/S&P500/11_Sectors_Daily_sep1989_mar_2021.csv"
# Crises files
input_file_crises <- "data/Crises/crises_data.csv"

# files <- list.files(path = "data/S&P500/", pattern="*.csv", full.names=TRUE) # List with all CSV-Files in the specified folder

# Read Data
data_stock_monthly <- read.csv2(file = input_file_stock_monthly, na="NA", header = TRUE) # Missing values get marked with "NA"
data_stock_daily <- read.csv2(file = input_file_stock_daily, na="NA", header = TRUE) # Missing values get marked with "NA"
data_crises <- read.csv2(file = input_file_crises, na="NA", header = TRUE)

# Use "data_stock_daily" or "data_stock_mothly"
data_stock <- data_stock_monthly
#data_stock <- data_stock_daily

# --- Data Processing ---

# Check if there are empty lines in data_crises (must be executed before converting dates)
data_crises <- data_crises[!apply(data_crises == "", 1, all), ] # Remove empty rows, doesn't remove row in original file

# Convert all dates in a Date-Format
data_stock$Date = as.Date.character(data_stock$Date, "%d.%m.%Y") # S&P500 Dates
data_stock_daily$Date = as.Date.character(data_stock_daily$Date, "%d.%m.%Y") # S&P500 Dates
data_crises$Start.Date = as.Date.character(data_crises$Start.Date, "%d.%m.%Y") # Crises Start Dates
data_crises$End.Date = as.Date.character(data_crises$End.Date, "%d.%m.%Y") # Crises End Date

# Convert all other character values into numeric values (must be executed after Date change)
data_stock <- data_stock %>%
  mutate_if(is.character,as.numeric)
data_stock_daily <- data_stock_daily %>%
  mutate_if(is.character,as.numeric)

# --- Visualization of monthly S&P500 & crises ---

# (First Option) Simply line chart of S&P500 Technology
plot(data_stock$Date, data_stock$TECHNOLOGIE, type="l", xlab="Year", ylab="Index Points", main="S&P500 IT - Monthly")
axis(1, at=seq(1990,2020,by=1)) # Should rename x-axis, doesn't work on x-axis
plot(data_stock_daily$Date, data_stock_daily$TECHNOLOGIE, type="l", xlab="Year", ylab="Index Points", main="S&P500 IT - Daily") # Daily plot looks very similar over so many years

subset_daily <- subset(data_stock_daily, Date >= as.Date("2012-01-13") & Date <= as.Date("2016-01-01"))
plot(subset_daily$Date, subset_daily$TECHNOLOGIE, type="l", xlab="Year", ylab="Index Points", main="S&P500 IT - Daily") 

# (Second Option) GGPLOT line chart of S&P500 Technology 
ggplot(data=data_stock, aes(x=Date))+
  labs(y="Index Points")+ # label y-axis
  ggtitle("S&P500 - Technologie")+ # Set plot-title
  scale_x_date(limits = as.Date(c("1989-09-01","2021-03-01")), date_breaks = "1 year", date_labels = "%Y")+ # Define x-axis
  geom_line(aes(y=TECHNOLOGIE, color="1"), linetype=1, size=1)+ # Create line-graph
  scale_color_manual(values="steelblue")+ # Set color for graph
  theme_light()+ # Set a light Theme
  # Remove legend on the right, center plot-title and align background grid to x-axis elements
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.grid.minor.x = element_blank())

# Show used color palette
display.brewer.pal(n = 11, name = 'Paired') # Can generate a max of 12 colors (n), "Paired" is the name of the specific palette

# ggplot - Data Processing
g <- ggplot(data=data_stock, aes(x=Date))+ 
  labs(y="Index Points")+ # y-axis label
  scale_x_date(limits = as.Date(c("1989-09-01","2021-03-01")),date_breaks = "2 years", date_labels = "%Y")+ # x-axis specification
  theme(legend.position = "right")+ # Position of the legend
  theme_light() # Set a light Theme

# Add Geom_line for each S&P500 sector to the ggplot
for(i in 2:ncol(data_stock)) {
  print(colnames(data_stock[i]))
  g <- g + geom_line(aes_string(y=colnames(data_stock[i]), color=factor(str_pad(i-1, 2, pad = "0"))), linetype=1, size=1)
}

# Get the labels for all categories
data_labels <- c(colnames(data_stock)[2:ncol(data_stock)])
g <- g + scale_color_manual(values=brewer.pal(n = 11, name = "Paired"), name="S&P500:", labels=data_labels) # line-color specification

g <- g + ggtitle("11 Sectors of S&P500")
g # print S&P500 plot

# Get all different types of crises
crises_types <- data_crises$Type[!duplicated(data_crises$Type)] 
color_count_list <- list() # Used to get the right color for each rect & text and for each type of crisis (dynamically)
plot_list <- list() # List for different plots for each crisis type

# Go through all crises_types
for(i in 1:length(crises_types)) {
  color_count_list[crises_types[i]] <- 0 # Initialize color_counts with 0
  plot_list[[crises_types[i]]] <- g # Create new ggplot for each crisis
  plot_list[[crises_types[i]]] <- plot_list[[crises_types[i]]] + ggtitle(crises_types[i])
}

# Go through each row in data-crises
for(i in 1:nrow(data_crises)) {
  print(data_crises[i,]$Crisis) # Print the name of the current crisis
  print(data_crises[i,]$Start.Date) # Print the Start-Date of the current crisis
  print(data_crises[i,]$End.Date) # Print the End-Date of the current crisis
  print("-----")
  
  # Increment the color-count for each crisis type sperate
  color_count_list[data_crises[i,]$Type] <- color_count_list[[data_crises[i,]$Type]] + 1
  
  # Check if Crisis-Period is at least 1 month (28 Days)
  if(julian(data_crises[i,]$End.Date, data_crises[i,]$Start.Date)<28) {
    data_crises[i,]$End.Date <- ymd(as.Date(data_crises[i,]$Start.Date)) %m+% months(1) # Add one month to the Start-Date of the crisis
  }
  
  # Create a new rectangle
  new_rect <- annotate("rect", xmin=data_crises[i,]$Start.Date, ymin=-Inf, xmax=data_crises[i,]$End.Date, ymax=Inf, fill=brewer.pal(n = 12, name = "Paired")[color_count_list[[data_crises[i,]$Type]]], alpha = 0.15) # Rectangle
  
  # Calc the Date between Start and End of the Crisis to center the text
  mid_date <- as.Date(data_crises[i,]$Start.Date + floor((data_crises[i,]$End.Date-data_crises[i,]$Start.Date)/2))
  
  # Create the text(label) for the current crisis
  new_text <- annotate(geom = "text", x=mid_date, y=2000, label = data_crises[i,]$Crisis, color=brewer.pal(n = 12, name = "Paired")[color_count_list[[data_crises[i,]$Type]]], angle=90, size=5, fontface = "bold")
  
  # Add rect and text to the specific plot in plot_list
  plot_list[[data_crises[i,]$Type]] <- plot_list[[data_crises[i,]$Type]] + new_rect + new_text
  
  
}

# Go through all plots in plot_list (contains plots for eache crisis type)
for(p in plot_list) {
  print(p) # Print the plot in "Plots"
  
  # Second_p can be used to print additional plots for a specific time slot
  #second_p <- p + scale_x_date(limits = as.Date(c("2006-09-01","2010-01-01")),date_breaks = "1 years", date_labels = "%Y")  # x-axis specification
  #print(second_p)
}

# Show all crisis plots together in a grid
#do.call(grid.arrange, c(plot_list, nrow=2))
 
# --- Visualization of Correlation ---

# Correlation Matrix between S&P500
subset_cor <- subset(data_stock, select = 2:ncol(data_stock))
stock_cor <- cor(subset_cor, method = c("spearman"), use="pairwise") # spearman is just a different method to Pearson / "pairwise" will exclude the missing data
stock_cor # prints Correlation matrix in console

corrplot(stock_cor) # Create Correlation plot
corrplot(stock_cor, method = "number") # Create Correlation plot with numbers
