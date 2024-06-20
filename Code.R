setwd("C:/Users/burgo/OneDrive/Documentos/Projetos de Análise de Dados/Projeto 3/Data")
getwd()

# Loading required libraries
library(dplyr)  # For data manipulation
library(ggplot2)  # For plotting
library(tidyr)  # For data reshaping

# Reading the data
data <- read.csv("vgsales.csv")

# List all unique platforms
unique_platforms <- data %>%
  select(Platform) %>%
  distinct() %>%
  arrange(Platform)

# Define portable platforms
portable_platforms <- c("DS", "3DS", "GB", "GBA", "GG", "PSP", "PSV", "WS")

# Define console platforms
console_platforms <- c("2600", "3DO", "DC", "GC", "GEN", "N64", "NES", "NG", "PC", "PCFX",
                       "PS", "PS2", "PS3", "PS4", "SAT", "SCD", "SNES", "TG16", "Wii", "WiiU",
                       "X360", "XB", "XOne")

#------------------------------------------------------------------

# Calculate annual sales for portable platforms
portable_sales <- data %>%
  filter(Platform %in% portable_platforms) %>%
  group_by(Year) %>%
  summarise(Portable_Sales = sum(Global_Sales, na.rm = TRUE))

# Calculate annual sales for console platforms
console_sales <- data %>%
  filter(Platform %in% console_platforms) %>%
  group_by(Year) %>%
  summarise(Console_Sales = sum(Global_Sales, na.rm = TRUE))

# Combine portable and console sales into a single table
sales_by_year <- full_join(portable_sales, console_sales, by = "Year") %>%
  arrange(Year) %>%
  filter(!is.na(Portable_Sales) & !is.na(Console_Sales))  # Remove rows with missing values

# Create pie charts for each year
years <- unique(sales_by_year$Year)

# Check if the 'Results' directory exists, if not, create it
results_dir <- "Results"
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
  if (dir.exists(results_dir)) {
    message("Directory 'Results' created successfully.")
  } else {
    stop("Failed to create directory 'Results'.")
  }
} else {
  message("Directory 'Results' already exists.")
}

# Loop to generate pie charts for each year
for (year in years) {
  # Subset data for the current year
  year_data <- sales_by_year %>% filter(Year == year)
  
  # Create a long-format data frame for plotting
  pie_data <- data.frame(
    Platform_Type = c("Portable", "Console"),
    Sales = c(year_data$Portable_Sales, year_data$Console_Sales)
  )
  
  # Add percentage column
  pie_data <- pie_data %>%
    mutate(Percentage = Sales / sum(Sales) * 100)
  
  # Create the pie chart
  pie_chart <- ggplot(pie_data, aes(x = "", y = Sales, fill = Platform_Type)) +
    geom_bar(stat = "identity", width = 1, color = "black") +  # Add black borders
    coord_polar("y") +
    labs(title = paste("Sales Distribution in", year),
         x = "",
         y = "") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right") +
    scale_fill_manual(values = c("Portable" = "skyblue", "Console" = "orange")) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), 
              color = "black", size = 4)
  
  # Save the pie chart
  file_path <- paste0(results_dir, "/sales_distribution_", year, ".png")
  ggsave(filename = file_path, plot = pie_chart, width = 6, height = 6)
  if (file.exists(file_path)) {
    message(paste("Chart saved successfully at:", file_path))
  } else {
    message(paste("Failed to save chart at:", file_path))
  }
}

# Filter data for the period from 1989 to 2016
portable_sales_filtered <- portable_sales %>% filter(Year >= 1989 & Year <= 2016)
console_sales_filtered <- console_sales %>% filter(Year >= 1989 & Year <= 2016)

# Calculate total sales of portables and consoles in the filtered period
total_portable_sales <- sum(portable_sales_filtered$Portable_Sales, na.rm = TRUE)
total_console_sales <- sum(console_sales_filtered$Console_Sales, na.rm = TRUE)

# Create a data frame for the overall comparison chart
total_sales_data <- data.frame(
  Platform_Type = c("Portable", "Console"),
  Sales = c(total_portable_sales, total_console_sales)
)

# Add percentage column
total_sales_data <- total_sales_data %>%
  mutate(Percentage = Sales / sum(Sales) * 100)

# Create pie chart for total sales
total_pie_chart <- ggplot(total_sales_data, aes(x = "", y = Sales, fill = Platform_Type)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Add black borders
  coord_polar("y") +
  labs(title = "Total Sales Comparison (1989-2016): Portable vs Console",
       x = "",
       y = "") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right") +
  scale_fill_manual(values = c("Portable" = "skyblue", "Console" = "orange")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4)

# Save the total sales pie chart
total_sales_file_path <- paste0(results_dir, "/total_sales_comparison_1989_2016.png")
ggsave(filename = total_sales_file_path, plot = total_pie_chart, width = 6, height = 6)
if (file.exists(total_sales_file_path)) {
  message(paste("Chart saved successfully at:", total_sales_file_path))
} else {
  message(paste("Failed to save chart at:", total_sales_file_path))
}




#---------------------------------------------------------------------------

# Define colors for each platform
platform_colors <- c("3DS" = "purple",
                     "DS" = "red",
                     "GBA" = "green",
                     "GB" = "grey",
                     "WS" = "orange",
                     "PSP" = "skyblue",
                     "PSV" = "yellow",
                     "GG" = "black")

# Function to create bar and line charts
create_sales_chart <- function(data, chart_type, filename) {
  plot <- ggplot(data, aes(x = as.factor(Year), y = Total_Sales, fill = Platform, color = Platform, group = Platform)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = chart_type,
         x = "Year",
         y = "Total Sales") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = platform_colors[unique(data$Platform)]) +
    scale_color_manual(values = platform_colors[unique(data$Platform)])
  
  print(plot)
  ggsave(filename = filename, plot = plot, width = 10, height = 6)
}

# Function to create pie chart and save
create_pie_chart <- function(data, title, filename) {
  total_sales <- data %>%
    group_by(Platform) %>%
    summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100)
  
  pie_chart <- ggplot(total_sales, aes(x = "", y = Percentage, fill = Platform)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar("y") +
    labs(title = title,
         x = "",
         y = "") +
    theme_void() +
    theme(legend.position = "right") +
    scale_fill_manual(values = platform_colors[unique(total_sales$Platform)]) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              position = position_stack(vjust = 0.5))
  
  print(pie_chart)
  ggsave(filename = filename, plot = pie_chart, width = 6, height = 6)
}

# Read data
data <- read.csv("vgsales.csv")

# Portable platforms and years filter
portable_platforms <- c("DS", "3DS", "GB", "GBA", "GG", "PSP", "PSV", "WS")
portable_sales <- data %>%
  filter(Platform %in% portable_platforms & Year >= 1989 & Year <= 2016) %>%
  group_by(Platform, Year) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE))

# Create and save bar chart
bar_chart_filename <- "Results/portable_sales_bar_chart.png"
create_sales_chart(portable_sales, "Total Sales of Portable Platforms (1989-2016)", bar_chart_filename)

# Create and save line chart
line_chart_filename <- "Results/portable_sales_line_chart.png"
create_sales_chart(portable_sales, "Total Sales of Portable Platforms (1989-2016)", line_chart_filename)

# Create and save yearly bar charts
for (year in unique(portable_sales$Year)) {
  year_data <- portable_sales %>% filter(Year == year)
  year_chart_filename <- paste0("Results/portable_sales_", year, ".png")
  create_sales_chart(year_data, paste("Total Sales of Portable Platforms (", year, ")", sep = ""), year_chart_filename)
}

# DS vs PSP sales comparison
sales_ds_psp <- data %>%
  filter(Platform %in% c("DS", "PSP"))

create_pie_chart(sales_ds_psp, "DS vs PSP Sales Percentage", "Results/ds_vs_psp_sales_percentage.png")

# 3DS vs PSVita sales comparison
sales_3ds_psv <- data %>%
  filter(Platform %in% c("3DS", "PSV"))

create_pie_chart(sales_3ds_psv, "3DS vs PSV Sales Percentage", "Results/3ds_vs_psv_sales_percentage.png")

#--------------------------------------------------------------------------------------



# Define console platforms for each family
xbox <- c("XB", "X360", "XOne")
playstation <- c("PS", "PS2", "PS3", "PS4")
nintendo <- c("NES", "SNES", "N64", "GC", "Wii", "WiiU")
sega <- c("DC", "SCD", "GEN", "SAT")

# Function to create pie chart for a specific family and save as image
create_pie_chart <- function(data, platforms, family_name) {
  # Filter data for platforms in this family
  family_data <- data %>%
    filter(Platform %in% platforms) %>%
    group_by(Platform) %>%
    summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Platform, " (", round(Percentage, 1), "%)"))
  
  # Create pie chart with ggplot
  pie_chart <- ggplot(family_data, aes(x = "", y = Total_Sales, fill = Platform)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar(theta = "y") +
    labs(title = paste("Global Sales Distribution for", family_name),
         fill = "Platform") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5))
  
  # Save the pie chart as image
  ggsave(filename = paste0("Results/", family_name, "_sales_percentage.png"), plot = pie_chart, width = 6, height = 6)
  
  return(pie_chart)
}

# Create pie charts for each family and save images (loop can be used for efficiency)
xbox_pie <- create_pie_chart(data, xbox, "Xbox")
playstation_pie <- create_pie_chart(data, playstation, "PlayStation")
nintendo_pie <- create_pie_chart(data, nintendo, "Nintendo")
sega_pie <- create_pie_chart(data, sega, "Sega")

# Function to categorize platforms into families
categorize_family <- function(platform) {
  if (platform %in% xbox) {
    "Xbox"
  } else if (platform %in% playstation) {
    "PlayStation"
  } else if (platform %in% nintendo) {
    "Nintendo"
  } else if (platform %in% sega) {
    "Sega"
  } else {
    NA
  }
}

# Apply function to categorize platforms (sapply can be replaced with vectorized approach)
data$Family <- lapply(data$Platform, categorize_family)  # Consider using vectorization for efficiency

# Filter data for platforms belonging to defined families
data_filtered <- data %>%
  filter(!is.na(Family))

# Calculate annual sales for each family
family_sales <- data_filtered %>%
  group_by(Year, Family) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(Year)

# Create line graph for annual sales
line_graph <- ggplot(family_sales, aes(x = Year, y = Total_Sales, color = Family, group = Family)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Add points to highlight annual sales
  labs(title = "Yearly Sales of Console Families",
       x = "Year",
       y = "Total Sales",
       color = "Console Family") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print and save line graph
print(line_graph)
ggsave(filename = "Results/console_families_yearly_sales.png", plot = line_graph, width = 10, height = 6)

#----------------------------------------------------------------------



# Summarize sales by genre for each region
genre_sales_na <- data %>%
  group_by(Genre) %>%
  summarise(Total_Sales = sum(NA_Sales, na.rm = TRUE))

genre_sales_eu <- data %>%
  group_by(Genre) %>%
  summarise(Total_Sales = sum(EU_Sales, na.rm = TRUE))

genre_sales_jp <- data %>%
  group_by(Genre) %>%
  summarise(Total_Sales = sum(JP_Sales, na.rm = TRUE))

# Function to create pie chart for genre sales by region
create_genre_pie_chart <- function(data, region) {
  data <- data %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Genre, " (", round(Percentage, 1), "%)"))
  
  pie_chart <- ggplot(data, aes(x = "", y = Total_Sales, fill = Genre)) +
    geom_bar(stat = "identity", width = 0.9, color = "black", size = 0.5) +  # Reduced width for margin and added color for separation
    coord_polar(theta = "y") +
    labs(title = paste("Genre Distribution in", region),
         fill = "Genre") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the pie chart
  ggsave(filename = paste0("Results/genre_distribution_", gsub(" ", "_", region), ".png"), plot = pie_chart, width = 8, height = 8)
  
  return(pie_chart)
}

# Create and save pie charts for each region
na_pie <- create_genre_pie_chart(genre_sales_na, "North America")
eu_pie <- create_genre_pie_chart(genre_sales_eu, "Europe")
jp_pie <- create_genre_pie_chart(genre_sales_jp, "Japan")

# Display the pie charts
print(na_pie)
print(eu_pie)
print(jp_pie)

#--------------------------------------------------------



# Categorize platforms into families
xbox <- c("XB", "X360", "XOne")
playstation <- c("PS", "PS2", "PS3", "PS4")
nintendo <- c("NES", "SNES", "N64", "GC", "Wii", "WiiU")
sega <- c("DC", "SCD", "GEN", "SAT")

# Function to create pie chart for a specific family
create_pie_chart <- function(data, family_name) {
  data <- data %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Genre, " (", round(Percentage, 1), "%)"))
  
  pie_chart <- ggplot(data, aes(x = "", y = Total_Sales, fill = Genre)) +
    geom_bar(stat = "identity", width = 0.9, color = "black", size = 0.5) +  # Black borders
    coord_polar(theta = "y") +
    labs(title = paste("Genre Distribution in", family_name),
         fill = "Genre") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the pie chart
  ggsave(filename = paste0("Results/genre_distribution_", gsub(" ", "_", family_name), ".png"), plot = pie_chart, width = 8, height = 8)
  
  return(pie_chart)
}

# Function to categorize platforms into families
categorize_family <- function(platform) {
  if (platform %in% xbox) {
    return("Xbox")
  } else if (platform %in% playstation) {
    return("PlayStation")
  } else if (platform %in% nintendo) {
    return("Nintendo")
  } else if (platform %in% sega) {
    return("Sega")
  } else {
    return(NA)
  }
}

# Apply the function to categorize platforms
data$Family <- sapply(data$Platform, categorize_family)

# Filter out rows where Family is NA
data_filtered <- data %>%
  filter(!is.na(Family))

# Summarize sales by genre for each family
genre_sales_family <- data_filtered %>%
  group_by(Family, Genre) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  ungroup()

# Create pie charts for each family and save the images
families <- c("Xbox", "PlayStation", "Nintendo", "Sega")
pie_charts <- lapply(families, function(family) {
  create_pie_chart(genre_sales_family %>% filter(Family == family), family)
})

# Display the charts
lapply(pie_charts, print)

#--------------------------------------------------------



data$Year <- as.numeric(as.character(data$Year))

# Remove rows with NA values in Year
data <- data %>% filter(!is.na(Year))

# Extract the decade from the year
data <- data %>%
  mutate(Decade = floor(Year / 10) * 10)

# Summarize global sales by genre for each decade
genre_sales_decade <- data %>%
  group_by(Decade, Genre) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  ungroup()

# Function to generate pie charts with margins
create_pie_chart <- function(data, decade) {
  data <- data %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Genre, " (", round(Percentage, 1), "%)"))
  
  pie_chart <- ggplot(data, aes(x = "", y = Total_Sales, fill = Genre)) +
    geom_bar(stat = "identity", width = 0.9, color = "black", size = 0.5) +  # Black borders
    coord_polar(theta = "y") +
    labs(title = paste("Genre Distribution in the", decade),
         fill = "Genre") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the pie chart
  ggsave(filename = paste0("Results/genre_distribution_", decade, ".png"), plot = pie_chart, width = 8, height = 8)
  
  return(pie_chart)
}

# Get unique decades
decades <- unique(genre_sales_decade$Decade)

# Create and display pie charts for each decade
lapply(decades, function(decade) {
  pie_chart <- create_pie_chart(genre_sales_decade %>% filter(Decade == decade), paste(decade, "s"))
  print(pie_chart)
})

#----------------------------------------------------------



# Function to categorize platforms into families
categorize_family <- function(platform) {
  xbox <- c("XB", "X360", "XOne")
  playstation <- c("PS", "PS2", "PS3", "PS4")
  nintendo <- c("NES", "SNES", "N64", "GC", "Wii", "WiiU")
  sega <- c("DC", "SCD", "GEN", "SAT")
  
  if (platform %in% xbox) {
    return("Xbox")
  } else if (platform %in% playstation) {
    return("PlayStation")
  } else if (platform %in% nintendo) {
    return("Nintendo")
  } else if (platform %in% sega) {
    return("Sega")
  } else {
    return(NA)
  }
}

# Apply the function to categorize platforms
data$Family <- sapply(data$Platform, categorize_family)

# Filter rows where Family is not NA
data_filtered <- data %>% filter(!is.na(Family))

# Summarize global sales by genre for each family
genre_sales_family <- data_filtered %>%
  group_by(Family, Genre) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  ungroup()

# Function to generate pie charts with margins
create_pie_chart <- function(data, family_name) {
  data <- data %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Genre, " (", round(Percentage, 1), "%)"))
  
  pie_chart <- ggplot(data, aes(x = "", y = Total_Sales, fill = Genre)) +
    geom_bar(stat = "identity", width = 0.9, color = "black", size = 0.5) +  # Black borders
    coord_polar(theta = "y") +
    labs(title = paste("Genre Distribution in", family_name),
         fill = "Genre") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the pie chart
  ggsave(filename = paste0("Results/genre_distribution_", family_name, ".png"), plot = pie_chart, width = 8, height = 8)
  
  return(pie_chart)
}

# Create and display pie charts for each family
families <- c("Xbox", "PlayStation", "Nintendo", "Sega")
pie_charts <- lapply(families, function(family) {
  pie_chart <- create_pie_chart(genre_sales_family %>% filter(Family == family), family)
  print(pie_chart)
})

#----------------------------------------



# Função para filtrar, resumir dados por família de console por editora, e combinar pequenas editoras
summarize_and_combine_publishers <- function(data, platforms) {
  summarized_data <- data %>%
    filter(Platform %in% platforms) %>%
    group_by(Publisher) %>%
    summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100)
  
  # Combinar editoras com menos de 2% das vendas em "Others"
  combined_data <- summarized_data %>%
    mutate(Publisher = ifelse(Percentage < 2, "Others", Publisher)) %>%
    group_by(Publisher) %>%
    summarise(Total_Sales = sum(Total_Sales)) %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100) %>%
    ungroup()
  
  combined_data
}

# Resumir e combinar editoras para cada família de console
xbox_publisher_sales <- summarize_and_combine_publishers(data, xbox)
playstation_publisher_sales <- summarize_and_combine_publishers(data, playstation)
nintendo_publisher_sales <- summarize_and_combine_publishers(data, nintendo)
sega_publisher_sales <- summarize_and_combine_publishers(data, sega)

# Função para criar gráficos de pizza com margens
create_pie_chart <- function(data, title) {
  data <- data %>%
    mutate(Label = paste0(Publisher, " (", round(Percentage, 1), "%)"))
  
  pie_chart <- ggplot(data, aes(x = "", y = Total_Sales, fill = Publisher)) +
    geom_bar(stat = "identity", width = 1, color = "black", size = 0.5) +  # Bordas pretas
    coord_polar(theta = "y") +
    labs(title = title, fill = "Publisher") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Salvar o gráfico de pizza
  ggsave(filename = paste0("Results/publisher_distribution_", gsub(" ", "_", tolower(title)), ".png"), plot = pie_chart, width = 8, height = 8)
  
  return(pie_chart)
}

# Criar e exibir gráficos de pizza para cada família de console
xbox_pie_chart <- create_pie_chart(xbox_publisher_sales, "Publisher Distribution for Xbox Platforms")
playstation_pie_chart <- create_pie_chart(playstation_publisher_sales, "Publisher Distribution for PlayStation Platforms")
nintendo_pie_chart <- create_pie_chart(nintendo_publisher_sales, "Publisher Distribution for Nintendo Platforms")
sega_pie_chart <- create_pie_chart(sega_publisher_sales, "Publisher Distribution for Sega Platforms")

print(xbox_pie_chart)
print(playstation_pie_chart)
print(nintendo_pie_chart)
print(sega_pie_chart)

#------------------------------------------------------------------------



# Function to filter, summarize data by console family by publisher, and combine small publishers
summarize_and_combine_publishers <- function(data, platforms) {
  summarized_data <- data %>%
    filter(Platform %in% platforms) %>%
    group_by(Publisher) %>%
    summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100)
  
  # Combine publishers with less than 2% sales into "Others"
  combined_data <- summarized_data %>%
    mutate(Publisher = ifelse(Percentage < 2, "Others", Publisher)) %>%
    group_by(Publisher) %>%
    summarise(Total_Sales = sum(Total_Sales)) %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100) %>%
    ungroup()
  
  return(combined_data)
}

# Summarize and combine publishers for each console family
xbox_publisher_sales <- summarize_and_combine_publishers(data, xbox)
playstation_publisher_sales <- summarize_and_combine_publishers(data, playstation)
nintendo_publisher_sales <- summarize_and_combine_publishers(data, nintendo)
sega_publisher_sales <- summarize_and_combine_publishers(data, sega)

# Function to create pie charts with margins
create_pie_chart <- function(data, title) {
  data <- data %>%
    mutate(Label = paste0(Publisher, " (", round(Percentage, 1), "%)"))
  
  pie_chart <- ggplot(data, aes(x = "", y = Total_Sales, fill = Publisher)) +
    geom_bar(stat = "identity", width = 1, color = "black", size = 0.5) +  # Black borders
    coord_polar(theta = "y") +
    labs(title = title, fill = "Publisher") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the pie chart
  ggsave(filename = paste0("Results/publisher_distribution_", gsub(" ", "_", tolower(title)), ".png"), plot = pie_chart, width = 8, height = 8)
  
  return(pie_chart)
}

# Create and display pie charts for each console family
xbox_pie_chart <- create_pie_chart(xbox_publisher_sales, "Publisher Distribution for Xbox Platforms")
playstation_pie_chart <- create_pie_chart(playstation_publisher_sales, "Publisher Distribution for PlayStation Platforms")
nintendo_pie_chart <- create_pie_chart(nintendo_publisher_sales, "Publisher Distribution for Nintendo Platforms")
sega_pie_chart <- create_pie_chart(sega_publisher_sales, "Publisher Distribution for Sega Platforms")

print(xbox_pie_chart)
print(playstation_pie_chart)
print(nintendo_pie_chart)
print(sega_pie_chart)

#-------------------------------------------------------------------



# Summarize sales data by publisher and year
sales_by_publisher_year <- data %>%
  group_by(Publisher, Year) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  ungroup()

# Filter to show only the top 10 publishers by total sales
top_publishers <- sales_by_publisher_year %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(Total_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 10) %>%
  pull(Publisher)

sales_by_publisher_year <- sales_by_publisher_year %>%
  filter(Publisher %in% top_publishers)

# Create the line graph
line_graph <- ggplot(sales_by_publisher_year, aes(x = Year, y = Total_Sales, color = Publisher, group = Publisher)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Optional: add points to highlight the data for each year
  labs(title = "Yearly Sales by Top 10 Publishers",
       x = "Year",
       y = "Total Sales (in millions)",
       color = "Publisher") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the line graph
print(line_graph)

# Save the graph as PNG
ggsave(filename = "Results/yearly_sales_top_10_publishers.png", plot = line_graph, width = 10, height = 8)

#-----------------------------------------------------------
#Other stuf for the analysis the code here is mostly for creating data to explain other data thus it is not organized as before

# Read the data
data <- read.csv("vgsales.csv")

# Create results directory if it does not exist
if (!dir.exists("results")) {
  dir.create("results")
}

# Filter data for Japanese sales
jp_sales <- data %>%
  select(Genre, Publisher, JP_Sales) %>%
  filter(JP_Sales > 0)

# Summarize sales by genre and publisher
jp_sales_summary <- jp_sales %>%
  group_by(Genre, Publisher) %>%
  summarise(Total_Sales = sum(JP_Sales, na.rm = TRUE)) %>%
  ungroup()

# Reclassify publishers and summarize
jp_sales_summary <- jp_sales_summary %>%
  mutate(Publisher = ifelse(Publisher == "Nintendo", "Nintendo", "Other")) %>%
  group_by(Genre, Publisher) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  ungroup()

# Function to create and save pie charts
create_pie_chart <- function(data, genre) {
  data <- data %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Publisher, " (", round(Percentage, 1), "%)"))
  
  p <- ggplot(data, aes(x = "", y = Total_Sales, fill = Publisher)) +
    geom_bar(stat = "identity", width = 0.9, color = "white", size = 0.5) +  # Reduced width for margin and added color for separation
    coord_polar(theta = "y") +
    labs(title = paste("Sales Distribution for", genre, "in Japan"),
         fill = "Publisher") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the plot as a PNG file
  ggsave(filename = paste0("results/NINTENDO_", genre, "_JP.png"), plot = p, width = 8, height = 6)
}

# List of unique genres
genres <- unique(jp_sales_summary$Genre)

# Create and save pie charts for each genre
for (genre in genres) {
  genre_data <- jp_sales_summary %>% filter(Genre == genre)
  create_pie_chart(genre_data, genre)
}




# Filter data for Japanese sales
jp_sales <- data %>%
  select(Genre, JP_Sales) %>%
  filter(JP_Sales > 0)

# Summarize total sales by genre
genre_sales_summary <- jp_sales %>%
  group_by(Genre) %>%
  summarise(Total_Sales = sum(JP_Sales, na.rm = TRUE)) %>%
  ungroup()

# Identify the top 5 genres based on total sales
top_5_genres <- genre_sales_summary %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 5)

# Add an "Other" category for all other genres
other_sales <- genre_sales_summary %>%
  filter(!Genre %in% top_5_genres$Genre) %>%
  summarise(Genre = "Other", Total_Sales = sum(Total_Sales))

# Combine top 5 genres with "Other" category
final_genre_sales <- bind_rows(top_5_genres, other_sales) %>%
  mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
         Label = paste0(Genre, " (", round(Percentage, 1), "%)"))

# Create and save the pie chart for top 5 genres
create_pie_chart <- function(data) {
  p <- ggplot(data, aes(x = "", y = Total_Sales, fill = Genre)) +
    geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +  # Add margin to each slice
    coord_polar(theta = "y") +
    labs(title = "Top 5 Genres in Japan",
         fill = "Genre") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the plot as a PNG file
  ggsave(filename = "results/NINTENDO_GENRE_JP.png", plot = p, width = 8, height = 6)
}

# Create and save the pie chart
create_pie_chart(final_genre_sales)



# Filter data for North American sales
na_sales <- data %>%
  select(Genre, Publisher, NA_Sales) %>%
  filter(NA_Sales > 0)

# Summarize sales by genre and publisher
na_sales_summary <- na_sales %>%
  group_by(Genre, Publisher) %>%
  summarise(Total_Sales = sum(NA_Sales, na.rm = TRUE)) %>%
  ungroup()

# Reclassify publishers and summarize
na_sales_summary <- na_sales_summary %>%
  mutate(Publisher = ifelse(Publisher == "Nintendo", "Nintendo", "Other")) %>%
  group_by(Genre, Publisher) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  ungroup()

# Function to create and save pie charts
create_pie_chart <- function(data, genre) {
  data <- data %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Publisher, " (", round(Percentage, 1), "%)"))
  
  p <- ggplot(data, aes(x = "", y = Total_Sales, fill = Publisher)) +
    geom_bar(stat = "identity", width = 0.9, color = "white", linewidth = 0.5) +  # Changed from size to linewidth
    coord_polar(theta = "y") +
    labs(title = paste("Sales Distribution for", genre, "in North America"),
         fill = "Publisher") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the plot as a PNG file
  ggsave(filename = paste0("results/NINTENDO_", genre, "_NA.png"), plot = p, width = 8, height = 6)
}

# List of unique genres
genres <- unique(na_sales_summary$Genre)

# Create and save pie charts for each genre
for (genre in genres) {
  genre_data <- na_sales_summary %>% filter(Genre == genre)
  create_pie_chart(genre_data, genre)
}





na_sales <- data %>%
  select(Genre, Publisher, NA_Sales) %>%
  filter(NA_Sales > 0)

eu_sales <- data %>%
  select(Genre, Publisher, EU_Sales) %>%
  filter(EU_Sales > 0)

# Summarize sales by genre and publisher for NA
na_sales_summary <- na_sales %>%
  group_by(Genre, Publisher) %>%
  summarise(Total_Sales = sum(NA_Sales, na.rm = TRUE)) %>%
  ungroup()

# Summarize sales by genre and publisher for EU
eu_sales_summary <- eu_sales %>%
  group_by(Genre, Publisher) %>%
  summarise(Total_Sales = sum(EU_Sales, na.rm = TRUE)) %>%
  ungroup()

# Reclassify publishers and summarize for NA
na_sales_summary <- na_sales_summary %>%
  mutate(Publisher = ifelse(Publisher == "Sony Computer Entertainment", "Sony Computer Entertainment", "Other")) %>%
  group_by(Genre, Publisher) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  ungroup()

# Reclassify publishers and summarize for EU
eu_sales_summary <- eu_sales_summary %>%
  mutate(Publisher = ifelse(Publisher == "Sony Computer Entertainment", "Sony Computer Entertainment", "Other")) %>%
  group_by(Genre, Publisher) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  ungroup()

# Function to create and save pie charts
create_pie_chart <- function(data, genre, region) {
  data <- data %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Publisher, " (", round(Percentage, 1), "%)"))
  
  p <- ggplot(data, aes(x = "", y = Total_Sales, fill = Publisher)) +
    geom_bar(stat = "identity", width = 0.9, color = "white", linewidth = 0.5) +
    coord_polar(theta = "y") +
    labs(title = paste("Sales Distribution for", genre, "in", region),
         fill = "Publisher") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the plot as a PNG file
  ggsave(filename = paste0("results/SONY_", genre, "_", region, ".png"), plot = p, width = 8, height = 6)
}

# List of unique genres for NA and EU
genres_na <- unique(na_sales_summary$Genre)
genres_eu <- unique(eu_sales_summary$Genre)

# Create and save pie charts for each genre in NA
for (genre in genres_na) {
  genre_data <- na_sales_summary %>% filter(Genre == genre)
  create_pie_chart(genre_data, genre, "NA")
}

# Create and save pie charts for each genre in EU
for (genre in genres_eu) {
  genre_data <- eu_sales_summary %>% filter(Genre == genre)
  create_pie_chart(genre_data, genre, "EU")
}




# Filter and summarize SCE sales for NA
sce_na_sales <- data %>%
  filter(Publisher == "Sony Computer Entertainment" & NA_Sales > 0) %>%
  group_by(Genre) %>%
  summarise(Total_Sales = sum(NA_Sales, na.rm = TRUE)) %>%
  ungroup()

# Filter and summarize SCE sales for EU
sce_eu_sales <- data %>%
  filter(Publisher == "Sony Computer Entertainment" & EU_Sales > 0) %>%
  group_by(Genre) %>%
  summarise(Total_Sales = sum(EU_Sales, na.rm = TRUE)) %>%
  ungroup()

# Filter and summarize SCE sales for JP
sce_jp_sales <- data %>%
  filter(Publisher == "Sony Computer Entertainment" & JP_Sales > 0) %>%
  group_by(Genre) %>%
  summarise(Total_Sales = sum(JP_Sales, na.rm = TRUE)) %>%
  ungroup()

# Function to create and save pie charts
create_genre_pie_chart <- function(data, region) {
  data <- data %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Genre, " (", round(Percentage, 1), "%)"))
  
  p <- ggplot(data, aes(x = "", y = Total_Sales, fill = Genre)) +
    geom_bar(stat = "identity", width = 0.9, color = "white", linewidth = 0.5) +
    coord_polar(theta = "y") +
    labs(title = paste("SCE Sales Distribution by Genre in", region),
         fill = "Genre") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the plot as a PNG file
  ggsave(filename = paste0("results/SCE_Genres_", region, ".png"), plot = p, width = 8, height = 6)
}

# Create and save pie charts for SCE sales by genre in NA and EU
create_genre_pie_chart(sce_na_sales, "NA")
create_genre_pie_chart(sce_eu_sales, "EU")
create_genre_pie_chart(sce_jp_sales, "JP")





ps4_xone_sales <- data %>%
  filter(Platform %in% c("PS4", "XOne")) %>%
  group_by(Platform) %>%
  summarise(Total_Sales = sum(NA_Sales + EU_Sales, na.rm = TRUE)) %>%
  ungroup()

# Function to create and save the pie chart
create_platform_pie_chart <- function(data) {
  data <- data %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Platform, " (", round(Percentage, 1), "%)"))
  
  p <- ggplot(data, aes(x = "", y = Total_Sales, fill = Platform)) +
    geom_bar(stat = "identity", width = 0.9, color = "white", linewidth = 0.5) +
    coord_polar(theta = "y") +
    labs(title = "Sales Distribution between PS4 and XOne",
         fill = "Platform") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the plot as a PNG file
  ggsave(filename = "results/PS4_vs_XOne_Sales.png", plot = p, width = 8, height = 6)
}

# Create and save the pie chart for PS4 vs XOne sales
create_platform_pie_chart(ps4_xone_sales)






# Filter data for the Sports genre and exclude Nintendo
sports_sales <- data %>%
  filter(Genre == "Sports" & Publisher != "Nintendo")

# Reclassify publishers into EA, Pro Evolution Soccer, and Others
sports_sales_summary <- sports_sales %>%
  mutate(Publisher = case_when(
    Publisher == "Electronic Arts" ~ "Electronic Arts",
    grepl("\\bPro Evolution Soccer\\b", Name, ignore.case = TRUE) ~ "Pro Evolution Soccer",
    TRUE ~ "Other"
  )) %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(NA_Sales + EU_Sales, na.rm = TRUE)) %>%
  ungroup()

# Function to create and save the pie chart
create_sports_pie_chart <- function(data) {
  data <- data %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Publisher, " (", round(Percentage, 1), "%)"))
  
  p <- ggplot(data, aes(x = "", y = Total_Sales, fill = Publisher)) +
    geom_bar(stat = "identity", width = 0.9, color = "white", linewidth = 0.5) +
    coord_polar(theta = "y") +
    labs(title = "Sales Distribution for Sports Genre: EA vs Others (Excluding Nintendo)",
         fill = "Publisher") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the plot as a PNG file
  ggsave(filename = "results/EA_vs_Others_Sports_Sales_Excluding_Nintendo_Pro_Evolution_Soccer.png", plot = p, width = 8, height = 6)
}

# Create and save the pie chart for EA vs Others in Sports genre excluding Nintendo and including Pro Evolution Soccer
create_sports_pie_chart(sports_sales_summary)




# Filter data for the Sports genre and exclude Nintendo
sports_sales <- data %>%
  filter(Genre == "Sports" & Publisher != "Nintendo")

# Reclassify publishers into EA, Pro Evolution Soccer, and Others
sports_sales_summary <- sports_sales %>%
  mutate(Publisher = case_when(
    Publisher == "Electronic Arts" ~ "Electronic Arts",
    grepl("\\bPro Evolution Soccer\\b", Name, ignore.case = TRUE) ~ "Pro Evolution Soccer",
    TRUE ~ Publisher
  )) %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(NA_Sales + EU_Sales, na.rm = TRUE)) %>%
  ungroup()

# Calculate the total sales for percentage calculation
total_sales <- sum(sports_sales_summary$Total_Sales)

# Reclassify publishers with less than 8% sales into "Other"
sports_sales_summary <- sports_sales_summary %>%
  mutate(Percentage = Total_Sales / total_sales * 100,
         Publisher = ifelse(Percentage < 8, "Other", Publisher)) %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  ungroup()

# Function to create and save the pie chart
create_sports_pie_chart <- function(data) {
  data <- data %>%
    mutate(Percentage = Total_Sales / sum(Total_Sales) * 100,
           Label = paste0(Publisher, " (", round(Percentage, 1), "%)"))
  
  p <- ggplot(data, aes(x = "", y = Total_Sales, fill = Publisher)) +
    geom_bar(stat = "identity", width = 0.9, color = "white", linewidth = 0.5) +
    coord_polar(theta = "y") +
    labs(title = "Sales Distribution for Sports Genre: EA vs Others (Excluding Nintendo)",
         fill = "Publisher") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3)
  
  # Save the plot as a PNG file
  ggsave(filename = "results/EA_vs_Others_Sports_Sales_Excluding_Nintendo_With_Percentage_Filter.png", plot = p, width = 8, height = 6)
}

# Create and save the pie chart for EA vs Others in Sports genre excluding Nintendo and including Pro Evolution Soccer
create_sports_pie_chart(sports_sales_summary)




# Filter and summarize sales by year
yearly_sales <- data %>%
  filter(Year >= 1980 & Year <= 2015) %>%
  group_by(Year) %>%
  summarise(Total_Sales = sum(NA_Sales + EU_Sales, na.rm = TRUE)) %>%
  ungroup()

# Calculate change in sales compared to previous year
yearly_sales <- yearly_sales %>%
  arrange(Year) %>%
  mutate(Change = Total_Sales - lag(Total_Sales, default = first(Total_Sales)))

# Function to assign color based on change
assign_color <- function(change) {
  ifelse(change > 0, "green", "red")
}

# Create and save the bar chart
p <- ggplot(yearly_sales, aes(x = as.factor(Year), y = Total_Sales, fill = assign_color(Change))) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales Over Years with Color Comparison",
       x = "Year",
       y = "Total Sales (in millions)",
       fill = "Comparison") +
  scale_fill_manual(values = c("green" = "green", "red" = "red"), guide = "none") +  # Changed guide to "none"
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the plot as a PNG file
ggsave(filename = "results/Yearly_Sales_Bar_Chart_Color_Comparison.png", plot = p, width = 10, height = 6)

