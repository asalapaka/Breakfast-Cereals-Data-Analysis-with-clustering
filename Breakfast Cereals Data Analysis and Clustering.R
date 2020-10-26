---
title: "Breakfast Cereals - Data Analysis and Clustering"
author: "Adarsh Salapaka"
output:
  html_document:
    highlight: tango
    number_sections: yes
    theme: cerulean
    toc: yes
    toc_float:
      collapsed: yes
      smooth_scroll: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Context
If you like to eat cereal, do yourself a favor and avoid this dataset at all costs. After seeing these data it will never be the same for me to eat Fruity Pebbles again.
Content
Fields in the dataset:
	• Name: Name of cereal 
	• mfr: Manufacturer of cereal 
		○ A = American Home Food Products; 
		○ G = General Mills
		○ K = Kelloggs
		○ N = Nabisco
		○ P = Post
		○ Q = Quaker Oats
		○ R = Ralston Purina 
	• type: 
		○ cold 
		○ hot 
	• calories: calories per serving 
	• protein: grams of protein 
	• fat: grams of fat 
	• sodium: milligrams of sodium 
	• fiber: grams of dietary fiber 
	• carbo: grams of complex carbohydrates 
	• sugars: grams of sugars 
	• potass: milligrams of potassium 
	• vitamins: vitamins and minerals - 0, 25, or 100, indicating the typical percentage of FDA recommended 
	• shelf: display shelf (1, 2, or 3, counting from the floor) 
	• weight: weight in ounces of one serving 
	• cups: number of cups in one serving 
	• rating: a rating of the cereals (Possibly from Consumer Reports?)


```{r message=FALSE, warning=FALSE}
# Install Packages
# install.packages("tidyr")
# install.packages("ggfortify")
# install.packages("ggpairs")
# install.packages("DT")
```

# Importing and Cleaning Data

## Import Data

We will use the `tidyverse` library for importing and wrangling the data.

```{r warning=FALSE, message=FALSE}
# Import Data

# Load tidyverse
library(tidyverse)

raw_data <- read_csv(file = "../Clustering Breakfast Cereals/cereal.csv")

```

## Data Cleaning and Wrangling

```{r warning=FALSE, message=FALSE}
# Change column names
data_cereal <- raw_data

colnames(data_cereal) <- c("Name", "Manufacturer", "Type", "Calories", "Protein", "Fat", "Sodium", "Fibre", "Carbohydrates", "Sugar", "Potassium", "Vitamins", "Shelf", "Weight", "Cups", "Rating")

# Create feature with full manufacturer name
data_cereal$Manufacturer_Name <- data_cereal$Manufacturer

data_cereal$Manufacturer_Name <- gsub(pattern = "P", replacement = "Post", x = data_cereal$Manufacturer_Name)
data_cereal$Manufacturer_Name <- gsub(pattern = "A", replacement = "American Home Food Products", x = data_cereal$Manufacturer_Name)
data_cereal$Manufacturer_Name <- gsub(pattern = "G", replacement = "General Mills", x = data_cereal$Manufacturer_Name)
data_cereal$Manufacturer_Name <- gsub(pattern = "K", replacement = "Kellogs", x = data_cereal$Manufacturer_Name)
data_cereal$Manufacturer_Name <- gsub(pattern = "N", replacement = "Nabisco", x = data_cereal$Manufacturer_Name)
data_cereal$Manufacturer_Name <- gsub(pattern = "Q", replacement = "Quaker Oats", x = data_cereal$Manufacturer_Name)
data_cereal$Manufacturer_Name <- gsub(pattern = "R", replacement = "Ralston Purina", x = data_cereal$Manufacturer_Name)

data_cereal$Manufacturer_Name

# Replace 'H' and 'C' in Type with Hot and Cold
data_cereal$Type <- gsub("H", "Hot", x = data_cereal$Type)
data_cereal$Type <- gsub("C", "Cold", x = data_cereal$Type)

# Change cereal type and shelf from character to factor
data_cereal$Type <- factor(data_cereal$Type)
data_cereal$Shelf <- factor(data_cereal$Shelf)
data_cereal$Manufacturer <- factor(data_cereal$Manufacturer)

sapply(data_cereal, FUN = class)
```

```{r warning=FALSE, message=FALSE}
summary(data_cereal)
```

Carbohydrates, sugars and Potassium have some negative values. Since this is not possible we can replace negative values with NA.

```{r warning=FALSE, message=FALSE}
# Replace negative values with NA
data_cereal$Carbohydrates[data_cereal$Carbohydrates < 0] <- NA
data_cereal$Sugar[data_cereal$Sugar < 0] <- NA
data_cereal$Potassium[data_cereal$Potassium < 0] <- NA

summary(data_cereal)
```

# Exploratory Data Analysis

## Browsing the Dataset

```{r warning=FALSE, message=FALSE}
library(DT)

datatable(data = data_cereal, 
          rownames = FALSE, 
          filter = "top",
          options = list(autoWidth = TRUE))
```


## Manufacturers

```{r warning=FALSE, message=FALSE}
library(tidyverse)

Manufacturers_Total <- data_cereal %>% 
  select(Manufacturer_Name, Type) %>% 
  group_by(Manufacturer_Name, Type) %>% 
  summarise(Total = n()) %>% 
  spread(key = Type, value = Total) %>%
  replace_na(replace = list(Manufacturer_Name = 0, Cold = 0, Hot = 0)) %>% 
  mutate(Total = Cold + Hot) %>% 
  arrange(desc(Total))
Manufacturers_Total

ggplot(data_cereal, aes(x = factor(Manufacturer_Name, levels = rev(Manufacturers_Total$Manufacturer_Name)), fill = Type)) +
  geom_bar() +
  coord_flip(expand = FALSE) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_x_discrete(name = "Manufacturer") +
  scale_y_continuous(name = "Count", minor_breaks = NULL) + 
  theme_minimal() +
  labs(title = "Number of Products by Manufacturer")
  
```

## Nutritionals

### Portion Sizes

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Weight, fill = Manufacturer_Name)) +
    geom_histogram() +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Weight (in ounces)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 70)) +
    labs(fill = "Manufacturer", title = "Distribution of Weight per Serving", subtitle = "Manufacturers use different weights for servings") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Weight*28.3495, fill = Manufacturer_Name)) +
    geom_histogram() +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Weight (g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 70)) +
    labs(fill = "Manufacturer", title = "Distribution of Weight per Serving", subtitle = "Manufacturers use different weights for servings") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Cups, fill = Manufacturer_Name)) +
    geom_histogram() +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Number of cups in one serving", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 35)) +
        labs(fill = "Manufacturer", title = "Distribution of Number of Cups per Serving", subtitle = "Different products have different cup sizes") +
    theme_minimal()


```


### Add nutritionals per ounce and 100g

```{r warning=FALSE, message=FALSE}
glimpse(data_cereal)
 
# Add nutritionals per ounce
data_cereal$Calories_oz <- data_cereal$Calories * data_cereal$Weight
data_cereal$Protein_oz <- data_cereal$Protein * data_cereal$Weight
data_cereal$Fat_oz <- data_cereal$Fat * data_cereal$Weight
data_cereal$Sodium_oz <- data_cereal$Sodium * data_cereal$Weight
data_cereal$Fibre_oz <- data_cereal$Fibre * data_cereal$Weight
data_cereal$Carbohydrates_oz <- data_cereal$Carbohydrates * data_cereal$Weight
data_cereal$Sugar_oz <- data_cereal$Sugar * data_cereal$Weight
data_cereal$Potassium_oz <- data_cereal$Potassium * data_cereal$Weight
data_cereal$Vitamins_oz <- data_cereal$Vitamins * data_cereal$Weight

# Add nutritionals per 100g

# 1 oz. = 28.3495g
# 100g = 3.5274 oz.
data_cereal$Calories_100g <- round(data_cereal$Calories_oz * 3.5274, 0)
data_cereal$Protein_100g <- round(data_cereal$Protein_oz * 3.5274, 1)
data_cereal$Fat_100g <- round(data_cereal$Fat_oz * 3.5274, 1)
data_cereal$Sodium_100g <- round(data_cereal$Sodium_oz * 3.5274, 1)
data_cereal$Fibre_100g <- round(data_cereal$Fibre_oz * 3.5274, 1)
data_cereal$Carbohydrates_100g <- round(data_cereal$Carbohydrates_oz * 3.5274, 1)
data_cereal$Sugar_100g <- round(data_cereal$Sugar_oz * 3.5274, 1)
data_cereal$Potassium_100g <- round(data_cereal$Potassium_oz * 3.5274, 1)
data_cereal$Vitamins_100g <- round(data_cereal$Vitamins_oz * 3.5274, 1)

glimpse(data_cereal)

summary(data_cereal)
```


### Scatterplot Matrix

```{r warning=FALSE, message=FALSE, fig.asp=1}
# Scatter Plot Matrix

# install.packages("GGally")
library(GGally)

# Create function to add regression line to scatter plot matrix
sm_regression <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(alpha = 0.4) + 
    geom_smooth(method=lm, fill="grey10", color="grey10", ...)
  p
}

data_cereal %>% 
  select(Manufacturer_Name, Calories_100g, Protein, Fat, Sodium, Fibre, Carbohydrates, Sugar, Potassium, Shelf, Rating) %>% 
  ggpairs(columns = 2:11, lower = list(continuous = sm_regression)) +
  theme_bw()

```

### Correlation Matrix

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  select(Calories = Calories_100g, Protein, Fat, Sodium, Fibre, Carbs = Carbohydrates, Sugar, Potassium, Shelf, Rating) %>% 
  ggcorr(palette = "RdBu", label = TRUE, label_round =  2)
```


### Calories

#### Summary of Calorie Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  select(Name, Manufacturer_Name, Type, Calories, Calories_oz, Calories_100g) %>% 
  datatable(rownames = NULL, filter = "top", colnames = c("Product", "Manufacturer", "Type", "Calories (g per serving)", "Calories (g per ounce)", "Calories (g per 100g)"))
  
```

There seem to be some mistakes in the dataset regarding calorie content. As there are products that have almost no calories (<90 kcal) and products that have close to the maximum amount of calories possible per 100g of product (900 kcal).

Since we know that fat has 9 kcal/g and protein and carbohydrates have 4 kcal/g we will recalculate the calories from the nutritional data available and replot the histogram of calories per 100g.

```{r warning=FALSE, message=FALSE}
# Protein: 4 kcal/g
# Carbohydrates: 4 kcal/g
# Fat: 9 kcal/g

# Calories = 9 * Fat + 4 * Protein + 4 * Carbohydrates
data_cereal$Calories_100g_calculated <- data_cereal$Fat_100g * 9 + data_cereal$Protein_100g * 4 + data_cereal$Carbohydrates_100g * 4 
```


```{r warning=FALSE, message=FALSE}
# Summary Table of Calories per 100g
data_cereal %>% 
  select(Manufacturer_Name, Calories_100g_calculated, Type) %>% 
  group_by(Manufacturer_Name) %>% 
  summarise(Average = round(mean(Calories_100g_calculated, na.rm = TRUE), 1),
            Median = round(median(Calories_100g_calculated, na.rm = TRUE), 1),
            Lowest = min(Calories_100g_calculated, na.rm = TRUE),
            Highest = max(Calories_100g_calculated, na.rm = TRUE),
            Count = n())
```

#### Distribution of Calorie Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Manufacturer_Name, y = Calories_100g_calculated, fill = Manufacturer_Name)) +
    geom_boxplot(show.legend = FALSE) +
    stat_summary(fun.y = mean, geom = "point", pch = 1, show.legend = FALSE) + # Add average to the boxplot
    scale_y_continuous(name = "Calories (g per 100g)", minor_breaks = NULL) +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() + 
    theme_minimal() +
    labs(x = "Manufacturer") +
    ggtitle(label = "Distribution of Calorie Content by Manufacturer")

```


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Calories_100g_calculated)) +
    geom_density(fill = "grey", alpha = 0.8, adjust = 3, linetype = 0) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Calories (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Proportion", expand = c(0,0)) +
    labs(fill = "Manufacturer", title = "Distribution of Calories in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Calories_100g_calculated, fill = Manufacturer_Name)) +
    geom_histogram() +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Calories (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 10), breaks = seq(0, 10, 2)) +
    labs(fill = "Manufacturer", title = "Distribution of Calories in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Calories_100g_calculated, fill = Type)) +
    geom_histogram() +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(name = "Calories (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 10), breaks = seq(0, 10, 2)) +
    labs(fill = "Manufacturer", title = "Distribution of Calories in Breakfast Cereals") +
    theme_minimal()

```

### Fat

#### Summary of Fat Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  select(Name, Manufacturer_Name, Type, Fat, Fat_oz, Fat_100g) %>% 
  datatable(rownames = NULL, filter = "top", colnames = c("Product", "Manufacturer", "Type", "Fat (g per serving)", "Fat (g per ounce)", "Fat (g per 100g)"))
  
```

```{r warning=FALSE, message=FALSE}
# Summary Table of Fat per 100g
data_cereal %>% 
  select(Manufacturer_Name, Fat_100g, Type) %>% 
  group_by(Manufacturer_Name) %>% 
  summarise(Average = round(mean(Fat_100g, na.rm = TRUE), 1),
            Median = round(median(Fat_100g, na.rm = TRUE), 1),
            Lowest = min(Fat_100g, na.rm = TRUE),
            Highest = max(Fat_100g, na.rm = TRUE),
            Count = n())
```

#### Distribution of Fat Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Manufacturer_Name, y = Fat_100g, fill = Manufacturer_Name)) +
    geom_boxplot(show.legend = FALSE) +
    stat_summary(fun.y = mean, geom = "point", pch = 1, show.legend = FALSE) + # Add average to the boxplot
    scale_y_continuous(name = "Fat (g per 100g)", minor_breaks = NULL) +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() + 
    theme_minimal() +
    labs(x = "Manufacturer") +
    ggtitle(label = "Distribution of Fat Content by Manufacturer")

```


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Fat_100g)) +
    geom_density(fill = "grey", alpha = 0.8, linetype = 0) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Fat (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Proportion", expand = c(0,0)) +
    labs(fill = "Manufacturer", title = "Distribution of Fat in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Fat_100g, fill = Manufacturer_Name)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Fat (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Manufacturer", title = "Distribution of Fat in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Fat_100g, fill = Type)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(name = "Fat (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Type", title = "Distribution of Fat in Breakfast Cereals") +
    theme_minimal()

```


### Protein

#### Summary of Protein Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  select(Name, Manufacturer_Name, Type, Protein, Protein_oz, Protein_100g) %>% 
  datatable(rownames = NULL, filter = "top", colnames = c("Product", "Manufacturer", "Type", "Protein (g per serving)", "Protein (g per ounce)", "Protein (g per 100g)"))
  
```

```{r warning=FALSE, message=FALSE}
# Summary Table of Protein per 100g
data_cereal %>% 
  select(Manufacturer_Name, Protein_100g, Type) %>% 
  group_by(Manufacturer_Name) %>% 
  summarise(Average = round(mean(Protein_100g, na.rm = TRUE), 1),
            Median = round(median(Protein_100g, na.rm = TRUE), 1),
            Lowest = min(Protein_100g, na.rm = TRUE),
            Highest = max(Protein_100g, na.rm = TRUE),
            Count = n())
```

#### Distribution of Protein Content


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Manufacturer_Name, y = Protein_100g, fill = Manufacturer_Name)) +
    geom_boxplot(show.legend = FALSE) +
    stat_summary(fun.y = mean, geom = "point", pch = 1, show.legend = FALSE) + # Add average to the boxplot
    scale_y_continuous(name = "Protein (g per 100g)", minor_breaks = NULL) +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() + 
    theme_minimal() +
    labs(x = "Manufacturer") +
    ggtitle(label = "Distribution of Protein Content by Manufacturer")

```


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Protein_100g)) +
    geom_density(fill = "grey", alpha = 0.8, linetype = 0) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Protein (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Proportion", expand = c(0,0)) +
    labs(fill = "Manufacturer", title = "Distribution of Protein in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Protein_100g, fill = Manufacturer_Name)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Protein (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Manufacturer", title = "Distribution of Protein in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Protein_100g, fill = Type)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(name = "Protein (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Type", title = "Distribution of Protein in Breakfast Cereals") +
    theme_minimal()

```



### Carbohydrates

#### Summary of Carbohydrates Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  select(Name, Manufacturer_Name, Type, Carbohydrates, Carbohydrates_oz, Carbohydrates_100g) %>% 
  datatable(rownames = NULL, filter = "top", colnames = c("Product", "Manufacturer", "Type", "Carbohydrates (g per serving)", "Carbohydrates (g per ounce)", "Carbohydrates (g per 100g)"))
  
```

```{r warning=FALSE, message=FALSE}
# Summary Table of Carbohydrates per 100g
data_cereal %>% 
  select(Manufacturer_Name, Carbohydrates_100g, Type) %>% 
  group_by(Manufacturer_Name) %>% 
  summarise(Average = round(mean(Carbohydrates_100g, na.rm = TRUE), 1),
            Median = round(median(Carbohydrates_100g, na.rm = TRUE), 1),
            Lowest = min(Carbohydrates_100g, na.rm = TRUE),
            Highest = max(Carbohydrates_100g, na.rm = TRUE),
            Count = n())
```

#### Distribution of Carbohydrates Content


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Manufacturer_Name, y = Carbohydrates_100g, fill = Manufacturer_Name)) +
    geom_boxplot(show.legend = FALSE) +
    stat_summary(fun.y = mean, geom = "point", pch = 1, show.legend = FALSE) + # Add average to the boxplot
    scale_y_continuous(name = "Carbohydrates (g per 100g)", minor_breaks = NULL) +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() + 
    theme_minimal() +
    labs(x = "Manufacturer") +
    ggtitle(label = "Distribution of Carbohydrates Content by Manufacturer")

```


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Carbohydrates_100g)) +
    geom_density(fill = "grey", alpha = 0.8, linetype = 0) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Carbohydrates (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Proportion", expand = c(0,0)) +
    labs(fill = "Manufacturer", title = "Distribution of Carbohydrates in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Carbohydrates_100g, fill = Manufacturer_Name)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Carbohydrates (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Manufacturer", title = "Distribution of Carbohydrates in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Carbohydrates_100g, fill = Type)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(name = "Carbohydrates (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Type", title = "Distribution of Carbohydrates in Breakfast Cereals") +
    theme_minimal()

```

### Sugar

#### Summary of Sugar Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  select(Name, Manufacturer_Name, Type, Sugar, Sugar_oz, Sugar_100g) %>% 
  datatable(rownames = NULL, filter = "top", colnames = c("Product", "Manufacturer", "Type", "Sugar (g per serving)", "Sugar (g per ounce)", "Sugar (g per 100g)"))
  
```

```{r warning=FALSE, message=FALSE}
# Summary Table of Sugar per 100g
data_cereal %>% 
  select(Manufacturer_Name, Sugar_100g, Type) %>% 
  group_by(Manufacturer_Name) %>% 
  summarise(Average = round(mean(Sugar_100g, na.rm = TRUE), 1),
            Median = round(median(Sugar_100g, na.rm = TRUE), 1),
            Lowest = min(Sugar_100g, na.rm = TRUE),
            Highest = max(Sugar_100g, na.rm = TRUE),
            Count = n())
```

#### Distribution of Sugar Content


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Manufacturer_Name, y = Sugar_100g, fill = Manufacturer_Name)) +
    geom_boxplot(show.legend = FALSE) +
    stat_summary(fun.y = mean, geom = "point", pch = 1, show.legend = FALSE) + # Add average to the boxplot
    scale_y_continuous(name = "Sugar (g per 100g)", minor_breaks = NULL) +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() + 
    theme_minimal() +
    labs(x = "Manufacturer") +
    ggtitle(label = "Distribution of Sugar Content by Manufacturer")

```


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Sugar_100g)) +
    geom_density(fill = "grey", alpha = 0.8, linetype = 0) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Sugar (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Proportion", expand = c(0,0)) +
    labs(fill = "Manufacturer", title = "Distribution of Sugar in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Sugar_100g, fill = Manufacturer_Name)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Sugar (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Manufacturer", title = "Distribution of Sugar in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Sugar_100g, fill = Type)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(name = "Sugar (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Type", title = "Distribution of Sugar in Breakfast Cereals") +
    theme_minimal()

```

### Fibre

#### Summary of Fibre Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  select(Name, Manufacturer_Name, Type, Fibre, Fibre_oz, Fibre_100g) %>% 
  datatable(rownames = NULL, filter = "top", colnames = c("Product", "Manufacturer", "Type", "Fibre (g per serving)", "Fibre (g per ounce)", "Fibre (g per 100g)"))
  
```

```{r warning=FALSE, message=FALSE}
# Summary Table of Fibre per 100g
data_cereal %>% 
  select(Manufacturer_Name, Fibre_100g, Type) %>% 
  group_by(Manufacturer_Name) %>% 
  summarise(Average = round(mean(Fibre_100g, na.rm = TRUE), 1),
            Median = round(median(Fibre_100g, na.rm = TRUE), 1),
            Lowest = min(Fibre_100g, na.rm = TRUE),
            Highest = max(Fibre_100g, na.rm = TRUE),
            Count = n())
```

#### Distribution of Fibre Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Manufacturer_Name, y = Fibre_100g, fill = Manufacturer_Name)) +
    geom_boxplot(show.legend = FALSE) +
    stat_summary(fun.y = mean, geom = "point", pch = 1, show.legend = FALSE) + # Add average to the boxplot
    scale_y_continuous(name = "Fibre (g per 100g)", minor_breaks = NULL) +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() + 
    theme_minimal() +
    labs(x = "Manufacturer") +
    ggtitle(label = "Distribution of Fibre Content by Manufacturer")

```


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Fibre_100g)) +
    geom_density(fill = "grey", alpha = 0.8, linetype = 0) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Fibre (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Proportion", expand = c(0,0)) +
    labs(fill = "Manufacturer", title = "Distribution of Fibre in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Fibre_100g, fill = Manufacturer_Name)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Fibre (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Manufacturer", title = "Distribution of Fibre in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Fibre_100g, fill = Type)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(name = "Fibre (g per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Type", title = "Distribution of Fibre in Breakfast Cereals") +
    theme_minimal()

```


### Sodium

#### Summary of Sodium Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  select(Name, Manufacturer_Name, Type, Sodium, Sodium_oz, Sodium_100g) %>% 
  datatable(rownames = NULL, filter = "top", colnames = c("Product", "Manufacturer", "Type", "Sodium (mg per serving)", "Sodium (mg per ounce)", "Sodium (mg per 100g)"))
  
```

```{r warning=FALSE, message=FALSE}
# Summary Table of Sodium per 100g
data_cereal %>% 
  select(Manufacturer_Name, Sodium_100g, Type) %>% 
  group_by(Manufacturer_Name) %>% 
  summarise(Average = round(mean(Sodium_100g, na.rm = TRUE), 1),
            Median = round(median(Sodium_100g, na.rm = TRUE), 1),
            Lowest = min(Sodium_100g, na.rm = TRUE),
            Highest = max(Sodium_100g, na.rm = TRUE),
            Count = n())
```

#### Distribution of Sodium Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Manufacturer_Name, y = Sodium_100g, fill = Manufacturer_Name)) +
    geom_boxplot(show.legend = FALSE) +
    stat_summary(fun.y = mean, geom = "point", pch = 1, show.legend = FALSE) + # Add average to the boxplot
    scale_y_continuous(name = "Sodium (mg per 100g)", minor_breaks = NULL) +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() + 
    theme_minimal() +
    labs(x = "Manufacturer") +
    ggtitle(label = "Distribution of Sodium Content by Manufacturer")

```


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Sodium_100g)) +
    geom_density(fill = "grey", alpha = 0.8, linetype = 0) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Sodium (mg per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Proportion", expand = c(0,0)) +
    labs(fill = "Manufacturer", title = "Distribution of Sodium in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Sodium_100g, fill = Manufacturer_Name)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Sodium (mg per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Manufacturer", title = "Distribution of Sodium in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Sodium_100g, fill = Type)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(name = "Sodium (mg per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Type", title = "Distribution of Sodium in Breakfast Cereals") +
    theme_minimal()

```

### Potassium

#### Summary of Potassium Content

```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  select(Name, Manufacturer_Name, Type, Potassium, Potassium_oz, Potassium_100g) %>% 
  datatable(rownames = NULL, filter = "top", colnames = c("Product", "Manufacturer", "Type", "Potassium (mg per serving)", "Potassium (mg per ounce)", "Potassium (mg per 100g)"))
  
```

```{r warning=FALSE, message=FALSE}
# Summary Table of Potassium per 100g
data_cereal %>% 
  select(Manufacturer_Name, Potassium_100g, Type) %>% 
  group_by(Manufacturer_Name) %>% 
  summarise(Average = round(mean(Potassium_100g, na.rm = TRUE), 1),
            Median = round(median(Potassium_100g, na.rm = TRUE), 1),
            Lowest = min(Potassium_100g, na.rm = TRUE),
            Highest = max(Potassium_100g, na.rm = TRUE),
            Count = n())
```

#### Distribution of Potassium Content


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Manufacturer_Name, y = Potassium_100g, fill = Manufacturer_Name)) +
    geom_boxplot(show.legend = FALSE) +
    stat_summary(fun.y = mean, geom = "point", pch = 1, show.legend = FALSE) + # Add average to the boxplot
    scale_y_continuous(name = "Potassium (mg per 100g)", minor_breaks = NULL) +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() + 
    theme_minimal() +
    labs(x = "Manufacturer") +
    ggtitle(label = "Distribution of Potassium Content by Manufacturer")

```


```{r warning=FALSE, message=FALSE}
data_cereal %>% 
  ggplot(aes(x = Potassium_100g)) +
    geom_density(fill = "grey", alpha = 0.8, linetype = 0) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Potassium (mg per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Proportion", expand = c(0,0)) +
    labs(fill = "Manufacturer", title = "Distribution of Potassium in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Potassium_100g, fill = Manufacturer_Name)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(name = "Potassium (mg per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Manufacturer", title = "Distribution of Potassium in Breakfast Cereals") +
    theme_minimal()

data_cereal %>% 
  ggplot(aes(x = Potassium_100g, fill = Type)) +
    geom_histogram(alpha = 0.8, bins = 8) +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    scale_x_continuous(name = "Potassium (mg per 100g)", expand = c(0,0)) +
    scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
    labs(fill = "Type", title = "Distribution of Potassium in Breakfast Cereals") +
    theme_minimal()

```

# Unsupervised Learning

## Principal Component Analysis (PCA)

```{r warning=FALSE, message=FALSE}
# install.packages("factoextra")
library(factoextra)

# Create subset for PCA
PCA_data <- data_cereal %>% 
  select(Name, Manufacturer_Name, Calories = Calories_100g_calculated, Protein = Protein_100g, Fat = Fat_100g, Sodium = Sodium_100g, Fibre = Fibre_100g, Carbohydrates = Carbohydrates_100g, Sugar = Sugar_100g, Potassium = Potassium_100g, Rating)

# Remove observations with NAs
PCA_data <- PCA_data[complete.cases(PCA_data),]

PCA_cereals <- prcomp(PCA_data[, 3:11], scale. = TRUE)

# Obtain Summary of PCA
summary(PCA_cereals)

# Obtain Scree Plot
fviz_eig(PCA_cereals)
```

```{r}
library(ggpubr)

# PCA Variables
fviz_pca_var(PCA_cereals, 
             col.var = "contrib", 
             repel = TRUE, 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Principal Component Analysis: Variable Contribution",
             legend.title = "Contribution"
             )

# PCA Biplot: Variables and Individuals
fviz_pca_biplot(PCA_cereals, 
                geom.ind = "point", 
                pointshape = 21, 
                pointsize = 3, 
                fill.ind = PCA_data$Manufacturer_Name,
                # col.ind = "Black", 
                alpha = 0.8, 
                mean.point = FALSE, 
                col.var = factor(c("Input", "Input", "Input", "Input", "Input", "Input", "Input", "Input", "Output")), # Colour inputs and outputs differently
                repel = TRUE, 
                legend.title = list(fill = "Manufacturer", color = "Parameters"),
                title = "Principal Component Analysis") +
  fill_palette("Set1") + # Palette for individuals
  color_palette(palette = "aaas") # Palette for variables

```


## K-means Clustering

We need to select the number of clusters that has the optimal value of within sum of squares error (WSS).

Once we have a scree plot, we will select the number of clusters where the WSS improves more slowly as the number of clusters increases.

```{r}
# Create subset of data for k-means clustering
kmeans_data <- data_cereal %>% 
  select(Name, Manufacturer_Name, Calories = Calories_100g_calculated, Protein = Protein_100g, Fat = Fat_100g, Sodium = Sodium_100g, Fibre = Fibre_100g, Carbohydrates = Carbohydrates_100g, Sugar = Sugar_100g, Potassium = Potassium_100g, Rating)

# Remove rows with NAs
kmeans_data <- kmeans_data[complete.cases(kmeans_data), ]

# Create a starting vector to add WSS values
wss <- 0

# Loop k-means algorithm for various numbers of clusters
for (i in 1:10) {
  kmeans.output <- kmeans(x = kmeans_data[, 3:11], centers = i, nstart = 20)
  # Save total within sum of squares to wss vector
  wss[i] <- kmeans.output$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
data.frame(Clusters = 1:10, WSS = wss) %>% 
  ggplot(aes(x = Clusters, y = WSS)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "Number of Clusters", breaks = 1:10, minor_breaks = NULL) +
  scale_y_continuous(name = "Within Groups Sum of Squares") +
  theme_minimal() +
  labs(title = "Scree Plot")


```

From the scree plot above, 3 or 4 seem to be the optimal number of clusters.

```{r}
kmeans.output <- kmeans(x = kmeans_data[, 3:11], centers = 3, nstart = 20)

kmeans.output

# Use clusplot function to plot cluster
library(factoextra)
kmeans.output <- eclust(x = kmeans_data[, 3:11], FUNcluster = "kmeans", k = 3, graph = FALSE)

kmeans.output

fviz_cluster(kmeans.output, ellipse = TRUE, ellipse.type = "norm", ellipse.level = 0.95, ellipse.alpha = 0.1) +
  theme_bw()
```