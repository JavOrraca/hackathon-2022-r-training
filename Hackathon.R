# 3.  Import Required Packages --------------

#install.packages('dplyr')  

library(ggplot2)
library(dplyr)
library(data.table)
library(plotly)



# 4. Import Data --------------  

# READ IN AN INTERNAL DATASET
df.diamonds <- diamonds


# READ IN CSV
setwd()
df.mapping <- read.csv('clarity_map.csv')
df.mapping <- fread('clarity_map.csv')



# 5. GROUP BY AND SUMMARISE -------------

df.agg <- df.diamonds %>%
  group_by(clarity) %>%
  summarise(Total.Price = sum(price), Observations = n())

dim(df.agg)








# 6. MUTATE -----

df.agg <- df.agg %>%
  mutate(average.price = Total.Price/Observations)

dim(df.agg)



# 7. INNER JOIN ----------

library(kableExtra)

df.mapping <- read.csv('clarity_map.csv')

df.mapping %>%
  kbl(caption = "") %>%
  kable_classic(full_width = F, html_font = "Cambria")


df.agg <- df.agg %>%
  inner_join(df.mapping, by = 'clarity')

dim(df.agg)


df.agg <- df.agg %>%
  mutate(clarity.map2 = ifelse(clarity %in% c('I1','SI2'),'Bad',
                               ifelse(clarity %in% c('SI1','VS2','VS1','VVS2'),'Medium',
                                      ifelse(clarity %in% c('VVS1','IF'),'Good','Undefined'))))



# 8. FILTER ------------

df.agg <- df.agg %>%
  filter(clarity.map == 'Good')
# 9. SELECT ----------

df.agg1 <- df.agg %>%
  select(c('clarity', 'Total.Price', 'Observations', 'average.price', 'clarity.map'))


df.agg2 <- df.agg %>%
  select(-c('clarity.map2'))

df.agg <- df.agg1
rm(df.agg1, df.agg2)



# 10. DPLYR SUMMARY --------

df.chained <- df.diamonds %>%
  group_by(clarity) %>%
  summarise(Total.Price = sum(price), Observations = n()) %>%
  mutate(average.price = Total.Price/Observations) %>%
  inner_join(df.mapping, by = 'clarity') %>%
  mutate(clarity.map2 = ifelse(clarity %in% c('I1','SI2'),'Bad',
                               ifelse(clarity %in% c('SI1','VS2','VS1','VVS2'),'Medium',
                                      ifelse(clarity %in% c('VVS1','1F'),'Good','Undefined')))) %>%
  filter(clarity.map == 'Good') %>%
  select(c('clarity', 'average.price'))



# 11. GGPLOT2 INTRO -------

# Create new variable carat.group
df.diamonds <- df.diamonds %>%
  mutate(carat.group = ifelse(carat < 1, 0.5,
                              ifelse(carat < 2, 1.5,
                                     ifelse(carat < 3, 2.5,
                                            ifelse(carat < 4, 3.5, 4.5)))))

# create new variable table.group
df.diamonds <- df.diamonds %>%
  mutate(table.group = ifelse(table < 60, 60,
                              ifelse(table < 70, 70, 95)))



# Create dataset for 1 variable visualizations
df.agg1 <- df.diamonds %>%
  group_by(carat.group) %>%
  summarise(Total.Price = sum(price), Observations = n()) %>%
  mutate(average.price = Total.Price/Observations)

# Create dataset for 2 variable visualizations
df.agg2 <- df.diamonds %>%
  group_by(carat.group, cut) %>%
  summarise(Total.Price = sum(price), Observations = n()) %>%
  mutate(average.price = Total.Price/Observations)

# Create dataset for 3 variable visualizations
df.agg3 <- df.diamonds %>%
  group_by(carat.group, cut, color) %>%
  summarise(Total.Price = sum(price), Observations = n()) %>%
  mutate(average.price = Total.Price/Observations)

# Create datset for 4 variable visualizations
df.agg4 <- df.diamonds %>%
  group_by(carat.group, cut, color, table.group) %>%
  summarise(Total.Price = sum(price), Observations = n()) %>%
  mutate(average.price = Total.Price/Observations)



# 12. 1 VARIABLE VIEW ----------

graph.object <- ggplot(df.agg1, aes(x = carat.group, y = average.price, group = 1))
graph.object

scatter1 <- graph.object + geom_point()
line1 <- graph.object + geom_line() 
bar1 <- graph.object + geom_bar(stat = "identity") 
scatterline1 <- scatter1 + geom_line()


scatter1
line1
bar1
scatterline1





# 13. 2 VARIABLE VIEW -------

graph.task2 <- ggplot(df.agg2,aes(x = carat.group, y = average.price, color = cut, fill = cut))

scatter2 <- graph.task2 + geom_point()
line2 <- graph.task2 + geom_line()
stackbar2 <- graph.task2 + geom_bar(stat = "identity",position = "stack")
fillbar2 <- graph.task2 + geom_bar(stat = "identity",position = "fill")
dodgebar2 <- graph.task2 + geom_bar(stat = "identity",position = "dodge")
scatterline2 <- scatter2 + geom_line()

scatter2
line2
stackbar2
fillbar2
dodgebar2
scatterline2








# 14. 3+ VARIABLE VIEW --------

graph.task3 <- ggplot(df.agg3,aes(x = carat.group, y = average.price, color = cut, fill = cut))

scatterline3 <- graph.task3 + geom_line() + geom_point() + facet_wrap(~color)
scatterline3
graph.task4 <- ggplot(df.agg4,aes(x = carat.group, y = average.price, color = cut, fill =cut))
scatterline4 <- graph.task4 + geom_line() + geom_point() + facet_grid(color ~ table.group) + xlab("Banded Ages") + ylab("Actual to Expected") + ggtitle("Dollar Weighted Actual to Expected Analysis Using 7580E")

scatterline4



# 15. PLOTLY --------

ggplotly(scatter1)
ggplotly(scatter2)
ggplotly(bar1)
ggplotly(stackbar2)
ggplotly(fillbar2)
ggplotly(dodgebar2)
ggplotly(line1)
ggplotly(line2)
ggplotly(scatterline1)
ggplotly(scatterline2)
ggplotly(scatterline3)
ggplotly(scatterline4)