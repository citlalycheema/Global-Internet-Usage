# Presentation 2
# Citlaly Cheema
# April 7, 2023

# load packages
library(tidyverse)
library(readxl)
library(patchwork)
library(lubridate)
library(haven)
library(dplyr)
library(ggplot2)

# set directory
setwd('/Users/Citlaly/Desktop/Data Analysis')

# load data
df = read_csv('gapminder_internet.csv')

# summary stats
summary(df)

# internet use rate and urban rate

myplot =
  ggplot(df,  aes(x = urbanrate, y = internetuserate, size = incomeperperson)) +
  xlim(25,100) +
  ylim(25, 100) +
  geom_point(shape = 19, fill = "gray", position = "jitter", color = "mediumblue") +
  theme_bw() +
  labs(x = 'Urban Rate', y = 'Internet Use Rate', title = "Correlation between Internet Use Rate and Urban Rate") +
  theme(
    text = element_text(family='Times New Roman'),
    plot.title = element_text(family='Times New Roman', face = 'bold', hjust = 0.5),
    plot.background = element_rect(fill='white',color=NA)
  )

# save plot
ggsave(
  filename ='myplot.svg',
  plot = myplot,
  height = 5, width = 7, dpi = 500
)


# the income per person vs urban rate
p1 =
  ggplot(df,  aes(x = incomeperperson , y = urbanrate)) +
  xlim(1000,10000)+
  geom_point(shape = 19, fill = "gray", position = "jitter", color = "#3399FF") +
  theme_classic() +
  labs(x = 'Income Per Person', y = 'Urban Rate') +
  coord_cartesian(expand=FALSE) +
  theme(
    text = element_text(family='Times New Roman'),
    plot.title = element_text(family='Times New Roman', face = 'bold', hjust = 0.5),
    plot.background = element_rect(fill='white',color=NA),
)

# the income per person vs internet use rate
p2 =
  ggplot(df,  aes(x = incomeperperson , y = internetuserate)) +
  xlim(1000,10000) +
  ylim(10,100) +
  geom_point(shape = 19, fill = "gray", position = "jitter", color = "#9966CC") +
  theme_classic() +
  labs(x = 'Income Per Person', y = 'Internet Use Rate') +
  coord_cartesian(expand=FALSE) +
  theme(
    text = element_text(family='Times New Roman'),
    plot.title = element_text(family='Times New Roman', face = 'bold', hjust = 0.5),
    plot.background = element_rect(fill='white',color=NA),
  )

# patchwork
p3 = p1 + p2 

ggsave(
  filename ='p3.svg',
  plot = p3,
  height = 5, width = 10, dpi = 400
)



