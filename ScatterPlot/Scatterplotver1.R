

'Scatter plot ver1'

library(tidyverse)
library(graphics)
library(readr)
library(readxl)
library(RColorBrewer)
library(pdftools)

# Extract data;

read_csv('BLI_31052020020115785.csv') %>% 
  filter(Indicator == 'Self-reported health') %>% 
  select(Country,Value) %>% 
  rename('SelfReported' = Value) -> dataX


read_csv('BLI_31052020020115785.csv') %>% 
  filter(Indicator == 'Life expectancy') %>% 
  select(Country,Value) %>% 
  rename('LifeExpect' = Value) -> dataY


dataX %>% 
  group_by(Country) %>% 
  summarise(SelfReported = mean(SelfReported)) -> dataX


dataY %>% 
  group_by(Country) %>% 
  summarise(LifeExpect = mean(LifeExpect)) -> dataY 


dataX %>% 
  left_join(dataY,by = c('Country','Country')) -> myData


myX <- as.vector(myData$SelfReported)
myY <- as.vector(myData$LifeExpect)
myX_des <- 'Self-reported Health (Scale from 0 to 100)'
myY_des <- 'Life Expectancy'


# Chart;


pdf_file <- 'scatterplotver1.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 11.69,height = 9)

par(
  
  mar = c(4,4,0.5,2),
  omi = c(0.5,0.5,1,0),
  family = 'Lato Light',
  las = 1
  
  
)

plot(type = 'n',xlab = myX_des,ylab = myY_des,myX,myY,
     xlim = c(30,90),ylim = c(65,90),axes = FALSE)

axis(1,col = par('bg'),col.ticks = 'grey81',lwd.ticks = 0.5,
     tick = -0.025)
axis(2,col = par('bg'),col.ticks = 'grey81',lwd.ticks = 0.5,
     tick = -0.025)


myC1 <- brewer.pal(5,name = 'PiYG')[5]
myC2 <- brewer.pal(5,name = 'PiYG')[4]
myC3 <- brewer.pal(5,name = 'PiYG')[1]
myC4 <- brewer.pal(5,name = 'PiYG')[2]



(myX > mean(myX)) & (myY > mean(myY))


myData[(myX > mean(myX)) & (myY > mean(myY)),] -> myPart1
myData[(myX <= mean(myX)) & (myY > mean(myY)),] -> myPart2
myData[(myX <= mean(myX)) & (myY <= mean(myY)),] -> myPart3
myData[(myX > mean(myX)) & (myY <= mean(myY)),] -> myPart4


myn1 <- nrow(myPart1)
myn2 <- nrow(myPart2)
myn3 <- nrow(myPart3)
myn4 <- nrow(myPart4)


symbols(myPart1[,2:3],bg = myC1,circles = rep(1,myn1),inches = 0.4,
        add = TRUE,xpd = TRUE,fg = 'white')
symbols(myPart2[,2:3],bg = myC2,circles = rep(1,myn2),inches = 0.4,
        add = TRUE,xpd = TRUE,fg = 'white')
symbols(myPart3[,2:3],bg = myC3,circles = rep(1,myn3),inches = 0.4,
        add = TRUE,xpd = TRUE,fg = 'white')
symbols(myPart4[,2:3],bg = myC4,circles = rep(1,myn4),inches = 0.4,
        add = TRUE,xpd = TRUE,fg = 'white')
text(myPart2[,2:3],myPart2$Country,cex = 0.9,pos = 1,
     offset = 1.75)
text(myPart4[,2:3],myPart4$Country,cex = 0.9,pos = 1,
     offset = 1.75)


# 2 coordinates;

abline(v = mean(myX,na.rm = TRUE),col = 'black',lty = 3)
abline(h = mean(myY,na.rm = TRUE),col = 'black',lty = 3)
text(mean(myData$SelfReported)+0.005*mean(myData$SelfReported),65,
     labels = 'High',family = 'Lato Black',adj = 0)
text(mean(myData$SelfReported)+0.005*mean(myData$SelfReported),90,
     labels = 'High',family = 'Lato Black',adj = 0)
text(mean(myData$SelfReported)-0.035*mean(myData$SelfReported),65.01,
     labels = 'Low',family = 'Lato Black',adj = 0)


# Titling;

mtext('Life Expectancy and Self-reported Health (OECD)',
      side = 3,adj = 0,line = 2.5,cex = 1.85,family = 'Lato Black')
mtext('Self-reported Health (scale from 0-100)',side = 3,
      adj = 0,line = 1,cex = 1.15,font = 3)
mtext('Source: Dantri.com.vn',side = 1,line = 4,adj = 1,
      cex = 1.15,font = 3)

dev.off()



# Convert image form;

pdf_convert(
  
  
  pdf = pdf_file,
  format = 'png',
  pages = NULL,
  filenames = NULL,
  dpi = 72,
  antialias = TRUE,
  opw = '',
  upw = '',
  verbose = TRUE
  
  
)


rm(list = ls())
cat('\f')












