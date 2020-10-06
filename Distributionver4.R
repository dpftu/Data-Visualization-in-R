
library(tidyverse)
library(readr)
library(readxl)
library(stringr)
library(RColorBrewer)
library(pdftools)


read_csv('men.txt',
         col_names = str_c('x',(1+1949):(111+1949),sep = '')) -> men

read_csv('women.txt',
         col_names = str_c('x',(1+1949):(111+1949),sep = '')) -> women


class <- vector('double')


for (i in seq(10,100,by = 10)) {
  
  class <- c(class,rep(i,10))
  
}


men %>% 
  mutate(class = class) %>% 
  select(class,x2010) %>% 
  group_by(class) %>% 
  summarise(x2010 = sum(x2010)) -> male 
women %>% 
  mutate(class = class) %>% 
  select(class,x2010) %>% 
  group_by(class) %>% 
  summarise(x2010 = sum(x2010)) -> female

male %>% 
  left_join(female,by = c('class','class')) %>% 
  rename(M = 'x2010.x',
         F = 'x2010.y',
         Group = 'class') %>% 
  mutate(bez = c(str_c(seq(0,80,by = 10),'to',seq(10,90,by = 10),sep = ' '),
                 '90 and\nOlder')) -> male_female


pdf_file <- 'Distributionver4.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 8,height = 8)

par(
  
  mai = c(0.2,0.25,0.8,0.25),
  omi = c(0.75,0.2,0.85,0.2),
  cex = 0.75,
  family = 'Lato Light',
  las = 1
  
)

right <- t(as.matrix(data.frame(800,male_female$F)))
left <- -t(as.matrix(data.frame(800,male_female$M)))


myColor_right <- c(par('bg'),rgb(255,0,210,150,maxColorValue = 255))
myColor_left <- c(par('bg'),rgb(191,239,255,maxColorValue = 255))


b1 <- barplot(right,axes = FALSE,horiz = TRUE,axis.lty = 0,
              border = NA,col = myColor_right,
              xlim = c(-8000,8000))


barplot(left,axes = FALSE,horiz = TRUE,axis.lty = 0,border = NA,
        col = myColor_left,xlim = c(-7500,7500),add = TRUE)

abline(v = seq(0,6000,by = 2000)+800,col = 'darkgrey',lty = 3)
abline(v = seq(-6000,0,by = 2000)-800,col = 'darkgrey',lty = 3)


mtext(format(seq(0,6000,by = 2000),big.mark = ','),
      at = seq(0,6000,by = 2000)+800,side = 1,line = 0.25,
      cex = 0.95,outer = FALSE)
mtext(format(abs(seq(-6000,0,by = 2000)),big.mark = ','),
      at = seq(-6000,0,by = 2000)-800,side = 1,line = 0.25,
      cex = 0.95)

text(0,b1,male_female$bez,cex = 1.15,font = 3)


mtext('Men',side = 3,line = 1,adj = 0.25,cex = 1.25,family = 'Lato Black')
mtext('Women',side = 3,line = 1,adj = 0.75,cex = 1.25,family = 'Lato Black')


# Titling;

mtext('Age structure of the population in Germany',side = 3,line = 2,
      adj = 0,cex = 1.75,family = 'Lato Black',outer = TRUE)
mtext('Values in thousand per year of Age',side = 3,line = -0.5,adj = 0,
      cex = 1.25,font = 3,outer = TRUE)
mtext('Source: Dantri.com.vn\nRedesigned by: Phan Tien Dung, Germany',
      side = 1,line = 2.5,adj = 0,cex = 0.95,font = 3,outer = TRUE)

dev.off()


# Converge image;

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





























