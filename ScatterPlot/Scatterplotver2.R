
'Scatter Plot ver2'

library(tidyverse)
library(graphics)
library(readr)
library(readxl)
library(pdftools)


pdf_file <- 'scatterplotver2.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 11.69,height = 9)
par(
  
  mar = c(4,4,0.5,2),
  omi = c(0.5,0.5,1,0),
  family = 'Lato Light',
  las = 1
  
)



# Data Preparation;

read.csv('struktbtwkr2005.csv',header = FALSE,sep = ';',dec = '.') -> df

df %>% 
  filter(V2 > 0 & V34 > 10) -> df
attach(df)


myXdes <- 'Unemployed population(%)'
myYdes <- 'Net migration (per 1,000 Inhabitants)'


plot(type= 'n',xlab = myXdes,ylab = myYdes,V34,V21,xlim = c(10,26),
     ylim = c(-20,35),axes = FALSE,cex.lab = 1.2) 
axis(side = 1,lwd.ticks = 0.5,cex.axis = 1.15,tick = -0.015)
axis(side = 2,lwd.ticks = 0.5,cex.axis = 1.15,tick = -0.015)


myC1 <- rgb(0,208,226,200,maxColorValue = 255)
myC2 <- rgb(255,0,210,150,maxColorValue = 255)
fit <- lm(formula = V21~V34)
df %>%
  mutate(fitted = as.numeric(fitted(fit)),
         residuals = as.numeric(resid(fit))) -> df

points(V34,df$fitted,col = myC2,type = 'l',lwd = 8)

df %>% 
  arrange(desc(-abs(residuals))) -> df_sorted


df_sorted_begin <- df_sorted[1:5,]
df_sorted[5+1:length(df$fitted),c('V34','V21')] -> myP1
df_sorted_begin[c('V34','V21')] -> myP2
sqrt(df_sorted$V6)/10 -> myR1
sqrt(df_sorted_begin$V6)/10 -> myR2



symbols(myP1,circles = myR1,inches = 0.3,bg = myC1,fg = 'white',add = TRUE)
symbols(myP2,circles = myR2,inches = 0.3,bg = myC2,fg = 'white',add = TRUE)



abline(v = mean(V34,na.rm = TRUE),col = 'black',lty = 3)
abline(h = mean(V21,na.rm = TRUE),col = 'black',lty = 3)



# Adding text;

text(x = 20,y = 20,labels = 'The five largest deviation are highlighted\npoints size: constituency area',
     adj = 0,cex = 1.15,font = 3)



# Titling;


mtext(text = 'Unemployed population,migration in Germany,2005',side = 3,
      adj = 0,line = 2,cex = 2.25,outer = TRUE,family = 'Lato Black')
mtext(text = 'County level, unemployment rate above 10 percent',side = 3,
      adj = 0,line = 0,cex = 1.25,outer = TRUE,font = 3)
mtext(text = 'Source: Dantri.com.vn. Redesigned by Phan Tien Dung',side = 1,
      line = 4.5,adj = 1,cex = 1.15,font = 3, family = 'Lato Black')


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




