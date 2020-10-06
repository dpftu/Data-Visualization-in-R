
'Scatter Plot ver3'

library(tidyr)
library(graphics)
library(readr)
library(readxl)
library(pdftools)



rm(list = ls())
cat('\f')

pdf_file <- 'scatterplotver3.pdf'
cairo_pdf(pdf_file,bg = 'grey98',height = 9,width = 11.69)
par(
  
  
  mai = c(0.85,1,0.25,0.25),
  omi = c(1,0.5,1,0.5),
  family = 'Lato Light',
  las = 1
  
  
)



myPersons <- read_excel('persons.xlsx')
myPersons %>% 
  filter(w > 0,s == 'm',name != 'Max Schmeling') -> myData
attach(myData)



plot(type = 'n',xlab = 'Height (cm)',ylab = 'Weight (kg)',x = h,y = w,
     xlim = c(160,220),ylim = c(50,125),axes = FALSE)
axis(side = 1,col = par('bg'),col.ticks = 'grey81',lwd.ticks = 0.5,
     tick = -0.025)
axis(side = 2,col = par('bg'),col.ticks = 'grey81',lwd.ticks = 0.5,
     tick = -0.025)


# Coloring;


myC1 <- rgb(255,0,210,maxColorValue = 255)
myC2 <- rgb(0,208,226,100,maxColorValue = 255)


myData %>% 
  select(h,w) %>%
  filter(w > 20*(h/100*h/100),w < 25*(h/100*h/100)) -> myP1
myData %>% 
  select(h,w) %>% 
  filter(w < 20*(h/100*h/100)) -> myP2
myData %>% 
  select(h,w) %>% 
  filter(w > 25*(h/100*h/100)) -> myP3
myData %>% 
  select(name,w,h) %>% 
  filter(w < 20*(h/100*h/100)) %>%
  select(name) %>% 
  as.matrix() -> myDes2
myData %>% 
  select(name,w,h) %>% 
  filter(w > 25*(h/100*h/100)) %>%
  select(name) %>% 
  as.matrix() -> myDes3



symbols(myP1,bg = myC1,fg = 'white',circles = rep(1,nrow(myP1)),
        inches = 0.25,add = TRUE)
symbols(myP2,bg = myC2,fg = 'white',circles = rep(1,nrow(myP2)),
        inches = 0.25,add = TRUE)
symbols(myP3,bg = myC2,fg = 'white',circles = rep(1,nrow(myP3)),
        inches = 0.25,add = TRUE)



# Texting;


text(myP2,labels = myDes2,cex = 0.75,pos = 1,offset = 1.55,
     family = 'Lato Black')
text(myP3,labels = myDes3,cex = 0.75,pos = 3,offset = 1.55,
     family = 'Lato Black')


# Add curves;


curve(20*(x/100*x/100),xlim = c(160,220),add = TRUE)
curve(25*(x/100*x/100),xlim = c(160,220),add = TRUE)


# Add horizontal vs vertical line;


abline(v = mean(h,na.rm = TRUE),lty = 3)
abline(h = mean(w,na.rm = TRUE),lty = 3)
text(x = 182.5,y = 52,labels = 'Average Height: 182 cm',
     adj = 0,font = 3)


# Titling;


mtext(text = 'Relationship between height and weight',side = 3,
      adj = 0,line = 1.75,cex = 1.85,outer = TRUE,family = 'Lato Black')
mtext(text = 'A couple of selected celebrities',side = 3,adj = 0,line = 0,
      cex = 1.25,outer = TRUE,font = 3)
mtext(text = 'Source: Dantri.com.vn.\nRedesigned by Phan Tien Dung',
      side = 1,line = 1,adj = 1,cex = 1.0,outer = TRUE,font = 3,
      family = 'Lato Black')


dev.off()


# Convert into image;


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







