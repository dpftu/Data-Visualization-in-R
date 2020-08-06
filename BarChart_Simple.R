

library(readxl)
library(readr)
library(tidyverse)
library(pdftools)




pdf_file <- 'BarCharts_Simple.pdf'
cairo_pdf(bg = 'grey98',filename = pdf_file,width = 9,height = 6.5)



par(
  
  omi = c(0.65,0.25,0.75,0.75),
  mai = c(0.3,2,0.35,0),
  mgp = c(3,3,0),
  family = 'Lato Light',
  las = 1
  
)



read_excel('ipsos.xlsx',
           sheet = 1,
           col_names = TRUE) %>% 
  arrange(Percent) -> IPSOS
attach(IPSOS)


#-------------------------------------------------------------------------------------------------------------------------#


barplot(Percent,names.arg = FALSE,horiz = TRUE,
        border = NA,xlim = c(0,100),col = 'grey',
        cex.names = 0.85,axes = FALSE) -> x


for (i in 1:nrow(IPSOS)) {
  
  if (Country[i] %in% c('Germany','Brazil')) {
    
    myFont <- 'Lato Black'
    
  }else{
    
    myFont <- 'Lato Light'
    
  }
  
  text(-8,x[i],Country[i],xpd = TRUE,adj = 1,cex = 0.85,family = myFont)
  text(-3,x[i],Percent[i],xpd = TRUE,adj = 1,cex = 0.85,family = myFont)
  
}

# Add other rectangulars:


rect(0,-0.5,20,28,col = rgb(191,239,255,80,maxColorValue = 255),border = NA)
rect(20,-0.5,40,28,col = rgb(191,239,255,120,maxColorValue = 255),border = NA)
rect(40,-0.5,60,28,col = rgb(191,239,255,80,maxColorValue = 255),border = NA)
rect(60,-0.5,80,28,col = rgb(191,239,255,120,maxColorValue = 255),border = NA)
rect(80,-0.5,100,28,col = rgb(191,239,255,80,maxColorValue = 255),border = NA)


IPSOS %>% 
  mutate(myValue2 = ifelse(Country %in% c('Brazil','Germany'),Percent,0)) %>% 
  .[['myValue2']] -> myValue2
myColor2 <- rgb(255,0,210,maxColorValue = 255)


barplot(myValue2,names.arg = FALSE,horiz = TRUE,
        border = NA,xlim = c(0,100),col = myColor2,
        cex.names = 0.85,axes = FALSE,add = TRUE) -> x2


arrows(45,-0.5,45,20.5,lwd = 1.5,length = 0,xpd = TRUE,col = 'dodgerblue')
arrows(45,-0.5,45,-0.75,lwd = 3,length = 0,xpd = TRUE)
arrows(45,20.5,45,20.75,lwd = 3,length = 0,xpd = TRUE)


text(41,20.5,'Average',adj = 1,xpd = TRUE,cex = 0.65,font = 3)
text(44,20.5,'45',adj = 1,xpd = TRUE,cex = 0.65,family = 'Lato',font = 4)
text(100,20.5,'All values in percent',adj = 1,cex = 0.65,xpd = TRUE,font = 3)


mtext(c(0,20,40,60,80,100),at = c(0,20,40,60,80,100),1,line = 0,cex = 0.8)
mtext("'I Definitely Believe in God or Supreme Being:'",
      side = 3,line = 1,adj = 0,cex = 1.2,family = 'Lato Black',
      outer = TRUE)
mtext('was said in 2010:',side = 3,line = 0,
      adj = 0.0025,cex = 0.9,outer = TRUE)
mtext(text = 'Redesign: Phan Tien Dung \nUniversity of Tuebingen',
      side = 1,line = 1,adj = 1,cex = 0.65,outer = TRUE,font = 3,
      family = 'Lato Black')


dev.off()


# Convert image format;


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


