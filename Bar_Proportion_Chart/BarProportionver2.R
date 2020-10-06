

'Bar Proportion ver2'

library(tidyverse)
library(graphics)
library(RColorBrewer)
library(readr)
library(readxl)
library(pdftools)



pdf_file <- 'barproportonver2.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 11,height = 8.5)
par(
  
  
  omi = c(0.5,0.5,1.1,0.5),
  family = 'Lato Light',
  las = 1
  
  
)

layout(matrix(seq(1,5,by = 1),nrow = 1,ncol = 5),widths = c(2,rep(1,4)),
       heights = c(1,1))
read_excel('income_five_classes.xlsx',
           sheet = 1,
           skip = 1,
           col_names = TRUE)  -> myData
tmyData <- t(myData)
transparencey <- seq(0,200,by = 50)
number_colors <- c(rep('black',4),'white')
pos <- c(rep(45,4),35)
par(cex = 1.05)


for (i in 1:5) {
  
  
  if (i == 1) {
    
    
    par(
      
      mai = c(0.25,1.75,0.25,0.15)
      
    )
    
    bp1 <- barplot(tmyData[,i],horiz = TRUE,cex.names = 1.15,axes = FALSE,
                   names.arg = gsub('.','',names(myData),fixed = TRUE),
                   xlim = c(0,60),col = rgb(43,15,52,0,maxColorValue = 255))
    
  } else {
    
    
    par(
      
      mai = c(0.25,0.1,0.25,0.15)
      
    )
    bp2 <- barplot(tmyData[,i],horiz = TRUE,axisnames = FALSE,axes = FALSE,
                   xlim = c(0,60),col = rgb(200,0,0,transparencey[i],maxColorValue = 255),
                   border = NA)
    
  }
  
  
  text(pos[i],bp1,adj = 1,labels = str_c(round(myData[i,],digits = 0),'%',sep = ''),
       col = number_colors[i],xpd = TRUE,cex = 1)
  mtext(seq(0,60,by = 15),at = seq(0,60,by = 15),side = 1,line = 0,cex = 0.95)
  arrows(0,-0.1,0,14.5,lwd = 2.15,length = 0,xpd = TRUE,col = 'dodgerblue')
  
  
  
}

# Titling;

title(main = 'Income Distribution over five classes in different Countries',
      line = 3,adj = 0,cex.main = 1.75,family = 'Lato Black',outer = TRUE)


myBreaks <- list()
myBreaks[[1]] <- 'In Mexico the richest 20% of income recipients hold over 64% of the overall income, in Norway'
myBreaks[[2]] <- 'the figure is 40%. Compared interntionally Germany is in the upper half.'


for (i in 1:length(myBreaks)) {
  
  
  mtext(myBreaks[[i]],side = 3,line = (1.8-i)*1.7,adj = 0,cex = 1.15,outer = TRUE)
  
  
}

mtext('Source: Dantri.com.vn\nRedesigned by: Phan Tien Dung, Germany.',side = 1,
      line = 2.25,adj = 1,font = 3)

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










