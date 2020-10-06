
'Pie Chart ver4'

library(tidyverse)
library(graphics)
library(stringr)
library(readr)
library(readxl)
library(RColorBrewer)
library(plotrix)
library(pdftools)


read_excel('worldenergymix.xlsx',sheet = 1) -> mydf
mydf %>% 
  select(-Region) -> mydf
myLabellings <- c('Oil','Coal','Gas','Renew.Energies','Nuclear\nEnergy')


pdf_file <- 'PieChartver4.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 10,height = 8)

par(
  omi = c(1,0.25,1,1),
  mai = c(0,2,0,0.5),
  cex.axis = 1.5,
  cex.lab = 1,
  xpd = TRUE,
  family = 'Lato Light',
  las = 1
)

myC1 <- rgb(80,80,80,155,maxColorValue = 255)
myC2 <- rgb(255,97,0,155,maxColorValue = 255)

radial.plot(mydf[2:3,],start = 1,grid.left = TRUE,labels = myLabellings,
            rp.type = 'p',main = '',line.col = c(myC1,myC2),
            poly.col = c(myC1,myC2),show.grid = TRUE,radial.lim = c(0,55),
            lwd = 8)

legend('bottomleft',legend = c('OECD','Asia'),pch = 15,col = c(myC1,myC2),
       bty = 'n',cex = 1.25)

# Titling;

mtext('Energy Mix: OECD and Asia by comparision',side = 3,line = 2,cex = 2.5,adj = 0,
      family = 'Lato Black',outer = TRUE)
mtext('All value in percent',side = 3,line = 0,cex = 1.25,adj = 0,font = 3,outer = TRUE)
mtext('Source: dantri.com.vn. Redesigned by: Phan Tien Dung',side = 1,line = 2,cex = 1,
      adj = 1,font = 3,outer = TRUE)

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




