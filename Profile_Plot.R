library(readxl)
library(readr)
library(tidyverse)
library(pdftools)
library(graphics)
library(RColorBrewer)


profile_plot <- function(df,
                         text_left = c(),
                         text_right = c(),
                         highlight_col = -1,
                         text_width_adjust = 30,
                         colours = NULL,
                         draw_segments = TRUE,
                         draw_mid = TRUE,
                         plot_legend = TRUE,
                         legend_n_col = NA){
  
  min_df <- floor(min(df))
  max_df <- ceiling(max(df))
  mid_df <- (min_df+max_df)/2
  
  par_default <- par(no.readonly = TRUE)
  
  plot.new()
  
  par(omi = c(0.25,1.5,0.95,1.5))
  
  left_text_width <-  max(strwidth(text_left))
  right_text_width <- max(strwidth(text_right))
  left_text_width <- left_text_width*text_width_adjust
  right_text_width <- right_text_width*text_width_adjust
  
  par(mai = c(0,2,0,2))
  
  if (plot_legend) {
    
    line_height = 0.66
    caption_height = -((nrow(df)%%3)+1)*line_height
    plot.window(xlim = c(min_df,max_df),ylim = c(caption_height,nrow(df)))
    
  }else {
    
    plot.window(xlim = c(min_df,max_df),ylim = c(0,nrow(df)))
    
  }
  
  y <- 1:nrow(df)
  
  if (draw_segments) {
    
    segments(min_df,y,max_df,y,lty = 'dotted',col = 'gray')
    
  }
  
  if (draw_mid) {
    
    lines(c(mid_df,mid_df),c(1,nrow(df)),lwd = 3,col = 'gray')
    
  }
  
  if (is.null(colours)) {
    
    colours <- rainbow(ncol(df))
    
  }
  
  if (highlight_col > 0) {
    
    par(lwd = 4,lty = 'solid')
    
  }
  
  for (i in 1:ncol(df)) {
    
    if (highlight_col == i) {
      
      par(lwd = 3,lty = 1)
      
    }else {
      
      par(lwd = 3,lty = 'solid')
      
    }
    
    for (j in 1:nrow(df)) {
      
      cur_y <- (nrow(df)+1)-j
      points(df[j,i],cur_y,col = colours[(i %% length(colours))+1],pch = 19)
      
      if (j > 1) {
        
        lines(c(df[j,i],df[j-1,i]),
              c(cur_y,cur_y+1),
              col = colours[(i %% length(colours))+1])
        
      }
      
    }
    
  }
  
  mtext(rev(text_left),at = y,adj = 1,side = 2,las = 2)
  mtext(rev(text_right),at = y,adj = 0,side = 4,las = 2)
  
  ticks <- c(round(min_df,3),
             round((min_df+mid_df)/2,3),
             round(mid_df,3),
             round((mid_df+max_df)/2,3),
             round(max_df,3))
  
  par(cex.axis = 0.9,mex = 0.5)
  axis(side = 1,at = ticks,labels = ticks,pos = 0.5)
  
  if (plot_legend) {
    
    if (is.na(legend_n_col)) {
      
      legend_n_col <- ceiling(ncol(df)/2)
      
    }
    
    legend(min_df,0,legend = colnames(df),pch = 19,
           col = colours,bg = NA,box.col = NA,ncol = legend_n_col)
    
  }
  
  par(par_default)
  
}


# Plot here;


pdf_file <- 'Profile_Plot.pdf'
cairo_pdf(filename = pdf_file,bg = 'grey98',height = 8,width = 12)
par(
  
  lheight = 1.15,
  mai = rep(0.25,4),
  omi = c(0.5,0.5,1.1,0.5),
  family = 'Lato Light',
  las = 1
  
  )


text_left <- NULL
text_right <- NULL
text_left <- c(text_left,'Individuals should take more responsibility for\nproviding for themselves.')
text_right <- c(text_right,'The state should take more \nresponsibility to ensure that everyone\nis provided for.')
text_left <- c(text_left,'People who are unemployed should\nhave to take any job available or lose\ntheir unemployment benefits.')
text_right <- c(text_right,'People who are unemployed should\nhave the right to refuse a job they\ndo not want.')
text_left <- c(text_left,'Competition is good. It stimulates\npeople to work hard and develop\nnew ideas.')
text_right <- c(text_right,'Competition is harmful. It brings\nout the worst in people.')
text_left <- c(text_left,'The government should give companies\nmore freedom.')
text_right <- c(text_right,'The state should control firms more\neffectively.')
text_left <- c(text_left,'Incomes should be made more equal.')
text_right <- c(text_right,'There should be greater incentives\nfor individual effort.')
text_left <- c(text_left,'Private ownership of business\nand industry should be increased.')
text_right <- c(text_right,'Government ownership of\nbusiness and industry should be\nincreased.')


# Creat hypothetical data
c1 <- c(3.5,4,6)
c2 <- c(3,3.5,5.5)
c3 <- c(2.3,2.7,2)
c4 <- c(4.75,5.35,5)
c5 <- c(3.8,4.2,5)
c6 <- c(4.8,5.5,3.75)
myData <- matrix(c(c1,c2,c3,c4,c5,c6),byrow = TRUE,nrow =  6)
colnames(myData) <- c('Male','Female',"Don't know")


myC1 <- 'Skyblue'
myC2 <- 'darkred'
myC3 <- 'red'


profile_plot(myData,text_left = text_left,text_right = text_right,
             colours = c(myC1,myC2,myC3),legend_n_col = 3)

mtext('Now, I would like to ask you to please tell me your views on different statements.',
      side = 3,line = 3,adj = 0,cex = 1.5,family = 'Lato Black',outer = TRUE)
mtext('Redesigned: Phan Tien Dung.\nUniversity of Tuebingen, Germany',side = 1,line = -0.5,adj = 1,
      cex = 0.95,font = 3,outer = TRUE,family = 'Lato Black')

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









