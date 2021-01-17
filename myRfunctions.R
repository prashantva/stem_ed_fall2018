# My functions
#options(echo=FALSE)
ccc<-function()
{
  # Clear all clutter
  graphics.off() # close all previous graphs
  rm(list=ls()) # clear all variables from the workspace
  #rm(list=setdiff(ls(envir = .GlobalEnv), "df"), envir = .GlobalEnv)
  cat("\014") # clear the console
}

libraries<-function()
{
  # Add the necessary libraries
  library("readxl")
  library("GGally")
  library("mice")
  library("corrgram");
  library("Hmisc");
  library("MASS");
  library("dplyr")
  library("ggpubr")
  library("car")
  library("pwr")
  library("corrplot")
  library("ggplot2")
  library("qqplotr")
  library("nortest")
  library("mctest")
  library("plot3D")
}

# Function for getting and cleaning the data and producing the data.frame
getNcleanXL<-function(excel_data_path)
{
  # ref: # https://stackoverflow.com/questions/36330570/mice-does-not-impute-certain-columns-but-also-does-not-give-an-error
  df_raw <-read_excel(excel_data_path);
  # We should deal with the outliers
  cols<-colnames(df_raw) # column names
  M<-nrow(df_raw);
  N<-ncol(df_raw);
  for (i in 1:N){
    #print(cols[i])
    df_raw[df_raw[,i] %in% "",cols[i]]="NA"; # replace empty values by "NA"
    suppressWarnings(df_raw[,i] <- as.numeric(as.matrix(df_raw[,i])));# replace "NA" characters by NA
    outliers<-boxplot.stats(as.matrix(data.frame(df_raw[, i])))$out # Find the outliers
    #print(outliers)
    df_raw[as.numeric(as.matrix(df_raw[,i])) %in% outliers, cols[i]] =as.numeric("NA"); # Remove the outliers with "NA"
    suppressWarnings(df_raw[,i] <- as.numeric(as.matrix(df_raw[,i]))); # Replace the empty values with NA
  }
  tempData <- mice(df_raw,m=5,maxit=50,meth='pmm',seed=500, printFlag=FALSE); #summary(tempData);
  df_completed <- mice::complete(tempData, 1)
  na_cols<-colnames(df_completed)[colSums(is.na(df_completed)) > 0]; # these are columns that still contain NA values
  df<-df_completed[ , !(names(df_completed) %in% na_cols)] # remove the columns that have NA values
  return(df) # return(df) 
}

library("ggplot2")
# Define my theme for the ggplots
my_theme=theme(panel.background = element_rect(fill = "white"),
                 panel.border = element_rect(color="black", fill="NA", size=1),
                 panel.grid.major = element_line(size = 1, linetype = 'solid', color = "lightgray"),
                 panel.grid.minor = element_line(size = 0.5, linetype = 'solid', color = "lightgray"),
                 axis.text=element_text(size=30),
                 axis.title=element_text(size=30),
                 legend.title=element_text(size=20),
                 legend.key.height= unit(0.27, "npc") # height of thr colorbar if it exists
)