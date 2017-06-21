##=====  Load Libraries

library(pacman)
p_load(data.table)
p_load(foreach)
p_load(plotly)
p_load(dplyr)
p_load(stringi)

##===== Functions

ReformatYear <- function(values){
  #takes in list of values (character type)
  #modification = removes all occurences of ","
  #returns modified list of values (character type)

  foreach(i=1:length(values), .combine = c) %do%{
    values[i] <- as.character(gsub(",","", values[i]))
  }
}


ReformatGender <- function(values){
  values <- stri_trim_both(as.character(values))
  old <- 'All persons'
  new <- 'All'
  values[which(values %in% old)] <- new
  values
}


ReformatAgeRange <- function(values){
  values <- stri_trim_both(as.character(values))
  old <- 'All 16+'
  new <- '16+'
  values[which(values %in% old)] <- new
  values
}


ReformatProportion <- function(values){
  #takes in list of values (numeric type)
  #modification = removes all occurences of "%"
  #returns modified list of values (numeric type)

  foreach(i=1:length(values), .combine = c) %do%{
    values[i] <- as.numeric(gsub("%","", values[i]))
  }
}


ReformatQuestion <- function(values){
  #takes in list of values (char type)
  #modification = fix spelling error in 'Drank alcohol on five on more days in the last week'
  #returns modified list of values (char type)
  old <- 'Drank alcohol on five on more days in the last week'
  new <- 'Drank alcohol on 5+ days'
  values[which(values %in% old)] <- new
  
  old <- 'Drank alcohol in the last week'
  new <- 'Drank alcohol'
  values[which(values %in% old)] <- new
  
  values
}



MakeBarPlot <- function(df){
  #takes in a dataframe
  #returns a bar plots: Question vs Porportion BY Years = 2005,2016
 df[Year %in% c(2005,2016) & AgeRange == '16+' & Gender == 'All',
    .(Year, Question, Proportion)
  ][, Change := c(NA,diff(Proportion)), by= 'Question'] -> table
  
  use.font <- list(
    family = "Courier New, monospace",
    size = 12,
    color = "black"
  )
  
  ax <- list(
    title = "",
    showticklabels = TRUE,
    zeroline = FALSE,
    showline = FALSE,
    showgrid = FALSE,
    tickfont = use.font
  )
  
  ay <- list(
    title = '',
    titlefont = use.font,
    showticklabels = FALSE,
    zeroline = FALSE,
    showline = FALSE,
    showgrid = FALSE
  )
  
  plot_ly(
    type = 'bar',
    table,
    x = ~Question,
    y = ~Proportion,
    color = ~Year,
    hoverinfo = 'text',
    text = ~paste0(
      "<br>",'Proportion: ', Proportion,'%',
      "<br>",'Change: ', Change,
      "<br>",'Year: ', Year
    )
  )%>%layout(
    title="Trend", titlefont = use.font, xaxis=ax, yaxis=ay ,
    annotations = list(x = ~Question, y = ~Proportion, text = c('', '-7.3%', '', '-7.2%', '', '+2%'),
                       xanchor = 'center', yanchor = 'bottom',
                       showarrow = FALSE),
    legend = list(x = 0.6, y = 0.7, orientation = 'h')
  )
  
  
  
}

MakeLinePlot <- function(df, age.range, question, counter){
  #takes in a dataframe, two category identifiers; (df, chr,chr)
  #modification = dataframe is filtered by given categories
  #returns a series plot: ProportionChange vs Year
  
  table <- df[AgeRange %in% age.range & Question %in% question]
  use.font <- list(
    family = "Courier New, monospace",
    size = 12,
    color = "black"
  )
  
  ax <- list(
    title = "",
    showticklabels = TRUE,
    zeroline = FALSE,
    showline = FALSE,
    showgrid = FALSE,
    tickfont = use.font
  )
  
  ay <- list(
    title = question,
    titlefont = use.font,
    showticklabels = FALSE,
    zeroline = FALSE,
    showline = FALSE,
    showgrid = FALSE
  )
  
  if (counter != 3){
    
    table %>% plot_ly(
      x = ~Year,
      y = ~ProportionChange,
      type = "scatter",
      mode = "lines",
      color = ~Gender,
      colors = c("#008080","#708090","#B8860B"),
      hoverinfo ="text",
      showlegend = FALSE,
      text =~paste0(
        '<br>', question,
        '<br>', 'Gender: ', Gender,
        '<br>', 'Change: ', ProportionChange,"%",
        '<br>', 'Year: ', Year
        
      )
    )%>%
      layout(
        title = age.range, 
        titlefont = use.font , 
        xaxis = ax, 
        yaxis = ay,
        autosize = T
      )
  }else{
    table %>% plot_ly(
      x = ~Year,
      y = ~ProportionChange,
      type = "scatter",
      mode = "lines",
      color = ~Gender,
      colors = c("#008080","#708090","#B8860B"),
      hoverinfo ="text",
      # showlegend = TRUE
      text =~paste0(
        '<br>', question,
        '<br>', 'Gender: ', Gender,
        '<br>', 'Change: ', ProportionChange,"%",
        '<br>', 'Year: ', Year
        
      )
    )%>%
      layout(
        title = age.range, 
        titlefont = use.font , 
        xaxis = ax, 
        yaxis = ay,
        autosize = T,
        legend = list(orientation = 'h')
      )
  }
  
  
  
}

FetchLinePlots <- function(df){
  #takes in a dataframe
  #modification = None
  #returns a series plot listed by Question an Age Range
  age.ranges <- unique(df$AgeRange)
  questions <- unique(df$Question)
  
  foreach(i=1:length(age.ranges)) %do%{
    return(
      foreach(j=1:length(questions)) %do%{
        return(MakeLinePlot(df, age.ranges[i], questions[j], j))
      }
    )
  } -> all.plots
  
  names(all.plots) <- age.ranges
  
  
  return(all.plots)
}

# ##===== fetch data from csv file in working directory
data <- fread('data.csv')
#glimpse(data)

### ################ Observation
### All columns are character types.
### 
### The Year column has some cases of
### Inconsistent formating, (2,007 instead of 2007).
### 
### Change "Age Range" column name to "AgeRange"
### to facilitate selection and data operations.
### Also, change 'All 16+' => '16+'
### 
### For Gender Column, replace 'All Persons' > 'All'
### 
### The proportion column must be numeric; hence, we shall remove the "%"
### and then convert values to numeric type
### 
### 
### The Question column has some cases of
### spelling errors, i.e replacing 'on' with 'or' in 
### 'Drank alcohol on five on more days in the last week'.
### 
### The Year-over-Year proportion changes must be calculated
### ################


####===== Reformating columns
data[, Year:= ReformatYear(Year)]
data[, Gender := ReformatGender(Gender)]
data[, Proportion := ReformatProportion(Proportion)]
data[, Question := ReformatQuestion(Question)]

####===== Change Age Range => AgeRange AND REFORMAT
setnames(data, "Age Range", "AgeRange")
data[, AgeRange := ReformatAgeRange(AgeRange)]

####===== Create PoportionChange variable
data[, ProportionChange := round(c(0,diff(Proportion)),2), by = c("Question", "AgeRange","Gender")]


####===== Make Line Plots for Proportion Changes
line.plots <- FetchLinePlots(data)

####===== Group Line Plots By Age Range
# test <- plotly::subplot(
#   get.plots$`16 to 24`,
#   nrows = 3,
#   shareX = TRUE, shareY = TRUE
# )


####===== Bar Plots By Question showing proportion difference for 2005 & 2016

