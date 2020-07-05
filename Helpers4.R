
ScatterShinyDark <- function(data,color1,color2,color3,color4,color5,teamname,alpha,left,right){
  #data<- all_team_minutes
  teamname <- gsub("-"," ",teamname)
  
  ggplot(data, aes(x=age_now, y=minutes)) +
    geom_rect(aes(xmin=left,xmax=right, ymin=-Inf,ymax= Inf), fill = color1, alpha=0.01 )+
    
    ggrepel::geom_text_repel(aes(label = name, family = "Spartan-Light"),color=color5, size = 3) +
    ggforce::geom_link(aes(x=age_now, xend=leave_age, y = minutes, yend = minutes,alpha = -stat(index)), color=color3) +
    ggforce::geom_link(aes(x=age_now, xend=join_age, y = minutes, yend = minutes, alpha = -stat(index)),color=color2)+
    geom_point(color=color4, size = 2) +
    dark_theme_gray() + 
    aes(ymin=0) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    labs(x = "Age at start of season",
         y = "Minutes played",
         title = paste("Age plot", data$Club[1]),
         subtitle = paste(data$CompName[1], data$Seas[1]),
         caption = "Made on shinynew.robinkoetsier.nl/AppTwo | An app by Robin Koetsier | @RobinWilhelmus ") +
    theme(
      text = element_text(family = "Spartan-Light"),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      plot.background = element_rect(fill = "grey10"),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey30", size = 0.2),
      panel.grid.minor = element_line(color = "grey30", size = 0.2),
      legend.position = "none")
  
}

ScatterShinyTimeDark <- function(data,color1,color2,color3,color4,color5,teamname,alpha,left,right){
  #data<- all_team_minutes
  teamname <- gsub("-"," ",teamname)
  
  ggplot(data, aes(x=age_now, y=minutes)) +
    geom_rect(aes(xmin=left,xmax=right, ymin=-Inf,ymax= Inf), fill = color1, alpha=0.01 )+
    
    ggrepel::geom_text_repel(aes(label = name, family = "Spartan-Light"),color=color5, size = 3) +
    # ggforce::geom_link(aes(x=age_now, xend=leave_age, y = minutes, yend = minutes,alpha = (1*-stat(index))), color=color3) +
    ggforce::geom_link(aes(x=age_now, xend=join_age, y = minutes, yend = minutes, alpha = -stat(index)),color=color2)+
    geom_point(color=color4, size = 2) +
    dark_theme_gray() + 
    aes(ymin=0) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    labs(x = "Age at start of season",
         y = "Minutes played",
         title = paste("Age plot", data$Club[1]),
         subtitle = paste(data$CompName[1], data$Seas[1]),
         caption = "Made on shinynew.robinkoetsier.nl/AppTwo | An app by Robin Koetsier | @RobinWilhelmus ") +
    theme(
      text = element_text(family = "Spartan-Light"),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      plot.background = element_rect(fill = "grey10"),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey30", size = 0.2),
      panel.grid.minor = element_line(color = "grey30", size = 0.2),
      legend.position = "none")
  
}

ScatterShinyContractDark <- function(data,color1,color2,color3,color4,color5,teamname,alpha,left,right){
  #data<- all_team_minutes
  teamname <- gsub("-"," ",teamname)
  
  ggplot(data, aes(x=age_now, y=minutes)) +
    geom_rect(aes(xmin=left,xmax=right, ymin=-Inf,ymax= Inf), fill = color1, alpha=0.01 )+
    
    ggrepel::geom_text_repel(aes(label = name, family = "Spartan-Light"),color=color5, size = 3) +
    ggforce::geom_link(aes(x=age_now, xend=leave_age, y = minutes, yend = minutes,alpha = (1*-stat(index))), color=color3) +
    #ggforce::geom_link(aes(x=age_now, xend=join_age, y = minutes, yend = minutes, alpha = -stat(index)),color=color2)+
    geom_point(color=color4, size = 2) +
    dark_theme_gray() + 
    aes(ymin=0) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    labs(x = "Age at start of season",
         y = "Minutes played",
         title = paste("Age plot", data$Club[1]),
         subtitle = paste(data$CompName[1], data$Seas[1]),
         caption = "Made on shinynew.robinkoetsier.nl/AppTwo | An app by Robin Koetsier | @RobinWilhelmus ") +
    theme(
      text = element_text(family = "Spartan-Light"),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      plot.background = element_rect(fill = "grey10"),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey30", size = 0.2),
      panel.grid.minor = element_line(color = "grey30", size = 0.2),
      legend.position = "none")
  
}

ScatterShinyNoDark <- function(data,color1,color2,color3,color4,color5,teamname,alpha,left,right){
  #data<- all_team_minutes
  teamname <- gsub("-"," ",teamname)
  
  ggplot(data, aes(x=age_now, y=minutes)) +
    geom_rect(aes(xmin=left,xmax=right, ymin=-Inf,ymax= Inf), fill = color1, alpha=0.01 )+
    
    ggrepel::geom_text_repel(aes(label = name, family = "Spartan-Light"),color=color5, size = 3) +
    # ggforce::geom_link(aes(x=age_now, xend=leave_age, y = minutes, yend = minutes,alpha = (1*-stat(index))), color=color3) +
    #  ggforce::geom_link(aes(x=age_now, xend=join_age, y = minutes, yend = minutes, alpha = -stat(index)),color=color2)+
    geom_point(color = color4, size = 2) +
    dark_theme_gray() + 
    aes(ymin=0) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    labs(x = "Age at start of season",
         y = "Minutes played",
         title = paste("Age plot", data$Club[1]),
         subtitle = paste(data$CompName[1], data$Seas[1]),
         caption = "Made on shinynew.robinkoetsier.nl/AppTwo | An app by Robin Koetsier | @RobinWilhelmus ") +
    theme(
      text = element_text(family = "Spartan-Light"),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      plot.background = element_rect(fill = "grey10"),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey30", size = 0.2),
      panel.grid.minor = element_line(color = "grey30", size = 0.2),
      legend.position = "none")
  
}



