TransfermarktShiny<- function(team_name, team_num, comp_code) {
  
  session <- bow(glue::glue("https://www.transfermarkt.com/{team_name}/leistungsdaten/verein/{team_num}/plus/1?reldata={comp_code}%262019/"))
  #session <- bow(url)
  # grab name from photo element instead
  result_name <- scrape(session) %>% 
    html_nodes("#yw1 .bilderrahmen-fixed") %>% 
    html_attr("title") 
  
  # grab age
  result_age <- scrape(session) %>% 
    html_nodes(".posrela+ .zentriert") %>% 
    html_text()
  
  # grab minutes played in league
  result_mins <- scrape(session) %>% 
    html_nodes("td.rechts") %>% 
    html_text()
  
  ## get length
  session <- bow(glue::glue("https://www.transfermarkt.com/{team_name}/kader/verein/{team_num}/saison_id/2019/plus/1"))
  
  result_name2 <- scrape(session) %>% 
    html_nodes("#yw1 .bilderrahmen-fixed") %>% 
    html_attr("title") 
  
  result_bday <- scrape(session) %>% 
    html_nodes(".posrela+ .zentriert") %>% 
    html_text()
  
  result_joinedteam <- scrape(session) %>% 
    html_nodes("td:nth-child(7)") %>% 
    html_text()
  
  result_leaveteam <- scrape(session) %>% 
    html_nodes("td:nth-child(9)") %>% 
    html_text()
  
  # place each vector into list
  resultados <- list(result_name, result_age, result_mins)
  
  col_name <- c("name", "age", "minutes")
  
  results_comb <- resultados %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_name)
  
  ## join + bday
  resultados2 <- list(result_name2, result_bday, 
                      result_joinedteam, result_leaveteam)
  
  col_name2 <- c("name", "bday", "join", "leave")
  
  results_comb2 <- resultados2 %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_name2)
  
  ## combine BOTH
  results_comb <- results_comb %>% 
    left_join(results_comb2) 
  
  
  session <- bow(
    glue::glue(
      "https://www.transfermarkt.com/{team_name}/kadernaechstesaison/verein/{team_num}/geruechte/0/anschluss/0/plus/1"))
  
  result_name3 <- scrape(session) %>% 
    html_nodes("#yw1 .bilderrahmen-fixed") %>% 
    html_attr("title") 
  
  Club <- scrape(session) %>% 
    html_nodes(".dataName span") %>% 
    html_text() 
  
  result_bday2 <- scrape(session) %>% 
    html_nodes(".posrela+ .zentriert") %>% 
    html_text()
  
  result_joinedteam2 <- scrape(session) %>% 
    html_nodes("td:nth-child(5)") %>% 
    html_text()
  
  result_leaveteam2 <- scrape(session) %>% 
    html_nodes("td:nth-child(7)") %>% 
    html_text()
  print("last joun test")
  # place each vector into list
  resultados3 <- list(result_name3, result_bday2, 
                      result_joinedteam2, result_leaveteam2)
  
  col_name3 <- c("name", "bday2", "join2", "leave2")
  
  results_comb3 <- resultados3 %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_name3)
  
  results_comb <- results_comb %>% 
    left_join(results_comb3) 
  results_comb$bday <-   ifelse(is.na(results_comb$bday) ,results_comb$bday2,results_comb$bday)
  results_comb$join <-   ifelse(is.na(results_comb$join) ,results_comb$join2,results_comb$join)
  results_comb$leave <-   ifelse(is.na(results_comb$leave) ,results_comb$leave2,results_comb$leave)
  results_comb <- select(results_comb,1,2,3,4,5,6)
  # fix "strings" into proper formats, calculate % of minutes appeared
  all_team_minutes <- results_comb %>% 
    mutate(age = as.numeric(age),
           minutes = minutes %>% 
             str_replace("\\.", "") %>% 
             str_replace("'", "") %>% 
             as.numeric(),
           bday = str_replace_all(bday, "\\(.*\\)", "") %>% mdy(),
           join = join %>% mdy(),
           join_age = interval(bday, join) / years(1),
           leave = leave %>% dmy(),
           leave_age = interval(bday, leave) / years(1),
           age_now = interval(bday, Sys.Date()) / years(1)) %>% 
    filter(!is.na(minutes)) 
  all_team_minutes$age2 <- (Sys.Date() - all_team_minutes$bday)/365.25
  all_team_minutes$name <- all_team_minutes$name %>% str_replace_all("^(\\w)\\w+ (?=\\w)", "\\1.")
  all_team_minutes$Club <- Club
  all_team_minutes$Comp <- comp_code
  #compName <- allComp %>% dplyr::filter(Competition_Code == comp_code)
  #all_team_minutes$CompName <- compName[1,2]
  return(all_team_minutes)
}

ScatterShiny <- function(data,color1,color2,color3,color4,color5,teamname,alpha,left,right){
  #data<- all_team_minutes
  teamname <- gsub("-"," ",teamname)
  
  ggplot(data, aes(x=age_now, y=minutes)) +
    geom_rect(aes(xmin=left,xmax=right, ymin=-Inf,ymax= Inf), fill = color1, alpha=0.01 )+
    
    ggrepel::geom_text_repel(aes(label = name, family = "Spartan-Light"),color=color5, size = 3) +
    ggforce::geom_link(aes(x=age_now, xend=leave_age, y = minutes, yend = minutes,alpha = -stat(index)), color=color3) +
    ggforce::geom_link(aes(x=age_now, xend=join_age, y = minutes, yend = minutes, alpha = -stat(index)),color=color2)+
    geom_point(color=color4, size = 2) +
    theme_bw() + 
    aes(ymin=0) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    labs(x = paste("Age on",format(Sys.time(), "%d %b %Y")),
         y = "Minutes played",
         title = paste("Age plot", data$Club[1]),
         subtitle = paste(data$Comp[1], data$Seas[1]),
         caption = "Made on shiny.robinkoetsier.nl/ShinyAppAge | An app by @RobinWilhelmus ") +
    theme(
      text = element_text(family = "Spartan-Light"),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      legend.position = "none")
  
}

ScatterShinyTime <- function(data,color1,color2,color3,color4,color5,teamname,alpha,left,right){
  #data<- all_team_minutes
  teamname <- gsub("-"," ",teamname)
  
  ggplot(data, aes(x=age_now, y=minutes)) +
    geom_rect(aes(xmin=left,xmax=right, ymin=-Inf,ymax= Inf), fill = color1, alpha=0.01 )+
    
    ggrepel::geom_text_repel(aes(label = name, family = "Spartan-Light"),color=color5, size = 3) +
    # ggforce::geom_link(aes(x=age_now, xend=leave_age, y = minutes, yend = minutes,alpha = (1*-stat(index))), color=color3) +
    ggforce::geom_link(aes(x=age_now, xend=join_age, y = minutes, yend = minutes, alpha = -stat(index)),color=color2)+
    geom_point(color=color4, size = 2) +
    theme_bw() + 
    aes(ymin=0) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    labs(x = paste("Age on",format(Sys.time(), "%d %b %Y")),
         y = "Minutes played",
         title = paste("Age plot", data$Club[1]),
         subtitle = paste(data$Comp[1], data$Seas[1]),
         caption = "Made on shiny.robinkoetsier.nl/ShinyAppAge | An app by @RobinWilhelmus ") +
    theme(
      text = element_text(family = "Spartan-Light"),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      legend.position = "none")
  
}

ScatterShinyContract <- function(data,color1,color2,color3,color4,color5,teamname,alpha,left,right){
  #data<- all_team_minutes
  teamname <- gsub("-"," ",teamname)
  
  ggplot(data, aes(x=age_now, y=minutes)) +
    geom_rect(aes(xmin=left,xmax=right, ymin=-Inf,ymax= Inf), fill = color1, alpha=0.01 )+
    
    ggrepel::geom_text_repel(aes(label = name, family = "Spartan-Light"),color=color5, size = 3) +
    ggforce::geom_link(aes(x=age_now, xend=leave_age, y = minutes, yend = minutes,alpha = (1*-stat(index))), color=color3) +
    #ggforce::geom_link(aes(x=age_now, xend=join_age, y = minutes, yend = minutes, alpha = -stat(index)),color=color2)+
    geom_point(color=color4, size = 2) +
    theme_bw() + 
    aes(ymin=0) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    labs(x = paste("Age on",format(Sys.time(), "%d %b %Y")),
         y = "Minutes played",
         title = paste("Age plot", data$Club[1]),
         subtitle = paste(data$Comp[1], data$Seas[1]),
         caption = "Made on shiny.robinkoetsier.nl/ShinyAppAge | An app by @RobinWilhelmus ") +
    theme(
      text = element_text(family = "Spartan-Light"),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      legend.position = "none")
  
}

ScatterShinyNo <- function(data,color1,color2,color3,color4,color5,teamname,alpha,left,right){
  #data<- all_team_minutes
  teamname <- gsub("-"," ",teamname)
  
  ggplot(data, aes(x=age_now, y=minutes)) +
    geom_rect(aes(xmin=left,xmax=right, ymin=-Inf,ymax= Inf), fill = color1, alpha=0.01 )+
    
    ggrepel::geom_text_repel(aes(label = name, family = "Spartan-Light"),color=color5, size = 3) +
    # ggforce::geom_link(aes(x=age_now, xend=leave_age, y = minutes, yend = minutes,alpha = (1*-stat(index))), color=color3) +
    #  ggforce::geom_link(aes(x=age_now, xend=join_age, y = minutes, yend = minutes, alpha = -stat(index)),color=color2)+
    geom_point(color = color4, size = 2) +
    theme_bw() + 
    aes(ymin=0) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    labs(x = paste("Age on",format(Sys.time(), "%d %b %Y")),
         y = "Minutes played",
         title = paste("Age plot", data$Club[1]),
         subtitle = paste(data$Comp[1], data$Seas[1]),
         caption = "Made on shiny.robinkoetsier.nl/ShinyAppAge | An app by @RobinWilhelmus ") +
    theme(
      text = element_text(family = "Spartan-Light"),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      legend.position = "none")
  
}



