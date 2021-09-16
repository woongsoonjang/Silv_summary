
lab_maker <- function(pr_type, st_prep=NULL){ 
  if (pr_type !=1) {
   switch(pr_type,
               NA,
               "Brushing",
               "Juvenile Spacing",
               c("Planting Area", "Planting Trees"),
               "Fertilization",
               "Survey")
} else if (pr_type ==1) {
  return(st_prep)
}
}

df_maker <- function(data, pr_type, year, st_prep=NULL){
  
  if (pr_type !=1) {
  var_name <- switch(pr_type,
                     NULL,
           "Brushing_area",
           "Juv_spcing_area",
           c("Planting_Area", "Planting_Trees_thousands"),
           "Fertilization",
           "Survey")
  } else if (pr_type ==1) {
  var_name <- st_prep
  }
  df <- data[, c("YR", var_name)]
  df <- subset(df, YR >= year[1] & YR <= year[2])
  df
}


Out_barchart <- function(data, pr_type, year, st_prep=NULL){
  mdata <-df_maker(data, pr_type, year, st_prep)

  if (pr_type !=4) {
  mdata <- melt(mdata, id="YR")  # data manipulation for stacked barchart
  ggplot(mdata, aes( y=value, x=YR, fill=variable)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_brewer(palette = "RdYlBu", 
                      name = "Practice type",
                      labels = lab_maker(pr_type, st_prep)) +
    labs(title = paste(switch(pr_type, "Site Prepartion",
                                        "Brushing",
                                        "Juvenile Spacing",
                                        "Planting",
                                        "Fertilization",
                                        "Survery"),
                              "on Crown Land"),
         x = "Year",
         y = "Hectares")
  
  } else if (pr_type ==4){
  
  ggplot(mdata, aes(x=YR)) +
      geom_bar( aes(y=Planting_Area/1000), stat="identity", fill="#69b3a2", size=.1, color="black", alpha=.4) + 
      geom_line(aes(y=Planting_Trees_thousands/1000), size=2, color=rgb(0.2, 0.6, 0.9, 1)) +
      scale_y_continuous(name = "Planting Area (Thousand Hectares)",
                         sec.axis = sec_axis(~. , name="Planted Number of Trees (Millions)")) +
      theme_ipsum() +
      theme(
        axis.title.y = element_text(color = "#69b3a2", size=13),
        axis.title.y.right = element_text(color = rgb(0.2, 0.6, 0.9, 1), size=13)
      ) +
      labs(x="Year", size=13) +
      ggtitle("Planting Area and Numer of Planted Trees on Crown Land")
  }
}


