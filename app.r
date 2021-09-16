library(shiny)
library(plotrix)
library(DT)
library(rgdal)
library(mapview)
library(leaflet)
library(leafem)
library(htmlwidgets)
library(leaflet.opacity)
library(sf)
library(raster)
library(readxl)
library(ggplot2)
library(reshape)
library(hrbrthemes)

source("global.r")

## Summary data
Prov_sum <- as.data.frame(read_excel("data/YR_SUM.xlsx", sheet="Summary"))
Plant_sum <- as.data.frame(read_excel("data/YR_SUM.xlsx", sheet="Planted_trees")) # this is not used for now
max.year <- max(Prov_sum$YR, na.rm = TRUE)
min.year <- min(Prov_sum$YR, na.rm = TRUE)

## SPATIAL DATA
bcdist <- st_read("data/ADM_NR_DST_polygon.shp")
bcdist1<- st_transform(bcdist, crs = 4326)
dist.maprecord <- as.character(bcdist$DSTRCT_NM)
rgn.maprecord <- as.character(bcdist$RGN_RG_NTM)
dist.list <- sort(unique(dist.maprecord))
rgn.list <- sort(unique(rgn.maprecord))



# Define UI ----
ui <- 
  navbarPage(title = "BC Silviculture Summary Dashboard", theme = "bcgov.css", 

      tabPanel("Province", 
          fluidRow(
              column(12, 
                     h3("This is a", span("DEMO", style = "color:red"), "for testing purposes.")),
              column(3, wellPanel(
                            selectInput(inputId="pr_type", label="Select type of practice",
                                        choices = c("Site Preperation" =1, 
                                                    "Brushing" =2, 
                                                    "Juvenile Spacing" =3,
                                                    "Planting" =4,
                                                    "Fertilization" =5,
                                                    "Survey" =6),
                                        selected = 1),
                            conditionalPanel(condition = "input.pr_type==1",
                                  selectInput(inputId="st_prep",label="Type of site preperation",
                                              choices = c("Burn","Mechanical", "Other"),
                                              multiple = TRUE,
                                              selected = c("Burn","Mechanical", "Other") )),
                    sliderInput("showyr", "Years of interest", min = min.year, max = max.year, value = c(max.year-12, max.year) ),
                    hr(),
                    "The statistics are based on" , 
                    tags$a(href="https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/silviculture/silviculture-statistics", "Silviculture Statistics"),
                    "of the Government of British Columbia.",
                    "For further detailed information, please refer to the",
                    tags$a(href="https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/silviculture/silviculture-reporting-results", "Reporting Silviculture Updates and Land Status Tracking System (RESULTS)"),
                    "application or",
                    tags$a(href="https://www2.gov.bc.ca/gov/content/governments/organizational-structure/ministries-organizations/ministries/forests-lands-natural-resource-operations-and-rural-development", "Forest Management System (FMS)"),
                    "(link will be updated once it is released)."
              )),
  
              column(6,
                     
                  uiOutput("barchart_pr"),
            ),
              column(3,
               
                  DTOutput("out_dt"),
                     
              ),
              column(width = 12,
                   style = "background-color:#003366; border-top:2px solid #fcba19;",
                   
                   tags$footer(class="footer",
                               tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                        tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                        )
                               )
                   )
            )
 
  )),
 



             tabPanel("District/Region",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Choose a Natural Resource District or Region to show it on the map"),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          selectInput("showdist", 
                                      label = "Choose a Natural Resource District",
                                      choices = as.list(c("none", dist.list)),
                                      selected = "none"),
                          
                          selectInput("showrgn", 
                                      label = "Choose a Natural Resource Region",
                                      choices = as.list(c("none", rgn.list)),
                                      selected = "none"),
                          
                        ),    
                        
                        mainPanel(
                          
                          leafletOutput(outputId = "NRDmap", height="86vh")
                          
                        )
                      ),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             )
  )


# Define server logic ----
server <- function(input, output, session) {
  
  output$barchart_pr <- renderUI({
    
    output$plot_temp <- renderPlot({
      par(mar=c(5.1,.5,4.1,2.1)) 
      Out_barchart(Prov_sum, as.numeric(input$pr_type), as.numeric(input$showyr), input$st_prep)
    })
    plotOutput("plot_temp", height="700px", width = "100%")
  })
  
  output$out_dt <- renderDT({
   
    temp_dt <- df_maker(Prov_sum, as.numeric(input$pr_type), as.numeric(input$showyr), input$st_prep)
    temp_lab <- lab_maker(as.numeric(input$pr_type), input$st_prep)
    
    dt.header = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          lapply(c("Year", temp_lab), th)
        )  
      )
    )
    )
    datatable(temp_dt,
              container =  dt.header , rownames = FALSE, 
              options = list(paging = FALSE, ordering=F, sDom  = '<"top">rt<"bottom">',
                             columnDefs=list(list(targets= '_all', class="dt-right"))),
              caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: left;',
                htmltools::tags$p('Unit: ', htmltools::em('Area = hectare, Planting Trees = 1,000 trees'))
              )              
              ) 
    
    
  })

  output$NRDmap <- renderLeaflet({
    
    showdist <- input$showdist
    showrgn <- input$showrgn
    
    leaflet() %>% 
      addTiles() %>% 
      addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
      addProviderTiles("Esri.WorldTerrain", group = "Terrain only") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>%
      fitBounds(lng1 = -139.06, lat1 = 48.30, lng2 = -114.03, lat2 = 60.00) %>%
      addLayersControl(
        baseGroups = c("Base map", "Terrain only", "Satellite view"),
        options = layersControlOptions(collapsed = FALSE),
      ) %>%
      addPolygons(data=bcdist1[dist.maprecord == showdist,], fillColor = "red", color="red", smoothFactor = 0.2, fillOpacity = 0.4, weight=2, opacity=1)%>%
      addPolygons(data=bcdist1[rgn.maprecord == showrgn,], fillColor = "black", color="black", smoothFactor = 0.2, fillOpacity = 0.4, weight=2, opacity=1) 
    
  },
  )
  

  
}

# Run the app ----
shinyApp(ui = ui, server = server)