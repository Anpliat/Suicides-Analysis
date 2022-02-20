####################################################################################################
#######################################     Prerequisite     #######################################

# In order for the app to run, the data from the 'Shiny_data_preparation.R' file must be run first!

####################################################################################################

fluidPage(
  titlePanel("Suicides"),
  fluidRow(
      column(width = 4,
             wellPanel(
                sliderInput("Year", "Select time period", min = 1980, max = 2016, value = c(2010, 2013)),   # value: einai to default pou emfanizei
             ),
             wellPanel(
               selectInput(inputId = "continent_choose",label = " Select continent(s)",
                           choices = c('All',unique(as.character(data999$continent))),selected = 'All')
             ),
             wellPanel(
               radioButtons("religion", "Main Religion",
                          choices = c('All religions' = "All",
                                      "Christianity" = 'Christianity',
                                      "Judaism" = "Judaism",
                                      "Islam" = "Islam",
                                      "Unaffiliated Religions" = "Unaffiliated Religions"),
                          selected = 'All')
            ),
            wellPanel(
              selectInput("xvar", "X-axis variable", "Avg_alc_consumption"),
              selectInput("yvar", "Y-axis variable", "Avg_suicide_rate"))
      ),

    tags$head(
      tags$style(HTML(".multicol {
                      -webkit-column-count: 4; # Chrome, Safari, Opera #
                      -moz-column-count: 4; # Firefox #
                      column-count: 4;}"))
    ),
    
     column(width = 8,
        wellPanel(
           controls <-list(h2("Select country(-ies)"), 
                           tags$div(align = 'left', class = 'multicol',style='height:220px',
                                    checkboxGroupInput(inputId  = 'countries_sel', label  = NULL,
                                                       choices  = sort(unique(as.character(data999$Country))), 
                                                      selected = sort(unique(as.character(data999$Country))),inline = FALSE))))
      ),

      column(width = 8,
             plotOutput("plot1", height=500, brush = brushOpts(id = "plot1_brush")),
             h4("Brushed points"),
             verbatimTextOutput("brush_info")
      ),
    )  # end of fluidRow
  )    # end of fluidPage


