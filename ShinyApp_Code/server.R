
#######################################     Prerequisite     #######################################

# In order for the app to run, the data from the 'Shiny_data_preparation.R' file must be run first!!

####################################################################################################

function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    minyear <- input$Year[1]
    maxyear <- input$Year[2]
    # Subsetting for years selected
    data_filtered <- data999 %>% filter(Year >= minyear,Year <= maxyear)
    ## Subsetting for continent choice
    if (input$continent_choose != "All"){
      data_filtered <- data_filtered[data_filtered$continent == input$continent_choose,]}
    ## Subsetting for religion choice
    if (input$religion != "All") {
      data_filtered <- data_filtered[data_filtered$Main.religion == input$religion,]}
    ## Subsetting for countries choosed
    data_filtered <- data_filtered %>% filter(Country %in% input$countries_sel)
    data999b<-as.data.frame(setDT(data_filtered)[,.("Avg_suicide_rate" = round(mean(Suicide_rate),2),
                                                    "Avg_alc_consumption" = round(mean(Alcohol_consumption),2)),
                                                    by = c("Country",'continent',"Main.religion")])
  })
  
  
  wrap_12 <- wrap_format(12)
  output$plot1 <- renderPlot({
    ggplot(selectedData(), aes(Avg_alc_consumption, Avg_suicide_rate, color = Main.religion, na.rm = TRUE)) +
      geom_point(aes(shape = Main.religion, color=Main.religion), size = 4) +
      scale_shape_manual(values = c("Christianity" = 18,"Unaffiliated Religions" = 18, 
                                    "Judaism" = 18, "Islam" = 18)) +
      scale_color_manual(values = c("Christianity" = "mediumseagreen","Unaffiliated Religions" = "red3", 
                                    "Judaism" = "saddlebrown", "Islam" = "blueviolet")) +
      theme_bw()+
      {if(input$religion == "All" & input$continent_choose == "All")annotate(geom="text", x=13, y=10, label="Christianity",color="mediumseagreen", size=5, fontface="bold")}+
      {if(input$religion == "All" & input$continent_choose == "All")annotate(geom="text", x=7.5, y=24, label=wrap_12('Unaffiliated Religions'),color="red3", size=5, fontface="bold")}+
      {if(input$religion == "All" & input$continent_choose == "All")annotate(geom="text", x=2.8, y=8, label="Judaism",color="saddlebrown", size=5, fontface="bold")}+
      {if(input$religion == "All" & input$continent_choose == "All")annotate(geom="text", x=1.5, y=4, label="Islam",color="blueviolet", size=5, fontface="bold")}+
      {if (input$continent_choose != "All")theme(legend.position = "bottom",
                                                 legend.background = element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"),
                                                 legend.title = element_blank())}+
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey90", size = 0.2),
          plot.title = element_text(size=22),
          axis.title.x=element_text(size=14),axis.title.y=element_text(size=14)) +
    labs(title = "Suicides vs. Alcohol consumption",
         x = "Alcohol cunsumption (per capita)",y = "Suicide Rate (per 100K)") +
    scale_x_continuous(breaks = seq(0, 16, 2),labels = paste(seq(0, 16, 2), "L", sep = "")) 
  })
  
  
  output$selection <- renderPrint(
    input$continent_choose)
  
  output$brush_info <- renderPrint({
    brushedPoints(selectedData(), input$plot1_brush, xvar = "Avg_alc_consumption", yvar = "Avg_suicide_rate")})
  
}  # end of function
  