library(shiny)
library(tidyverse)
library(rsconnect)
library(DT)

# Loading data 
ctd<- read_csv("ctdspatialclean.csv") # cleaned data sets from first phase cleaning 
intchl<- read_csv("intchlspatialclean.csv")
chl<- read_csv("chlspatialclean.csv")


# Creating spatial order
spatial_order <- c("PWS", "inner", "middle", "outer", "slope") # creating an order so in the figures it displays this way 
spatial_labels <- c("PWS" = "PWS", 
                    "inner" = "Inner shelf", # making the labels more descriptive 
                    "middle" = "Middle shelf", 
                    "outer" = "Outer shelf", 
                    "slope" = "Slope")

# Define the colors for each spatial zone
spat_colors <- c(
  "PWS" = "#8E0F28",
  "inner" = "#87BFCC",
  "middle" = "#3A8299",
  "outer" = "#1E3F66",
  "slope" = "darkgrey"
)

# UI

ui <- fluidPage(
  titlePanel("Spring Bloom Temperature and Chlorophyll Analysis by Spatial Zone"), # title 
  
  sidebarLayout( # where you will select year 
    sidebarPanel(
      selectInput("year", "Select Year", choices = unique(ctd$year), selected = 2015) # where it will slect year from and starting on 2015
    ),
    mainPanel( # outputs
      plotOutput("tempPlot"), # will give temp boxplot
      uiOutput("avgTemp"), # will say average temp
      DTOutput("chlTable"), #integrated chlor table 
      plotOutput("chlProfile"),  # chlorophyll profile
    )
  )
)

# Define server 
server <- function(input, output) {
  
  
  filtered_data <- reactive({ # filter  ctd data based on selected year
    ctd %>%
      filter(year == input$year)
  })
  
  filtered_chl_data <- reactive({ # filter int chl data on selected year 
    intchl %>%
      filter(year == input$year)
  })
  
  filtered_chl_profile_data <- reactive({ # filter chl total for profile on selected year
    chl %>%
      filter(year == input$year)
  })
  
  
  output$tempPlot <- renderPlot({ # output the CTD temperature  boxplot plot
    ggplot(filtered_data(), aes(x = spatial, y = temp, fill = spatial)) + # chosing the data 
      geom_boxplot() +
      scale_x_discrete(limits = spatial_order, labels = spatial_labels) + # creating limits of x axis 
      scale_fill_manual(values = spat_colors) + # color selection 
      labs(title = paste("Spring Bloom Temperature Distribution by Spatial Zone for", input$year), # title of figure 
           x = "Spatial Zone", y = "Temperature (°C)") + #renaming x and y 
      theme_minimal() #minimal theme
  })
  
  
  output$avgTemp <- renderUI({ # output average temperature for each spatial zone 
    avg_temp <- filtered_data() %>%
      group_by(spatial) %>%
      summarise(avg_temp = mean(temp, na.rm = TRUE)) %>% # summarising the mean of each spatial zone 
      arrange(match(spatial, spatial_order)) # how it will arrange
    
    avg_text <- paste("<b>Average Temperature for each Spatial Zone in", input$year, ":</b><br>") # this is how it will show as a list with line breaks 
    
    for (i in 1:nrow(avg_temp)) { # created a for loop through each spatial zone and then how it will paste that text and will also use the spat_colors 
      zone <- spatial_labels[avg_temp$spatial[i]]
      color <- spat_colors[avg_temp$spatial[i]]
      avg_text <- paste(avg_text, 
                        "<ul><li style='color:", color, "'>", zone, ": ", 
                        round(avg_temp$avg_temp[i], 2), "°C</li></ul>", sep = "")
    }
    
    HTML(avg_text) # how will will create that text above 
  })
  
  
  output$chlTable <- renderDT({ # integrated cholorphyll output table 
    chl_data <- filtered_chl_data() %>%
      group_by(spatial) %>%
      summarise(chl_int_total = round(mean(chl_int_total, na.rm = TRUE), 3)) %>% # summarise and only keeping 3 decimals 
      arrange(match(spatial, spatial_order)) %>%
      mutate(spatial = spatial_labels[spatial]) # how will keep the changed spatial labels 
    
    datatable(chl_data, # creating the datatable with the DT package 
              options = list(pageLength = 5, 
                             searching = FALSE, 
                             paging = FALSE), # I didnt want these dispayed
              rownames = FALSE, 
              colnames = c("Spatial Zone", 
                           "Integrated Chlorophyll a (mg m²)"))  # Column names it will output 
  })
  
  
  output$chlProfile <- renderPlot({ # output chlorophyll profile plot with geom_path
    
    filtered_chl_data <- chl %>%
      filter(year == input$year) %>% # I was having some trouble so I removed NA values and it worked
      filter(!is.na(chltot), !is.na(depth))  
    
    
    filtered_chl_data$spatial <- factor(filtered_chl_data$spatial, levels = spatial_order) # making sure in correct order 
    
    ggplot(filtered_chl_data, aes(x = chltot, y = depth, color = spatial, group = spatial)) + # plot for chl profile 
      geom_path() +  # chosing geom_path for this 
      scale_y_reverse(breaks = seq(0, 50, 10)) +  # reverse the y-axis and set breaks every 10 meters
      scale_x_continuous(breaks = seq(0, max(filtered_chl_data$chltot, na.rm = TRUE), by = 1)) +  # Set x-axis breaks for chltot
      scale_color_manual(values = spat_colors) +  # using custom colors
      labs(title = paste("Chlorophyll a Profile by Spatial Zone for", input$year),
           x = expression(Chlorophyll~alpha~(mu*g/L)),  # label for Chlorophyll (μg/L)
           y = "Depth (m)") +
      theme_bw() +  # bw theme
      facet_wrap(~ spatial, scales = "fixed", labeller = labeller(spatial = spatial_labels), ncol = 5) +  # faceting with 5 columns across
      theme(axis.title.x = element_text(size = 12), # editing some of the text sizes
            axis.title.y = element_text(size = 12), 
            strip.text = element_text(size = 12), 
            legend.position = "none", # removing legend 
            strip.placement = "outside",  # moves the strip labels outside
            strip.background = element_blank(),  #  removes strip background
            panel.spacing = unit(1, "lines")) +  #  some spacing between facets
      theme(axis.text.x.top = element_text(margin = margin(b = 5)),  # margin below x-axis text
            axis.title.x.top = element_text(size = 12, margin = margin(b = 10))) +  #  top x-axis label
      scale_x_continuous(breaks = seq(0, max(filtered_chl_data$chltot, na.rm = TRUE), by = 1), 
                         position = "top")  # axis now on top
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
