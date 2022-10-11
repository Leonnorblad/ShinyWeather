# Setup ####
library(shiny)
library(Weather4cats)

# Function to render plot ####
render_plot <- function(city, date, W_variable){
  
  # API data
  data <- get_weather_forecast(city = city, date=date)
  
  # Transform date object
  date <- as.POSIXlt(gsub("T", " ", data$Time))
  
  # Time in plot = hour
  data["time"] <- strftime(date, format="%H")
  
  # Get the date of the day
  date_of_day <- strftime(date, format="%Y-%m-%d")
  
  # Construct the title 
  title <- paste0(W_variable, ", ", data$City[1])
  
  # construct the subtitle
  subtitle <- paste0(lubridate::day(date_of_day)," ", stringr::str_to_title(lubridate::month(date_of_day, label = TRUE, abbr=FALSE), ","), " ", lubridate::year(date_of_day))[1]
  
  # Line plots
  if (any(c("Temperature"==W_variable,
            "Humidity"==W_variable,
            "Cloudcover"==W_variable))){
    plot <- ggplot2::ggplot(data=data, ggplot2::aes_string(x="time", y=W_variable, group=1)) + ggplot2::geom_line() + ggplot2::geom_point()
  }
  
  # Bar plot
  if(W_variable=="Precipitation"){
    plot <- ggplot2::ggplot(data=data, ggplot2::aes_string(x="time", y=W_variable, group = 1)) +
      ggplot2::geom_bar(stat='identity') +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::labs(y="Precipitation (mm)")
  }
  # Vector plot
  if(W_variable=="Wind speed"){
    plot <- ggplot2::ggplot(data=data, ggplot2::aes(x=time, y=Wind_speed)) +
      ggplot2::geom_text(ggplot2::aes(angle=-Wind_direction+90), label="-->", size=6) +
      ggplot2::geom_point(size=2) + ggplot2::labs(y="Wind speed (m/s)")
  }
  
  # Change y-labs
  if(W_variable=="Temperature"){
    plot <- plot + ggplot2::labs(y="Temperature (\u00b0 C)")
  }
  
  if(W_variable=="Humidity"){
    plot <- plot + ggplot2::labs(y="Humidity (%)")
  }
  
  if(W_variable=="Cloudcover"){
    plot <- plot + ggplot2::labs(y="Cloudcover (%)")
  }
  
  # Add theme (to all plots)
  plot <- plot +
    ggplot2::labs(title = title,
         x = "Hour",
         subtitle = subtitle) + ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(face="bold", hjust=0.5, size = 16),
          plot.subtitle = ggplot2::element_text(hjust=0.5),
          axis.title = ggplot2::element_text(size=14),
          axis.text = ggplot2::element_text(size=12))
  
  # Change the scale for the variables in % from 0 to 100
  if(any("Humidity"==W_variable, "Cloudcover"==W_variable)){
    plot <- plot + ggplot2::scale_y_continuous(limits=c(0,100))
  }
  return(plot)
}

# The app ####
ui <- fluidPage(
  selectInput("area", label = "Area", choices = c("Link\u00f6ping", "Stockholm", "Uppsala", "Malm\u00f6", "G\u00f6teborg")),
  selectInput("var", label = "Variable", choices = c("Temperature", "Humidity", "Cloudcover", "Precipitation", "Wind speed")),
  dateInput("date", label="Date", value=Sys.Date(), min=Sys.Date(), max=Sys.Date()+7),
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({render_plot(city = input$area, date = input$date, W_variable = input$var)
  })
}

shinyApp(ui, server)
