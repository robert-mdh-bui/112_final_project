library(shiny)
library(tidyverse)
library(babynames)
library(fst)

fixedflights <- read_fst("/Users/alexmarketos/Desktop/data18-fixed.fst")

fixedflightsdays <- fixedflights %>%
  separate(FL_DATE, into = c("year", "month", "day"), sep = "-") %>%
  filter(ORIGIN_CITY_NAME == "Minneapolis, MN") %>%
  group_by(month, day) %>%
  summarize(avgdelay = mean(DEP_DELAY, na.rm=T)) %>%
  ungroup()

ui <- fluidPage(
  selectInput("month", "Month", choices = list(January = "1", February = "2",
                                               March = "3", April = "4", May = "5",
                                               June = "6", July = "7", August = "8",
                                               September = "9", October = "10",
                                               November = "11", December = "12")),
  selectInput("day", "Day", choices = list(`1`="1", `2`="2",`3`="3",`4`="4",`5`="5",
                                           `6`="6",
                                           `7`="7",`8`="8",`9`="9",`10`="10",`11`="11",
                                           `12`="12",`13`="13",`14`="14",`15`="15",
                                           `16`="16",
                                           `17`="17",`18`= "18",`19`="19",`20`="20",
                                           `21`="21",
                                           `22`="22",`23`="23",`24`="24",`25`="25",
                                           `26`="26",
                                           `27`="27",`28`="28",`29`="29",`30`="30",
                                           `31`= "31")),
  submitButton(text = "What's My Average Delay?"),
  tableOutput(outputId = "Delay")
)

server <- function(input, output) {
  output$Delay <- renderTable({
    fixedflightsdays %>% 
      filter(month == input$month, day == input$day) %>%
      select(avgdelay)
  })
}

shinyApp(ui = ui, server = server)