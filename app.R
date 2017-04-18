library(ggplot2)
library(plotly)
library(reshape2)

fert_main <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', skip = 3)
life_main <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', skip = 3)
pop_main <- read.csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv', skip = 3)
meta <- read.csv('Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')

fert_main_meta <- merge(fert_main, meta, by = 'Country.Code')
fert_main_meta <- fert_main_meta[!(is.na(fert_main_meta$Region) | fert_main_meta$Region==""), ]
fert_main_meta <- subset(fert_main_meta, select = -c(Indicator.Code,X2015, X2016,X.x,IncomeGroup,SpecialNotes,TableName,X.y))
colnames(fert_main_meta)[4:58] <- seq(1960,2014,1)
fert_main_meta <- melt(fert_main_meta, id = c('Country.Code', 'Country.Name', 'Region','Indicator.Name'))
colnames(fert_main_meta)[5:6] <- c('Year', 'Fertility.Rate')
fert_main_meta <- subset(fert_main_meta, select = -c(Indicator.Name))

life_main_meta <- merge(life_main, meta, by = 'Country.Code')
life_main_meta <- life_main_meta[!(is.na(life_main_meta$Region) | life_main_meta$Region==""), ]
life_main_meta <- subset(life_main_meta, select = -c(Indicator.Code,X2015, X2016,X.x,IncomeGroup,SpecialNotes,TableName,X.y))
colnames(life_main_meta)[4:58] <- seq(1960,2014,1)
life_main_meta <- melt(life_main_meta, id = c('Country.Code', 'Country.Name', 'Region','Indicator.Name'))
colnames(life_main_meta)[5:6] <- c('Year', 'Life.Expectancy')
life_main_meta <- subset(life_main_meta, select = -c(Indicator.Name))

pop_main_meta <- merge(pop_main, meta, by = 'Country.Code')
pop_main_meta <- pop_main_meta[!(is.na(pop_main_meta$Region) | pop_main_meta$Region==""), ]
pop_main_meta <- subset(pop_main_meta, select = -c(Indicator.Code,X2015, X2016,X.x,IncomeGroup,SpecialNotes,TableName,X.y))
colnames(pop_main_meta)[4:58] <- seq(1960,2014,1)
pop_main_meta <- melt(pop_main_meta, id = c('Country.Code', 'Country.Name', 'Region','Indicator.Name'))
colnames(pop_main_meta)[5:6] <- c('Year', 'Population')
pop_main_meta <- subset(pop_main_meta, select = -c(Indicator.Name))

final_df <- merge(fert_main_meta,life_main_meta, by = c('Country.Code','Country.Name','Year','Region'))
final_df <- merge(final_df,pop_main_meta, by = c('Country.Code','Country.Name','Year','Region'))
final_df$Year <- as.numeric(as.character(final_df$Year))


ui <- fluidPage(
  headerPanel('Gapminder Plot'),
  sidebarLayout(
    sidebarPanel(
      sliderInput("pop", "Population:", 1, 8, 4, ticks = F)
    ),
    mainPanel(
      plotlyOutput("P1", width = "100%"),
      sliderInput("year", "Year:", min=1960, max=2014,
                  step=1, value = 1960, animate = animationOptions(interval=100), sep="", width = 550)
    ), position = 'right')
)

server <- function(input, output) {
  
  final<- reactive(subset(final_df, Year == input$year))
  pp <- reactive(input$pop)
  output$P1 <- renderPlotly({
    plt <- ggplot(final(),aes(y=Fertility.Rate, x=Life.Expectancy))
    plt <- plt + geom_point(aes(colour = Region, text = paste("Region: ", Region, "</br> Country Name: ", Country.Name, 
                                                              "</br> Population: ", Population, "</br> Life Expectancy: ", 
                                                              round(Life.Expectancy,3), "</br> Fertility Rate: ", round(Fertility.Rate,3)), size = Population)) 
    plt <- plt + scale_size(range = c(1,pp()),guide = 'none') + xlab('Life Expectancy') 
    plt <- plt + scale_x_continuous(breaks = seq(10,90,10), limits = c(10,90))+ scale_y_continuous(breaks = seq(0,9,1), limits = c(0,9))
    plt <- plt + ylab('Fertility Rate') + theme_bw() + theme(legend.title = element_blank())
    plt <- plt + scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d'))
    ggplotly(plt,tooltip = c("text"))
  }) 
  
}


shinyApp(ui = ui, server = server)
