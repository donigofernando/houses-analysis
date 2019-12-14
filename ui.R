header <- dashboardHeader(
    title = tags$a(href="http://algorit.ma", 
                   tags$img(height = "40px", src="logo_light_trans.png")
            ),
    tags$li(a(href = 'https://www.kaggle.com/harlfoxem/housesalesprediction',
              icon("kaggle"),
              title = "Dataset Source"),
            class = "dropdown"),
    tags$li(a(href = 'https://www.linkedin.com/in/donigofernando/',
              icon("linkedin"),
              title = "About the Author"),
            class = "dropdown")
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            text = "House Type",
            tabName = "type",
            icon = icon("home")
        ),
        menuItem(
            text = "House Location",
            tabName = "loc",
            icon = icon("map-marked-alt")
        )
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "type",
            fluidRow(
                box(
                    collapsible = T,
                    width=12,
                    selectInput(
                        inputId = "city_options",
                        label = "Select City:",
                        choices = levels(houses$city),
                        multiple = F,
                        selected = "Seattle"
                    )
                )
            ),
            fluidRow(
                tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                valueBoxOutput("tipe.terlaku"),
                valueBoxOutput("harga.terlaku"),
                valueBoxOutput("grade.terlaku")
            ),
            fluidRow(
                box(
                    title="The Best-Selling House Type", solidHeader=T,
                    collapsible = T,
                    width=12,
                    plotlyOutput("terlaku_plot", height = 300)
                )
            )
        ),
        tabItem(
            tabName = "loc",
            fluidRow(
                box(
                    title="Choose Your House Type Here!", solidHeader=T,
                    collapsible = T,
                    width = 12,
                    fluidPage(
                        column(
                            width = 9,
                            fluidRow(
                                column(
                                    width=12,
                                    sliderInput(
                                        inputId = "luasarea_options",
                                        label = "Land Area:",
                                        min = 0,
                                        max = 1700000,
                                        value = c(5000,500000)
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    width=4,
                                    numericInput(
                                        inputId = "bedroom_options",
                                        label = "Bedrooms:",
                                        min = min(houses$bedrooms),
                                        max = max(houses$bedrooms),
                                        value = 1
                                    )
                                ),
                                column(
                                    width=4,
                                    numericInput(
                                        inputId = "bathroom_options",
                                        label = "Bathrooms:",
                                        min = min(houses$bathrooms),
                                        max = max(houses$bathrooms),
                                        value = 1
                                    )
                                ),
                                column(
                                    width=4,
                                    numericInput(
                                        inputId = "floor_options",
                                        label = "Floors:",
                                        min = min(houses$floors),
                                        max = max(houses$floors),
                                        value = 1
                                    )
                                )
                            )
                        ),
                        column(
                            width = 3,
                            align = "center",
                            br(), br(), br(),
                            actionButton(
                                inputId = "search",
                                label = "Search",
                                icon("search"), 
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                            )
                        )
                    )
                )
            ),
            fluidRow(
                box(
                    title="Highest Price/SquareFeet", solidHeader=T,
                    collapsible = T,
                    width=6,
                    fluidRow(
                        column(
                            width = 8, offset = 2,
                            valueBoxOutput(width = NULL, "city.termahal")
                        )
                    ),
                    fluidRow(
                        column(
                            width = 8, offset = 2,
                            valueBoxOutput(width = NULL, "harga.termahal")
                        )
                    )
                ),
                box(
                    title="Lowest Price/SquareFeet", solidHeader=T,
                    collapsible = T,
                    width=6,
                    fluidRow(
                        column(
                            width = 8, offset = 2,
                            valueBoxOutput(width = NULL, "city.termurah")
                        )
                    ),
                    fluidRow(
                        column(
                            width = 8, offset = 2,
                            valueBoxOutput(width = NULL, "harga.termurah")
                        )
                    )
                )
            ),
            fluidRow(
                box(
                    title="Highest Price/Area", solidHeader=T,
                    collapsible = T,
                    width=6,
                    plotlyOutput("termahal_plot", height = 300)
                ),
                box(
                    title="Lowest Price/Area", solidHeader=T,
                    collapsible = T,
                    width=6,
                    plotlyOutput("termurah_plot", height = 300)
                )
            )
        )
    )
)

dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body,
    skin = "blue"
)