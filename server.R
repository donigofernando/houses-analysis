function(input,output){
    city_react <- reactive({
        city_options <- input$city_options
        data_output <- houses %>%
            filter(city %in% city_options) %>%
            group_by(bedrooms,bathrooms,floors) %>%
            summarise(jumlah=n(), mean.price = round(mean(price), 0), mean.grade = round(mean(grade), 0)) %>%
            ungroup() %>%
            arrange(desc(jumlah)) %>% 
            head(5) %>% 
            mutate(text = paste("Amount of House Sold: ", jumlah, "\nHouse Type:", "\n", bedrooms, " Bedrooms\n", bathrooms, " Bathrooms\n", floors, " Floors"))
        data_output <- cbind(No. = 1:5, data_output)
        data_output$No. <- sub("^", "Type ", data_output$No.)
        
        return(data_output)
    })

    output$terlaku_plot <- renderPlotly({
        data_input <- city_react()
        plot_city <- ggplot(data_input, aes(x=No., y=jumlah, text = text)) +
            geom_col() +
            labs(x="House Type", y="Amount of House Sold") +
            theme_minimal()
            theme(legend.position = "none")
        ggplotly(plot_city, tooltip = "text")
    })
    
    vbox1_react <- reactive({
        city_options <- input$city_options
        data_output <- houses %>%
            filter(city == city_options) %>%
            group_by(bedrooms,bathrooms,floors) %>%
            summarise(jumlah=n(), mean.price = round(mean(price), 0), mean.grade = round(mean(grade), 0)) %>%
            ungroup() %>%
            arrange(desc(jumlah)) %>%
            head(1)

        return(data_output)
    })

    output$tipe.terlaku <- renderValueBox({
        data_input <- vbox1_react()
        valueBox(
            value = tags$p(HTML(paste(data_input$bedrooms, " Bedrooms<br/>", data_input$bathrooms, " Bathrooms<br/>", data_input$floors, " Floors")), style = "font-size: auto;"),
            subtitle = "Best-Selling House Type",
            icon = icon("award"),
            width = 4
        )
    })

    output$harga.terlaku <- renderValueBox({
        data_input <- vbox1_react()
        valueBox(
            value = paste("$", data_input$mean.price),
            subtitle = "Average Price",
            icon = icon("dollar-sign"),
            width = 4
        )
    })

    output$grade.terlaku <- renderValueBox({
        data_input <- vbox1_react()
        valueBox(
            value = paste(data_input$mean.grade, " of 10"),
            subtitle = "Average Grade",
            icon = icon("thumbs-up"),
            width = 4
        )
    })
    
    v <- reactiveValues(do = F)
    
    observeEvent(input$search,{
        v$do <- input$search
    })
    
    termahal_react <- reactive({
        if (v$do == F) return()
        
        isolate({
            luasarea_options <- input$luasarea_options
            bedroom_options <- input$bedroom_options
            bathroom_options <- input$bathroom_options
            floor_options <- input$floor_options
            data_output <- houses %>%
                filter(sqft_lot >= luasarea_options[1], sqft_lot <= luasarea_options[2]) %>%
                filter(bedrooms == bedroom_options) %>%
                filter(bathrooms == bathroom_options) %>%
                filter(floors == floor_options) %>%
                mutate(price.per.area = price/sqft_lot) %>% 
                group_by(city) %>% 
                summarise(mean.price.per.area = round(mean(price.per.area), 0)) %>% 
                ungroup() %>% 
                arrange(desc(mean.price.per.area)) %>% 
                head(5) %>% 
                mutate(text = paste("Price/Area:", mean.price.per.area))
            
            return(data_output)
        })
    })
    
    output$termahal_plot <- renderPlotly({
        data_input <- termahal_react()
        plot_termahal <- ggplot(data_input, aes(x=reorder(city, mean.price.per.area), y=mean.price.per.area, text = text)) +
            geom_col() +
            labs(x=NULL, y=NULL) +
            theme_minimal() +
            theme(legend.position = "none")
        ggplotly(plot_termahal, tooltip = "text")
    })
    
    termahal5_react <- reactive({
        if (v$do == F) return()
        
        isolate({
            luasarea_options <- input$luasarea_options
            bedroom_options <- input$bedroom_options
            bathroom_options <- input$bathroom_options
            floor_options <- input$floor_options
            data_output <- houses %>%
                filter(sqft_lot >= luasarea_options[1], sqft_lot <= luasarea_options[2]) %>%
                filter(bedrooms == bedroom_options) %>%
                filter(bathrooms == bathroom_options) %>%
                filter(floors == floor_options) %>%
                mutate(price.per.area = price/sqft_lot) %>% 
                group_by(city) %>% 
                summarise(mean.price.per.area = round(mean(price.per.area), 0)) %>% 
                ungroup() %>% 
                arrange(desc(mean.price.per.area)) %>% 
                head(1)
            
            return(data_output)
        })
    })
    
    output$city.termahal <- renderValueBox({
        data_input <- termahal5_react()
        valueBox(
            value = data_input$city,
            subtitle = "City",
            icon = icon("map-marked-alt"),
            color = "green"
        )
    })
    
    output$harga.termahal <- renderValueBox({
        data_input <- termahal5_react()
        valueBox(
            value = paste("$", data_input$mean.price.per.area),
            subtitle = "Average Price/Area",
            icon = icon("dollar-sign"),
            color = "green"
        )
    })
    
    termurah_react <- reactive({
        if (v$do == F) return()
        
        isolate({
            luasarea_options <- input$luasarea_options
            bedroom_options <- input$bedroom_options
            bathroom_options <- input$bathroom_options
            floor_options <- input$floor_options
            data_output <- houses %>%
                filter(sqft_lot >= luasarea_options[1], sqft_lot <= luasarea_options[2]) %>%
                filter(bedrooms == bedroom_options) %>%
                filter(bathrooms == bathroom_options) %>%
                filter(floors == floor_options) %>%
                mutate(price.per.area = price/sqft_lot) %>% 
                group_by(city) %>% 
                summarise(mean.price.per.area = round(mean(price.per.area), 0)) %>% 
                ungroup() %>% 
                arrange(mean.price.per.area) %>% 
                head(5) %>% 
                mutate(text = paste("Price/Area:", mean.price.per.area))
            
            return(data_output)
        })
    })
    
    output$termurah_plot <- renderPlotly({
        data_input <- termurah_react()
        plot_termurah <- ggplot(data_input, aes(x=reorder(city, mean.price.per.area), y=mean.price.per.area, text = text)) +
            geom_col() +
            labs(x=NULL, y=NULL) +
            theme_minimal() +
            theme(legend.position = "none")
        ggplotly(plot_termurah, tooltip = "text")
    })
    
    termurah5_react <- reactive({
        if (v$do == F) return()
        
        isolate({
            luasarea_options <- input$luasarea_options
            bedroom_options <- input$bedroom_options
            bathroom_options <- input$bathroom_options
            floor_options <- input$floor_options
            data_output <- houses %>%
                filter(sqft_lot >= luasarea_options[1], sqft_lot <= luasarea_options[2]) %>%
                filter(bedrooms == bedroom_options) %>%
                filter(bathrooms == bathroom_options) %>%
                filter(floors == floor_options) %>%
                mutate(price.per.area = price/sqft_lot) %>% 
                group_by(city) %>% 
                summarise(mean.price.per.area = round(mean(price.per.area), 0)) %>% 
                ungroup() %>% 
                arrange(mean.price.per.area) %>% 
                head(1)
            
            return(data_output)
        })
    })
    
    output$city.termurah <- renderValueBox({
        data_input <- termurah5_react()
        valueBox(
            value = data_input$city,
            subtitle = "City",
            icon = icon("map-marked-alt"),
            color = "red"
        )
    })
    
    output$harga.termurah <- renderValueBox({
        data_input <- termurah5_react()
        valueBox(
            value = paste("$", data_input$mean.price.per.area),
            subtitle = "Average Price/Area",
            icon = icon("dollar-sign"),
            color = "red"
        )
    })
}