shinyServer(function(input, output) {
  analytics <-
    function(instrument,
             startdate,
             enddate,
             indicator) {
      
      # get the data
      reticulate::source_python("dse.py")
      
      df <-
        dse_hist(startdate,
                 enddate,
                 instrument)
      
      df <-
        mutate(df, DATE = base::as.Date(DATE, format = "%Y-%m-%d"))
      
      # filter out where HIGH, LOW or OPEN values are zero for the stock
      df <-
        filter(df,!(HIGH == 0 |
                      LOW == 0 |
                      OPENP == 0 |
                      CLOSEP == 0))  %>% distinct(DATE, .keep_all = T)
      
      # candlestick plot generation
      # Color or VOLUME bars
      barcols <- c()
      for (i in 1:length(df$DATE)) {
        x <- ifelse(df$CLOSEP[i] > df$OPENP[i], "#009E73", "#F95959")
        barcols[i] <- x
      }
      
      # Range selector
      rangeselectorlist = list(
        x = 0,
        y = 0.9,
        bgcolor = "#0099cc",
        font = list(color = "white"),
        
        buttons = list(
          list(
            count = 1,
            label = "reset",
            step = "all"
          ),
          list(
            count = 1,
            label = "1yr",
            step = "year",
            stepmode = "backward"
          ),
          list(
            count = 3,
            label = "3 mo",
            step = "month",
            stepmode = "backward"
          ),
          list(
            count = 1,
            label = "1 mo",
            step = "month",
            stepmode = "backward"
          ),
          list(step = "all")
        )
      )
      
      candleplot <- plot_ly(
        df,
        type = "candlestick",
        x = ~ DATE,
        open = ~ OPENP,
        high = ~ HIGH,
        low = ~ LOW,
        close = ~ CLOSEP,
        yaxis = "y",
        increasing = list(line = list(color = "#009E73")),
        decreasing = list(line = list(color = "#F95959")),
        name = "Price",
        height = 600,
        width = 1600
      ) %>%
        
        add_bars(
          data = df,
          x = ~ DATE,
          y = ~ VOLUME,
          marker = list(color = barcols),
          yaxis = "y2",
          inherit = F,
          name = "Vol"
        ) %>%
        
        layout(
          plot_bgcolor = "rgb(250,250,250)",
          xaxis = list(
            title = "",
            domain = c(0, 2),
            rangeslider = list(visible = F),
            rangeselector = rangeselectorlist
          ),
          yaxis = list(domain = c(0.22, 0.9), title = "PRICE"),
          yaxis2 = list(
            domain = c(0, 0.18),
            side = "left",
            title = "VOLUME"
          ),
          showlegend = F,
          
          annotations = list(
            list(
              x = 0,
              y = 1,
              xanchor = "left",
              yanchor = "top",
              xref = "paper",
              yref = "paper",
              text = paste0(instrument),
              font = list(size = 30, family = "serif"),
              showarrow = FALSE
            ),
            
            list(
              x = 0.8,
              y = 0.95,
              xanchor = "left",
              yanchor = "top",
              xref = "paper",
              yref = "paper",
              text = paste0("[", paste(range(df$DATE), collapse = " / "), "]"),
              font = list(size = 15, family = "serif"),
              showarrow = FALSE
            ),
            
            list(
              x = 0,
              y = 0.18,
              xanchor = "left",
              yanchor = "top",
              xref = "paper",
              yref = "paper",
              text = paste0(""),
              font = list(size = 10, family = "serif"),
              showarrow = FALSE
            )
          )
        )
      
      # Bollinger Bands
      # data preparation
      bbands <- data.frame(BBands(df[, "CLOSEP"]))
      bbands <- bind_cols(DATE = df$DATE, PRICE = df$CLOSEP, bbands)
      
      # plot making
      bbands_plot <-
        plot_ly(
          bbands,
          x = ~ DATE,
          y = ~ PRICE,
          type = "scatter",
          mode = "lines",
          name = "Current Price",
          width = 1000
        ) %>% add_trace(
          y = ~ up,
          name = 'Middle Band',
          line = list(dash = "dot")) %>% add_trace(
          y = ~ mavg,
          name = 'Middle Band',
          line = list(dash = "dot")) %>% add_trace(
          y = ~ dn,
          name = 'Lower Band',
          line = list(dash = "dot")) %>% layout(
          title = paste("[Bollinger Band]", instrument),
          xaxis = list(title = "Date"),
          yaxis = list (title = "Price")
        )
      
      # MACD
      # data preparation
      macd <- data.frame(MACD(df[, "CLOSEP"]))
      
      macd <- mutate(macd, Hist = macd - signal)
      macd <-
        mutate(macd, Color = ifelse(Hist >= 0, "#009E73", "#CC79A7"))
      
      macd <- bind_cols(DATE = df$DATE, macd)
      macd <- macd[34:nrow(macd), ]
      
      # plot making
      macd_plot <-
        plot_ly(
          macd,
          x = ~ DATE,
          y = ~ macd,
          type = "scatter",
          mode = "lines",
          name = "MACD",
          width = 1000
        ) %>% add_trace(
          y = ~ signal,
          name = 'MACD Signal',
          type = "scatter",
          mode = "lines"
        ) %>% add_trace(
          y = ~ Hist,
          name = 'MACD Histogram',
          type = "bar",
          marker = list(color = ~ Color),
          showlegend = F
        ) %>% layout(
          title = paste("[MACD]", instrument),
          xaxis = list(title = "Date"),
          yaxis = list (title = "Amount")
        )
      
      
      # shape the data (date and closing price) for VaR and CVaR
      df <- df[, c(1, 7)]
      R_series <- xts(df[,-1], order.by = df$DATE)
      R_series <- Return.calculate(R_series)
      R_series[is.na(R_series)] <- 0
      R_series[!is.finite(R_series)] <- 0
      
      var <- VaR(R_series,
                 method = "gaussian",
                 portfolio_method = "single")
      
      # also calcuate VaR for 5 (1 week) and 20 (1 month) days
      day <- c(5, 20)
      var_time <- c(var) * sqrt(day)
      var_time <- round(c(var, var_time), 2) * 100
      
      cvar <-
        ES(R_series,
           method = "gaussian",
           portfolio_method = "single")
      cvar_time <- c(cvar) * sqrt(day)
      cvar_time <- round(c(cvar, cvar_time), 5) * 100
      
      df_vcvar <- data.frame(Days = c(1, 5, 20),
                             VaR = var_time,
                             CVaR = cvar_time)
      
      vcvar <-
        plot_ly(
          df_vcvar,
          x =  ~ Days,
          y =  ~ VaR,
          type = "scatter",
          mode = "lines+markers",
          name = "Value at Risk",
          marker = list(color = "#009E73"),
          width = 800
        ) %>%
        add_trace(
          y =  ~ CVaR,
          name = "Conditional Value at Risk",
          #type = "scatter",
          mode = "lines+markers",
          marker = list(color = "#F95959")
        ) %>%
        layout(yaxis = list(title = "LOSS (in %)"),
               #barmode = "group",
               xaxis = list(title = "Days"))
      
      # # filter the data within the selected time
      # index <- filter(
      #   index,
      #   DATE >= base::as.Date(startdate, format = "%Y-%m-%d") &
      #     DATE <= base::as.Date(enddate, format = "%Y-%m-%d")
      # )
      #
      # # keep only the Date and closing prices
      # index <- index[, c(1, 2)]
      #
      # # turn the data into xts
      # index_series <- xts(index[,-1], order.by = index$DATE)
      #
      # # calculate the return series
      # index_series <- Return.calculate(index_series)
      # names(index_series) <- "INDEX"
      #
      # # calculate beta for the selected insturment
      # beta <- CAPM.beta(R_series, index_series)
      #
      # beta <- data.frame(Instrument = instrument, Beta = beta)
      #
      
      return(
        list(
          candleplot = candleplot,
          bbands_plot = bbands_plot,
          macd_plot = macd_plot,
          vcvar = vcvar
        )
      )
    }
  
  output$output_candleplot <- renderPlotly({
    analytics(input$instrument,
              input$startdate,
              input$enddate,
              input$indicator)$candleplot
  })
  output$output_vcvar <- renderPlotly({
    analytics(input$instrument,
              input$startdate,
              input$enddate,
              input$indicator)$vcvar
  })
  
  output$output_bbands <-
    renderPlotly({
      analytics(input$instrument,
                input$startdate,
                input$enddate,
                input$indicator)$bbands_plot
    })
  
  output$output_macd <-
    renderPlotly({
      analytics(input$instrument,
                input$startdate,
                input$enddate,
                input$indicator)$macd_plot
    })
  
  lapply(c(
    "output_candleplot",
    "output_bbands",
    "output_macd",
    "output_vcvar"
  ), function(x)
    outputOptions(output, x, suspendWhenHidden = F))
  
})
