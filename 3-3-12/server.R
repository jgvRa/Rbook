#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
require('shiny');require('highcharter');require("plyr");require("quantmod")
# source GEX functions
source("/Volumes/6TB/R/gex_fun.R")
# Define server logic required to draw a chart
function(input, output, session) {

    output$h <- renderUI({
      # get latest GEX from files
      all_gamma <- get_all_gamma()
      XTS <- gex2xts()
      XTS <- tail(XTS, input$bars)
      
      hcGopts <- getOption("highcharter.global")
      hcGopts$useUTC <- FALSE
      options(highcharter.global = hcGopts)
      # charts GEX profile
      h1 <- highchart() %>%
        hc_add_series(data = all_gamma[all_gamma$underlyingClose <= all_gamma$flip_prc,],hcaes(x = 'underlyingClose', y = 'total'), type = 'area',color="red") %>%
        hc_add_series(data = all_gamma[all_gamma$underlyingClose >= all_gamma$flip_prc,],hcaes(x = 'underlyingClose', y = 'total'), type = 'area',color="green") %>%
        hc_title(text=paste0("$",all_gamma$und_sym[1]," GEX - For: ",format(as.Date(all_gamma$exp_date[1]),"%b %d, %Y")," Expiry"), align="center") %>%
        hc_subtitle(text=paste0("Flip Price: $",all_gamma$flip_prc[1], " | Current Price: $",all_gamma$und_prc[1])) %>%
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(formatter = JS(
          "function() {
             return this.x + ' | ' + (this.y / 1000000000).toFixed(2) + 'B'
           }"
        ))
      # charts stock and GEX levels
      h2 <- highchart(type="stock") %>%
        hc_add_series(round(XTS$min_gex,2), type="line", color="#E22F2F",name='min_gex') %>%
        hc_add_series(round(XTS$flip_prc,2), type="line", color="grey",name="flip_prc") %>%
        hc_add_series(round(XTS$max_gex,2), type="line", color="#0BE600",name='max_gex') %>%
        hc_add_series(round(XTS$und_prc,2), type="line", color="black",name=und_sym) %>%
        hc_title(text=paste0(und_sym," Vs Intraday-GEX"))
       
      # return both charts side by side
      hw_grid(h1,h2, ncol = 2)
       
    })

}
