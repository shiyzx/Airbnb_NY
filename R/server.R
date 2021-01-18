shinyServer(function(input, output,session){

  #map
  observe({
    selected1=unique(airbnb[airbnb$neighbourhood_group==input$selected0,'neighbourhood'])
    updateSelectizeInput(
      session,'selected1',
      choices = selected1,
      selected = selected1[1]
    )
  })
  
  
  listingairbnb <- reactive({
    selected0   = input$selected0
    selected1   = input$selected1
    selected2   = input$selected2
    pricerange  = input$pricerange
    nightrange  = input$nightrange
    reviewrange = input$reviewrange
    scorerange  = input$reviewscore
    airbnb %>%
      select(neighbourhood_group, neighbourhood,room_type, longitude,latitude,price,minimum_nights, reviews_per_month,review_scores_rating) %>%
      filter(neighbourhood_group == selected0 
             & neighbourhood == selected1 
             & room_type == selected2
             & minimum_nights == nightrange
             & (price<pricerange[2]&price>pricerange[1])
             & (reviews_per_month<reviewrange[2]&reviews_per_month>reviewrange[1])
             & (review_scores_rating<scorerange[2]&review_scores_rating>scorerange[1])
             )
  })  
  
  output$map <- renderLeaflet({
    leaflet()%>% setView(lng = -73.9059, lat = 40.7128, zoom = 11) %>% 
      addTiles() %>% addMarkers(lng = listingairbnb()$longitude, lat = listingairbnb()$latitude)
  })
  

  rstable<-reactive({

    airbnb %>%
      filter(neighbourhood_group == input$selected0
             & neighbourhood == input$selected1
             & room_type == input$selected2
             & minimum_nights == input$nightrange
             & (price<input$pricerange[2]&price>input$pricerange[1])
             & (reviews_per_month<input$reviewrange[2]&reviews_per_month>input$reviewrange[1])
             & (review_scores_rating<input$reviewscore[2]&review_scores_rating>input$reviewscore[1])
      ) %>%
      select(id,name,price,reviews_per_month,review_scores_rating)
  })


  output$roomtable<-DT::renderDataTable({
    datatable(
      rstable(),
      extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = 
          list('copy', 'print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          ))
        
      )
    )
    
  })
  
  # sentiment plot
  
  # rangessentimentplot <- reactiveValues(x = NULL, y = NULL)
  
  output$sentimentplot<-renderPlot({
    
    rstable()%>%
      select(id)->roomtable
    roomtable
    
    airbnbroom %>% 
      right_join(roomtable,"id") %>% 
      select(id,linenumber,word) %>% 
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(word, sentiment, id) %>%
      ungroup() %>% 
      group_by(sentiment,id) %>% 
      slice_max(order_by = n, n=5) %>% 
      ungroup() %>%
      mutate(word = reorder_within(word, n,id)) %>% 
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(sentiment~id, scales = "free") +
      scale_x_reordered() +
      scale_y_continuous(expand = c(0,0)) +
      labs(y = "Contribution to sentiment", x = NULL) +
      coord_flip()
    
    
  })
  
  # observeEvent(input$sentimentplot_dblclick, {
  #   brush <- input$sentimentplot_brush
  #   if (!is.null(brush)) {
  #     rangessentimentplot$x <- c(brush$xmin, brush$xmax)
  #     rangessentimentplot$y <- c(brush$ymin, brush$ymax)
  #     
  #   } else {
  #     rangessentimentplot$x <- NULL
  #     rangessentimentplot$y <- NULL
  #   }
  # })
  
  
  
  
  # plot
  rangesplot1 <- reactiveValues(x = NULL, y = NULL)
  
  listingairbnbplot <- airbnb %>%
    select(neighbourhood_group, neighbourhood, room_type, price, minimum_nights)

  output$plot1 <- renderPlot({
    if(input$room_price=="room1"){
    listingairbnbplot %>% 
      filter(minimum_nights == input$selected4
             &price >= input$selected3[1] & price <= input$selected3[2]) %>% 
      ggplot(aes(x=neighbourhood_group,fill=room_type))+
      geom_bar(position = position_dodge())+
        theme_bw()+
        coord_cartesian(xlim = rangesplot1$x, ylim = rangesplot1$y, expand = FALSE)
    }
    
    else{
      listingairbnbplot %>% 
        filter(minimum_nights == input$selected5
               &neighbourhood_group == input$selected7) %>% 
        ggplot(aes(x=neighbourhood, y=price,fill=room_type))+
        geom_bar(position = position_dodge(),stat = "identity") +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
        theme_bw()+
        coord_cartesian(xlim = rangesplot1$x, ylim = rangesplot1$y, expand = FALSE)
    }
    })
  #observe plot1
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      rangesplot1$x <- c(brush$xmin, brush$xmax)
      rangesplot1$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rangesplot1$x <- NULL
      rangesplot1$y <- NULL
    }
  })
  

  #summaryname
  
  output$summaryname<-renderPrint({
    
    if(input$room_price=="price1"){
      airbnb %>% 
        filter(neighbourhood_group==input$selected7) %>% 
        select(neighbourhood) %>% 
        distinct()->a
      a[[1]]
    }
    
  })
  
  # 1v plot
  rangesplot2.1 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2.1<-renderPlot({
    
    if(input$vartype=="numeric"){
      plot_type<-switch(input$plot.type2.1,
                        "Histogram"         =	geom_histogram(bins = input$bin2.1, color="white"),
                        "Density Plot"      =	geom_density(outline.type = "full"),
                        "Frequency Polygon" =	geom_freqpoly(bins=30)
      )
      
      ggplot(p2_num,aes(x=!!input$var2.1_n))+
        plot_type+
        theme_bw()+
        coord_cartesian(xlim = rangesplot2.1$x, ylim = rangesplot2.1$y, expand = FALSE)
    }
    else {
      p2_fct %>% 
        group_by(!!input$var2.1_f) %>% 
        summarise(n=n()) %>% 
        ggplot(aes(x="",y=n,fill=!!input$var2.1_f))+
        geom_bar(width = 1, stat = "identity")+
        coord_polar("y", start=0)+
        theme_bw()
        
    }
  })
  
  observeEvent(input$plot2.1_dblclick, {
    brush <- input$plot2.1_brush
    if (!is.null(brush)) {
      rangesplot2.1$x <- c(brush$xmin, brush$xmax)
      rangesplot2.1$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rangesplot2.1$x <- NULL
      rangesplot2.1$y <- NULL
    }
    
  })
  

  
  output$summary2.1_n<-renderPrint({
    if(input$vartype=="numeric"){
    p2_num %>% 
      select(!!input$var2.1_n) %>% 
      summary()
    }
    
  })
  
  output$summary2.1_f<-renderPrint({
    
    if(input$vartype=="factor"){
      p2_fct %>% 
        select(!!input$var2.1_f) %>% 
        summary()
    }
    
  })
  
  
  
  #page2 1v test
  output$static<-renderTable({
    if(input$vartype=="numeric"&input$ttest2==T){
      t.test(p2_num[[input$var2.1_n]],mu=input$num2.1.1,conf.level = (1-input$num2.1.2))%>%
        tidy() %>%
        select(`P-value` = p.value, Estimate =estimate,`T-statistic`=statistic
               , 'Confidence Lower' = conf.low, 'Confidence Higher' = conf.high)
    }
    
    
  })
  
  #page2 2v plot
  rangesplot2.2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2.2<-renderPlot({
    logx_type<-switch(input$logx,
                      "0"     = scale_x_log10(),
                      "1"     =	NULL
    )
    logy_type<-switch(input$logy,
                      "0"     = scale_y_log10(),
                      "1"     =	NULL
    )
    if(is.double(airbnb_p2[[input$varx2.2]])+is.double(airbnb_p2[[input$vary2.2]])==2){
      ols_type<-switch(input$ols,
                       "0"     =	geom_smooth(method = "lm",se=F),
                       "1"     =	NULL
      )
      ggplot(airbnb_p2,aes(x=!!input$varx2.2,y=!!input$vary2.2))+
        geom_point()+
        ols_type+
        logx_type+
        logy_type+
        theme_bw()+
        coord_cartesian(xlim = rangesplot2.2$x, ylim = rangesplot2.2$y, expand = FALSE)
    }
    else if(is.double(airbnb_p2[[input$varx2.2]])==1&is.double(airbnb_p2[[input$vary2.2]])==0){
      validate(need(input$logy==F, 'the y variable is factor, cannot transform '),
               need(input$ols==F,  'cannot add a ols on factor variables'))
      ggplot(airbnb_p2,aes(x=!!input$varx2.2,y=!!input$vary2.2))+
        geom_boxplot()+
        logx_type+
        theme_bw()+
        coord_cartesian(xlim = rangesplot2.2$x, ylim = rangesplot2.2$y, expand = FALSE)
    }
    else if(is.double(airbnb_p2[[input$varx2.2]])==0&is.double(airbnb_p2[[input$vary2.2]])==1){
      validate(need(input$logx==F, 'the x variable is factor, cannot transform '),
               need(input$ols==F,  'cannot add a ols on factor variables'))
      ggplot(airbnb_p2,aes(x=!!input$varx2.2,y=!!input$vary2.2))+
        geom_boxplot()+
        logy_type+
        theme_bw()+
        coord_cartesian(xlim = rangesplot2.2$x, ylim = rangesplot2.2$y, expand = FALSE)
    }
    else if(is.double(airbnb_p2[[input$varx2.2]])+is.double(airbnb_p2[[input$vary2.2]])==0){
      validate(need(input$logx==F, 'the x variable is factor, cannot transform '),
               need(input$logy==F, 'the y variable is factor, cannot transform '),
               need(input$ols==F,  'cannot add a ols on factor variables'))
      ggplot(airbnb_p2,aes(x=!!input$varx2.2,y=!!input$vary2.2))+
        geom_jitter()+
        theme_bw()+
        coord_cartesian(xlim = rangesplot2.2$x, ylim = rangesplot2.2$y, expand = FALSE)
    }
  })
  
  observeEvent(input$plot2.2_dblclick, {
    brush <- input$plot2.2_brush
    if (!is.null(brush)) {
      rangesplot2.2$x <- c(brush$xmin, brush$xmax)
      rangesplot2.2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rangesplot2.2$x <- NULL
      rangesplot2.2$y <- NULL
    }
    
  })
  
  
  #page2 2v test
  
  output$code2.2<-renderPrint({
    
    if(input$test.type2.2=="s"){
      validate(need(is.double(airbnb_p2[[input$varx2.2]])+is.double(airbnb_p2[[input$vary2.2]])==2
                    ,'need numeric variables to summary'))
      if(input$logx==1&input$logy==1){
        lm(log(airbnb_p2[[input$vary2.2]])~log(airbnb_p2[[input$varx2.2]]), airbnb_p2) %>% 
          summary()
      }
      else if(input$logx==0&input$logy==1){
        lm(log(airbnb_p2[[input$vary2.2]])~airbnb_p2[[input$varx2.2]], airbnb_p2) %>% 
          summary()
      }
      else if(input$logx==1&input$logy==0){
        lm(airbnb_p2[[input$vary2.2]]~log(airbnb_p2[[input$varx2.2]]), airbnb_p2) %>% 
          summary()
      }
      else if(input$logx==0&input$logy==0){
        lm(airbnb_p2[[input$vary2.2]]~airbnb_p2[[input$varx2.2]], airbnb_p2) %>% 
          summary()
      }
      
    }#end summary
    
    else if(input$test.type2.2=="c"){
      validate(need(is.double(airbnb_p2[[input$varx2.2]])+is.double(airbnb_p2[[input$vary2.2]])==0
                    ,'need factor variables to chisq-test'))
      
      airbnb_p2 %>% 
        select(input$varx2.2,input$vary2.2)->a
      chisq.test(table(a))
    }#end chi-test
    
    else if(input$test.type2.2=="a"){
      validate(need(is.double(airbnb_p2[[input$varx2.2]])+is.double(airbnb_p2[[input$vary2.2]])==1
                    ,'need factor or numeric variables to ANOVA'))
      if(is.double(airbnb_p2[[input$varx2.2]])==1){
        airbnb.aov <- aov(airbnb_p2[[input$varx2.2]] ~ airbnb_p2[[input$vary2.2]], data = airbnb_p2)
        summary(airbnb.aov)
        
      }
      else if(is.double(airbnb_p2[[input$vary2.2]])==1){
        airbnb.aov <- aov(airbnb_p2[[input$vary2.2]] ~ airbnb_p2[[input$varx2.2]], data = airbnb_p2)
        summary(airbnb.aov)
        
      }
      
    }# end anova test
    
  })
  
  #code2.1
  
  output$code2.1<-renderPrint({
    
    airbnb_p2 %>% 
      select(input$varx2.2,input$vary2.2) %>% 
      summary()
    
  })
  

  #page2 2v chi-test
  output$static2<-renderTable({
    
    if(input$test.type2.2=="a"){
      if(is.double(airbnb_p2[[input$varx2.2]])==1
         &is.double(airbnb_p2[[input$vary2.2]])==0){
        airbnb.aov<-aov(airbnb_p2[[input$varx2.2]] ~ airbnb_p2[[input$vary2.2]]
                        , data = airbnb_p2)
        data.frame(TukeyHSD(airbnb.aov)[[1]]) 
      }
    }
    
    else if(input$test.type2.2=="c"){
      validate(need(is.double(airbnb_p2[[input$varx2.2]])+is.double(airbnb_p2[[input$vary2.2]])==0
                    ,'need factor variables to chisq-test'))
      airbnb_p2 %>% 
        select(!!input$varx2.2,!!input$vary2.2)->a
      chisq.test(table(a)) %>% 
        tidy() 
      
    }
    
    
  })
  
  
  #plot 2.3
  
  output$plot2.3<-renderPlot({
    
    logx_type2.3<-switch(input$logx2.3,
                         "0"     = scale_x_log10(),
                         "1"     =	NULL
    )
    change_type<-switch(input$change2.3,
                        "0"     = coord_flip(),
                        "1"     =	NULL
    )
    if(is.double(airbnb_p2[[input$varx2.3]])){
      ggplot(airbnb_p2,aes(x=!!input$varx2.3,y=!!input$vary2.3))+
        geom_boxplot()+
        facet_wrap(~p2_fct[[input$varz2.3]])+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
        theme(axis.text.y = element_text(angle = -30, hjust = 1, vjust = 1))+
        logx_type2.3+
        change_type+
        theme_bw()
      
    }
    else if(!is.double(airbnb_p2[[input$varx2.3]])){
      validate(need(input$logx2.3==0,'variable is not numeric, cannot log transform'))
      ggplot(airbnb_p2,aes(x=!!input$varx2.3,y=!!input$vary2.3))+
        geom_jitter()+
        facet_wrap(~p2_fct[[input$varz2.3]])+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
        theme(axis.text.y = element_text(angle = -30, hjust = 1, vjust = 1))+
        change_type+
        theme_bw()
        
    }
    
  })
  
  
  #code3
  output$code3<-renderPrint({
    airbnb_p2 %>% 
      select(input$varx2.3,input$vary2.3,input$varz2.3) %>% 
      summary()
  })
  
  
  #datasheets
  output$selecttable <- DT::renderDataTable({
    
    datatable(
      rstable(),
      extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = 
          list('copy', 'print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          ))
        
      )
    )
  })
  
  output$table <- DT::renderDataTable({
    datatable(airbnb_p2, rownames=FALSE)
 
  })
  
  output$othertable <- DT::renderDataTable({
    datatable(airbnb_other, rownames=FALSE)
    
  })
  


  
})