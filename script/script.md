This file contains the data analysis as requested. I also developed a
Dashboard in order to better display and interact with Outvio’s
databases.

## Dashboard Link

### Technical Assessment

Regarding the data analysis, the first step is to load the required
packages.

    # Loading Packages

    library('tidyverse') # Package for data manipulation
    library('lubridate') # Manipulate date variables 
    library('plotly') # Package for data visualization
    library('DT') # Render some nice HTML tables

After that, it is time to read and import the data. After import
databases using R, it was necessary to fix some variables types and
create useful features.

    # Reading data ----

    # Packages - Reading CSV data

    packages <- read.csv('data/packages.csv',  na.strings=c("NA","NaN", ""))

    # Products - Reading CSV data

    products <- read.csv('data/products.csv',  na.strings=c("NA","NaN", ""))

    # Shipments - Read CSV data and fix date formats

    shipments <- read.csv('data/shipments.csv', na.strings=c("NA","NaN", "")) %>%
      mutate(createdAt = lubridate::date(createdAt),
             deliverDate = lubridate::date(deliverDate),
             estimatedDeliverDate = lubridate::date(estimatedDeliverDate),
             pickupDate = lubridate::date(pickupDate),
             processDate = lubridate::date(processDate),
             deliveryTime = as.numeric(difftime(deliverDate, createdAt, units = "days")), # Create DeliveryTime as the difference in days between createdAt and deliverDate
             predictedDiff = as.numeric(difftime(estimatedDeliverDate, deliverDate, units = "days")), # Time difference between predicted and delivered dates
             delayed = ifelse(deliverDate > estimatedDeliverDate, 'Delayed', 'On Time')) # Create a variable that classifies delivered orders into Delayed or On Time

------------------------------------------------------------------------

## Minimum requirements

This section presents the resolution of the minimum requirements of this
task.

### 1. Average delivery time per courier

DeuschePost presents the highest deliveryTime, followed by transaher and
fedex.

    shipments %>% 
      filter(!is.na(deliverDate)) %>% # Remove not delivered orders
      group_by(courier) %>%
      summarise(mean = mean(deliveryTime)) %>%
      arrange(desc(mean))

    ## # A tibble: 33 x 2
    ##    courier        mean
    ##    <chr>         <dbl>
    ##  1 deutschePost  12   
    ##  2 transaher     11   
    ##  3 fedex          9.44
    ##  4 dpd            9.13
    ##  5 zeleris        6.33
    ##  6 schenker       5.5 
    ##  7 spring         5.5 
    ##  8 publicCorreos  5.23
    ##  9 ups            5.17
    ## 10 dhl            4.86
    ## # ... with 23 more rows

### 2. Average delivery time per shipping method

“dhl express - gpt - priority (packet tracked)” presents the highest
deliveryTime.

    shipments %>% 
      filter(!is.na(deliverDate)) %>% # Remove not delivered orders
      group_by(courier) %>%
      summarise(mean = mean(deliveryTime)) %>%
      arrange(desc(mean))

    ## # A tibble: 33 x 2
    ##    courier        mean
    ##    <chr>         <dbl>
    ##  1 deutschePost  12   
    ##  2 transaher     11   
    ##  3 fedex          9.44
    ##  4 dpd            9.13
    ##  5 zeleris        6.33
    ##  6 schenker       5.5 
    ##  7 spring         5.5 
    ##  8 publicCorreos  5.23
    ##  9 ups            5.17
    ## 10 dhl            4.86
    ## # ... with 23 more rows

### 3. Average products per order

    shipments_aux <- shipments %>%
      separate_rows(packages) %>% 
      filter(!packages %in% c('oid', ':', '')) # Unnest and create one row per package

    packages_aux <- packages %>% 
      separate_rows(products) %>%
      filter(!products %in% c('oid', ':', '')) # Unnest and create on row per product

    products_per_order <- shipments_aux %>%
      inner_join(packages_aux, by = c('packages' = 'X_id')) %>% # Join Shipments and Packages data
      group_by(X_id) %>%
      summarise(products = n_distinct(products)) %>% # Count # of products whithin each order
      ungroup() %>%
      summarise(products_per_order = mean(products))

    c('In average, each order presents 2.85 products.')

    ## [1] "In average, each order presents 2.85 products."

------------------------------------------------------------------------

## Aditional Analysis

In order to better explore the provided data bases, I formulate some
questions to be answered through data analysis.

### 1. Which are the Most popular Courier and Method

    # Top 10 most popular Courier

    shipments %>% 
      group_by(courier) %>%
      summarise(count = n_distinct(X_id)) %>%
      arrange(desc(count)) %>%
      top_n(10) %>%
      plot_ly(y = ~count, 
              x = ~reorder(courier,desc(count)), 
              type = 'bar')

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-01294fa0ea877661e58f">{"x":{"visdat":{"13c01bbc558b":["function () ","plotlyVisDat"]},"cur_data":"13c01bbc558b","attrs":{"13c01bbc558b":{"y":{},"x":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"reorder(courier, desc(count))","type":"category","categoryorder":"array","categoryarray":["envialia","mrw","correos","tipsa","glsNew","ups","seur","nacex","omniva","smartpost"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"count"},"hovermode":"closest","showlegend":false},"source":"A","config":{"showSendToCloud":false},"data":[{"y":[796,732,453,371,341,315,302,160,108,77],"x":["envialia","mrw","correos","tipsa","glsNew","ups","seur","nacex","omniva","smartpost"],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

    # Top 10 most popular method

    shipments %>% 
      group_by(method) %>%
      summarise(count = n_distinct(X_id)) %>%
      arrange(desc(count)) %>%
      top_n(10) %>%
      plot_ly(y = ~count, 
              x = ~reorder(method,desc(count)), 
              type = 'bar')

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-cc6ac7530afc620c7869">{"x":{"visdat":{"13c073c13978":["function () ","plotlyVisDat"]},"cur_data":"13c073c13978","attrs":{"13c073c13978":{"y":{},"x":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"reorder(method, desc(count))","type":"category","categoryorder":"array","categoryarray":["envialia entrega antes de las 19:00","mrw urgente 19 expedicion","express","tipsa premium","ups standard","seur 24","courier business parcel","omniva parcel machine service","eurobusiness small parcel","mrw urgente 14 expediciÃ³n"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"count"},"hovermode":"closest","showlegend":false},"source":"A","config":{"showSendToCloud":false},"data":[{"y":[790,617,412,369,311,290,161,103,97,87],"x":["envialia entrega antes de las 19:00","mrw urgente 19 expedicion","express","tipsa premium","ups standard","seur 24","courier business parcel","omniva parcel machine service","eurobusiness small parcel","mrw urgente 14 expediciÃ³n"],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

------------------------------------------------------------------------

### 2. How accurate is the delivery prediction?

#### Delayed vs On Time orderds

Comparing the effective deliver time with the predicted, 16% of the
orders have been delayed through the analyze time period.

    shipments %>% 
      filter(!is.na(delayed)) %>% # Remove not delivered orders
      group_by(delayed) %>%
      summarise(count = n_distinct(X_id)) %>%
      ungroup() %>%
      mutate(perc = count/sum(count)) %>% # Criando a visão percentual
      plot_ly(y = ~perc, 
              x = ~delayed, 
              type = 'bar') %>%
      layout(barmode = 'stack')

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-b51b79d617692b5c98de">{"x":{"visdat":{"13c03b753709":["function () ","plotlyVisDat"]},"cur_data":"13c03b753709","attrs":{"13c03b753709":{"y":{},"x":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"barmode":"stack","xaxis":{"domain":[0,1],"automargin":true,"title":"delayed","type":"category","categoryorder":"array","categoryarray":["Delayed","On Time"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"perc"},"hovermode":"closest","showlegend":false},"source":"A","config":{"showSendToCloud":false},"data":[{"y":[0.161621479336668,0.838378520663332],"x":["Delayed","On Time"],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

#### Delayed vs On Time orders per Courier

The plot is comparing On Time delivered orders versus Delayed orders per
courier. Envialia presents a difficult situation, since it is the most
popular courier and, regarding the main couriers, is the one that
presents more delayed orders.

    shipments %>% 
      filter(!is.na(delayed)) %>% # Remove not delivered orders
      group_by(delayed, courier) %>%
      summarise(count = n_distinct(X_id),
                predictedDiff = mean(predictedDiff)) %>%
      ungroup() %>%
      group_by(courier) %>%
      mutate(perc = count/sum(count)) %>% # Criando a visão percentual
      ungroup() %>%
      plot_ly(y = ~count, 
              x = ~reorder(courier, desc(count)),
              color = ~delayed,
              type = 'bar') %>%
      layout(barmode = 'stack')

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-1d4ce27ff340e603a32a">{"x":{"visdat":{"13c03862376c":["function () ","plotlyVisDat"]},"cur_data":"13c03862376c","attrs":{"13c03862376c":{"y":{},"x":{},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"barmode":"stack","xaxis":{"domain":[0,1],"automargin":true,"title":"reorder(courier, desc(count))","type":"category","categoryorder":"array","categoryarray":["envialia","mrw","correos","tipsa","glsNew","ups","seur","omniva","nacex","sending","smartpost","publicCorreos","gls","dpd","CORREOS","dhlParcel","ctt","fedex","dhl","spring","zeleris","glsShipIt","MRW","SEUR","schenker","deutschePost","tnt","paack","posti","transaher","zelerisCustom"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"count"},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"y":[66,2,2,1,1,7,223,13,1,36,85,3,4,4,6,2,11,27,1,2,62,1,1,51,2],"x":["correos","ctt","deutschePost","dhl","dhlParcel","dpd","envialia","fedex","gls","glsNew","mrw","MRW","nacex","omniva","publicCorreos","schenker","sending","seur","SEUR","smartpost","tipsa","tnt","transaher","ups","zeleris"],"type":"bar","name":"Delayed","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"y":[381,10,16,1,13,18,16,560,3,30,300,3,615,3,91,98,1,1,50,62,270,5,62,6,301,2,259,7,1],"x":["correos","CORREOS","ctt","deutschePost","dhl","dhlParcel","dpd","envialia","fedex","gls","glsNew","glsShipIt","mrw","MRW","nacex","omniva","paack","posti","publicCorreos","sending","seur","SEUR","smartpost","spring","tipsa","tnt","ups","zeleris","zelerisCustom"],"type":"bar","name":"On Time","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

#### Delayed vs On Time orders per Courier (Percentage)

The plot is comparing On Time delivered orders versus Delayed orders per
courier. Envialia presents a difficult situation, since it is the most
popular courier and, regarding the main couriers, is the one that
presents more delayed orders.

    shipments %>% 
      filter(!is.na(delayed)) %>% # Remove not delivered orders
      group_by(delayed, courier) %>%
      summarise(count = n_distinct(X_id),
                predictedDiff = mean(predictedDiff)) %>%
      ungroup() %>%
      group_by(courier) %>%
      mutate(perc = count/sum(count)) %>% # Criando a visão percentual
      ungroup() %>%
      plot_ly(y = ~perc, 
              x = ~reorder(courier, desc(count)),
              color = ~delayed,
              type = 'bar') %>%
      layout(barmode = 'stack')

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-109beb254c12d735a93d">{"x":{"visdat":{"13c0431df1f":["function () ","plotlyVisDat"]},"cur_data":"13c0431df1f","attrs":{"13c0431df1f":{"y":{},"x":{},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"barmode":"stack","xaxis":{"domain":[0,1],"automargin":true,"title":"reorder(courier, desc(count))","type":"category","categoryorder":"array","categoryarray":["envialia","mrw","correos","tipsa","glsNew","ups","seur","omniva","nacex","sending","smartpost","publicCorreos","gls","dpd","CORREOS","dhlParcel","ctt","fedex","dhl","spring","zeleris","glsShipIt","MRW","SEUR","schenker","deutschePost","tnt","paack","posti","transaher","zelerisCustom"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"perc"},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"y":[0.147651006711409,0.111111111111111,0.666666666666667,0.0714285714285714,0.0526315789473684,0.304347826086957,0.284802043422733,0.8125,0.032258064516129,0.107142857142857,0.121428571428571,0.5,0.0421052631578947,0.0392156862745098,0.107142857142857,1,0.150684931506849,0.0909090909090909,0.166666666666667,0.03125,0.170798898071625,0.333333333333333,1,0.164516129032258,0.222222222222222],"x":["correos","ctt","deutschePost","dhl","dhlParcel","dpd","envialia","fedex","gls","glsNew","mrw","MRW","nacex","omniva","publicCorreos","schenker","sending","seur","SEUR","smartpost","tipsa","tnt","transaher","ups","zeleris"],"type":"bar","name":"Delayed","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"y":[0.852348993288591,1,0.888888888888889,0.333333333333333,0.928571428571429,0.947368421052632,0.695652173913043,0.715197956577267,0.1875,0.967741935483871,0.892857142857143,1,0.878571428571429,0.5,0.957894736842105,0.96078431372549,1,1,0.892857142857143,0.849315068493151,0.909090909090909,0.833333333333333,0.96875,1,0.829201101928375,0.666666666666667,0.835483870967742,0.777777777777778,1],"x":["correos","CORREOS","ctt","deutschePost","dhl","dhlParcel","dpd","envialia","fedex","gls","glsNew","glsShipIt","mrw","MRW","nacex","omniva","paack","posti","publicCorreos","sending","seur","SEUR","smartpost","spring","tipsa","tnt","ups","zeleris","zelerisCustom"],"type":"bar","name":"On Time","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

#### How long the delyed orders took to arrive?

60% of the delayed orders were delayed by only one day. Almost 90% of
the delayed orders were delayed by a maximum of 5 days.

    shipments %>% 
      filter(delayed == 'Delayed') %>% # Remove not delivered orders
      group_by(predictedDiff) %>%
      summarise(count = n_distinct(X_id)) %>%
      ungroup() %>%
      mutate(perc = count/sum(count)) %>% # Criando a visão percentual
      plot_ly(x = ~predictedDiff, 
              y = ~perc, 
              type = 'bar')

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-58ccdbb6ac775fefd13b">{"x":{"visdat":{"13c04f1a2b55":["function () ","plotlyVisDat"]},"cur_data":"13c04f1a2b55","attrs":{"13c04f1a2b55":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"predictedDiff"},"yaxis":{"domain":[0,1],"automargin":true,"title":"perc"},"hovermode":"closest","showlegend":false},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[-25,-23,-20,-15,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1],"y":[0.00162866449511401,0.00162866449511401,0.00162866449511401,0.00162866449511401,0.00325732899022801,0.00162866449511401,0.00814332247557003,0.00325732899022801,0.0130293159609121,0.00977198697068404,0.0260586319218241,0.0211726384364821,0.0211726384364821,0.0602605863192182,0.0586319218241042,0.164495114006515,0.602605863192182],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

------------------------------------------------------------------------

### 3. How can we compare couriers regarding performance?

One option to estimate which are the top performers courier is to look
for the orders volume of delivers versus delivery Time.

    shipments %>%
      filter(!is.na(deliverDate)) %>% # Remove not delivered orders
      group_by(courier) %>%
      summarise(volume = n_distinct(X_id),
                deliveryTime = mean(deliveryTime)) %>%
      plot_ly(x = ~deliveryTime,
             y = ~volume,
             text = ~courier)

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-55ef78346e60d08c9cc3">{"x":{"visdat":{"13c024c42e9b":["function () ","plotlyVisDat"]},"cur_data":"13c024c42e9b","attrs":{"13c024c42e9b":{"x":{},"y":{},"text":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20]}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"deliveryTime"},"yaxis":{"domain":[0,1],"automargin":true,"title":"volume"},"hovermode":"closest","showlegend":false},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[3.30201342281879,2.8,3.44444444444444,12,4.85714285714286,4.84210526315789,9.1304347826087,3.59514687100894,3,9.4375,4.41935483870968,3.67857142857143,3.33333333333333,2.8,3.48428571428571,4.5,3.2375,2.1588785046729,3,1,5.23333333333333,5.5,3.49315068493151,3.50836120401338,3.5,1.625,5.5,4.45730027548209,4.33333333333333,11,5.17096774193548,6.33333333333333,4],"y":[447,10,18,3,14,19,23,783,1,16,31,336,3,5,700,6,160,107,1,1,60,2,73,299,6,64,10,363,3,1,310,9,1],"text":["correos","CORREOS","ctt","deutschePost","dhl","dhlParcel","dpd","envialia","ENVIALIA","fedex","gls","glsNew","glsShipIt","internal","mrw","MRW","nacex","omniva","paack","posti","publicCorreos","schenker","sending","seur","SEUR","smartpost","spring","tipsa","tnt","transaher","ups","zeleris","zelerisCustom"],"type":"scatter","mode":"markers","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

    shipments %>%
      filter(!is.na(deliverDate) & delayed == 'On Time') %>% # Remove not delivered orders
      group_by(courier) %>%
      summarise(volume = n_distinct(X_id),
                deliveryTime = mean(deliveryTime)) %>%
      plot_ly(x = ~deliveryTime,
             y = ~volume,
             text = ~courier)

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-1399045cc8a281286982">{"x":{"visdat":{"13c01cd40bb":["function () ","plotlyVisDat"]},"cur_data":"13c01cd40bb","attrs":{"13c01cd40bb":{"x":{},"y":{},"text":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20]}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"deliveryTime"},"yaxis":{"domain":[0,1],"automargin":true,"title":"volume"},"hovermode":"closest","showlegend":false},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[3.01837270341207,2.8,3.125,7,4.15384615384615,4.88888888888889,5.875,3.0375,6.66666666666667,4.23333333333333,3.49,3.33333333333333,3.26016260162602,4,3.12087912087912,2,3,1,5.14,3.14516129032258,3.24814814814815,3.2,1.48387096774194,5.83333333333333,3.33222591362126,4,4.63706563706564,6.71428571428571,4],"y":[381,10,16,1,13,18,16,560,3,30,300,3,615,3,91,98,1,1,50,62,270,5,62,6,301,2,259,7,1],"text":["correos","CORREOS","ctt","deutschePost","dhl","dhlParcel","dpd","envialia","fedex","gls","glsNew","glsShipIt","mrw","MRW","nacex","omniva","paack","posti","publicCorreos","sending","seur","SEUR","smartpost","spring","tipsa","tnt","ups","zeleris","zelerisCustom"],"type":"scatter","mode":"markers","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

### 4. How is the orders behavior during the time? Is there any sazonality?

    shipments_aux <- shipments %>%
      separate_rows(packages) %>% 
      filter(!packages %in% c('oid', ':', '')) # Unnest and create one row per package

    packages_aux <- packages %>% 
      separate_rows(products) %>%
      filter(!products %in% c('oid', ':', '')) # Unnest and create on row per product

    shipments_aux %>%
      inner_join(packages_aux, by = c('packages' = 'X_id')) %>% # Join Shipments and Packages data
      group_by(X_id) %>%
      summarise(products = n_distinct(products)) %>% # Count # of products whithin each order
      ungroup() %>%
      summarise(products_per_order = mean(products)) 

    ## # A tibble: 1 x 1
    ##   products_per_order
    ##                <dbl>
    ## 1               2.85
