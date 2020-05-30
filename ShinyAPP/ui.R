
ui<-navbarPage("PRC2016020",
               tabPanel("Descrizione",
                        fluidPage(
                          fluidRow(),
                   
                          fluidRow(
                            tabsetPanel(type="tabs",
                                        tabPanel("Dati identificativi del progetto", includeHTML("generale.html")),
                                        tabPanel("Razionale", includeHTML("intro.html")),
                                        tabPanel("Conoscenze disponbili",includeHTML("d1.html")),
                                        tabPanel("Nuove conoscenze",includeHTML("d2.html")),
                                        tabPanel("Metodologia",includeHTML("d3.html")),
                                        tabPanel("Trasferibilità e diffusione dei risultati",includeHTML("d4.html")),
                                        tabPanel("Valore aggiunto",includeHTML("d5.html")),
                                        tabPanel("Risorse",
                                                 br(),
                                                 br(),
                                                 column(12, div(align="center",
                                                 tableOutput("risorse")))),
                                        tabPanel("Timing",
                                                 br(),
                                                 br(),
                                                 hr(),
                                                 timevisOutput("timeline"),hr())
                                        
                                        
                                        )
                        ), 
                        hr()
                        
                        )
               ),
               
               tabPanel("Unità Operative",
                        fluidPage(
                         
                          fluidRow(
                            column(12, div(align="center",
                                           DT::dataTableOutput("UO")))
                          )
                          
                           )
               ),
               
               
               
               
               
               tabPanel("Dataset",
                        fluidPage(
                          fluidRow(
                            column(12, div(align="center",
                                           DT::dataTableOutput("dati")))
                          )
                        )
                        ),
              tabPanel("Risultati",
                       fluidPage(
                         fluidRow(
                          
                        "In base alla letteratura consultata e soprattutto considerando le  risorse a disposizione",
                        br(),
                        "il progetto si focalizzerà sullo studio di ceppi di Enterobacteriacee isolati dai campioni di feci di animali selvatici",
                        br(),

                        "In questa sezione vengono riportati i risultati preliminari generati e aggiornati", 
                         "costantemente a partire dal dataset.",
                        br()
                       
                         
                           
                         ),
                        
                        helpText(tags$strong("dati aggiornati al", Sys.Date())),
                        
                        tabsetPanel(type = "tabs",

                             tabPanel("Campioni", 
                        br(),

                        fluidRow(
                          tags$p("In questa tabella è riportato il numero di campioni di feci esaminati",
                                 "classificabili in base a SPECIE, ZONA di provenienza e U.O."),
                          tags$p("La tabella è interattiva è può essere strutturata come si preferisce"),
  
                        rpivotTableOutput("campioni"))),
                        
                        tabPanel("Ceppi", 
                        br(),
  
                        
                        
                        fluidRow(
                          tags$p("In questa tabella è riportato il numero di ceppi di Enterobacteriacee isolati dai campioni di feci ",
                                 "ed identificati mediante gallerie biochimiche, classificati secondo la SPECIE e la ZONA di provenienza delle feci "),
            
                          tags$p("La tabella è interattiva è può essere strutturata come si preferisce"),
                  
                           
                           rpivotTableOutput("summ")
                           
                           
                           
                         )), 
                        # br(),
                        # br(),
                        # hr(),
                        
                        tabPanel("Antibiotico resistenza", 
                                 br(),
                        fluidRow(
                          column(4, div(
                                       br(),
                                          selectInput("ceppo", "CEPPO",
                                                      c(unique(as.character(ds$identificazione
                                        )))),
                                       br(),
                                       tags$p("Il barplot a destra riporta per ogni ceppo selezionato la % di ceppi isolati resistenti ai diversi antibiotici testati")
                                       
                                       )),
                       
                          column(6, div(align='center',
                                        br(),
                                        plotOutput("plotRes")
                                        ))
                        ),
                        hr(),
                        br(),
                        fluidRow(
                          column(4, div(
                            br(),
                            selectInput("specie", "SPECIE",
                                        c(unique(as.character(ds$SPECIE
                                        )))),
                            br(),
                            
                            tags$p("Il grafico a destra riporta sotto forma di heatmap il profilo di resistenza ( % di ceppi resistenti)",
                           "dei singoli generi, dopo aver selezionato la SPECIE d'interesse")
                            
                            )),
                          
                          column(6, div(align='center',
                                        br(),
                                       
                                        plotOutput("hmap")))

                          
                        )
                         
                       ), 
                       
                       tabPanel("Multiple Antimicrobial Resistance Index",
                                br(),
                                br(),
                                fluidRow(column(4,
                                  div(
                                    selectInput("zona", "Area di campionamento", selected="BINAGO",
                                                c(unique(as.character(ds$ZONA
                                                )))),
                                      br(), br(), br(), br(), br(), br(),
                                      tags$p("Questo indice è una misura della pressione selettiva esercitata dagli antimicrobici",
                                             "sulle popolazioni batteriche di una determinata specie o ambiente." ,
                                             "Valori >0.20 indicano una pressione selettiva elevata che spiega la resistenza multipla osservata.",
                                             "Sui dati del progetto Il MAR Index viene calcolato per le differenti specie di",
                                             "animali selvatici campionati, condizionalmente all'area di provenienza",
                                             "Si tratta di un indice molto utilizzato in letteratura come strumento descrittivo e di confronto"
                                              )))
                                ,
                                
                                
                               column(6,
                                  div(align="center",
                                      plotOutput("MAR"))))
              
                       )))),
              
              
              
              
              tabPanel("Utilizzo risorse",
                       
                       fluidPage(
                         
                         fluidRow(
                          
                           tabsetPanel(type = "tabs",
                                         tabPanel("U.O.1",
                                                  br(),
                                                  br(),
                                                  helpText("Materiali di consumo"),
                                                  br(),
                                                  br(),
                                                  tableOutput("consumi"),
                                                  br(),
                                                  hr(),
                                                  helpText("Dettaglio consumi"),
                                                  tableOutput("cons")
                                                ),
                                        
                                                tabPanel("U.O.2", "No dati"),
                                       tabPanel("U.O.3", "No dati")
                                       
                                       )))),
              tabPanel("Report",
                       tags$iframe(style="height:800px; width:100%",src="report.pdf")
                       )
                       
              )
                     
                         
                
                       
               
      
                          
                          
                          
                

