library(shiny)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)
library(shinythemes)

# df_agent <- read.csv("df_agent.csv", 
#                                  delim = ";", escape_double = FALSE, col_types = cols(CO2_raw = col_number()), 
#                                  trim_ws = TRUE) %>%
#   dplyr::filter(!is.na(Agent))

df_agent <- read.csv("df_agent.csv", sep = ";", dec = ",")
df_missions <- read.csv("df_missions.csv", sep = ";", dec = ",")


# df_imported <- read.csv2("C:/Users/Surface User/Desktop/ecoTRACEQ/data BGES pour app.csv") %>%
#   dplyr::filter(!is.na(Agent))

# quota tous tous motifs
# quota tous colloques
# 
# quota perm tous motifs
# quota perm colloques
# 
# supprimer avion avec checkbox ou tu selectionnes : France, Europe, Asie, Ameriques...
# supprimer avion en fonction de distance : slider



### UI ###
ui <- navbarPage(
    theme = shinytheme("yeti"),

    # Application title
    strong("ecoTRACES"),

    # Sidebar with a slider input for number of bins
    tabPanel("Mise en place de quotas",
    sidebarLayout(
        sidebarPanel(
          h5(strong("Appliquez vos mesures de rédution")),
          h5("Toutes les mesures proposées ci-dessous sont des quotas calculés en tonne de CO2."),
          tags$br(),
          tags$style(".my-class {font-size: 75%; line-height: 1.6;}"),
            sliderInput("mes1",
                        "quota permanents, colloques",
                        min = 0,
                        max = 10,
                        value = 10,
                        step = 0.5),
          sliderInput("mes2",
                      "quota externes, colloques",
                      min = 0,
                      max = 10,
                      value = 10,
                      step = 0.5),
            sliderInput("mes3",
                        "quota tous personnels, colloques",
                        min = 0,
                        max = 10,
                        value = 10,
                        step = 0.5),
            sliderInput("mes4",
                        "quota permanents, tous motifs",
                        min = 0,
                        max = 30,
                        value = 30,
                        step = 0.5),
          sliderInput("mes5",
                      "quota externes, tous motifs",
                      min = 0,
                      max = 30,
                      value = 30,
                      step = 0.5),
            sliderInput("mes6",
                        "quota tous personnels, tous motifs",
                        min = 0,
                        max = 30,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h5(strong("Bilan après mise en place des mesures :")),
          textOutput("text"),
          tags$br(),
          h5(strong("Impact de la réduction selon le statut de l'agent :")),
          tags$br(),
          DT::dataTableOutput("dt"),
          tags$br(),
          h5(strong("A l'échelle de l'agent :")),
          plotOutput("diff_avant_apres"),
          tags$br()
          )
      )
    ),
    tabPanel("Agir sur les missions",
             sidebarLayout(
               sidebarPanel(
                 h5(strong("Appliquez vos mesures de rédution")),
                 tags$br(),
                 checkboxGroupInput("avion", strong("Destination autorisée en avion"),
                                    choices = c(unique(df_missions$destination)),
                                    selected = c(unique(df_missions$destination)),
                                    inline = TRUE
                 ),
                 tags$br(),
                 sliderInput("distance_car",
                             strong("Distance autorisée en voiture"),
                             min = 0,
                             max = 5000,
                             value = 5000)
               ),
               mainPanel(
                 textOutput("text_dest"),
                 tags$br(),
                 plotOutput("detail_transport"),
                 tags$br(),
                 plotOutput("detail_destination")
                 # DT::dataTableOutput("destination")
                 # DT::dataTableOutput("transport")
               )
             )
    )
)

### SERVER ###
server <- function(input, output) {
  # 
  # data_agent <- reactive({
  #   df_agent <- 
  #     df_imported %>%
  #     dplyr::group_by(Agent)
  # })
  
  df_mes <- reactive({
    
    ### mesure : quota perm, colloques
    quota_perm_coll <- input$mes1 * 1000
    
    df_agent_reduc_perm_coll <- df_agent %>% 
      dplyr::mutate(colloques = case_when(
        df_agent$statut == "permanents" & df_agent$colloques > quota_perm_coll ~ quota_perm_coll,
        TRUE ~ df_agent$colloques)) %>%
      dplyr::mutate(total = rowSums(.[2:4]))
    
    ### mesure : quota ext, colloques
    quota_ext_coll <- input$mes2 * 1000
    previous_df <- df_agent_reduc_perm_coll
    
    df_agent_reduc_ext_coll <- previous_df %>% 
      dplyr::mutate(colloques = case_when(
        previous_df$statut == "externes" & previous_df$colloques > quota_ext_coll ~ quota_ext_coll,
        TRUE ~ previous_df$colloques)) %>%
      dplyr::mutate(total = rowSums(.[2:4]))
  
    ### mesure : quota tous, colloques
    quota_all_coll <- input$mes3 * 1000
    previous_df <- df_agent_reduc_ext_coll
    
    df_agent_reduc_all_coll <- previous_df %>% 
      dplyr::mutate(colloques = case_when(
        previous_df$colloques > quota_all_coll ~ quota_all_coll,
        TRUE ~ previous_df$colloques)) %>%
      dplyr::mutate(total = rowSums(.[2:4]))
    
    ### mesure : quota perm, tout
    quota_perm_all <- input$mes4 * 1000
    previous_df <- df_agent_reduc_all_coll
    
    df_agent_reduc_perm_all <- previous_df %>% 
      dplyr::select(-c("colloques","autres","etude_terrain")) %>%
      dplyr::mutate(total = case_when(
        previous_df$statut == "permanents" & previous_df$total >  quota_perm_all ~  quota_perm_all,
        TRUE ~ previous_df$total))
    
    ### mesure : quota ext, tout
    quota_ext_all <- input$mes5 * 1000
    previous_df <- df_agent_reduc_perm_all
    
    df_agent_reduc_ext_all <- previous_df %>% 
      dplyr::mutate(total = case_when(
        previous_df$statut == "externes" & previous_df$total >  quota_ext_all ~  quota_ext_all,
        TRUE ~ previous_df$total))
    
    ### mesure : quota tous, tout
    quota_all_all <- input$mes6 * 1000
    previous_df <- df_agent_reduc_ext_all
    
    df_agent_reduc_all_all <- previous_df %>% 
      dplyr::mutate(total = case_when(
        previous_df$total > quota_all_all ~ quota_all_all,
        TRUE ~ previous_df$total))
    
    df_agent_reduc_all_all
    
  })


output$dt <- DT::renderDataTable(
  df_mes(), #%>%
    # dplyr::group_by(statut) %>%
    # dplyr::summarise(total = sum(as.numeric(total)/1000)) %>%
    # dplyr::mutate(across(where(is.numeric), \(x) round(x, 0))),
  rownames = FALSE,
  # extensions = c("Buttons", "Scroller"),
  # options = list(
  #   pageLength = 10,
  #   # lengthMenu = c(5, 10, 15, 20),
  #   # buttons = c("csv", "pdf", "copy"),
  #   # dom = "Bfrtip",
  #   # scrollX = 250
  # )
)
  
  bges_reduit <- reactive({
   sum(df_mes()$total)
    # sum(df_imported$CO2_raw)))
  })
  
  pourcentage_reduction <- reactive({
    1 - sum(df_mes()$total) / sum(df_agent$total)
  })
  
  output$text <- renderText({paste0("A l'échelle du labo, le bilan carbone sur 3 ans est de 437,8 tonnnes. Grâce à vos mesures, il est désormais de ", 
                                    round(bges_reduit()/1000, 1), " tonnes de CO2",
                                    " et le pourcentage de réduction est de ", 
                                    round(pourcentage_reduction() * 100, 1), 
                                    ". Le nombre d'agent impacté par vos mesures est de ", 
                                    length(which(c(df_mes()$total == df_agent$total) == FALSE)), ".")})
  
  # length(which(c(df_agent$total == df_mes()$total) == TRUE))

  data_plot <- reactive({
    data.frame(Agent_moyen = c("avant", "après"), CO2 = c(sum(df_agent$total), sum(df_mes()$total))) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))
    
  })
  
  ############### !!! corriger nombre ci-dessous ! #######################
  output$diff_avant_apres <- renderPlot(
    
    ggplot(data_plot(), aes(x = Agent_moyen, y = CO2/1000/145)) + geom_col() +
      labs(x = "Agent moyen", y = "Emission en tonne de CO2") 
    # hum le 145 n'est plus bon si tu as modif les jeux de données !
    
  )
  
  ####################################### next panel #######################################
  
  # PB ici je prends toutes les missions et non pas seulement celles faites en avion !
  df_destination <- reactive({
    df_missions %>%
      dplyr::filter(mode == "plane") %>%
      dplyr::filter(destination %in% input$avion) %>%
      dplyr::group_by(destination) %>%
      dplyr::summarise(total = sum(as.numeric(CO2eq_kg))/1000) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2)))
  })
  
  
  output$text_dest <- renderText({paste("Grâce à vos mesures, la réduction des émissions de CO2 est de", 
                                        round(100 - sum(df_destination()$total) / 366 * 100, 1), "%")})
  
  
  output$detail_transport <- renderPlot(
    
    ggplot(df_missions %>%
             dplyr::group_by(mode) %>%
             dplyr::summarise(total = sum(as.numeric(CO2eq_kg))/1000) %>%
             dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))), 
           aes(x = mode, y = total)) + geom_col() +
      labs(x = "Destination", y = "Emission en tonne de CO2")
    
  )
  
  output$detail_destination <- renderPlot(
    
    ggplot(df_missions %>%
             dplyr::group_by(destination) %>%
             dplyr::summarise(total = sum(as.numeric(CO2eq_kg))/1000) %>%
             dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))), 
           aes(x = destination, y = total)) + geom_col() +
      labs(x = "Destination", y = "Emission en tonne de CO2")
    
  )
  
  
  # output$detail_destination <- renderPlot(
  #   
  #   ggplot(data_plot(), aes(x = Agent_moyen, y = CO2/1000/145)) + geom_col() +
  #     labs(x = "Agent moyen", y = "Emission en tonne de CO2")
  #   
  # )
  
  # output$destination <- DT::renderDataTable(
  #   df_destination(),
  #   rownames = FALSE,
  #   extensions = c("Buttons", "Scroller"),
  #   options = list(
  #     pageLength = 10,
  #     # lengthMenu = c(5, 10, 15, 20),
  #     buttons = c("csv", "pdf", "copy"),
  #     dom = "Bfrtip",
  #     scrollX = 250
  #   )
  # )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
