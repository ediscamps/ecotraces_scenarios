library(shiny)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)

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
ui <- fluidPage(

    # Application title
    titlePanel("ecoTRACES"),

    # Sidebar with a slider input for number of bins 
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
                        value = 30),
            radioButtons("avion",
              label = strong("Destination autorisée en avion"),
              choices = c("France", "Europe", "Afrique", "Asie", "Océanie", "Amérique"),
              inline = TRUE
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tags$br(),
          h5(strong("Bilan après mise en place des mesures :")),
          textOutput("text"),
          tags$br(),
          tags$br(),
          h5("A l'échelle de l'agent :"),
          plotOutput("diff_avant_apres"),
          DT::dataTableOutput("dt")
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
  df_mes(),
  rownames = FALSE,
  extensions = c("Buttons", "Scroller"),
  options = list(
    pageLength = 10,
    # lengthMenu = c(5, 10, 15, 20),
    buttons = c("csv", "pdf", "copy"),
    dom = "Bfrtip",
    scrollX = 250
  )
)
  
  bges_reduit <- reactive({
   sum(df_mes()$total)
    # sum(df_imported$CO2_raw)))
  })
  
  pourcentage_reduction <- reactive({
    1 - sum(df_mes()$total) / sum(df_agent$total)
  })
  
  output$text <- renderText({paste("A l'échelle du labo, le bilan carbone sur 3 ans est de 414 tonnnes. Grâce à vos mesures, il est désormais de", round(bges_reduit()/1000, 1), "tonnes de CO2",
                                   "et le pourcentage de réduction est de", round(pourcentage_reduction() * 100, 1))})
  

  data_plot <- reactive({
    data.frame(Agent_moyen = c("avant", "après"), CO2 = c(sum(df_agent$total), sum(df_mes()$total))) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))
    
  })
  
  
  output$diff_avant_apres <- renderPlot(
    
    ggplot(data_plot(), aes(x = Agent_moyen, y = CO2/1000/145)) + geom_col() +
      labs(x = "Agent moyen", y = "Emission en tonne de CO2")
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
