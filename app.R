library(shiny)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)
library(shinythemes)
library(tidyr)
library(shinydashboard)

df_missions <- read.csv("df_missions.csv", sep = ";", dec = ",")

### UI ###



ui_tab1 <- fluidPage(
  img(src='ecotraces-logo.png', height = "100px", align = "center"),
  h2("Bienvenue dans l'outil EcoTRACES de conception de scénarios dédié aux missions"),
  br(),
  br(),
  h4(strong("Notes importantes")),
  "Ce logiciel est pensé pour tester l'efficacité (en terme de % de réduction des émissions de gaz à effet de serre) de quelques mesures clefs, dans le contexte du laboratoire TRACES.",
  br(),br(),
  "Il s'appuie sur les données des bilans de gaz à effet de serre (BGES) réalisés en 2019, 2022 et 2023. Ces données étant imparfaites par nature, il est important de garder à l'esprit que 
  les chiffres utilisés ici ne doivent être utilisés que pour estimer des ordres de grandeur.",
  br(),br(),
  "Par exemple, le motif (colloque, terrain, etc.) d'un nombre important de missions n'est pas connu, et les calculs s'appuient donc sur des estimations réalisées à partir des missions dont le motif est connu.",
  br(),
  br(),
  h4(strong("Comment utiliser cet outil ?")),
  "En cliquant dans le menu à gauche sur Paramétrages des mesures, vous pourrez sélectionner quelques mesures (report modal, quotas). Le résultat en terme de pourcentage de réduction s'affichera sur la gauche.",
  br(),
  br(),
  br(),
  br(),
  em("Cette application a été conçue par Emmanuel Discamps & Marc Thomas, dans le but d'aider l'ensemble des personnels de TRACES lors des ateliers EcoTRACES visant à la conception de mesures de réduction de nos émissions."),
  br(),
  em("Nous espérons bientôt ajouter de nouvelles fonctionnalités, comme une mesure de report modal avec durée de trajet en train (ex : <6h), ou des exemptions pour les quotas (ex : missions longues de 1 mois)")
)


ui_tab2 <- fluidPage(
  
h3(strong("1) Appliquer des mesures de report modal")),
"Le report modal consiste à remplacer l'avion par d'autres moyens de transport (ex : train) pour certaines destinations.",
tags$br(),br(),
sliderInput("distance_plane",
            strong("Distance (km) en dessous de laquelle l'avion est interdit"),
            min = 0,
            max = 24000,
            value = 0,
            step = 100),
h6("Vous pouvez utiliser les flèches du clavier pour ajuster plus précisément"),
tags$br(),

checkboxGroupInput("avion", strong("Destinations non autorisées en avion"),
                   choices = c(unique(df_missions$destination)),
                   selected = NULL,
                   inline = F
),
tags$br(),
# sliderInput("distance_car",
#             strong("Distance autorisée en voiture"),
#             min = 0,
#             max = 5000,
#             value = 5000)

  # textOutput("text_dest"),
  # h5(strong("Bilan après mise en place des mesures :")),
  # "A l'échelle du labo, le BGES sur 3 ans est de 437,8 t eCO2.",
  # textOutput("text_report_bges_reduit"), 
  # textOutput("text_report_p_reduc"),
  
  tags$br(),
  plotOutput("detail_transport"),
  tags$br(),
  plotOutput("detail_destination"),
  # DT::dataTableOutput("destination")
  # DT::dataTableOutput("transport")
tags$br(),br(),
             h3(strong("2) Appliquer des mesures de quotas")),
             h5("Toutes les mesures proposées ci-dessous sont des quotas calculés en tonne de CO2, sur 3 ans."),
             tags$br(),
             tags$style(".my-class {font-size: 75%; line-height: 1.6;}"),
             sliderInput("mes1",
                         "quota permanents, colloques, sur 3 ans",
                         min = 0,
                         max = 10,
                         value = 10,
                         step = 0.5),
             sliderInput("mes2",
                         "quota externes, colloques, sur 3 ans",
                         min = 0,
                         max = 10,
                         value = 10,
                         step = 0.5),
             sliderInput("mes3",
                         "quota tous personnels, colloques, sur 3 ans",
                         min = 0,
                         max = 10,
                         value = 10,
                         step = 0.5),
             sliderInput("mes4",
                         "quota permanents, tous motifs, sur 3 ans",
                         min = 0,
                         max = 30,
                         value = 30,
                         step = 0.5),
             sliderInput("mes5",
                         "quota externes, tous motifs, sur 3 ans",
                         min = 0,
                         max = 30,
                         value = 30,
                         step = 0.5),
             sliderInput("mes6",
                         "quota tous personnels, tous motifs, sur 3 ans",
                         min = 0,
                         max = 30,
                         value = 30),
           
           
           # Show a plot of the generated distribution
            br(),br(),
             h5(strong("Impact de la réduction selon le statut de l'agent :")),
             tags$br(),
             plotOutput("histo_quota"),
             tags$br(),
             # # DT::dataTableOutput("dt"),
             # tags$br(),
             # h5(strong("Moyenne agent :")),
             # plotOutput("diff_avant_apres"),
             # tags$br()
           
         )


ui_tab3 <- fluidPage(
      h5("Retrouvez ici les données brutes."),
  
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Emissions de CO2 par agent (avant mesures)",
          tags$br(),
          DT::dataTableOutput("df_agent_brut_table")
        ),
        tabPanel(
          "Emissions de CO2 par agent (après mesures)",
          tags$br(),
          DT::dataTableOutput("df_agent_reduit_table")
        ),
        tabPanel(
          "Emissions de CO2 par mission",
          tags$br(),
          DT::dataTableOutput("df_mission_brut_table")
        )
      )
    )
  
)





ui <- dashboardPage(
  
  dashboardHeader(title = "EcoTRACES"),
  dashboardSidebar(width = 350,
    sidebarMenu(style = "position: fixed;",
                menuItem("Introduction", tabName = "tab1"),
                menuItem("Paramétrages des mesures", tabName = "tab2"),
                menuItem("Tableaux de données", tabName = "tab3"),

      br(),
      "BGES labo sur 3 ans (avant mesures) : 437,8 t eCO2",
      tags$br(),
      tags$br(),
      h5(strong("Bilan après mise en place des mesures sélectionnées :")),
      textOutput("text_quota_bges_reduit"), 
      textOutput("text_quota_p_reduc"),
      tags$br(),
      textOutput("text_report_n_missions"),
      textOutput("text_quota_n_agent"),
      tags$br()
      )
  
  ),
  dashboardBody(
    
    # controlling horizontal lines
    tags$head(
      tags$style(HTML("hr {border-top: 2px solid #000000;}"))
    ),
    
    tabItems(
      tabItem("tab1",
              ui_tab1
      ),
      tabItem("tab2",
              ui_tab2
      ),
      tabItem("tab3",
              ui_tab3
      )
      
     
    ),

    
  )
  
 
)


### SERVER ###
server <- function(input, output) {

  
  ####################################### report modal #######################################
  
  df_missions_reduc <- reactive({
    df_missions %>% 
      dplyr::filter(!(mode == "plane" & distance_km < (input$distance_plane*2))) %>%
      dplyr::filter(!(mode == "plane" & destination %in% input$avion))
    
    # je multiplie par 2 puisque l'objectif est d'exprimer une distance aller, mais la distance tot compte l'AR
  })
  
  # bges_reduit_plane_km <- reactive({
  #   sum(df_missions_reduc()$CO2eq_kg)
  # })
  # 
  # pourcentage_reduction_plane_km <- reactive({
  #   1 - bges_reduit_plane_km() / sum(df_missions$CO2eq_kg)
  # })
  
  output$text_report_bges_reduit <- renderText(paste0("BGES réduit : ",round(bges_reduit_plane_km()/1000, 1)," t eCO2"))
  
  output$text_report_p_reduc <- renderText(paste0("Pourcentage de réduction : ",round(pourcentage_reduction_plane_km() * 100, 1)," %"))
  
  output$text_report_n_missions <- renderText(paste("Nombre de missions impactées par le report modal :",
                                                    nrow(df_missions)-nrow(df_missions_reduc())))
  

  
  df_missions_reduc_pour_plot <- reactive({
    
    df_mes_modal <- df_missions %>% 
      dplyr::filter(!(mode == "plane" & distance_km < (input$distance_plane*2))) %>%
      dplyr::filter(!(mode == "plane" & destination %in% input$avion)) %>%
      dplyr::group_by(mode) %>%
      dplyr::summarise(total = sum(as.numeric(CO2eq_kg))/1000) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
      dplyr::mutate(mesures = "après")
    
    df_sans_mes_modal <- df_missions %>%
      dplyr::group_by(mode) %>%
      dplyr::summarise(total = sum(as.numeric(CO2eq_kg))/1000) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
      dplyr::mutate(mesures = "avant")
    
    df_compare <- rbind(df_mes_modal, df_sans_mes_modal)
    
    return(df_compare)
    
  })

  output$detail_transport <- renderPlot(
    
    ggplot(df_missions_reduc_pour_plot(), aes(x=mode, y=total, fill= mesures)) +
      geom_col(position = position_dodge()) +
      scale_fill_manual(values = c("grey", "black")) +
      labs(x = "Moyen de transport", y = "Emission en tonne de CO2") 
    
  )
  
  
  df_missions_reduc_pour_plot_2 <- reactive({
    
    df_mes_modal <- df_missions %>% 
      dplyr::filter(!(mode == "plane" & distance_km < (input$distance_plane*2))) %>%
      dplyr::filter(!(mode == "plane" & destination %in% input$avion)) %>%
      dplyr::group_by(destination) %>%
      dplyr::summarise(total = sum(as.numeric(CO2eq_kg))/1000) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
      dplyr::mutate(mesures = "après")
    
    df_sans_mes_modal <- df_missions %>%
      dplyr::group_by(destination) %>%
      dplyr::summarise(total = sum(as.numeric(CO2eq_kg))/1000) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
      dplyr::mutate(mesures = "avant")
    
    df_compare <- rbind(df_mes_modal, df_sans_mes_modal)
    
    return(df_compare)
    
  })
  
  output$detail_destination <- renderPlot(
    
    # ggplot(df_missions %>%
    #          dplyr::group_by(destination) %>%
    #          dplyr::summarise(total = sum(as.numeric(CO2eq_kg))/1000) %>%
    #          dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))), 
    #        aes(x = destination, y = total)) + geom_col() +
    #   labs(x = "Destination", y = "Emission en tonne de CO2")
    
    ggplot(df_missions_reduc_pour_plot_2(), aes(x = destination, y = total, fill = mesures)) +
      geom_col(position=position_dodge()) +
      scale_fill_manual(values = c("grey", "black")) +
      labs(x = "Moyen de transport", y = "Emission en tonne de CO2")
  )
  
  
  
  # output$df_agent_reduit <- DT::renderDataTable(
  #   df_agent_reduc() %>%
  #     dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))),
  #   rownames = FALSE,
  #   extensions = c("Buttons", "Scroller"),
  #   options = list(
  #     pageLength = 10,
  #     lengthMenu = c(5, 10, 15, 20),
  #     buttons = c("csv", "pdf", "copy"),
  #     dom = "Bfrtip",
  #     scrollX = 250
  #   )
  # )
  
  # output$df_mission_reduit <- DT::renderDataTable(
  #   df_missions %>% 
  #     dplyr::filter(!(mode == "plane" & distance_km < (input$distance_plane*2))) %>%
  #     dplyr::filter(!(mode == "plane" & destination %in% input$avion)) %>%
  #     dplyr::summarise(total = sum(as.numeric(CO2eq_kg))/1000) %>%
  #     dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))),
  #   rownames = FALSE,
  #   extensions = c("Buttons", "Scroller"),
  #   options = list(
  #     pageLength = 10,
  #     lengthMenu = c(5, 10, 15, 20),
  #     buttons = c("csv", "pdf", "copy"),
  #     dom = "Bfrtip",
  #     scrollX = 250
  #   )
  # )
  
  
  
  
  ############tentative d'integrations des deux types de mesures ensembles
  
  
  df_agent <- reactive({
    
    x <- df_missions_reduc() %>%
      select(agent, statut, motif, CO2eq_kg) %>%
      group_by(agent)%>%
      mutate(total = sum(CO2eq_kg)) %>%
      arrange(agent) %>%
      pivot_wider(names_from = motif, values_from = CO2eq_kg, values_fn = sum, values_fill = 0)
    
    p_coll <- round(sum(x$colloques) / (sum(x$total)-sum(x$inconnu)),3)
    p_etude <- round(sum(x$etude_terrain) / (sum(x$total)-sum(x$inconnu)),3)
    p_autres <- round(sum(x$autres) / (sum(x$total)-sum(x$inconnu)),3)
    
    y <- x %>%
      mutate(colloques = round(colloques + inconnu*p_coll,2)) %>%
      mutate(etude_terrain = round(etude_terrain + inconnu*p_etude,2)) %>%
      mutate(autres = round(autres + inconnu*p_autres,2)) %>%
      select(-inconnu)
    
     return(y)
  })

  
  df_agent_reduc <- reactive({
    
    ### mesure : quota perm, colloques
    quota_perm_coll <- input$mes1 * 1000
    
    df_agent_reduc_perm_coll <- df_agent() %>%
      dplyr::mutate(colloques = case_when(
        statut == "permanents" & colloques > quota_perm_coll ~ quota_perm_coll,
        TRUE ~ colloques)) %>%
      dplyr::mutate(total = colloques + etude_terrain + autres)
    
    ### mesure : quota ext, colloques
    quota_ext_coll <- input$mes2 * 1000
    previous_df <- df_agent_reduc_perm_coll
    
    df_agent_reduc_ext_coll <- previous_df %>%
      dplyr::mutate(colloques = case_when(
        statut == "externes" & colloques > quota_ext_coll ~ quota_ext_coll,
        TRUE ~ colloques)) %>%
      dplyr::mutate(total = colloques + etude_terrain + autres)

    ### mesure : quota tous, colloques
    quota_all_coll <- input$mes3 * 1000
    previous_df <- df_agent_reduc_ext_coll

    df_agent_reduc_all_coll <- previous_df %>%
      dplyr::mutate(colloques = case_when(
        colloques > quota_all_coll ~ quota_all_coll,
        TRUE ~ colloques)) %>%
      dplyr::mutate(total = colloques + etude_terrain + autres)

    ### mesure : quota perm, tout
    quota_perm_all <- input$mes4 * 1000
    previous_df <- df_agent_reduc_all_coll

    df_agent_reduc_perm_all <- previous_df %>%
      dplyr::select(-c("colloques","autres","etude_terrain")) %>%
      dplyr::mutate(total = case_when(
        statut == "permanents" & total >  quota_perm_all ~  quota_perm_all,
        TRUE ~ total))

    ### mesure : quota ext, tout
    quota_ext_all <- input$mes5 * 1000
    previous_df <- df_agent_reduc_perm_all

    df_agent_reduc_ext_all <- previous_df %>%
      dplyr::mutate(total = case_when(
        statut == "externes" & total >  quota_ext_all ~  quota_ext_all,
        TRUE ~ total))

    ### mesure : quota tous, tout
    quota_all_all <- input$mes6 * 1000
    previous_df <- df_agent_reduc_ext_all

    df_agent_reduc_all_all <- previous_df %>%
      dplyr::mutate(total = case_when(
        total > quota_all_all ~ quota_all_all,
        TRUE ~ total))
    
    df_agent_reduc_all_all
    
  })


# output$dt <- DT::renderDataTable(
#   df_agent_reduc() %>%
#     dplyr::group_by(statut) %>%
#     dplyr::summarise(total = sum(as.numeric(total)/1000)) %>%
#     dplyr::mutate(across(where(is.numeric), \(x) round(x, 0))),
#   rownames = FALSE,
#   # extensions = c("Buttons", "Scroller"),
#   # options = list(
#   #   pageLength = 10,
#   #   # lengthMenu = c(5, 10, 15, 20),
#   #   # buttons = c("csv", "pdf", "copy"),
#   #   # dom = "Bfrtip",
#   #   # scrollX = 250
#   # )
# )
  
  bges_reduit <- reactive({
   sum(df_agent_reduc()$total)
    # sum(df_imported$CO2_raw)))
  })
  
  pourcentage_reduction <- reactive({
    1 - sum(df_agent_reduc()$total) / sum(df_missions$CO2eq_kg)
  })
  
    output$text_quota_bges_reduit <- renderText(paste0("BGES réduit : ", round(bges_reduit()/1000, 1)," t eCO2"))
    output$text_quota_p_reduc <- renderText(paste0("Pourcentage de réduction : ", round(pourcentage_reduction() * 100, 1)," %"))
    output$text_quota_n_agent <- renderText(paste("Nombre d'agents impactés par les quotas :",
      length(which(round(df_agent_reduc()$total,0) < round(df_agent()$total,0)))))
    # length(which(round(df_agent_reduc()$total,1) < round(df_agent()$total,1)))))


  data_plot <- reactive({
    data.frame(Agent_moyen = c("avant", "après"), CO2 = c(sum(df_agent()$total), sum(df_agent_reduc()$total))) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))
    
  })
  
  
  ##### graph histo pour quota
  
  df_quota_pour_plot <- reactive({
    
    df_mes_quota <- df_agent_reduc() %>%
      dplyr::group_by(statut) %>%
      dplyr::summarise(total = sum(as.numeric(total))/1000) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
      dplyr::mutate(mesures = "après")
    
    df_sans_mes_quota <- df_agent() %>%
      dplyr::group_by(statut) %>%
      dplyr::summarise(total = sum(as.numeric(total))/1000) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
      dplyr::mutate(mesures = "avant")
    
    df_compare <- rbind(df_mes_quota, df_sans_mes_quota)
    
    return(df_compare)
    
  })
  
  
  output$histo_quota <- renderPlot(
    
    ggplot(df_quota_pour_plot(), aes(x=statut, y=total, fill= mesures)) +
      geom_col(position = position_dodge()) +
      scale_fill_manual(values = c("grey", "black")) +
      labs(x = "Statut des agents", y = "Emission en tonne de CO2") 
    
  )
  
  
  
  ################## out datatables
  
  output$df_agent_brut_table <- DT::renderDataTable(
    df_agent() %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 15, 20),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollX = 250
    )
  )
  
  output$df_agent_reduit_table <- DT::renderDataTable(
    df_agent_reduc() %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 15, 20),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollX = 250
    )
  )
  
  
  output$df_mission_brut_table <- DT::renderDataTable(
    df_missions %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 15, 20),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollX = 250
    )
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############### !!! corriger nombre ci-dessous ! #######################
  output$diff_avant_apres <- renderPlot(
    
    ggplot(data_plot(), aes(x = Agent_moyen, y = CO2/1000/145)) + geom_col() +
      labs(x = "Agent moyen", y = "Emission en tonne de CO2") 
    # hum le 145 n'est plus bon si tu as modif les jeux de données !
    
  )
  
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
