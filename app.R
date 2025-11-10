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
"Ici, les vols de correspondance (par exemple un trajet Toulouse - Paris avant de prendre un vol international depuis Paris) ne sont PAS concernés.",
tags$br(),br(),
sliderInput("distance_plane",
            strong("Distance (km) en dessous de laquelle l'avion est interdit (hors correspondances)"),
            min = 0,
            max = 24000,
            value = 0,
            step = 100),
h6("Vous pouvez utiliser les flèches du clavier pour ajuster plus précisément"),
tags$br(),

checkboxGroupInput("avion", strong("Destinations non autorisées en avion (hors correspondances)"),
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
             h5("Toutes les mesures proposées ci-dessous sont des quotas calculés en tonne de CO2, par an."),
             tags$br(),
             tags$style(".my-class {font-size: 75%; line-height: 1.6;}"),

h4("Définissez d'abord des quotas généraux pour toutes les missions (personnels = permanents, docs/post-docs, associés) :"),
h5("Pour rappel, un vol AR en avion hors Europe consomme environ 2 t eCO2/an."),
tags$br(),

sliderInput("mes6",
            "quota personnels, tous motifs, t eCO2/an",
            min = 0,
            max = 10,
            value = 10,
            step = 0.25),
sliderInput("mes5",
            "quota externes, tous motifs, t eCO2/an",
            min = 0,
            max = 10,
            value = 10,
            step = 0.25),

h4("Vous pouvez ensuite définir des quotas plus stricts pour les colloques (personnels = permanents, docs/post-docs, associés):"),
tags$br(),
             sliderInput("mes3",
                         "quota personnels, colloques, t eCO2/an",
                         min = 0,
                         max = 5,
                         value = 5,
                         step = 0.25), 
sliderInput("mes2",
                         "quota externes, colloques,t eCO2/an",
                         min = 0,
                         max = 5,
                         value = 5,
                         step = 0.25),

h4("Et enfin des quotas plus stricts pour les permanents :"),
tags$br(),
            
             sliderInput("mes4",
                         "quota permanents, tous motifs, t eCO2/an",
                         min = 0,
                         max = 10,
                         value = 10,
                         step = 0.25),
sliderInput("mes1",
            "quota permanents, colloques, t eCO2/an",
            min = 0,
            max = 5,
            value = 5,
            step = 0.25),
             
           
           
           # Show a plot of the generated distribution
            br(),br(),
             h5(strong("Impact de la réduction selon le statut de l'agent :")),
             tags$br(),
             plotOutput("histo_quota"),
            # plotOutput("histo_quota_motif"),
             tags$br(),
             # # DT::dataTableOutput("dt"),
             # tags$br(),
             # h5(strong("Moyenne agent :")),
             # plotOutput("diff_avant_apres"),
             # tags$br()
           
h3(strong("3) Mesures d'ajustement des quotas")),
"Vous pouvez ajuster ici plus finement les quotas entre personnels ayant des besoins différents (par exemple, de missions sur le terrain à l'international).",
tags$br(),br(),

h4("Vous pouvez ici multiplier le quota des agents avec des missions de terrains à l'international, tout en maintenant le même taux de réduction :"),
sliderInput("ajust1",
            "Facteur de multiplication du quota des agents avec du terrain hors Europe, par rapport aux autres",
            min = 1,
            max = 30,
            value = 1,
            step = 0.25),
textOutput("text_p_field_international"),
textOutput("text_quota6_general"),
textOutput("text_quota6_intern"),
textOutput("text_quota6_nointern"),
textOutput("text_quota4_general"),
textOutput("text_quota4_intern"),
textOutput("text_quota4_nointern"),

br(),
h4("Vous pouvez également décider d'exclure des quotas les missions longues, en utilisant le paramètre ci-dessous."),
sliderInput("days_slider",
            strong("Durée minimale des missions longues (en jours) :"),
            min = 1,
            max = 300,
            value = 300,
            step = 1),

"Au-delà de ce nombre de jours, la mission ne compte plus dans le quota par agent."
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
  dashboardSidebar(width = 360,
    sidebarMenu(style = "position: fixed;",
                menuItem("Introduction", tabName = "tab1"),
                menuItem("Paramétrages des mesures", tabName = "tab2"),
                menuItem("Tableaux de données", tabName = "tab3"),

      br(),
      textOutput("text_report_bges_original"),
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

  ####################################### calcul du BGES original avant mesures #######################################
  
  bges_original <- reactive({
    sum(df_missions$CO2eq_kg)
  })
  
  output$text_report_bges_original <- renderText(paste0("BGES sur 3 ans (avant mesures) : ",round(bges_original()/1000,1)," t eCO2"))
  
  
  ####################################### report modal #######################################
  
  df_missions_reduc <- reactive({
    df_missions %>% 
      dplyr::filter(!(mode == "plane" & distance_km < (input$distance_plane*2))) %>%
      dplyr::filter(!(mode == "plane" & destination %in% input$avion))
    
    # je multiplie par 2 puisque l'objectif est d'exprimer une distance aller, mais la distance tot compte l'AR
  })
  
  # bges_reduit_plane_km <- reactive({
  #  sum(df_missions_reduc()$CO2eq_kg)
  # })
  # # 
  # # pourcentage_reduction_plane_km <- reactive({
  # #   1 - bges_reduit_plane_km() / sum(df_missions$CO2eq_kg)
  # # })
  
  # output$text_report_bges_reduit <- renderText(paste0("BGES réduit : ",bges_reduit_plane_km()/1000," t eCO2"))
  # 
  # output$text_report_p_reduc <- renderText(paste0("Pourcentage de réduction : ",round(pourcentage_reduction_plane_km() * 100, 1)," %"))
  
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
    
    ggplot(df_missions_reduc_pour_plot(), aes(x=mode, y=total, fill= factor(mesures, c("avant","après")))) +
      geom_col(position = position_dodge()) +
      scale_fill_manual(values = c("grey", "black")) +
      labs(x = "Moyen de transport", y = "Emission en tonne de CO2", fill = "mesures") 
    
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
    
    ggplot(df_missions_reduc_pour_plot_2(), aes(x = destination, y = total, fill = factor(mesures, c("avant","après")))) +
      geom_col(position=position_dodge()) +
      scale_fill_manual(values = c("grey", "black")) +
      labs(x = "Moyen de transport", y = "Emission en tonne de CO2", fill = "mesures")
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
  
  
            # les missions longues sont ecartees ici
    df <- df_missions_reduc() %>%
      dplyr::mutate(motif = case_when(
        days > input$days_slider ~  "missions_longues",
        TRUE ~ motif))

    
    x <- df %>%
      select(agent, statut, motif, CO2eq_kg) %>%
      group_by(agent)%>%
      mutate(total = sum(CO2eq_kg)) %>%
      arrange(agent) %>%
      pivot_wider(names_from = motif, values_from = CO2eq_kg, values_fn = sum, values_fill = 0)
    
    # p_coll <- round(sum(x$colloques) / (sum(x$total)-sum(x$inconnu)),3)
    # p_etude <- round(sum(x$etude_terrain) / (sum(x$total)-sum(x$inconnu)),3)
    # p_autres <- round(sum(x$autres) / (sum(x$total)-sum(x$inconnu)),3)
    p_coll <- sum(x$colloques) / (sum(x$total)-sum(x$inconnu))
    p_etude <- sum(x$etude_terrain) / (sum(x$total)-sum(x$inconnu))
    p_autres <- sum(x$autres) / (sum(x$total)-sum(x$inconnu))
<<<<<<< HEAD
    p_longues <- sum(x$missions_longues) / (sum(x$total)-sum(x$inconnu))
=======
<<<<<<< HEAD
    p_longues <- sum(x$missions_longues) / (sum(x$total)-sum(x$inconnu))
=======
>>>>>>> 99fd674208ff34bf3ad62752eb775deb5f1780b4
>>>>>>> 9cb97065e089d84b704d0512e07c906718130fab
    
    
    # y <- x %>%
    #   mutate(colloques = round(colloques + inconnu*p_coll,2)) %>%
    #   mutate(etude_terrain = round(etude_terrain + inconnu*p_etude,2)) %>%
    #   mutate(autres = round(autres + inconnu*p_autres,2)) %>%
    #   select(-inconnu)
    
    y <- x %>%
      mutate(colloques = colloques + inconnu*p_coll) %>%
      mutate(etude_terrain = etude_terrain + inconnu*p_etude) %>%
      mutate(autres = autres + inconnu*p_autres) %>%
<<<<<<< HEAD
      mutate(missions_longues = missions_longues + inconnu*p_longues) %>%
=======
<<<<<<< HEAD
      mutate(missions_longues = missions_longues + inconnu*p_longues) %>%
=======
>>>>>>> 99fd674208ff34bf3ad62752eb775deb5f1780b4
>>>>>>> 9cb97065e089d84b704d0512e07c906718130fab
      select(-inconnu)
    
     return(y)
  })
  
  
  df_agent_reduc <- reactive({
    
    ### mesure : quota perm, colloques
    quota_perm_coll <- input$mes1 * 3000
    
    df_agent_reduc_perm_coll <- df_agent() %>%
      dplyr::mutate(colloques = case_when(
        statut == "permanents" & colloques > quota_perm_coll ~ quota_perm_coll,
        TRUE ~ colloques)) %>%
      dplyr::mutate(total_quota = colloques + etude_terrain + autres)
    
    ### mesure : quota ext, colloques
    quota_ext_coll <- input$mes2 * 3000
    previous_df <- df_agent_reduc_perm_coll
    
    df_agent_reduc_ext_coll <- previous_df %>%
      dplyr::mutate(colloques = case_when(
        statut == "externes" & colloques > quota_ext_coll ~ quota_ext_coll,
        TRUE ~ colloques)) %>%
      dplyr::mutate(total_quota = colloques + etude_terrain + autres)

    ### mesure : quota personnels, colloques
    quota_all_coll <- input$mes3 * 3000
    previous_df <- df_agent_reduc_ext_coll

    df_agent_reduc_all_coll <- previous_df %>%
      dplyr::mutate(colloques = case_when(
        statut != "externes" & colloques > quota_all_coll ~ quota_all_coll,
        TRUE ~ colloques)) %>%
      dplyr::mutate(total_quota = colloques + etude_terrain + autres)

    ### mesure : quota perm, tout
    quota_perm_all <- input$mes4 * 3000
    previous_df <- df_agent_reduc_all_coll

    df_agent_reduc_perm_all <- previous_df %>%
      dplyr::select(-c("colloques","autres","etude_terrain")) %>%
      dplyr::mutate(total_quota = case_when(
        statut == "permanents" & total_quota >  quota_perm_all ~  quota_perm_all,
        TRUE ~ total_quota))

    ### mesure : quota ext, tout
    quota_ext_all <- input$mes5 * 3000
    previous_df <- df_agent_reduc_perm_all

    df_agent_reduc_ext_all <- previous_df %>%
      dplyr::mutate(total_quota = case_when(
        statut == "externes" & total_quota >  quota_ext_all ~  quota_ext_all,
        TRUE ~ total_quota))

    ### mesure : quota personnels, tout
    quota_all_all <- input$mes6 * 3000
    previous_df <- df_agent_reduc_ext_all

    df_agent_reduc_all_all <- previous_df %>%
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 9cb97065e089d84b704d0512e07c906718130fab
      dplyr::mutate(total_quota = case_when(
        statut != "externes" & total_quota > quota_all_all ~ quota_all_all,
        TRUE ~ total_quota)) %>%
    dplyr::mutate(total = total_quota + missions_longues)
    
<<<<<<< HEAD
=======
=======
      dplyr::mutate(total = case_when(
        statut != "externes" & total > quota_all_all ~ quota_all_all,
        TRUE ~ total))
>>>>>>> 99fd674208ff34bf3ad62752eb775deb5f1780b4
>>>>>>> 9cb97065e089d84b704d0512e07c906718130fab
    
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
  
    output$text_quota_bges_reduit <- renderText(paste0("BGES réduit : ", round(bges_reduit()/1000,1)," t eCO2"))
    output$text_quota_p_reduc <- renderText(paste0("Pourcentage de réduction : ", round(pourcentage_reduction() * 100, 1)," %"))
    output$text_quota_n_agent <- renderText(paste("Nombre d'agents impactés par les quotas (uniquement) :", length(which(round(df_agent_reduc()$total,1) < round(df_agent()$total,1)))))
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
    
    ggplot(df_quota_pour_plot(), aes(x=statut, y=total, fill= factor(mesures, c("avant","après")))) +
      geom_col(position = position_dodge()) +
      scale_fill_manual(values = c("grey", "black")) +
      labs(x = "Statut des agents", y = "Emission en tonne de CO2", fill = "mesures") 
    
  )
  
  # ##### graph histo pour quota par motifs NE MARCHE PAS CAR BESOIN DE PIVOT, A FAIRE PLUS TARD
  # 
  # df_quota_pour_plot_motif <- reactive({
  #   
  #   df_mes_quota <- df_agent_reduc() %>%
  #     dplyr::group_by(motif) %>%
  #     dplyr::summarise(total = sum(as.numeric(total))/1000) %>%
  #     dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  #     dplyr::mutate(mesures = "après")
  #   
  #   df_sans_mes_quota <- df_agent() %>%
  #     dplyr::group_by(motif) %>%
  #     dplyr::summarise(total = sum(as.numeric(total))/1000) %>%
  #     dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  #     dplyr::mutate(mesures = "avant")
  #   
  #   df_compare <- rbind(df_mes_quota, df_sans_mes_quota)
  #   
  #   return(df_compare)
  #   
  # })
  # 
  # 
  # output$histo_quota_motif <- renderPlot(
  #   
  #   ggplot(df_quota_pour_plot_motif(), aes(x=motif, y=total, fill= factor(mesures, c("avant","après")))) +
  #     geom_col(position = position_dodge()) +
  #     scale_fill_manual(values = c("grey", "black")) +
  #     labs(x = "Motif des missions", y = "Emission en tonne de CO2", fill = "mesures") 
  #   
  # )
  # 
  
  
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
  
  
# adapter les quotas plus finement
 
  
  pourcentage_field_international <- reactive({
    df_missions %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2)))
    
    
    df_missions_noext <-  df_missions %>%
      select(agent, statut, motif, destination, CO2eq_kg) %>%
      filter(motif== "etude_terrain") %>%
      filter(statut != "externes")
    
    df_missions_noext_international <-  df_missions %>%
      select(agent, statut, motif, destination, CO2eq_kg) %>%
      filter(motif== "etude_terrain") %>%
      filter(statut != "externes")  %>%
      filter(destination %in% c("Afrique", "Asie", "Amériques", "Proche-Orient"))
    
    num_agent_international <- nrow(table(df_missions_noext_international$agent, df_missions_noext_international$destination))
    num_agent_all <- nrow(table(df_missions_noext$agent, df_missions_noext$destination))
    
    num_agent_international/num_agent_all
  })
  
  output$text_p_field_international <- renderText(paste0("Pourcentage d'agents (hors externes) avec des missions de terrain/étude hors Europe : ", round(pourcentage_field_international() * 100, 1)," %"))
  
  output$text_quota4_general <- renderText(paste0("Quota permanents non ajusté (permanents, tous motifs) : ", input$mes4," t eCO2 par an"))
  output$text_quota4_intern <- renderText(paste0("Quota permanents ajusté SANS terrain hors Europe (permanents, tous motifs) : ", input$mes4 * (1/ (pourcentage_field_international()*input$ajust1 - pourcentage_field_international() + 1))," t eCO2 par an"))
  output$text_quota4_nointern <- renderText(paste0("Quota permanents ajusté AVEC terrain hors Europe (permanents, tous motifs) : ", input$mes4 * input$ajust1 / (pourcentage_field_international()*input$ajust1 - pourcentage_field_international() + 1)," t eCO2 par an"))
  output$text_quota6_general <- renderText(paste0("Quota général non ajusté (tous personnels, tous motifs) : ", input$mes6," t eCO2 par an"))
  output$text_quota6_intern <- renderText(paste0("Quota général ajusté SANS terrain hors Europe (tous personnels, tous motifs) : ", input$mes6 * (1/ (pourcentage_field_international()*input$ajust1 - pourcentage_field_international() + 1))," t eCO2 par an"))
  output$text_quota6_nointern <- renderText(paste0("Quota général ajusté AVEC terrain hors Europe (tous personnels, tous motifs) : ", input$mes6 * input$ajust1 / (pourcentage_field_international()*input$ajust1 - pourcentage_field_international() + 1)," t eCO2 par an"))
  
  
  
  
  
  ############### !!! corriger nombre ci-dessous ! #######################
  output$diff_avant_apres <- renderPlot(
    
    ggplot(data_plot(), aes(x = Agent_moyen, y = CO2/1000/145)) + geom_col() +
      labs(x = "Agent moyen", y = "Emission en tonne de CO2") 
    # hum le 145 n'est plus bon si tu as modif les jeux de données !
    
  )
  
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
