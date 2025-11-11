library(shiny)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)
library(shinythemes)
library(tidyr)
library(shinydashboard)

df_missions <- read.csv("df_missions.csv", sep = ";", dec = ",")
quota_max <- 10 #quota max t par an pour définition max des sliders

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
  "En cliquant dans le menu à gauche sur les onglets Paramétrages, vous pourrez sélectionner quelques mesures (report modal, quotas). Le résultat en terme de pourcentage de réduction s'affichera sur la gauche.",
  br(),
  br(),
  br(),
  br(),
  em("Cette application a été conçue par Emmanuel Discamps dans le but d'aider l'ensemble des personnels de TRACES lors des ateliers EcoTRACES visant à la conception de mesures de réduction de nos émissions. Merci à Marc Thomas pour son aide dans le codage des premières versions."),
  br(),
)


ui_tab2 <- fluidPage(
  
h3(strong("Appliquer des mesures de report modal")),
"Le report modal consiste à remplacer l'avion par d'autres moyens de transport (ex : train) pour certaines destinations.",
"Ici, les vols de correspondance (par exemple un trajet Toulouse - Paris avant de prendre un vol international depuis Paris) ne sont PAS concernés.",
tags$br(),br(),
sliderInput("distance_plane",
            strong("Distance (km) en dessous de laquelle l'avion est interdit (hors correspondances, trajet aller)"),
            min = 0,
            max = 3000,
            value = 0,
            step = 100),
h6("Vous pouvez utiliser les flèches du clavier pour ajuster plus précisément"),
em("Dans le contexte de TRACES, les correspondances approximatives entre distance et durée de trajet en train sont :"),br(),
em("- environ 700 km pour 5h ou 6h de trajet (afin d'exclure Paris), ce qui surestime le % de réduction (car cela exclu Dijon, Genève, Madrid, Nantes, Nice, etc qui ne devraient pas l'être)"),br(),
em("- environ 900 km pour 7h de trajet"),br(),
em("- environ 1000 km pour 8h de trajet"),br(),
tags$br(),

checkboxGroupInput("countries_plane", strong("Destinations non autorisées en avion (hors correspondances)"),
                   choices = c(unique(df_missions$destination)),
                   selected = NULL,
                   inline = F
),
tags$br(),
  
  tags$br(),
  plotOutput("detail_transport"),
  tags$br(),
  plotOutput("detail_destination"),
  # DT::dataTableOutput("destination")
  # DT::dataTableOutput("transport")
)


ui_tab3 <- fluidPage(
  

  h3(strong("Appliquer des mesures de quotas")),
 
  radioButtons("type_quotas", "Type de quotas", c("Individuels", "Collectif", "Hybride")),
  
  conditionalPanel("input.type_quotas == 'Collectif'", 
  sliderInput("quota_collectif","Quota 100% collectif (t eq CO2 / an):", 0, 100000, 0),
  "A vous de décider des modalités de répartitions et de distributions des quotas !"),
  
  conditionalPanel("input.type_quotas == 'Hybride'", 
                   br(),
                   h4(strong("Réserve de quota collectif (quotas hybrides) :")),
                  "Ce premier paramètre permet de réserver une partie des émissions pour le collectif.",br(),
                   "Plus cette valeur est élevée, plus les quotas individuels devront être bas pour augmenter le % de réduction des émissions.",
                   sliderInput("quota_hybride","Quota collectif (t eq CO2 / an):", 0, 150, 0),),
  
  conditionalPanel("input.type_quotas != 'Collectif'", 
  tags$br(),
  tags$style(".my-class {font-size: 75%; line-height: 1.6;}"),
  h4(strong("Quotas individuels (tous motifs) :")),
  
  h5("Définissez d'abord des quotas généraux pour toutes les missions, en fonction des statuts (personnels = permanents, docs/post-docs, associés)."),
  h5("La distinction avec/sans recherche hors Europe permet de différencier les membres dont les zones de terrains/études privilégiées sont extra-européennes."),
  h5("Pour rappel, un vol AR en avion hors Europe consomme environ 2 t eCO2/an."),
  tags$br(),
  
  sliderInput("mes_pers_all_close",
              "quota personnels sans recherche hors Europe, tous motifs, t eCO2/an",
              min = 0,
              max = quota_max,
              value = quota_max,
              step = 0.25),
  
  sliderInput("mes_pers_all_far",
              "quota personnels AVEC recherche hors Europe, tous motifs, t eCO2/an",
              min = 0,
              max = quota_max,
              value = quota_max,
              step = 0.25),

  sliderInput("mes_perm_all_close",
              "quota permanents sans recherche hors Europe, tous motifs, t eCO2/an",
              min = 0,
              max = quota_max,
              value = quota_max,
              step = 0.25),
  sliderInput("mes_perm_all_far",
              "quota permanents AVEC recherche hors Europe, tous motifs, t eCO2/an",
              min = 0,
              max = quota_max,
              value = quota_max,
              step = 0.25),
  sliderInput("mes_docspostdocs_all_close",
              "quota docs/postdocs sans recherche hors Europe, tous motifs, t eCO2/an",
              min = 0,
              max = quota_max,
              value = quota_max,
              step = 0.25),
  sliderInput("mes_docspostdocs_all_far",
              "quota docs/postdocs AVEC recherche hors Europe, tous motifs, t eCO2/an",
              min = 0,
              max = quota_max,
              value = quota_max,
              step = 0.25),
  sliderInput("mes_ext_all",
              "quota externes, tous motifs, t eCO2/an",
              min = 0,
              max = quota_max,
              value = quota_max,
              step = 0.25),
  br(),
  h4(strong("Quotas individuels (colloques) :")),
  h5("Vous pouvez ensuite définir des quotas plus stricts pour les colloques (personnels = permanents, docs/post-docs, associés)."),
  tags$br(),
  sliderInput("mes_pers_coll",
              "quota personnels, colloques, t eCO2/an",
              min = 0,
              max = quota_max,
              value = quota_max,
              step = 0.25), 
 
  sliderInput("mes_perm_coll",
              "quota permanents, colloques, t eCO2/an",
              min = 0,
              max = quota_max,
              value = quota_max,
              step = 0.25),
  sliderInput("mes_docspostdocs_coll",
              "quota docs/postdocs, colloques, t eCO2/an",
              min = 0,
              max = quota_max,
              value = quota_max,
              step = 0.25),
  sliderInput("mes_ext_coll",
              "quota externes, colloques,t eCO2/an",
              min = 0,
              max = quota_max,
              value = quota_max,
              step = 0.25),

  
  
  # Show a plot of the generated distribution
  br(),br(),
  h5(strong("Impact de la réduction sur les BGES par statut :")),
  tags$br(),
  plotOutput("histo_quota"),
  h5(strong("Répartition des émissions par agent (tou.tes) :")),
  tags$br(),
  plotOutput("histo_agent_all"),
  h5(strong("Répartition des émissions par agent (permanents) :")),
  tags$br(),
  plotOutput("histo_agent_perm"),
  
  h5(strong("Répartition des émissions par agent (docs & postdocs) :")),
  tags$br(),  
  plotOutput("histo_agent_docspostdocs"),


  tags$br(),
 
  
  #### multiplicateur = n'est plus utilisé
  # h4(strong("Multiplicateur d'ajustement pour terrains éloignés :")),
  # h5("Vous pouvez ajuster ici plus finement les quotas entre personnels ayant des besoins différents (par exemple, de missions sur le terrain à l'international)."),
  # h5("Vous pouvez ici multiplier le quota des agents avec des missions de terrains à l'international, tout en maintenant le même taux de réduction :"),
  # sliderInput("ajust1",
  #             "Facteur de multiplication du quota des agents avec du terrain hors Europe, par rapport aux autres",
  #             min = 1,
  #             max = 30,
  #             value = 1,
  #             step = 0.25),
  # textOutput("text_p_field_international"),
  # textOutput("text_quota6_general"),
  # textOutput("text_quota6_intern"),
  # textOutput("text_quota6_nointern"),
  # textOutput("text_quota4_general"),
  # textOutput("text_quota4_intern"),
  # textOutput("text_quota4_nointern"),
  
  br(),
  h4(strong("Mesure d'exemption pour missions longues :")),
  h5("Vous pouvez également décider d'exclure des quotas les missions longues, en utilisant le paramètre ci-dessous."),
  sliderInput("days_slider",
              strong("Durée minimale des missions longues (en jours) :"),
              min = 1,
              max = 300,
              value = 300,
              step = 1),
  
  "Au-delà de ce nombre de jours, la mission ne compte plus dans le quota par agent.",br(),
  "Ex : si vous sélectionnez 30, seules les missions de moins d'un mois seront décomptées dans le quota d'un agent."
)
)




ui_tab4 <- fluidPage(
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
  dashboardSidebar(width = 400,
    sidebarMenu(style = "position: fixed;",
                menuItem("Introduction", tabName = "tab1"),
                menuItem("Paramétrages du report modal", tabName = "tab2"),
                menuItem("Paramétrages des quotas", tabName = "tab3"),
                menuItem("Tableaux de données", tabName = "tab4"),

      br(),
      textOutput("text_report_bges_original"),
      tags$br(),
      tags$br(),
      h5(strong("Bilan après mise en place des mesures sélectionnées :")),
      textOutput("text_quota_bges_reduit"), 
      textOutput("text_quota_p_reduc"),
      tags$br(),
      h5(strong("Mesures de report modal :")),
      textOutput("text_report_mesures_dist"),
    textOutput("text_report_mesures_pays1"),
    textOutput("text_report_mesures_pays2"),
      textOutput("text_report_n_missions"),
    tags$br(),
    h5(strong("Mesures de quotas :")),
    textOutput("text_report_type_quotas"),
    textOutput("text_report_quota_collectif"),
    textOutput("text_report_quota_hybride"),
    conditionalPanel("input.type_quotas != 'Collectif'",
     htmlOutput("text_quotas"),
     textOutput("text_multi"),
     textOutput("text_report_missions_longues"),
     textOutput("text_quota_n_agent")),
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
      ),
      tabItem("tab4",
              ui_tab4
      )
      
     
    ),

    
  )
  
 
)


### SERVER ###
server <- function(input, output, session) {

  ####################################### calcul du BGES original avant mesures #######################################
  
  bges_original <- reactive({
    sum(df_missions$CO2eq_kg)
  })

  observeEvent(input$type_quotas, {
    updateSliderInput(session, "quota_collectif", value = ceiling(as.numeric(bges_original()/3000)), max = ceiling(as.numeric(bges_original()/3000)))
  }
               )
  
  output$bges_original <- reactive({
    sum(df_missions$CO2eq_kg)
  })
  output$text_report_bges_original <- renderText(paste0("BGES sur 3 ans (avant mesures) : ",round(bges_original()/1000,1)," t eCO2"))
  
  
  ####################################### report modal #######################################
  
  df_missions_reduc <- reactive({
    df_missions %>% 
      dplyr::filter(!(mode == "plane" & distance_km < (input$distance_plane*2))) %>%
      dplyr::filter(!(mode == "plane" & destination %in% input$countries_plane))
    
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
  
  
  output$text_report_mesures_dist <- renderText(ifelse(input$distance_plane==0,"" ,paste("Distance avion interdit : <", input$distance_plane, "km")))
  output$text_report_mesures_pays1 <- renderText(ifelse(is.null(input$countries_plane), "", "Destinations avion interdites :"))
  output$text_report_mesures_pays2 <- renderText(paste(paste(input$countries_plane, sep = ",")))
  output$text_report_n_missions <- renderText(paste("Nombre estimé de missions impactées par le report modal :",
                                                    nrow(df_missions)-nrow(df_missions_reduc())))
  
  
  df_missions_reduc_pour_plot <- reactive({
    
    df_mes_modal <- df_missions %>% 
      dplyr::filter(!(mode == "plane" & distance_km < (input$distance_plane*2))) %>% #la distance est multipliée par 2, puisque la distance labos1pt5 est AR
      dplyr::filter(!(mode == "plane" & destination %in% input$countries_plane)) %>%
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
      dplyr::filter(!(mode == "plane" & destination %in% input$countries_plane)) %>%
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
  
  
  ############tentative d'integrations des deux types de mesures ensembles
  
  ##output text pour resumer sur le côté
  output$text_report_type_quotas <- renderText(paste("Type de quotas : ",input$type_quotas))
  output$text_report_quota_hybride <- renderText(ifelse(input$type_quotas=="Hybride",paste("Quota collectif : ",input$quota_hybride, "t/an"),""))
  output$text_report_quota_collectif <- renderText(ifelse(input$type_quotas=="Collectif",paste("Quota collectif :", input$quota_collectif, "t/an"),""))
  output$text_report_missions_longues <- renderText(ifelse(input$days_slider == 300, "", paste("Exemption pour missions longues : au-delà de ", input$days_slider, "jours")))
  
  
  
  df_agent <- reactive({
  
    # # identification des agents avec terrains éloignés = ancienne version, auto mais marche mal
    # df_type_terrain_agents <-  df_missions %>%
    #   select(agent, statut, motif, destination) %>%
    #   filter(motif== "etude_terrain") %>%
    #   group_by(agent) %>%
    #   arrange(agent) %>%
    #   mutate(type_terrain = case_when(
    #     destination %in% c("Afrique", "Asie", "Amériques", "Proche-Orient") ~  "far",
    #     TRUE ~ "close"))    %>%
    #   select(agent, type_terrain) %>%
    #   group_by(agent, type_terrain) %>%
    #   tally() %>%
    #   spread(type_terrain, n) %>%
    #   mutate(type_terrain = case_when(
    #     far>0 ~  "far",
    #     TRUE ~ "close")) %>%
    #   select(agent, type_terrain)

    
  
            # les missions longues sont ecartees ici
    df <- df_missions_reduc() %>%
      dplyr::mutate(motif = case_when(
        days > input$days_slider ~  "missions_longues",
        TRUE ~ motif))

    
    x <- df %>%
      select(agent, statut, motif, CO2eq_kg, type_terrain) %>%
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
    p_longues <- sum(x$missions_longues) / (sum(x$total)-sum(x$inconnu))
    
    
    # y <- x %>%
    #   mutate(colloques = round(colloques + inconnu*p_coll,2)) %>%
    #   mutate(etude_terrain = round(etude_terrain + inconnu*p_etude,2)) %>%
    #   mutate(autres = round(autres + inconnu*p_autres,2)) %>%
    #   select(-inconnu)
    
    y <- x %>%
      mutate(colloques = colloques + inconnu*p_coll) %>%
      mutate(etude_terrain = etude_terrain + inconnu*p_etude) %>%
      mutate(autres = autres + inconnu*p_autres) %>%
      mutate(missions_longues = missions_longues + inconnu*p_longues) %>%
      select(-inconnu)
    
    return(y)
    # ### ancienne version avec calcul auto des terrains eloignes
    # z <- merge(y, df_type_terrain_agents, by.x=1, by.y = 1, all = T)
    # 
    # 
    #  return(z)
  })
  
  
  
  df_agent_reduc <- reactive({
    
    ### mesure : quota perm, colloques
    quota_perm_coll <- input$mes_perm_coll * 3000
    
  previous_df <- df_agent() %>%
      dplyr::mutate(colloques = case_when(
        statut == "permanents" & colloques > quota_perm_coll ~ quota_perm_coll,
        TRUE ~ colloques)) %>%
      dplyr::mutate(total_quota = colloques + etude_terrain + autres) %>%
    dplyr::mutate(total = total_quota + missions_longues)
    
    ### mesure : quota ext, colloques
    quota_ext_coll <- input$mes_ext_coll * 3000
    previous_df <- previous_df %>%
      dplyr::mutate(colloques = case_when(
        statut == "externes" & colloques > quota_ext_coll ~ quota_ext_coll,
        TRUE ~ colloques)) %>%
      dplyr::mutate(total_quota = colloques + etude_terrain + autres)%>%
      dplyr::mutate(total = total_quota + missions_longues)

    ### mesure : quota personnels, colloques
    quota_pers_coll <- input$mes_pers_coll * 3000
    previous_df <- previous_df %>%
      dplyr::mutate(colloques = case_when(
        statut != "externes" & colloques > quota_pers_coll ~ quota_pers_coll,
        TRUE ~ colloques)) %>%
      dplyr::mutate(total_quota = colloques + etude_terrain + autres)%>%
      dplyr::mutate(total = total_quota + missions_longues)

    ### mesure : quota docspostdocs, colloques
    quota_docspostdocs_coll <- input$mes_docspostdocs_coll * 3000
    previous_df <- previous_df %>%
      dplyr::mutate(colloques = case_when(
        statut == "doc_postdoc" & colloques > quota_docspostdocs_coll ~ quota_docspostdocs_coll,
        TRUE ~ colloques)) %>%
      dplyr::mutate(total_quota = colloques + etude_terrain + autres) %>%
      dplyr::mutate(total = total_quota + missions_longues)

    
    
    ### AVANT DE PASSER AU "tous motifs", nécessaire de simplifier le tableau
    previous_df <- previous_df %>%
      dplyr::select(-c("colloques","autres","etude_terrain"))
    
    
    
    ### mesure : quota perm, tout far
    quota_perm_all_far <- input$mes_perm_all_far * 3000
    previous_df <- previous_df %>%
      dplyr::mutate(total_quota = case_when(
        statut == "permanents" & type_terrain == "far" & total_quota >  quota_perm_all_far ~  quota_perm_all_far,
        TRUE ~ total_quota))%>%
      dplyr::mutate(total = total_quota + missions_longues)

    ### mesure : quota perm, tout close
    quota_perm_all_close <- input$mes_perm_all_close * 3000
    previous_df <- previous_df %>%
    dplyr::mutate(total_quota = case_when(
        statut == "permanents" & type_terrain == "close" & total_quota >  quota_perm_all_close ~  quota_perm_all_close,
        TRUE ~ total_quota))%>%
      dplyr::mutate(total = total_quota + missions_longues)


### mesure : quota ext, tout
quota_ext_all <- input$mes_ext_all * 3000
previous_df <- previous_df %>%
  dplyr::mutate(total_quota = case_when(
    statut == "externes" & total_quota >  quota_ext_all ~  quota_ext_all,
    TRUE ~ total_quota))%>%
  dplyr::mutate(total = total_quota + missions_longues)

    ### mesure : quota docspostdocs, tout far
    quota_docspostdocs_all_far <- input$mes_docspostdocs_all_far * 3000
    previous_df <- previous_df %>%
      dplyr::mutate(total_quota = case_when(
        statut == "doc_postdoc"& type_terrain == "far" & total_quota >  quota_docspostdocs_all_far ~  quota_docspostdocs_all_far,
        TRUE ~ total_quota))%>%
      dplyr::mutate(total = total_quota + missions_longues)

    ### mesure : quota docspostdocs, tout close
    quota_docspostdocs_all_close <- input$mes_docspostdocs_all_close * 3000
    previous_df <- previous_df %>%
      dplyr::mutate(total_quota = case_when(
        statut == "doc_postdoc" & type_terrain == "close" & total_quota >  quota_docspostdocs_all_close ~  quota_docspostdocs_all_close,
        TRUE ~ total_quota))%>%
      dplyr::mutate(total = total_quota + missions_longues)
    
    
    ### mesure : quota personnels, tout far
    quota_pers_all_far <- input$mes_pers_all_far * 3000
    previous_df <- previous_df %>%
      dplyr::mutate(total_quota = case_when(
        statut != "externes"& type_terrain == "far" & total_quota > quota_pers_all_far ~ quota_pers_all_far,
        TRUE ~ total_quota)) %>%
    dplyr::mutate(total = total_quota + missions_longues)

    ### mesure : quota personnels, tout close
    quota_pers_all_close <- input$mes_pers_all_close * 3000
    previous_df <- previous_df %>%
      dplyr::mutate(total_quota = case_when(
        statut != "externes" & type_terrain == "close" & total_quota > quota_pers_all_close ~ quota_pers_all_close,
        TRUE ~ total_quota)) %>%
      dplyr::mutate(total = total_quota + missions_longues)

    return(previous_df)
    
  })

  
  bges_reduit <- reactive({
    case_when(
      input$type_quotas == "Individuels" ~ sum(df_agent_reduc()$total),
      input$type_quotas == "Collectif" ~ input$quota_collectif*3000,
      input$type_quotas == "Hybride" ~ sum(df_agent_reduc()$total)+input$quota_hybride*3000
                                               )
    
  })
  
  pourcentage_reduction <- reactive({
    1 - bges_reduit() / sum(df_missions$CO2eq_kg)
  })
  
    output$text_quota_bges_reduit <- renderText(paste0("BGES réduit : ", round(bges_reduit()/1000,1)," t eCO2"))
    output$text_quota_p_reduc <- renderText(paste0("Pourcentage de réduction : ", round(pourcentage_reduction() * 100, 1)," %"))
    output$text_quota_n_agent <- renderText(paste("Nombre estimé d'agents impactés par les quotas :", length(which(round(df_agent_reduc()$total,1) < round(df_agent()$total,1)))))
    # length(which(round(df_agent_reduc()$total,1) < round(df_agent()$total,1)))))


  data_plot <- reactive({
    data.frame(Agent_moyen = c("avant", "après"), CO2 = c(sum(df_agent()$total), sum(df_agent_reduc()$total))) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))
    
  })
  
  
  output$text_quotas <- renderUI({
    str1 <- ifelse(input$mes_pers_all_far==quota_max, "", paste("Quota personnels avec rech. hors Europe, tous motifs :", input$mes_pers_all_far, "t/an"))
    str2 <- ifelse(input$mes_ext_all==quota_max, "", paste("Quota externes, tous motifs :", input$mes_ext_all, "t/an"))
    str3 <- ifelse(input$mes_perm_all_far==quota_max, "", paste("Quota permanents avec rech. hors Europe, tous motifs :", input$mes_perm_all_far, "t/an"))
    str4 <- ifelse(input$mes_docspostdocs_all_far==quota_max, "", paste("Quota docspostdocs avec rech. hors Europe, tous motifs :", input$mes_docspostdocs_all_far, "t/an"))
    str5 <- ifelse(input$mes_pers_coll==quota_max, "", paste("Quota personnels, colloques :", input$mes_pers_coll, "t/an"))
    str6 <- ifelse(input$mes_ext_coll==quota_max, "", paste("Quota externes, colloques :", input$mes_ext_coll, "t/an"))
    str7 <- ifelse(input$mes_perm_coll==quota_max, "", paste("Quota permanents, colloques :", input$mes_perm_coll, "t/an"))
    str8 <- ifelse(input$mes_docspostdocs_coll==quota_max, "", paste("Quota docspostdocs, colloques :", input$mes_docspostdocs_coll, "t/an"))
    str1bis <- ifelse(input$mes_pers_all_close==quota_max, "", paste("Quota personnels sans rech. hors Europe, tous motifs :", input$mes_pers_all_close, "t/an"))
    str3bis <- ifelse(input$mes_perm_all_close==quota_max, "", paste("Quota permanents sans rech. hors Europe, tous motifs :", input$mes_perm_all_close, "t/an"))
    str4bis <- ifelse(input$mes_docspostdocs_all_close==quota_max, "", paste("Quota docspostdocs sans rech. hors Europe, tous motifs :", input$mes_docspostdocs_all_close, "t/an"))
    
    markdown(paste(str1bis, str1, str3bis,str3,str4bis,str4,str2,str5,str6,str7,str8, sep = '\n\n'))
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
  
  
# # adapter les quotas plus finement avec multiplicateur = n'est plus utilisé
#  
#   pourcentage_field_international <- reactive({
#     df_missions %>%
#       dplyr::mutate(across(where(is.numeric), \(x) round(x, 2)))
#     
#     
#     df_missions_noext <-  df_missions %>%
#       select(agent, statut, motif, destination, CO2eq_kg) %>%
#       filter(motif== "etude_terrain") %>%
#       filter(statut != "externes")
#     
#     df_missions_noext_international <-  df_missions %>%
#       select(agent, statut, motif, destination, CO2eq_kg) %>%
#       filter(motif== "etude_terrain") %>%
#       filter(statut != "externes")  %>%
#       filter(destination %in% c("Afrique", "Asie", "Amériques", "Proche-Orient"))
#     
#     num_agent_international <- nrow(table(df_missions_noext_international$agent, df_missions_noext_international$destination))
#     num_agent_all <- nrow(table(df_missions_noext$agent, df_missions_noext$destination))
#     
#     num_agent_international/num_agent_all
#   })
#   
#   output$text_p_field_international <- renderText(paste0("Pourcentage d'agents (hors externes) avec des missions de terrain/étude hors Europe : ", round(pourcentage_field_international() * 100, 1)," %"))
#   
#   output$text_multi <- renderText(ifelse(input$ajust1 == 1, "", paste("Multiplicateur terrains hors EU : x", input$ajust1)))
#   
#   output$text_quota4_general <- renderText(paste0("Quota permanents non ajusté (permanents, tous motifs) : ", input$mes_perm_all," t eCO2 par an"))
#   output$text_quota4_intern <- renderText(paste0("Quota permanents ajusté SANS terrain hors Europe (permanents, tous motifs) : ", input$mes_perm_all * (1/ (pourcentage_field_international()*input$ajust1 - pourcentage_field_international() + 1))," t eCO2 par an"))
#   output$text_quota4_nointern <- renderText(paste0("Quota permanents ajusté AVEC terrain hors Europe (permanents, tous motifs) : ", input$mes_perm_all * input$ajust1 / (pourcentage_field_international()*input$ajust1 - pourcentage_field_international() + 1)," t eCO2 par an"))
#   output$text_quota6_general <- renderText(paste0("Quota général non ajusté (tous personnels, tous motifs) : ", input$mes_pers_all," t eCO2 par an"))
#   output$text_quota6_intern <- renderText(paste0("Quota général ajusté SANS terrain hors Europe (tous personnels, tous motifs) : ", input$mes_pers_all * (1/ (pourcentage_field_international()*input$ajust1 - pourcentage_field_international() + 1))," t eCO2 par an"))
#   output$text_quota6_nointern <- renderText(paste0("Quota général ajusté AVEC terrain hors Europe (tous personnels, tous motifs) : ", input$mes_pers_all * input$ajust1 / (pourcentage_field_international()*input$ajust1 - pourcentage_field_international() + 1)," t eCO2 par an"))
#   

  #histo repartition emissions par agent = le plus important
  
  dfplot_agent_all <-  reactive({
    x <- df_agent() %>% arrange(total)
    x <- cbind(x, n_agent = seq(1, length(x$agent), 1))
    x <- x %>% mutate(mesures = "avant") %>% select(n_agent, total, mesures)
    
    y <- df_agent_reduc() %>% arrange(total)
    y <- cbind(y, n_agent = seq(1, length(y$agent), 1))
    y <- y %>% mutate(mesures = "après") %>% select(n_agent, total, mesures)
    
    df_compare <- rbind(x, y)
    
    return(df_compare)
  })
  
  dfplot_agent_perm <-  reactive({
    x <- df_agent() %>% arrange(total) %>% filter(statut == "permanents")
    x <- cbind(x, n_agent = seq(1, length(x$agent), 1))
    x <- x %>% mutate(mesures = "avant") %>% select(n_agent, total, mesures)
    
    y <- df_agent_reduc() %>% arrange(total) %>% filter(statut == "permanents")
    y <- cbind(y, n_agent = seq(1, length(y$agent), 1))
    y <- y %>% mutate(mesures = "après") %>% select(n_agent, total, mesures)
    
    df_compare <- rbind(x, y)
    
    return(df_compare)
  })
  
  dfplot_agent_docspostdocs <-  reactive({
    x <- df_agent() %>% arrange(total) %>% filter(statut == "doc_postdoc")
    x <- cbind(x, n_agent = seq(1, length(x$agent), 1))
    x <- x %>% mutate(mesures = "avant") %>% select(n_agent, total, mesures)
    
    y <- df_agent_reduc() %>% arrange(total) %>% filter(statut == "doc_postdoc")
    y <- cbind(y, n_agent = seq(1, length(y$agent), 1))
    y <- y %>% mutate(mesures = "après") %>% select(n_agent, total, mesures)
    
    df_compare <- rbind(x, y)
    
    return(df_compare)
  })
  
  output$histo_agent_all <- renderPlot(
    ggplot(dfplot_agent_all(), aes(n_agent, total/3000, fill= factor(mesures, c("avant","après"))))+
      geom_col(position = position_identity())+
      scale_fill_manual(values = c("grey", "black")) +
      labs(x = "Agents (tous statuts)", y = "Emissions missions (t eqCO2 / an)", fill = "mesures")
  )
  
  output$histo_agent_perm <- renderPlot(
    ggplot(dfplot_agent_perm(), aes(n_agent, total/3000, fill= factor(mesures, c("avant","après"))))+
      geom_col(position = position_identity())+
      scale_fill_manual(values = c("grey", "black")) +
      labs(x = "Agents (permanents)", y = "Emissions missions (t eqCO2 / an)", fill = "mesures")
  )
  output$histo_agent_docspostdocs <- renderPlot(
    ggplot(dfplot_agent_docspostdocs(), aes(n_agent, total/3000, fill= factor(mesures, c("avant","après"))))+
      geom_col(position = position_identity())+
      scale_fill_manual(values = c("grey", "black")) +
      labs(x = "Agents (docs & postdocs)", y = "Emissions missions (t eqCO2 / an)", fill = "mesures")
  )


  
}

# Run the application 
shinyApp(ui = ui, server = server)
