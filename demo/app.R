

library(shiny)
library(xgboost)
library(dplyr)
library(ggplot2)
library(epiR)
library(lubridate)

load("pub_mods.rda")
source("../functions.R", encoding = "UTF-8")

# Get variable importances for sorting

vi <- lapply(pub_mods$mods,xgb.importance,feature_names = NULL)

vi_gain <- data.frame(Feature = vi[[1]]$Feature, 
                      Gain = vi[[1]]$Gain,
                      stringsAsFactors = F)

for(i in 2:length(vi)){
    vi_gain <- full_join(vi_gain,data.frame(Feature = vi[[i]]$Feature, 
                                            Gain = vi[[i]]$Gain,
                                            stringsAsFactors = F),by = "Feature")
}

vi_gain <- vi_gain %>%
    mutate_all(list( ~ ifelse(is.na(.),0,.))) %>%
    mutate(mean_Gain = rowMeans(select(., starts_with("Gain"))))

get_feats <- function(x){
    x['feature_names']
}

named_char <- function(x,names){
    out = as.character(x)
    names(out) = names
    return(out)
}

feats <- unique(unlist(lapply(pub_mods$mods,get_feats),use.names = F))
case <- as.data.frame(t(pub_mods$medvals))
data <- reactive(case)

mchoice <- c("disp_cat_",
             "amb_meds_",
             "amb_int_",
             "amb_calltypes_",
             "amb_airway_",
             "amb_breathing_",
             "amb_circ_",
             "amb_pulse_",
             "amb_skincond_",
             "amb_breathsounds_",
             "amb_ptmeds_",
             "amb_medhist_")

# Build static plot for score density plot

probs <- quantile(pub_mods$scale, probs = seq(0, 1, .2))
equants <- ecdf(pub_mods$scale)
colors <- c("#0000FF","#0000FF","#00FF00","#FFFF00","#FFA500","#FF0000","#FF0000")
dens <- density(pub_mods$scale)
dens_df <- data.frame(x=dens$x, y=dens$y)
dens_df$quant <- factor(findInterval(dens_df$x,probs))

dens_plot <- ggplot(dens_df, aes(x,y))  + 
    geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) +
    geom_line() +
    scale_x_continuous(breaks=seq(-5,5,1)) +
    scale_fill_manual(values = colors,guide="none") +
    theme_void() +
    theme(axis.text.x = element_text())

roc_plot <- ggplot(pub_mods$roc, aes(x,y, color = get_names(label),group = label))  + 
    geom_line(size = 1) +
    scale_x_continuous(breaks=seq(0,1,0.2)) +
    scale_y_continuous(breaks=seq(0,1,0.2)) +
    theme_void() +
    theme(axis.text.x = element_text(),
          axis.text.y = element_text(),
          legend.position = c(0.8, 0.2),
          legend.title = element_blank())
    
for(i in mchoice){
    assign(i, data.frame(var = feats[grepl(i,feats)],
                         stringsAsFactors = F) %>%
        inner_join(select(vi_gain,var = Feature, mean_Gain),by = "var") %>%
        arrange(desc(mean_Gain)))
    
    assign(i,named_char(get(i)$var,get_names(get(i)$var)))
}

# Define UI for application that draws a histogram
ui <- shinyUI(
    fixedPage(title="openTriage Demo",

    
    titlePanel("openTriage - Prehospital risk score demo"),
    sidebarLayout(
        sidebarPanel(
               
               p("Patient/call information",style = "font-size:24px;"),
               
               radioButtons("amb_pout",
                            "Dispatch Priority",
                            choices = c("1A" = 4,
                                        "1B" = 3,
                                        "2A" = 2,
                                        "2B" = 1),
                            selected = 3, inline = TRUE),
               
               selectizeInput("disp_cat_",
                              "Dispatch category",
                              choices = c(disp_cat_),
                              multiple = T),
               
               sliderInput("disp_age",
                           "Age",
                           min = 18,
                           max = 100,
                           value = pub_mods$medvals["disp_age"]),
               
               radioButtons("disp_gender",
                            "Gender",
                            choices = c("Female" = 1,
                                        "Male" = 0), 
                            inline = TRUE),
               
               selectizeInput("amb_calltypes_",
                              "Ambulance category",
                              choices = c(amb_calltypes_),
                              multiple = T),
               
               radioButtons("amb_crit",
                            "Subjective Patient status",
                            choices = c("Non-Critical" = 0,
                                        "Critical" = 1), 
                            inline = TRUE),
               hr(),
               p("Ambulance interventions",style = "font-size:24px;"),
               
               selectizeInput("amb_int_",
                              "Prehospital interventions",
                              choices = amb_int_[!amb_int_ %in% c("amb_int_o2","amb_int_ekg")],
                              multiple = T,
                              selected = "amb_int_iv"),
        
                sliderInput("amb_int_o2",
                            "Oxygen administration (LPM)",
                            min = 0,
                            max = 25,
                            value = 0),
               
               radioButtons("amb_int_ekg",
                            "12-lead EKG",
                            choices = c("None" = 0,
                                        "Taken" = 1,
                                        "sent to CICU" = 2), 
                            inline = TRUE),
        
                selectizeInput("amb_meds_",
                               "Prehospital medications",
                               choices = c("Other",amb_meds_),
                               multiple = T),
               hr(),
        p("Patient initial vital signs",style = "font-size:24px;"),
        
        sliderInput("amb_v_br",
                    "Breathing rate",
                    min = 0,
                    max = 40,
                    value = pub_mods$medvals["amb_v_br"]),
        sliderInput("amb_v_spo2",
                    "SpO2",
                    min = 60,
                    max = 100,
                    value = pub_mods$medvals["amb_v_spo2"]),
        sliderInput("amb_v_pr",
                    "Pulse",
                    min = 0,
                    max = 200,
                    value = pub_mods$medvals["amb_v_pr"]),
        sliderInput("amb_v_bp",
                    "Systolic BP",
                    min = 0,
                    max = 250,
                    value = pub_mods$medvals["amb_v_bp"]),
        sliderInput("amb_v_temp",
                    "Temperature",
                    min = 30,
                    max = 50,
                    value = pub_mods$medvals["amb_v_temp"]),
        sliderInput("amb_v_gcs",
                    "Glasgow Coma Scale",
                    min = 0,
                    max = 15,
                    value = pub_mods$medvals["amb_v_temp"]),
        radioButtons("amb_v_avpu",
                     "Level of Consciousness",
                     choices = c("Alert" = 4,
                                 "Verbal" = 3,
                                 "Painful" = 2,
                                 "Unconscious" = 1),
                    selected = 4),
        hr(),
        p("Times",style = "font-size:24px;"),
        
        sliderInput("disp_hour",
                    "Hour of call",
                    min = 0,
                    max = 23,
                    value = hour(Sys.time())),
        
        sliderInput("disp_month",
                    "Month of call",
                    min = 1,
                    max = 12,
                    value = month(Sys.time())),
        
        sliderInput("amb_time_disp",
                    "Call to Dispatch time (min)",
                    min = 0,
                    max = 60,
                    value = pub_mods$medvals["amb_time_disp"]),
        
        sliderInput("amb_time_scene",
                    "On-scene time (min)",
                    min = 0,
                    max = 120,
                    value = pub_mods$medvals["amb_time_scene"]),
        
        sliderInput("amb_time_toed",
                    "Transport time to ED (min)",
                    min = 0,
                    max = 120,
                    value = pub_mods$medvals["amb_time_toed"]),
        hr(),
        p("Signs and Symptoms",style = "font-size:24px;"),
        
        selectizeInput("amb_airway_",
                       "Airway",
                       choices = amb_airway_,
                       multiple = T,
                       selected = "amb_airway_Fri"),
        selectizeInput("amb_breathing_",
                       "Breathing",
                       choices = amb_breathing_,
                       multiple = T,
                       selected = "amb_breathing_Normal"),
        selectizeInput("amb_circ_",
                       "Circulation",
                       choices = amb_circ_,
                       multiple = T,
                       selected = "amb_circ_Normal"),
        selectizeInput("amb_pulse_",
                       "Pulse quality",
                       choices = amb_pulse_,
                       multiple = T,
                       selected = "amb_pulse_Regelbunden"),
        selectizeInput("amb_skincond_",
                       "Skin Condition",
                       choices = amb_skincond_,
                       multiple = T,
                       selected = "amb_skincond_Normal"),
        selectizeInput("amb_breathsounds_",
                       "Lung sounds",
                       choices = amb_breathsounds_,
                       multiple = T,
                       selected = "amb_breathsounds_Normal"),
        hr(),
        p("Patient history",style = "font-size:24px;"),
        
        selectizeInput("amb_ptmeds_",
                       "Patient medications",
                       choices = amb_ptmeds_,
                       multiple = T),
        selectizeInput("amb_medhist_",
                       "Medical history",
                       choices = amb_medhist_,
                       multiple = T)),
        mainPanel(fluidPage(width = "60%",
             
             div(style="display: inline-block; padding-bottom: 10px; width: 75px;",submitButton("Predict")),
             div(style="display: inline-block; font-size: 28px; vertical-align:middle;",htmlOutput("ambRisk")),
             tabsetPanel(type = "tabs",
                         tabPanel("Distribution",
                                  h4("Score distribution (quintiles):"),
                                  plotOutput("ambRiskPlot",
                                             height = "100px"),
                                  h4("Component likelihoods:"),
                                  lapply(1:length(pub_mods$mods), function(i) {
                                      htmlOutput(names(pub_mods$mods[i]))
                                  })
                                  ),
                         tabPanel("Diagnostics",
                                  h4("ROC curves"),
                                  plotOutput("ambRiskROC",
                                             height = "300px"),
                                  h4("Diagnostics for triage decisions at this threshold"),
                                  tableOutput("diag")
                                  ),
                         tabPanel("Predictors",
                                  h4("Predictor effects vs. median and overall model gain"),
                                  tableOutput("varimp")
                                  ),
                         tabPanel("About",
                                  tagList(
                                   p(),
                                   "This app demonstrates the behaviour of a risk assessment instrument reflecting a
                                   patient's risk for deterioration at the time of handoff from an ambulance to ED staff based on 
                                   data available in prehospital care records. The instrument is based on simplified models from 
                                   an article currently under review, with a preprint available", 
                                   a("here",href="https://www.medrxiv.org/content/10.1101/19007021v1"),
                                   p(),
                                   "A patient with median values for each predictor included in the models
                                   is described by default. Modify the model parameters in the sidebar to see how the risk assessment
                                   instrument reacts. If no choice is made for the multiple choice values, a missing/other value is assumed.
                                   Multiple choice options are sorted in order of descending average variable importance across all models.",
                                   p(),
                                   "The raw score is displayed for the patient at the top of the screen. The Distribution tab displays
                                   its relative position with respect to the cross-validated scores of the full population. 
                                   Tick this checkbox",div(style = "display: inline-block;width: 10px;height: 10px",checkboxInput("boot","")), 
                                   "to display scores for model predictions based on 10 bootstrap resampled datasets to consider as a measure of model certainty (Experimental feature!)
                                   The likelihood of each component outcome included in the score displayed underneath. ROC curves associated with 
                                   each outcome are provided in the Diagnostics tab, along with the position of the current score on the curves for 
                                   each outcome. Some 2x2 diagnostics for a decision rule using the risk score of the currently described patient 
                                   as a threshold value is provided beneath the ROC curve. Finally, the Predictors tab provides a table
                                   reporting the difference in risk score for each variable, if that variable had been set to the median value."),
                                   p(),
                                   fluidRow(style="text-align: center;padding:60px;",
                                           a(img(src="uu.png", width = "200px"), 
                                             href="http://ucpr.se/projects/emdai/"),
                                           p(),
                                           a(img(src="as.png", width = "200px"), 
                                             href="https://www.akademiska.se/for-vardgivare/verksamhetsomraden/ambulanssjukvard/"),
                                           p(),
                                           a(img(src="vinnova.png", width = "200px"), 
                                             href="https://www.vinnova.se/en/p/emdai-a-machine-learning-based-decision-support-tool-for-emergency-medical-dispatch/")
                                   )
                                   )
                         )
               ))
    )
))

# Define server logic
server <- function(input, output, session) {

    data <- reactive({
            
            out <- case %>%
                mutate(amb_pout = input$amb_pout,
                       disp_age = input$disp_age,
                       disp_gender = input$disp_gender,
                       disp_hour = input$disp_hour,
                       disp_month = input$disp_month,
                       amb_time_disp = input$amb_time_disp,
                       amb_time_scene = input$amb_time_scene,
                       amb_time_toed = input$amb_time_toed,
                       amb_crit = input$amb_crit,
                       amb_int_o2 = input$amb_int_o2,
                       amb_int_ekg = input$amb_int_ekg,
                       amb_v_br = input$amb_v_br,
                       amb_v_spo2 = input$amb_v_spo2,
                       amb_v_pr = input$amb_v_pr,
                       amb_v_bp = input$amb_v_bp,
                       amb_v_temp = input$amb_v_temp,
                       amb_v_gcs = input$amb_v_gcs,
                       amb_v_avpu = as.numeric(input$amb_v_avpu),
                       amb_any = ifelse(length(input$amb_int_) > 0, 1, 0),
                       amb_meds = ifelse(length(input$amb_meds_) > 0, 1, 0),
                       amb_txp = 1) 
            for(i in mchoice){
                out[,names(out) %in% input[[i]]] <- 1
            }
        
            return(out)
    })
    
    
    
    preds <- reactive({
        
        d <- data() %>%
            as.matrix() %>%
            Matrix::Matrix(sparse = TRUE)
        
        out <- lapply(pub_mods$mods, function(i) {
            predict(i,d)
        })
        
        return(out)
        
    })
    
    output$news <- renderText({
        out <- data() %>%
            select(starts_with("amb_v_"),amb_int_o2) %>%
            news_calc()
        
        return(paste("NEWS:",out$news_full))
    })
    
    ambRisk <- reactive({
        
        p <- as.data.frame(preds())
        
        out <- predict_composite(p,
                                 pub_mods$scale,
                                 c(1,1,1))
        return(out)
    })
    
    bootpreds <- reactive({
        
            d <- data() %>%
                as.matrix() %>%
                Matrix::Matrix(sparse = TRUE)
            
            out <- lapply(pub_mods$bootmods, function(i) {
                lapply(i, function(j) {
                    predict(j,d)
                })
            })
            paste(out)
            return(out)  
            
    })
    
    bootambRisk <- reactive({
        
        pred <- lapply(bootpreds(),as.data.frame)
        
        out <- lapply(1:length(pred), function(x,p,s,w){
            predict_composite(p[[x]], s[[x]], w)
        },p = pred,
        s = pub_mods$bootscale,
        w = c(1,1,1))
        
        return(out)
    })
    
    output$bootambRisk <- renderText({
        paste(sd(as.numeric(bootambRisk())))
    })
    
    output$ambRiskPlot <- renderPlot({
        
        p <- ambRisk()
        if(input$boot) pb <- as.numeric(bootambRisk())
         dens_plot +
            { if(input$boot) geom_vline(xintercept = pb, color = "Grey")} +
            geom_vline(xintercept = p) +
            geom_label(aes(x = p, y = 0.03, 
                           label = paste0(round(equants(p)*100)," percentile")))
        
        
    })
    
    lapply(1:length(pub_mods$mods), function(i) {
        output[[names(pub_mods$mods[i])]] <- renderText(paste0(get_names(names(pub_mods$mods[i])),": ",
                                                               round(as.numeric(preds()[i]),3)*100,"% "))

    })
    
    output$ambRisk <- renderText({
        
        paste("Score:",round(ambRisk(),2))
    })
    
    output$ambRiskROC <- renderPlot({
        
        p <- ambRisk()
        
        roc_df <- pub_mods$roc %>%
            group_by(label) %>%
            filter(abs(threshold - rep(p,length(threshold))) == min(abs(threshold - rep(p,length(threshold))))) %>%
            ungroup()

        out <- roc_plot +
            geom_point(data = roc_df, aes(x,y,
                                          fill = get_names(label)),size = 3,shape = 21, color = "black")
        
        return(out)
        
    })
    
    output$varimp <- renderTable({
        
        d0 <- data()
        r <- as.numeric(ambRisk())
        
        if(length(d0) == length(pub_mods$medvals) && 
            names(d0) == names(pub_mods$medvals)){
            
            diff <- data.frame("name" = names(d0),
                            "data" = as.numeric(d0),
                            "median" = pub_mods$medvals) %>%
                filter(data != median) %>%
                left_join(select(vi_gain,name = Feature,mean_Gain), by = "name") %>%
                mutate(diff = data - median,
                       mean_Gain = mean_Gain*100)
            
            if(nrow(diff) > 0){
                
                out <- rep(NA,nrow(diff))
                
                for(i in seq_along(1:nrow(diff))){
                    
                    d <- d0
                    
                    d[names(d) == diff$name[i]] <- diff$median[i]
                    
                    d <- d %>%
                        as.matrix() %>%
                        Matrix::Matrix(sparse = TRUE)
                    
                    
                    p <- lapply(pub_mods$mods, function(i) {
                        predict(i,d)
                    })
                    
                    o <- predict_composite(as.data.frame(p),
                                           pub_mods$scale,
                                           c(1,1,1))
                    out[i] <- o
                }
                
                diff$score <- out
                diff$scorediff <- r - diff$score
                diff$name <- get_names(as.character(diff$name))
                diff <- arrange(diff,desc(abs(scorediff)))
                diff <- select(diff, "Predictor name" = name,"Risk diff. vs. median" = scorediff, "Average gain (%)" = mean_Gain)
                
            }else{
                paste("All variables equal to median!")
            }
            
        }else{
            paste("Non-matching names!")
        }
        
            
        return(diff)
        
    })
    
    output$diag <- renderTable({
            
            p <- ambRisk()
            
            diag_df <- pub_mods$roc %>%
                group_by(label) %>%
                filter(abs(threshold - rep(p,length(threshold))) == min(abs(threshold - rep(p,length(threshold)))))
            
            tt <- lapply(diag_df$tt,epi.tests)
            
            out <- sapply(tt,function(x){
                sapply(x$rval,function(y){
                    paste0(round(y$est,2)," (",
                           round(y$lower,2),"-",
                           round(y$upper,2),")")
                })
            })
            
            colnames(out) <- get_names(diag_df$label)
            out <- out[rownames(out) %in% c("se", "sp", "diag.acc", "diag.or", "ppv", "npv"),]
            rownames(out) <- get_names(rownames(out))
            
            return(out)
            
        },rownames = T)
    
    output$test <- renderPrint({
        print(input$boot)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
