library(shiny)
library(shinythemes)
library(DT)
library(summarytools)
library(gtsummary)
library(gt)
library(flextable)
library(ggstatsplot)
library(tidyverse)
library(pander)
library(ggpubr)
library(ggsci)
library(pander)
library(psych)
library(scales)
library(bslib)
library(thematic)
library(foreign)

getReport <- function(x, values = "label", head="label"){
  print(x)
  df <- POST(url = 'https://redcap.ghid-kccr.org/api/',
             body = list(
               content='report',
               format='json',
               report_id= x,
               csvDelimiter='',
               rawOrLabel= values,
               token = "9C435B4AC255981BEFD49E32EB5AD2C2",
               rawOrLabelHeaders= head,
               exportCheckboxLabel='false',
               returnFormat='json'),
             encode = "form")
  content(df, as = "text") %>%
    fromJSON() %>%
    return()
}
pings <- as.data.frame(getReport(167))

#pings <- read_rds("pings2")
ui <- navbarPage(
    selected = "Service Use", collapsible = TRUE, inverse = FALSE,
    title = tags$strong(tags$h2("PING HOUSE CONSULT")),
    theme = shinytheme("spacelab"),
    tabPanel(tags$h5("Descriptives"), icon = icon("user-md"),
             fluidPage(
                 tabsetPanel(
                   navbarMenu("Demography",icon = icon("line-chart"),
                    tabPanel("Age", plotOutput("age")),
                    tabPanel("Sex", plotOutput("sex")),
                    tabPanel("Eduaction", plotOutput("edu")),
                    tabPanel("Location", plotOutput("location")),
                    tabPanel("Occupation", plotOutput("occupation")),
                    tabPanel("Experience", plotOutput("exp")),
                    tabPanel("Education Level by Gender", tableOutput("edug"))
                    ),
                    navbarMenu("Facility",
                               tabPanel("Facility", plotOutput("facility")),
                               tabPanel("Facility Level", plotOutput("level")),
                               tabPanel("Facility Level by Gender", tableOutput("gt")),
                               tabPanel("Occupation by Facility", tableOutput("obf")),
                               tabPanel("Location by Facility", tableOutput("lofa"))
                               ),
                   navbarMenu("Response to PINGS",
                              tabPanel("Response", plotOutput("response")),
                              tabPanel("Perception by leading Occupation", tableOutput("resco")))
                               ))
    ),
    tabPanel(tags$h5("Analytics"), icon = icon("hospital"),
        fluidPage(
             tabsetPanel(
                 tabPanel("Measles",icon = icon("child"), textOutput("measles"), 
                   tags$p(tags$strong(tags$h1("Measles")),
                     tags$br(),
                     tags$p("A viral infection that's serious for small children but is easily preventable by a vaccine."),
                     tags$p("The disease spreads through the air by respiratory droplets produced from coughing or sneezing."),
                     tags$p("Measles symptoms don't appear until 10 to 14 days after exposure."),
                     tags$p("They include cough, runny nose, inflamed eyes, sore throat, fever and a red, blotchy skin rash."),
                     tags$p("There's no treatment to get rid of an established measles infection, but over-the-counter fever reducers 
 or vitamin A may help with symptoms."))
                     ),
 tabPanel("Buruli Ulcer",icon = icon("bug"), textOutput("buruli_Ulcer"),
          tags$p(tags$strong(tags$h1("Buruli Ulcer")),
                 tags$br(),
                 tags$p("A rare and exotic bacterial infection of the skin and soft tissue."), 
                 tags$p("Buruli ulcer is caused by infection with the organism Mycobacterium ulcerans."),
                 tags$p("It's found mainly in tropical areas, including West Africa and Australia."),
                 tags$p("Buruli ulcer often starts as a painless swelling of the affected area, which is usually the arms, legs or face."),
                 tags$p("Large ulcers eventually develop, usually on the arms or legs."),
                 tags$p("Treatment involves antibiotics. Left untreated, it can lead to permanent disfigurement and disability.")
          )),
 tabPanel("Tuberculosis", icon = icon("blind"), textOutput("tuberculosis"),
          tags$p(tags$strong(tags$h1("Tuberculosis")),
                 tags$br(),
                 tags$p("A potentially serious infectious bacterial disease that mainly affects the lungs."),
                 tags$p("The bacteria that cause TB are spread when an infected person coughs or sneezes."),
                 tags$p("Most people infected with the bacteria that cause tuberculosis don't have symptoms."),
                 tags$p("When symptoms do occur, they usually include a cough (sometimes blood-tinged), weight loss, night sweats and fever."),
                 tags$p("Treatment isn't always required for those without symptoms."),
                 tags$p("Patients with active symptoms will require a long course of treatment involving multiple antibiotics.")
          )),
 tabPanel("Malaria",icon = icon("thermometer-full"), textOutput("malaria"),
          tags$p(tags$strong(tags$h1("Malaria")),
                 tags$br(),
                 tags$p("A disease caused by a plasmodium parasite, transmitted by the bite of infected mosquitoes."),
                 tags$p("The severity of malaria varies based on the species of plasmodium."),
                 tags$p("Symptoms are chills, fever and sweating, usually occurring a few weeks after being bitten."),
                 tags$p("People travelling to areas where malaria is common typically take protective drugs before, during and after their trip."),
                 tags$p(" Treatment includes antimalarial drugs.")))
                 )
             )
    ),             
 footer = (
     fluidRow(
                column(4, h6("About PINGS", class = "text-center text-danger"), title = "PINGS (Phone-Based Interventions under Nurse Guidance after Stroke) 
                       is a nurse-led mHealth approach to optimizing BP control among stroke survivors in Ghana."),
                column(4, h6("Team Members", class = "text-center text-warning"), title = "* Dr Alexis Beyuo * Dr Melvin Agbogbatey * Mr Kwaku Duah * Mr Patrick Agasiya Nsor"),
                column(4, h6("Address", class = "text-center text-info"), title = "Global Health and Infectuous Disease Group at the KCCR in Tropical Medicine, KNUST - Kumasi, Ghana"),
     )
 )
)

server <- function(input, output) {
  output$age <- renderPlot({
    age <- as.numeric(pings$age)
    hist(age, main = "Age Distribution", xlab = "Age", ylab = "Count")
   # pings %>% select(age) %>% 
   #   drop_na %>%
   #   ggplot(aes(age)) + 
   #   geom_histogram(aes(y=..density.., fill=..count..), binwidth = 5) +
   #   labs(title = "Graph of Age Distribution", x = "Age", y = "Count") +
   #   geom_density(alpha=.01, fill="darksalmon") +
   #   geom_vline(aes(xintercept = median(age, na.rm=T)),
   #              color="blue", linetype="dashed", size=1) +
   #   scale_fill_gradient("x", low="green", high="red") +
   #   ggthemes::theme_clean()
  })
  
  
  output$sex <- renderPlot({
    pings %>% select(sex) %>% filter(sex!="") %>% drop_na() %>%  pull() %>% table() %>% 
      pie(main="Sex Distribution of Respondents")
})
  
  
  output$edu <- renderPlot({
  pings %>% select(edu) %>% 
    drop_na %>%
    ggplot(aes(edu)) + 
      labs(title = "Education Level", x = "Education Level", y = "Count") +
      geom_bar()+
    ggthemes::theme_clean()
  })
  
  
    output$location <- renderPlot({
      pings %>% select(location) %>% filter(location!="") %>%
        drop_na %>%
        ggplot(aes(location)) + 
        labs(title = "Respondent's Location", x = "Location", y = "Count") +
        geom_bar() +
        ggthemes::theme_clean()+
        coord_flip()
    })
    
    
    output$occupation <- renderPlot({
    pings %>% select(occupation) %>% filter(occupation != "") %>% 
      drop_na() %>% 
        ggplot(aes(occupation)) + 
        labs(title = "Respondent's Occupation", x = "Occupation", y = "Count") +
        geom_bar() +
        ggthemes::theme_clean()+
        coord_flip()
    })
    
    
    output$exp <- renderPlot({
      experience <- as.numeric(pings$experience)
      hist(experience, main = "Years of work Experience", xlab = "Years of work Experience", ylab = "Count")
    })
    
    
   edug <-  pings %>% 
      select(edu, sex) %>% 
      filter(sex != ""&edu != "") %>% 
      tbl_cross(row = edu, 
                col = sex, 
                missing = "no", 
                margin = "column") %>% 
      bold_labels() %>% 
      modify_footnote(all_stat_cols()~"Frequency") %>% 
      modify_caption("**Table 3. Education Level by Gender**")%>% 
      as_gt()
   output$edug <- render_gt(edug)
   
   
    output$facility <- renderPlot({
      pings %>% select(facility) %>% filter(facility != "") %>% 
        drop_na() %>% 
        ggplot(aes(facility)) + 
        labs(title = "Respondent's facility", x = "facility", y = "Count") +
        geom_bar() +
        ggthemes::theme_clean() +
        coord_flip()
    })
    
    
    output$level <- renderPlot({
      pings %>% select(facility_level) %>% filter(facility_level != "") %>% 
        drop_na() %>% 
        ggplot(aes(facility_level)) + 
        labs(title = "Respondent's facility level", x = "facility level", y = "Count") +
        geom_bar() +
        ggthemes::theme_clean()+
        coord_flip()
    })
    
    
      fasex <- pings %>% 
        select(facility_level, sex) %>% 
        filter(sex != ""&facility_level !="") %>% #table() %>% 
        tbl_cross(row = facility_level, col = sex, missing = "no", margin = "column") %>%
        bold_labels() %>% 
        modify_footnote(all_stat_cols()~"Frequency(%)")  %>%
        modify_caption("**Table 1. Response from Facility level by Sex**") %>%
        as_gt()
      output$gt <- render_gt(fasex)
    
      
     ofa <- pings %>% 
        select(occupation, facility) %>%
        filter(facility != "" & occupation != "") %>% 
        tbl_cross(col = facility, row = occupation, missing = "no", margin = "column") %>% 
        bold_labels() %>% 
        modify_footnote(all_stat_cols() ~ "Frequenc(%)") %>% 
        modify_caption("**Table 2. Respondents Occupation by their Facility**")%>%
        as_gt()
     output$obf <- render_gt(ofa)
     
     
     lofa <- pings %>% select(location, facility) %>% 
       filter(facility != ""&location != "") %>% 
       tbl_cross(row = location, col = facility, missing = "no", margin = 'column') %>% 
       bold_labels() %>% 
       modify_footnote(all_stat_cols()~"Frequency(%)") %>% 
       modify_caption("**Table 1. Respondents Occupation by their Facility**") %>% 
       as_gt()
     output$lofa <- render_gt(lofa)
     
     
    output$response <- renderPlot({
      pings %>% 
        pivot_longer(16:27, names_to = "Perception", values_to = "Response") %>% 
        mutate(
          Perception = fct_recode(Perception, 
                                  "PINGS meets my Approval" = "approved",
                                  "PINGS is Appealing" = "appealing",
                                  "I Like PINGS" = "like",
                                  "I Welcome PINGS" = "welcome",
                                  "PINGS seems Fitting" = "fitting",
                                  "PINGS is Suitable" = "suitable",
                                  "PINGS is Applicable" = "applicable",
                                  "PINGS is Good a Match" = "good_match",
                                  "PINGS is Implementable"  = "implementable",
                                  "PINGS is Possible" = "possible",
                                  "PINGS is Doable" = "doable",
                                  "PINGS seems Easy"  = "easy")) %>% 
        filter(Response != "") %>% 
        drop_na() %>% 
        ggplot(aes(y = Perception, fill = Response)) +
        geom_bar()+
        labs(x='Response', y = 'Perception', title = "Respondent's Response to PINGS") +
        scale_fill_manual(values = c("palegreen4", "green4", "red", "orange3", "snow3")) +
        ggthemes::theme_clean() +
        theme(
          legend.position = "bottom"
        )})
    

   pings %>%
      pivot_longer(16:27,names_to = "Response", values_to = "status") %>% 
      mutate(
        Response = fct_recode(Response, 
                              "PINGS meet my Approval" = "approved",
                              "PINGS is Appealing" = "appealing",
                              "I Like PINGS" = "like",
                              "I Welcome PINGS" = "welcome",
                              "PINGS seems Fitting" = "fitting",
                              "PINGS is Suitable" = "suitable",
                              "PINGS is Applicable" = "applicable",
                              "PINGS is Good a Match" = "good_match",
                              "PINGS is Implementable"  = "implementable",
                              "PINGS is Possible" = "possible",
                              "PINGS is Doable" = "doable",
                              "PINGS seems Easy"  = "easy")
      )
    resco <- ping %>% select(occupation, Response) %>% filter(
      occupation == c("Medical Doctor", "Nurse", "Biomedical Lab scientist", "Pharmacist", "Physician Assistant")) %>% 
      tbl_cross(
        row = Response, 
        col = occupation, 
        missing = "no", 
        margin = "column"
      )%>% 
      modify_footnote(all_stat_cols()~"Frequency(%)") %>% 
      modify_caption("**Table 5. Respondent's Response to PINGS by their Occupation**") %>%
      as_gt()
    output$resco <- render_gt(resco)
    
}
shinyApp(ui = ui, server = server)
