#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinythemes)
library(gt)
library(rstanarm)
library(readxl)
library(ggthemes)
library(gtsummary)
library(broom.mixed)

ovs_low <- readRDS("ovs-low.Rds")

ovs_high <- readRDS("ovs-high.Rds")

ovs_by_pres <- readRDS("ovs-by-pres.Rds")

regression <- readRDS("regression.Rds")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
                
                #united theme provides nice orange look
                
                navbarPage(
                    "Exploring the Supreme Court",
                    tabPanel("Home",
                             titlePanel("Exploring the Ideological Tendencies of the 
                            Supreme Court"),
                             br(),
                             br(),
                             selectInput("graph_choice", "Choose OVS Direction:", 
                                         c("Low", "High")),
                             
                             #slider graph adds nice touch to choose between low/high
                             
                             h4("Description of Opposite Voter Score (OVS)"),
                             p("The Opposite Voter Score (OVS) for each justice represents 
                 the average percentage of the opposing ideological bloc of
                 the Court alongside which the justice voted on each narrowly 
                 decided case. For 
                   this project, the expected ideological bloc was determined 
                   by the party of the president who appointed the justice."),
                             br(),
                             h4("Method"),
                             p("The OVS was created by combining multiple datasets, 
                 filtering data down to narrowly decided cases and 
                   calculating the number of justices of each ideology on both
                   sides of every case. The data includes cases with votes of
                   6-3, 5-4, 4-4 and 3-3. For each justice in each case, the 
                   number of members of the opposite ideological bloc who voted
                   along with him or her was tabulated, as well as the total
                   number of ideologically opposed justices on the Court. The
                   aggregate of the latter was divided by the aggregate of the
                   former to find the decimal OVS. A percentage, rather than
                   a raw number, was used to prevent biases that could arise 
                   from the ideologically skewed nature of a given Court."),
                             br(),
                             
                             #heavy descriptions at the top, but graph is still
                             #immediately visible, so it is fine
                             
                             plotOutput("ovsPlot"),
                             
                             br(),
                             p("Eight out of nine current justices are among the lowest
                   OVS ratings. The ninth, Justice Amy Coney Barrett, has not 
                   yet been included in the data."),
                             
                             #light captions to add to the graph analysis
                             
                             br(),
                             p("The plot below shows the average OVS ratings of each 
                   modern president's Supreme Court appointments. Those with 
                   lower scores have proven most successful in introducing their 
                   ideology into the Court."),
                             
                             plotOutput("ovspresPlot"),
                             
                             br(),
                             p("Barack Obama's two selections, Justices Elena Kagan and 
                   Sonia Sotomayor, have proven consistently liberal. George 
                   H.W. Bush's selection of David Souter, who shifted to become 
                   a reliable member of the liberal voting bloc, significantly 
                   raises his average OVS."),
                             br()
                             
                             #finish the tab with br() for extra space
                             
                             
                             
                    ),
                    
                    tabPanel("Model",
                             titlePanel("Regression Model"),
                             br(),
                             h4("Premise"),
                             p("This model relies on a simple measurement of Decision Ratio,
                   the number of majority votes divided by the number of minority 
                   votes. More tightly decided cases will have lower Decision 
                   Ratios (a 5-4 decision will have a ratio of 1.25, while a 
                   7-2 will have a 3.5), and this (specifically for a given 
                   legal issue) is the output number under the
                   Beta column off of which the lower numbers are based. The 
                   model regresses Decision Ratio on the type of legal issue at
                   stake and whether or not the decision altered an existing
                   legal precedent."),
                             br(),
                             p("There are seven types of legal issues listed in the model, 
                 each with a unique identifier number. They are defined as 
                   follows:"),
                             p("10050 - search and seizure (other than as pertains to 
                   vehicles or Crime Control Act)"),
                             p("20020 - Voting Rights Act of 1965, plus amendments"),
                             p("20260 - immigration and naturalization: permanent 
                   residence"),
                             p("30010 - First Amendment, miscellaneous 
                   (cf. comity: First Amendment)"),
                             p("30140 - campaign spending (cf. governmental corruption)"),
                             p("30160 - free exercise of religion"),
                             p("30180 - parochiaid: government aid to religious schools, 
                   or religious requirements in public schools"),
                             p("50020 - abortion: including contraceptives"),
                             br(),
                             p("The model also features a variable called 
                   precedentAlteration, for which a value of 1 indicates that 
                   a decision has altered a legal precedent."),
                             br(),
                             
                             gt_output("regressionTable"),
                             
                             #avoid drawing conclusions below, statistically insig.
                             
                             br(),
                             h4("Analysis"),
                             p("The (Intercept) of 3.0 (which falls between a 6-3 and 7-2 
                 decision on the full Court) indicates that this is the predicted
                   mean Decision Ratio of search and seizure cases. The model
                   also predicted the following mean Decision Ratios for each
                   type of legal issue. Note that the precedentAlteration Beta
                   value of -.62 suggests that the alteration of a precedent
                   may be accompanied by a far tighter judicial decision."),
                             p("Voting Rights Act of 1965, plus amendments - 3.10"),
                             p("immigration and naturalization: permanent residence - 2.83"),
                             p("First Amendment, miscellaneous (cf. comity: First 
                 Amendment) - 2.56"),
                             p("campaign spending (cf. governmental corruption) - 2.40"),
                             p("free exercise of religion - 3.32"),
                             p("parochiaid: government aid to religious schools, 
                   or religious requirements in public schools - 2.85"),
                             p("abortion: including contraceptives - 2.65"),
                             br(),
                             p("Recall that higher Decision Ratios indicate greater 
                   consensus on the Court. This data might suggest that the 
                   most contentious issues facing the Court are those that 
                   pertain to abortion, campaign spending, the First Amendment and 
                   government aid to religious schools/religious requirements
                   in public schools. It may similarly suggest that the free 
                   exercise of religion, in contrast with the intersection of 
                   religion and schools, generates large consensus on the 
                   Court."),
                             br(),
                             h4("Conclusions"),
                             p("However, each confidence interval, because it includes 0,
                   renders our conclusions statistically insignificant at the 
                   95% confidence level. While some decisions are closer than
                   others (e.g. the First Amendment) to being conclusive, we 
                   are unable to interpret any of the results in such a way."),
                             br()
                             
                             #finish the tab with br() for extra space
                             
                             
                    ),
                    
                    tabPanel("About",
                             titlePanel("Background"),
                             h3("Overview"),
                             h4("The Supreme Court is one of the United States' foremost 
                    institutions. Each US president covets the opportunity to 
                    appoint justices who share his ideological leanings, and
                    satisfaction with those appointments typically rests on 
                    the extent to which they remain faithful to their liberal 
                    or conservative judicial philosophy. This project explores 
                    the ideological consistency of Supreme Court justices, 
                    more specifically how frequently they vote out of lockstep 
                    from the other justices appointed by presidents of the same
                    party. It also reviews the extent to which other factors, 
                    including the type of legal issue at stake and whether the
                    decision set a precedent, affect how decisively the Court
                    rules."),
                             br(),
                             h3("Data"),
                             
                             #have larger headings than usual and larger text
                             
                             h4("This project uses data from the following sources:"),
                             h4(a("The Supreme Court Database",
                                  href = "http://scdb.wustl.edu/data.php")),
                             p("This comphrensive database by Washington University Law has 
                   records of every case dating back decades, with individual 
                   justice votes and other information regarding legal issues 
                   and precedents."),
                             h4(a("US Presidents",
                                  href = "https://www.kaggle.com/harshitagpt/us-presidents")),
                             p("This data by Harshita Gupta provides information on each 
                 U.S. president, including party affiliation."),
                             h4(a("Supreme Court Justices",
                                  href = "https://data.world/kgarrett/supreme-court-justices")),
                             p("This dataset, created by Kelly Garrett, contains information
                   including the president who appointed each justice."),
                             br(),
                             h3("About Me"),
                             p("I am a first-year at Harvard College with an interest in 
                   data science. I will likely study Social Studies or 
                   Government, and I am deeply interested in the Supreme Court 
                   and American government. I created this project with the hope 
                   of better understanding how the Supreme Court and its 
                   justices function."),
                             br(),
                             
                             #provide LinkedIn URL via a() command
                             
                             p("Feel free to contact me via ",
                               a("LinkedIn.",
                                 href = "https://www.linkedin.com/in/lucasgazianis/")),
                             br(),
                             p("The source code on Github can be found ", 
                               a("here.", 
                                 href = "https://github.com/lgazianis/supreme-court-project")),
                             br()
                             
                             #finish the tab with br() for extra space
                             
                             
                    )
                    
                    
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$ovsPlot <- renderPlot({
        if(input$graph_choice == "Low"){
            ovs_low %>%
                ggplot(mapping = aes(x = fct_reorder(justiceName, 
                                                     opposite_voting_score), 
                                     y = opposite_voting_score,
                                     color = Party, fill = CurrentStatus)) + 
                geom_col() + 
                scale_fill_manual(name = "On Current Court", 
                                  values = c("pink", "orange")) + 
                scale_color_manual(values = c("blue", "red")) + 
                theme_excel() + 
                theme(plot.title = element_text(face = "bold"),
                      axis.text = element_text(angle = 30)) + 
                labs(title = "Lowest Opposite Voting Scores (OVS) for Justices",
                     subtitle = "8/9 Current Justices Among the Lowest",
                     x = "Justice",
                     y = "Opposite Voting Score")
            
        } else{
            ovs_high %>%
                ggplot(mapping = aes(x = fct_reorder(justiceName, 
                                                     opposite_voting_score),
                                     y = opposite_voting_score,
                                     color = Party, fill = CurrentStatus)) + 
                geom_col() + 
                scale_fill_manual(name = "On Current Court", 
                                  values = c("pink", "orange")) + 
                scale_color_manual(values = c("blue", "red")) + 
                theme_excel() + 
                theme(plot.title = element_text(face = "bold"),
                      axis.text = element_text(angle = 30)) + 
                labs(title = "Highest Opposite Voting Scores (OVS) for Justices",
                     subtitle = "None Currently Reside on Supreme Court",
                     x = "Justice",
                     y = "Opposite Voting Score")
            
        }
        
        
    })
    
    
    output$ovspresPlot <- renderPlot({
        ovs_by_pres %>%
            ggplot(mapping = aes(x = president, y = pres_score, fill = Party)) + 
            geom_col(alpha = .7) + 
            scale_fill_manual(values = c("blue", "red")) + 
            labs(title = "Avg. Justice Opposite Voting Score (OVS) by Appointing President",
                 subtitle = "Percentage of Justices from Opposite Ideological Block Voted For",
                 x = "President Appointed",
                 y = "Average OVS Among Appointed Justices") +
            theme_economist() +
            theme(plot.title = element_text(size = 14),
                  axis.text = element_text(angle = 20),
                  axis.title.x = element_text(hjust = 4))
        
    })
    
    output$regressionTable <- render_gt({
        tbl_regression(regression, intercept = TRUE) %>%
            as_gt() %>%
            tab_header(title = "Regression of SCOTUS Decision Ratio",
                       subtitle = "The Effect of Issue Type and Whether 
                       a Precedent Was Set on Decision Ratio")
        
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
