#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(SarpBulletinTools)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## SINGLE AVALANCHE PROBLEM
  ## ************************
  
  output$hzdPlot <- renderPlot({
    
    ## COMPLETELY GENERIC
    if (input$agency == "Gen" & input$avProb == "Gen") {
      
      if (input$withElev == "No") {
      
        ## Load data
        load("TM.rda")
      
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      } else {
        
        ## Load data
        load("TM_Elev.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Alp"), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Alp"), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Alp"), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Alp"), PlotType = "PROB", ProbPlotDR = input$probDR, ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      }
    
    ##  AGENCY ONLY 
    } else if (input$agency != "Gen" & input$avProb == "Gen") {
      
      if (input$withElev == "No") {
      
        ## Load data
        load("TM_Agency.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
      
    } else {
      
      ## Load data
      load("TM_Agency_Elev.rda")
      
      ## Plot chart
      if (input$plotType == "Obs") {
        plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Alp"), PlotType = "Obs")
      } else if (input$plotType == "PredMost") {
        plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Alp"), RuleLwd = 2)
      } else if (input$plotType == "PredProd") {
        plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Alp"), DRClrProdSum = T, RuleLwd = 2)
      } else if (input$plotType == "PredProb") {
        plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Alp"), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
      }
      
    }
      
    ## AV PROBLEM ONLY  
    } else if (input$agency == "Gen" & input$avProb != "Gen") {
      
      if (input$withElev == "No") {

        ## Load data
        load("TM_AvProb.rda")
  
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      } else {
        
        ## Load data
        load("TM_AvProb_Elev.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Alp"), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Alp"), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Alp"), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Alp"), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      }
      
      ## AV PROBLEM ONLY  
    } else if (input$agency != "Gen" & input$avProb != "Gen") {
      
      if (input$withElev == "No") {
      
        ## Load data
        load("TM_Agency_AvProb.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      } else {
        
        ## Load data
        load("TM_Agency_AvProb_Elev.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Alp"), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Alp"), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Alp"), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Alp"), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      }
      
    } else {
      
      
    }
      
    
  })
  
  output$hzdPlot2 <- renderPlot({
    
    ## COMPLETELY GENERIC
    if (input$agency == "Gen" & input$avProb == "Gen") {
      
      if (input$withElev != "No") {
        
        ## Load data
        load("TM_Elev.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Tl"), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Tl"), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Tl"), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Tl"), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      }
      
      ##  AGENCY ONLY 
    } else if (input$agency != "Gen" & input$avProb == "Gen") {
      
      if (input$withElev != "No") {
        
        ## Load data
        load("TM_Agency_Elev.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Tl"), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Tl"), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Tl"), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Tl"), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      }

      ## AV PROBLEM ONLY  
    } else if (input$agency == "Gen" & input$avProb != "Gen") {
      
      if (input$withElev != "No") {
        
        ## Load data
        load("TM_AvProb_Elev.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Tl"), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Tl"), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Tl"), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Tl"), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      }

      ## AV PROBLEM ONLY  
    } else if (input$agency != "Gen" & input$avProb != "Gen") {
      
      if (input$withElev != "No") {
        
        ## Load data
        load("TM_Agency_AvProb_Elev.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Tl"), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Tl"), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Tl"), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Tl"), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      }

    } else {
      
      
    }
    
    
  })
  
  
  output$hzdPlot3 <- renderPlot({
    
    ## COMPLETELY GENERIC
    if (input$agency == "Gen" & input$avProb == "Gen") {
      
      if (input$withElev != "No") {
        
        ## Load data
        load("TM_Elev.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Btl"), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Btl"), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Btl"), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(ELEV = "Btl"), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      }
      
      ##  AGENCY ONLY 
    } else if (input$agency != "Gen" & input$avProb == "Gen") {
      
      if (input$withElev != "No") {
        
        ## Load data
        load("TM_Agency_Elev.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Btl"), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Btl"), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Btl"), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AGENCY = input$agency, ELEV = "Btl"), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      }
      
      ## AV PROBLEM ONLY  
    } else if (input$agency == "Gen" & input$avProb != "Gen") {
      
      if (input$withElev != "No") {
        
        ## Load data
        load("TM_AvProb_Elev.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Btl"), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Btl"), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Btl"), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, ELEV = "Btl"), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      }
      
      ## AV PROBLEM ONLY  
    } else if (input$agency != "Gen" & input$avProb != "Gen") {
      
      if (input$withElev != "No") {
        
        ## Load data
        load("TM_Agency_AvProb_Elev.rda")
        
        ## Plot chart
        if (input$plotType == "Obs") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Btl"), PlotType = "Obs")
        } else if (input$plotType == "PredMost") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Btl"), RuleLwd = 2)
        } else if (input$plotType == "PredProd") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Btl"), DRClrProdSum = T, RuleLwd = 2)
        } else if (input$plotType == "PredProb") {
          plotCTreeModelSingleProbOnHzdChart(CTModelObj, ScenGen = list(AVPROB = input$avProb, AGENCY = input$agency, ELEV = "Btl"), PlotType = "PROB", ProbPlotDR = input$probDR, RuleLwd = 2)
        }
        
      }
      
    } else {
      
    }
    
  })
    
  
  ## DECISION AID
  ## ************
  
  output$hzdChartProb <- renderPlot({
    
    plotHzdChartBase(main = "Single Avalanche Problem")
    points(input$SZ[1], input$LH[1], pch = 21, bg = "blue", cex = 2)
    lines(x = input$SZ, y = rep(input$LH[2], 2), col = "blue", lwd = 3)
    lines(x = rep(input$SZ[2], 2), y = input$LH, col = "blue", lwd = 3)
    if (input$LH[2]>input$LH[1]) {
      lines(x = c(input$SZ[1]-0.5,input$SZ[1]) , y = rep(input$LH[2], 2), col = "blue", lwd = 3, lty = 3)
    }
    if (input$SZ[2]>input$SZ[1]) {
      lines(x = rep(input$SZ[2], 2), y = c(input$LH[1]-0.5,input$LH[1]), col = "blue", lwd = 3, lty = 3)
    }
    
    
  output$histPred <- renderPlot({
    
    ## Load data
    load("TM.rda")
    
    ## Create new data
    NewData <- data.frame(LH = ordered(input$LH[1], levels=c(1:9), labels=LevelsLikelihood),
                          SZ = ordered(input$SZ[1], levels=c(1:9), labels=LevelsSize))
    
    Pred <- 100*as.numeric(predict(CTModelObj$CTree, newdata = NewData, type = "prob"))
    
    b <- barplot(Pred, col=c(ClrDRLow, ClrDRMod, ClrDRCon, ClrDRHig),
            main = "Predicted danger ratings", ylim = c(0,80), ylab = "Assignment probability")
    box()
    axis(1, at = b, labels = c("Low", "Moderate", "Considerable", "High/Ext"))
    text(x = b, y = Pred+5, labels = paste0(round(Pred), "%"))
    
  })  
    
  })
  
})
