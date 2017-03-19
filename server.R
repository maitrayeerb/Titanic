#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library('ggplot2') 
library('gtools')
library('mice') 
library('randomForest') 
source('helpers.R')

train <- read.csv("train.csv", stringsAsFactors = FALSE)
test  <- read.csv("test.csv", stringsAsFactors = FALSE)


data_tr_tst  <- smartbind(train, test) # bind training & test data

data_tr_tst <- splitName(data_tr_tst)
data_tr_tst <- groupFamily(data_tr_tst)
data_tr_tst <- fillMissing(data_tr_tst)
data_tr_tst <- person(data_tr_tst)

train <- data_tr_tst[1:891,]
test <- data_tr_tst[892:1309,]

set.seed(1)


rf <- randomForest(factor(Survived) ~ Pclass + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           Fsize + Person,
                         data = train, ntree = 50)


importance    <- importance(rf)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = importance[,1])

test$Survived <- predict(rf, test)




shinyServer(function(input, output) {

      output$sub_plot <- renderPlot({
        
      if(input$dataSet == "train"){
        if (input$var == "Importance"){
          
          ggplot(varImportance, aes(x=reorder(Variables, Importance), y=Importance)) +
            geom_bar(stat="identity", fill="#53cfff") +
            coord_flip() +
            theme_light(base_size=20) +
            xlab("Variable") +
            ylab("Importance") 
            
        }else{
          if (input$var == "Family"){
            
            ggplot(train, aes(x = FsizeD, fill = factor(Survived))) +
              geom_bar(stat='count', position='dodge') +
              labs(x = 'Traveling alone or with family') +
              theme_light(base_size=20)
            
          }else{
            if (input$var == "Embarked"){
                  ggplot(train, aes(Embarked, fill = factor(Survived))) + 
                    geom_bar(stat='count', position='dodge') +
                    labs(x = 'Embarked')+
                theme_light(base_size=20)
                  
                }else{
              if (input$var == "PClass"){
                    ggplot(train, aes(Pclass, fill = factor(Survived))) + 
                      geom_bar(stat='count', position='dodge') +
                      labs(x = 'Passenger Class') +
                     theme_light(base_size=20)
                    
                  }else{
                if (input$var == "Gender & Age"){
                      ggplot(train, aes(Age, fill = factor(Survived))) + 
                        geom_histogram(binwidth = 18) +
                        labs(x = 'Age')+
                         theme_light(base_size=20)+
                        facet_grid(Person~.)
                }else{
                  if (input$var == "Title"){
                    ggplot(train, aes(Title, fill = factor(Survived))) + 
                      geom_bar(stat='count', position='dodge') +
                      labs(x = 'Title')+
                      theme_light(base_size=20)
                  }
                    }
                    
                  }
                }
                
              }
          }
        
      }  else{
        if(input$dataSet == "test"){
          if (input$var == "Importance"){
            
            ggplot(varImportance, aes(x=reorder(Variables, Importance), y=Importance)) +
              geom_bar(stat="identity", fill="#53cfff") +
              coord_flip() +
              theme_light(base_size=20) +
              xlab("Variable") +
              ylab("Importance") 
          }else{
            if (input$var == "Family"){
              
              ggplot(test, aes(x = FsizeD, fill = factor(Survived))) +
                geom_bar(stat='count', position='dodge') +
                labs(x = 'Traveling alone or with family')+
                theme_light(base_size=20)
              
            }else{
                if (input$var == "Embarked"){
                    ggplot(test, aes(Embarked, fill = factor(Survived))) + 
                      geom_bar(stat='count', position='dodge') +
                      labs(x = 'Embarked')+
                    theme_light(base_size=20)
                    
                  }else{
                    if (input$var == "PClass"){
                      ggplot(test, aes(Pclass, fill = factor(Survived))) + 
                        geom_bar(stat='count', position='dodge') +
                        labs(x = 'Passenger Class')+
                        theme_light(base_size=20)
                      
                    }else{
                      if (input$var == "Gender & Age"){
                        ggplot(test, aes(Age, fill = factor(Survived))) + 
                          geom_histogram(binwidth = 18) +
                          facet_grid(Person~.)+
                          labs(x = 'Age') +
                          theme_light(base_size=20)
                      }else{
                        if (input$var == "Title"){
                          ggplot(test, aes(Title, fill = factor(Survived))) + 
                            geom_bar(stat='count', position='dodge') +
                            labs(x = 'Title')+
                            theme_light(base_size=20)
                        }
                      }
                      
                    }
                  }
                  
                }
              }
            }
            
      }

    })

    
  }
)

