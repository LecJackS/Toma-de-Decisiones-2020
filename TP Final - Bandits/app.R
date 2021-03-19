# Example of a three armed bandit task using the ShinyPsych package
#
# Code sections:
#   - Section 0: Load Libraries
#   - Section A: assign external values
#   - Section B: Define overall layout
#   - Section C: Define reactive values
#   - Section D: Page layouts
#   - Section F: Event (e.g. button) actions
#       - Section F1: Page navigation button
#       - Section F2: Event Control
#   - Section G: Save Data
# HOLA CAMBIÉ ALGO
# Hola! Pruebo cambiar otra cosa
# Y aca pruebo cambiar mas cosas
# ESTO LO CAMBIÓ ALE: LOS OTROS NO LO VEN porque es un assigment? Lo veo igual! (soy jack)
# Section 0: Load Libraries ====================================================

library(shiny)
library(shinyjs)
library(ShinyPsych)
library(rdrop2)
#token <- drop_auth(new_user =T)
#saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
#token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
#drop_acc(dtoken = token)
drop_auth(rdstoken = "droptoken.rds")
#drop_auth(rdstoken = "droptoken.rds")

# Section A: assign external values ============================================
#includeScript(system.file("myscript.js", package = "mypackage"))



# Dropbox directory to save data
outputDir <- "ShinyPsych/Bandit"

# Vector with page ids used to later access objects
idsVec <- c("Instructions", "Demographics", "Goodbye")

# create page lists for the instructions and the last page
instructions.list <- createPageList(defaulttxt = FALSE,
                                    fileName = "Local_Instructions_Bandit.txt",
                                    globId   = "Instructions")#,
#location = "dropbox")
#demographics.list <- createPageList(fileName = "Demographics")
demographics.list <- createPageList(defaulttxt = FALSE,
                                    fileName = "Local_Demographics.txt",
                                    globId   = "Demographics")
#goodbye.list <- createPageList(fileName = "Goodbye")
goodbye.list <- createPageList(defaulttxt = FALSE,
                                    fileName = "Local_Goodbye.txt",
                                    globId   = "Goodbye")

# prepare a list with game parameters 5 16 16 16 16
banditDistList <- list("nTrials" = c(16, rep(16, 4)),  # trials for practice trial and game trials
                       "distributionType" = matrix(   # draw values from different distributions
                         rep(c("unif", "unif"), 6),
                         ncol = 2, byrow = TRUE),
                       "min" = matrix(c(c(-0.1,0.2),
                                        c(-0.2,0.4),
                                        c(0.2,0.4),
                                        c(-0.4,-0.1),
                                        c(-0.3,-0.1)), # arguments for uniform dist
                                      ncol = 2, byrow = TRUE),
                       "max" = matrix(c(c(0.9,1.2),
                                        c(0.8,1.4),
                                        c(1.2,1.4),
                                        c(0.6,0.9),
                                        c(0.7,0.9)), # arguments for uniform dist
                                      ncol = 2, byrow = TRUE)
)

# create the outcome lists for the bandit, rounded to 1 digit after the comma
banditContainer <- createBanditList(nArms = 2, roundDigits = 0,
                                    distList = banditDistList,
                                    differentDists = TRUE)
# <---------------------------------------------------------------------------------------------
#
.banditCustomMessage <- function(Arms, contrlVals, distribList, sess, containerOb,
                                 rDigits){
  
  if (Arms == 2){
    
    sess$sendCustomMessage(type = 'envBanditTwoArms',
                           list(enOne = containerOb$outcomes[[contrlVals$banditGame]][,1], # outcome values
                                enTwo = containerOb$outcomes[[contrlVals$banditGame]][,2], # outcome values
                                nTrials = distribList$nTrials[contrlVals$banditGame], # number of trials
                                game = contrlVals$banditGame, # number of current game
                                nDigits = rDigits)) # number of digits to round total points to
  }
}
# local multi
localMultiArmedBanditPage <- function(ctrlVals, nArms, distList, container,
                                      roundDigits, session, ...) {
  
  if (nArms >= 2 && nArms <= 6){
    
    .banditCustomMessage(Arms = nArms, contrlVals = ctrlVals,
                         distribList = distList, sess = session,
                         containerOb = container, rDigits = roundDigits)
    
    banditFunNames <- c("localTwoArmedBanditPage", "threeArmedBanditPage",
                        "fourArmedBanditPage", "fiveArmedBanditPage",
                        "sixArmedBanditPage")
    
    banditPage <- get(banditFunNames[nArms-1])(ctrlVals, ...)
    
  } else {
    
    stop(paste(nArms,
               "is no valid input for nArms. It must be a number between 2 and 6."))
  }
  
  banditPage
  
}
# <---------------------------------------------------------------------------------------------
# function from n_armned_bandit_page.R
localTwoArmedBanditPage <- function(ctrlValsList, gameTitle = "Ronda ",
                                    nTrials = NULL, clickCounter = "Clics restantes",
                                    gameTitlePractice = "Ronda de práctica",
                                    withPracticeGame = TRUE,
                                    pointTitle = "Puntos ganados", nGames = 10,
                                    buttonLabel = "Clic para continuar a la siguiente ronda"){
  
  if (isTRUE(withPracticeGame)){
    list(
      shiny::fixedRow(
        
        # call the neGame() javascript function to prepare objects
        shiny::tag("script", 'newGame();'),
        
        # set up written info displayed above the boxes
        shiny::column(12,
                      shiny::fixedRow(shiny::br()),
                      shiny::fixedRow(shiny::column(12, align = "left",
                                                    shiny::h3(
                                                      ifelse(ctrlValsList$banditGame == 1,
                                                             gameTitlePractice,
                                                             paste(gameTitle,
                                                                   ctrlValsList$banditGame - 1,
                                                                   "de ", nGames))))),
                      shiny::fixedRow(
                        shiny::column(6, align="right",
                                      shiny::p(id = "clicksRemaining", style="font-size: 90%", nTrials)),
                                      #shiny::HTML('<p id="clicksRemaining" style="font-size: 100%"
                                     #             html="nTrials"><p/>')),
                        shiny::column(6, align="left",
                                      shiny::h6(class = "upperParams", style="font-size: 70%", clickCounter))
                                      #shiny::HTML('<p id="clicksRemaining" style="font-size: 100%">clickCounter<p/>'))
                      ),
                      
                      shiny::fixedRow(
                        shiny::column(6, align="right",
                                      shiny::p(id = "pointCounter", style="font-size: 90%", "0")),
                                      #shiny::HTML('<p id="pointCounter" style="font-size: 100%">0<p/>')),
                        shiny::column(6, align="left",
                                      shiny::h6(class = "upperParams", style="font-size: 70%", pointTitle))
                      ),
                      
                      shiny::fixedRow(shiny::br()),
                      
                      # set up logic to update the boxes via javascript.
                      # Arguments passed to js come from newGame() js function and
                      # the sendCustomMessage called just before this function
                      shiny::fixedRow(
                        #shiny::column(1, align="center",
                        #              shiny::HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                        shiny::column(5, align="center",
                                      shiny::h6(style="font-size: 70%", "A"),
                                      shiny::HTML('<h1 id="deck1" style="
                                                                      	width: 2.8em;
                                                                      	height: 2.8em;
                                                                      	margin: 0 auto;
                                                                      	background-color: #FAFAFA;
                                                                      	border:2px solid #666666;
                                                                      	text-align: center;
                                                                      	line-height: 2.8em;
                                                                      	font-size: 200%;
                                                                      	margin-bottom: 1em;"
                                                  onclick="localUpdateBanditTwoArms(\'deck1\', \'deck2\',
                                                  \'pointCounter\', \'clicksRemaining\', enOne, enTwo, ind, 1, outcome, outcomeCum,
                                                  selection, nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits); "> </h1>')
                                      
                        ),
                        shiny::column(5, align="center",
                                      shiny::h6(style="font-size: 70%", "B"),
                                      shiny::HTML('<h1 id="deck2" style="
                                                                      	width: 2.8em;
                                                                      	height: 2.8em;
                                                                      	margin: 0 auto;
                                                                      	background-color: #FAFAFA;
                                                                      	border:2px solid #666666;
                                                                      	text-align: center;
                                                                      	line-height: 2.8em;
                                                                      	font-size: 200%;
                                                                      	margin-bottom: 1em;"
                                                                      	onclick="localUpdateBanditTwoArms(\'deck2\', \'deck1\',
                                                  \'pointCounter\', \'clicksRemaining\', enTwo, enOne, ind, 2, outcome, outcomeCum,
                                                  selection, nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits)"> </h1>')),
                        #shiny::column(1, align="center",
                        #              shiny::HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>')),
                        shiny::column(12, align="center", shiny::hr()),
                        
                        shiny::column(12, align="center", 
                                      
                                      shiny::sidebarLayout(
                                        shiny::sidebarPanel(
                                          shiny::column(12, align="center",
                                                        shinyjs::hidden(
                                                          shiny::radioButtons("pregunta1", h4("¿Cuál creés que es la mejor caja en esta ronda?"),
                                                                              choiceNames = list(h4("A"), h4("B")),
                                                                              choiceValues = list(1,2),
                                                                              selected = 0)
                                                        )),
                                          shiny::column(12, align="center",
                                                        shinyjs::hidden(
                                                          shiny::radioButtons("pregunta2", h4("¿Cuál creés que es la mejor caja en esta ronda?"),
                                                                              choiceNames = list(h4("A"), h4("B")),
                                                                              choiceValues = list(1,2),
                                                                              selected = 0)
                                                        ))
                                        ),
                                        shiny::mainPanel(
                                          shiny::column(12, align="center",
                                                        shinyjs::hidden(
                                                          shiny::sliderInput("confianza1", h4("¿Qué tan seguro/a estás?"),
                                                                             min = 0, max = 100,
                                                                             value = 50))
                                          ),
                                          shiny::column(12, align="center",
                                                        shinyjs::hidden(
                                                          shiny::sliderInput("confianza2", h4("¿Qué tan seguro/a estás?"),
                                                                             min = 0, max = 100,
                                                                             value = 50))
                                          ),
                                        )
                                        
                                      )
                                      
                        ),
                        
                        
                      ),
                      
                      # set up the continue Button
                      shiny::fixedRow(
                        shiny::column(6,
                                      shinyjs::hidden(
                                        shiny::actionButton("continueOtherHalf",
                                                            label = "Continuar",
                                                            style =  "margin-top: 2em; margin-left: 5.3em; margin-bottom: 3em" )
                                        
                                        
                                      ),
                                      offset =  4),
                        shiny::column(6,
                                      shinyjs::hidden(
                                        shiny::actionButton("continueBandit",
                                                            label = buttonLabel,
                                                            style =  "margin-top: 2em; margin-left: 5.3em; margin-bottom: 3em" )
                                        
                                        
                                      ),
                                      offset =  4))
        ))
    )
    
  } else{
    print("> Error! Not implemented!")
  }
  
}


# Section B: Define overall layout =============================================

ui <- fixedPage(
  
  title = "ShinyBandit",      # App title
  uiOutput("MainAction"),
  useShinyjs(),# For Shinyjs functions
  includeScriptFiles(fileList = "bandit", nArms = 2), # include appropriate css and js scripts
  #tags$script(src = "Local_banditTwoArms_Functions.js")
  shiny::includeScript("Local_banditTwoArms_Functions.js")
)

server <- function(input, output, session) {
  
  output$MainAction <- renderUI( {
    PageLayouts()
    
  })
  
  # Section C: Define Reactive Values ==========================================
  
  # CurrentValues controls page setting such as which page to display
  CurrentValues <- createCtrlList(firstPage = "instructions", # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = TRUE,           # create a completion code
                                  complName = "EP-Bandit",    # first element of completion code
                                  task = "bandit")            # the task(s) used in the app
  
  # GameData controls task settings and is used to store the task data
  GameData <- createTaskCtrlList(task = "bandit")
  
  # Section D: Page Layouts ====================================================
  
  PageLayouts <- reactive({
    
    # insert created completion code that it can later be displayed
    goodbye.list <- changePageVariable(pageList = goodbye.list, variable = "text",
                                       oldLabel = "completion.code",
                                       newLabel = CurrentValues$completion.code)
    
    # display instructions page
    if (CurrentValues$page == "instructions") {
      
      return(
        # create html logic of instructions page
        createPage(pageList = instructions.list,
                   pageNumber = CurrentValues$Instructions.num,
                   globId = "Instructions", ctrlVals = CurrentValues)
        
      )}
    
    # display task page
    if (CurrentValues$page == "game") {
      
      return(
        # create html logic of task page and handle client side communications
        localMultiArmedBanditPage(ctrlVals = CurrentValues, nArms = 2, distList = banditDistList,
                                  session = session, container = banditContainer, roundDigits = 1,
                                  nTrials = banditDistList$nTrials[CurrentValues$banditGame],
                                  nGames = length(banditDistList$nTrials) - 1, withPracticeGame = TRUE)
        # <-------------------------------------------------------------------------------------------------
        #localTwoArmedBanditPage(ctrlValsList=CurrentValues,
        #                        nTrials = banditDistList$nTrials[CurrentValues$banditGame],
        #                        nGames = length(banditDistList$nTrials) - 1)
        #(ctrlValsList, gameTitle = "Game ",
        #  nTrials = NULL, clickCounter = "Clicks Remaining",
        #  gameTitlePractice = "Practice Game",
        #  withPracticeGame = TRUE,
        #  pointTitle = "Points Earned", nGames = 10,
        #  buttonLabel = "Click to Continue to next Game")
        # <-------------------------------------------------------------------------------------------------
        
        
      )}
    
    
    if (CurrentValues$page == "postPractice"){
      return(
        list(
          tags$br(), #tags$br(), tags$br(),
          h2("Terminaste las pantallas de practica.", class = "firstRow"),
          p(paste("En las pantallas de práctica obtuviste ",
                  GameData$points.cum[length(GameData$points.cum)],
                  "puntos (que no se van a guardar).")),
          p("Ahora empieza el juego de verdad. En total vas a jugar 4 rondas y en cada una vas a poder hacer 16 clics."),
          p("Recordá:"),
          tags$ul(
            tags$li(strong("Las chances de ganar de cada caja no cambian durante la ronda, pero sí cambian de manera aleatoria de una ronda a la siguiente.")),
            tags$li("Entonces, en cada ronda hay una caja mejor que otra (con más chances de dar puntos), aunque al comienzo de la ronda no podés saber cuál es."),
            tags$li("(A medida que jugás, podés aprender para obtener la mayor cantidad de puntos posibles)"),
            p("En algunos momentos te vamos a hacer dos preguntas: cuál creés que es la mejor caja en esa ronda y cuán seguro/a estás de eso (del 1 al 100). Estas respuestas no afectan en nada a tu puntaje o a tus chances de ganar."),  
            p(strong("Recordá que tu objetivo en cada ronda es ganar la mayor cantidad de puntos posibles, pero solo tenés 16 clics: ¡usalos bien!")),
            p(strong("Hacé clic en continuar cuando estés preparado/a para empezar.")),
            tags$br(),
            actionButton(inputId = "gt_game",
                         label = "Empezar Ronda 1", class = "continueButtons")
          )
        )
      )
    }
    
    # 4) END OF GAME PAGE
    if (CurrentValues$page == "endGame") {
      return(
        div(class = "gameInfo", checked = NA,
            list(
              tags$br(), tags$br(),tags$br(),
              p(paste("Terminaste la ronda", CurrentValues$banditGame - 2, "con",
                      GameData$points.cum[length(GameData$points.cum)], "puntos.")),
              p("Presioná el botón de abajo cuando estés listo/a para seguir con la siguiente ronda."),
              p("Recordá que las chances de ganar de cada caja van a ser distintas en la siguiente ronda, pero no cambian durante la ronda."),
              tags$br(),
              actionButton(inputId = "gt_games",
                           label = paste0("Empezar Ronda ", CurrentValues$banditGame - 1),
                           class = "continueButtons"))))
    }
    
    if (CurrentValues$page == "lastEndGame") {
      
      return(
        div(class = "gameInfo", checked = NA,
            list(
              tags$br(), tags$br(),tags$br(),
              h3("¡Terminaste todas las rondas!", class = "firstRow"),
              p(paste("En la última ronda ganaste ", GameData$points.cum[length(GameData$points.cum)],
                      "puntos.")),
              p("En total, en las 4 rondas ganaste ", sum(GameData$outcome),
                "puntos."),
              p("Para terminar, en la siguiente pantalla te hacemos algunas últimas preguntas."),
              tags$br(),
              actionButton(inputId = "gt_demographics",
                           label = "Continuar", class = "continueButtons"))))
    }
    
    if (CurrentValues$page == "demographics"){
      
      return(
        createPage(pageList = demographics.list, pageNumber = CurrentValues$Demographics.num,
                   globId = "Demographics", ctrlVals = CurrentValues)
      )}
    
    
    # P5) Goodbye
    if (CurrentValues$page == "goodbye") {
      
      return(
        createPage(pageList = goodbye.list, pageNumber = CurrentValues$Goodbye.num,
                   globId = "Goodbye", ctrlVals = CurrentValues, continueButton = FALSE)
      )}
    
  })
  
  
  # Section F: Event (e.g.; button) actions ======================================
  
  # Section F1: Page Navigation Buttons ----------------------
  
  
  observeEvent(input[["Instructions_next"]],{
    GameData$pregunta  = rep(NA, 5*2)
    GameData$confianza = rep(NA, 5*2)
    # llevo la cuenta de las preguntas hechas
    GameData$idx = 0
    nextPage(pageId = "instructions", ctrlVals = CurrentValues, nextPageId = "game",
             pageList = instructions.list, globId = "Instructions")
  })
  
  observeEvent(input[["pregunta1"]], {
    shinyjs::show("confianza1")
    #print(input[["pregunta"]])
  })
  observeEvent(input[["pregunta2"]], {
    shinyjs::show("confianza2")
    #print(input[["pregunta"]])
  })
  
  observeEvent(input[["confianza1"]], {
    if(input[["confianza1"]] != 50){
      nTrials <- 16
      #print(">>> ind.length before if")
      #print(input[["ind.length"]])
      if(input[["ind.length"]]+1 < nTrials){
        # Middle of a game
        shinyjs::show("continueOtherHalf")
      }
      else{
        # End of game
        shinyjs::show("continueBandit")
      }
    }
    #
    #print(input[["confianza"]])
  })
  
  observeEvent(input[["confianza2"]], {
    if(input[["confianza2"]] != 50){
      nTrials <- 16
      #print(">>> ind.length before if")
      #print(input[["ind.length"]])
      if(input[["ind.length"]]+1 < nTrials){
        # Middle of a game
        shinyjs::show("continueOtherHalf")
      }
      else{
        # End of game
        shinyjs::show("continueBandit")
      }
    }
    #
    #print(input[["confianza"]])
  })
  
  observeEvent(input[["continueOtherHalf"]], {
    # Guardo respuestas
    GameData$idx = GameData$idx + 1
    idx = GameData$idx
    GameData$pregunta[idx]  = ifelse( is.null(input[["pregunta1"]]), -1, input[["pregunta1"]] )
    GameData$confianza[idx] = input[["confianza1"]]
    # Muestro bandits de nuevo
    shinyjs::hide("continueOtherHalf")
    shinyjs::hide("pregunta1")
    shinyjs::hide("confianza1")
    # Reset bandit numbers from before
    shinyjs::html(id="deck1", html="")
    shinyjs::html(id="deck2", html="")
    # Show bandits again
    shinyjs::show("deck1")
    shinyjs::show("deck2")
    
  })
  
  observeEvent(input[["ind.length"]], {
    shinyjs::delay(ms=700, shinyjs::html(id="deck1", html=""))
    shinyjs::delay(ms=700, shinyjs::html(id="deck2", html=""))
    #print(">>> Intento numero:")
    #print(input[["ind.length"]])
    # ind.length starts at zero!
    nTrials <- 16
    if(input[["ind.length"]]+1 == nTrials/2){
      shinyjs::delay(ms=700, shinyjs::hide("deck1"))
      shinyjs::delay(ms=700, shinyjs::hide("deck2"))
      #shinyjs::reset(id="pregunta")
      shinyjs::delay(ms=700, shinyjs::show("pregunta1"))
      #shinyjs::show("pregunta")
      #shinyjs::hide("deck1")
      #shinyjs::hide("deck2")
    }
  })
  
  observeEvent(input[["continueBandit"]], {
    GameData$idx = GameData$idx + 1
    # Guardar respuestas
    #print(GameData$idx)
    idx = GameData$idx
    GameData$pregunta[idx]  = ifelse( is.null(input[["pregunta2"]]), -1, input[["pregunta2"]] )
    GameData$confianza[idx] = input[["confianza2"]]
    # Nueva página
    nextBanditPage(ctrlVals = CurrentValues, distList = banditDistList,
                   gameData = GameData, withPracticeGame = TRUE)
  })
  
  observeEvent(input[["gt_game"]], {
    CurrentValues$page <- "game"
  })
  
  observeEvent(input[["gt_game_pregs"]], {
    CurrentValues$page <- "game"
  })
  
  observeEvent(input[["gt_games"]], {
    CurrentValues$page <- "game"
  })
  observeEvent(input[["gt_demographics"]], {
    GameData$pregunta  = rep(GameData$pregunta,  each=8) #each
    GameData$confianza = rep(GameData$confianza, each=8)
    #print(">>> Final values")
    #print(GameData$pregunta)
    #print(GameData$confianza)
    CurrentValues$page <- "demographics"
  })
  
  # Section F2: Event Control ----------------------
  
  # game control
  observeEvent(input[["gameNr"]], {
    #shinyjs::reset(id="pregunta")
    shinyjs::show("pregunta2")
    #shinyjs::show("titulo")
    
    #shinyjs::toggle("deck1")
    # Esta funcion es la que muestra el boton de continuar
    appendBanditValues(ctrlVals = CurrentValues, distList = banditDistList,
                       input = input, gameData = GameData)
    # Oculto boton ahsta qua responda confianzas
    shinyjs::hide("continueBandit")
  })
  
  # Make sure answers are selected
  observeEvent(reactiveValuesToList(input),{
    
    onInputEnable(pageId = "instructions", ctrlVals = CurrentValues,
                  pageList = instructions.list, globId = "Instructions",
                  inputList = input, charNum = 4)
    
    onInputEnable(pageId = "demographics", ctrlVals = CurrentValues,
                  pageList = demographics.list, globId = "Demographics",
                  inputList = input)
    
    onInputEnable(pageId = "game", ctrlVals = CurrentValues,
                  pageList = banditDistList, globId = "Game",
                  inputList = input)
    
  })
  
  # Section G: Save data =========================================================
  
  saveData <- function(data) {
    outputDir <- "TDD/data"
    data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    #write.csv(data, filePath, row.names = FALSE, quote = TRUE)
    #write.csv(data, file='prueba2.csv', row.names = F, quote = T, sep=',', col.names=colnames(data))
    write.csv(data, filePath, row.names = T, quote = T)
    # Upload the file to Dropbox
    drop_upload(filePath, path = outputDir)
  }
  
  observeEvent(input[["Demographics_next"]], {(
    
    # Create progress message
    withProgress(message = "Saving data...", value = 0, {
      
      incProgress(.25)
      
      # Create a list to save data
      data.list <- list(  "id" = input$Instructions_workerid,
                          "trial" = GameData$trial,
                          "time" = GameData$time,
                          "selection" = GameData$selection,
                          "pregunta" = GameData$pregunta,
                          "confianza"= GameData$confianza,
                          "outcomes" = GameData$outcome,
                          "points.cum" = GameData$points.cum,
                          "game" = GameData$game,
                          "completion.code" = CurrentValues$completion.code,
                          "option.order" = banditContainer$option.order,
                          "age" = input$Demographics_age,
                          "sex" = input$Demographics_sex)
      
      # # save Data
      # if (!is.null(input$Instructions_mail) &&
      #     nchar(input$Instructions_mail) > 4){
      #   saveData(data.list, location = "mail", outputDir = outputDir,
      #            partId = data.list$id, suffix = "_g",
      #            mailSender = "shinypsych@gmail.com",
      #            mailReceiver = input$Instructions_mail,
      #            mailBody = "Your data sent by the ShinyPsych app demo.",
      #            mailSubject = paste("ShinyPsych data for id", data.list$id))
      # } else {
      #   saveData(data.list, location = "dropbox", outputDir = outputDir,
      #            partId = data.list$id, suffix = "_g")
      # }
      print(">>> All data:")
      print(data.frame(data.list))
      saveData(data.frame(data.list))
      CurrentValues$page <- "goodbye"
    })
  )})
  
}

# Create app!
shinyApp(ui = ui, server = server)
