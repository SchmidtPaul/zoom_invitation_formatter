library(shiny)
library(lubridate)
library(stringr)

# Custom function to parse German date format
parse_german_date <- function(date_string) {
  german_months <- c("Jan\\.", "Feb\\.", "März", "Apr\\.", "Mai", "Juni", 
                     "Juli", "Aug\\.", "Sept\\.", "Okt\\.", "Nov\\.", "Dez\\.")
  english_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  for (i in 1:length(german_months)) {
    date_string <- str_replace(date_string, german_months[i], english_months[i])
  }
  
  parsed_date <- parse_date_time(date_string, orders = c("d.b Y H:M p", "d.b Y"))
  return(parsed_date)
}

# UI Definition
ui <- fluidPage(
  titlePanel("Zoom Invite Processor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(
        "Paste the text automatically provided by zoom for each meeting.",
        "You can find your scheduled meetings ", 
        a("here", href = "https://us02web.zoom.us/meeting#/upcoming", target = "_blank"),
        "."
      ),
      textAreaInput("textInput", "Paste your text here:", rows = 10),
      textInput("endTime", "Enter end time (HH:MM)", value = ""),
      textInput("lecturerName", "Name of lecturer", value = "Dr. Paul Schmidt"),
      textAreaInput("additionalInfo", "Additional Info at the end", value = "Hinweise zur Vorbereitung\nhttps://schmidtpaul.github.io/dsfair_quarto/ch/misc/workshopprep.html", rows = 3),
      br(), br()
    ),
    
    mainPanel(
      h3("Original"),
      verbatimTextOutput("contents"),
      uiOutput("translatedContent")
    )
  )
)

# Server-Funktion
server <- function(input, output) {
  originalContent <- reactive({
    if (is.null(input$textInput) || input$textInput == "") {
      return("Waiting for input")
    }
    input$textInput
  })
  
  output$translatedContent <- renderUI({
    if (nchar(input$endTime) > 0 && nchar(input$textInput) > 0) {
      fluidRow(
        column(6, 
               h3("Deutsch"),
               verbatimTextOutput("german"),
               downloadButton("downloadGerman", "Export German")
        ),
        column(6, 
               h3("English"),
               verbatimTextOutput("english"),
               downloadButton("downloadEnglish", "Export English")
        )
      )
    }
  })
  
  editedContent <- reactive({
    if (is.null(input$textInput) || input$textInput == "") {
      return(list(de = "Waiting for input", en = "Waiting for input"))
    }
    
    lines <- strsplit(input$textInput, "\n")[[1]]
    
    # Extract workshop title
    workshop_line <- lines[grep("^Thema:", lines)]
    workshop_title <- str_trim(sub("^Thema:\\s*", "", workshop_line))
    
    # Extract date and time information
    zeit_line <- lines[grep("^Zeit:", lines)]
    jeden_tag_line <- lines[grep("^\\s+Jeden Tag,", lines)]
    
    start_date_time <- str_extract(zeit_line, "\\d+\\.\\w+\\.\\s+\\d{4}\\s+\\d{2}:\\d{2}\\s+[AP]M")
    
    parsed_start <- parse_german_date(start_date_time)
    start_date <- format(parsed_start, "%d. %b. %Y")
    start_time <- format(parsed_start, "%H:%M")
    
    # Format date for filename
    file_date <- format(parsed_start, "%Y%m%d")
    
    # Check if it's a multi-day event
    is_multi_day <- length(jeden_tag_line) > 0
    
    if (is_multi_day) {
      end_date <- str_extract(jeden_tag_line, "\\d+\\.\\w+\\.\\s+\\d{4}")
      parsed_end <- parse_german_date(end_date)
      end_date <- format(parsed_end, "%d. %b. %Y")
      date_string_de <- sprintf("Tage: %s - %s", start_date, end_date)
      date_string_en <- sprintf("Days: %s - %s", start_date, end_date)
    } else {
      date_string_de <- sprintf("Datum: %s", start_date)
      date_string_en <- sprintf("Date: %s", start_date)
    }
    
    # Extract URL
    url_line <- lines[grep("^https://", lines)]
    url <- str_trim(url_line)
    
    # Extract Meeting-ID and Kenncode
    meeting_id_line <- lines[grep("^Meeting-ID:", lines)]
    meeting_id <- str_trim(sub("^Meeting-ID:\\s*", "", meeting_id_line))
    
    kenncode_line <- lines[grep("^Kenncode:", lines)]
    kenncode <- str_trim(sub("^Kenncode:\\s*", "", kenncode_line))
    
    formatted_content_de <- paste(
      sprintf("%s lädt Sie zu einem geplanten Zoom-Meeting ein.\n\n", input$lecturerName),
      sprintf("Workshop: %s\n", workshop_title),
      sprintf("%s\n", date_string_de),
      sprintf("Zeit: %s - %s\n\n", start_time, input$endTime),
      "Beitreten Zoom Meeting\n",
      sprintf("%s\n\n", url),
      sprintf("Meeting-ID: %s\n", meeting_id),
      sprintf("Kenncode: %s\n\n", kenncode),
      input$additionalInfo,
      sep = ""
    )
    
    formatted_content_en <- paste(
      sprintf("%s is inviting you to a planned zoom meeting.\n\n", input$lecturerName),
      sprintf("Workshop: %s\n", workshop_title),
      sprintf("%s\n", date_string_en),
      sprintf("Time: %s - %s\n\n", start_time, input$endTime),
      "Join zoom meeting\n",
      sprintf("%s\n\n", url),
      sprintf("Meeting-ID: %s\n", meeting_id),
      sprintf("Code: %s\n\n", kenncode),
      input$additionalInfo,
      sep = ""
    )
    
    list(de = formatted_content_de, en = formatted_content_en, 
         file_date = file_date, workshop_title = workshop_title)
  })
  
  output$contents <- renderText({ originalContent() })
  output$german <- renderText({ if (nchar(input$endTime) > 0 && nchar(input$textInput) > 0) editedContent()$de })
  output$english <- renderText({ if (nchar(input$endTime) > 0 && nchar(input$textInput) > 0) editedContent()$en })
  
  output$downloadGerman <- downloadHandler(
    filename = function() {
      content <- editedContent()
      sanitized_title <- gsub("[^[:alnum:]]", "_", content$workshop_title)
      paste0("ZoomEinladung_", content$file_date, "_", sanitized_title, ".txt")
    },
    content = function(file) {
      cat(editedContent()$de, file = file)
    }
  )
  
  output$downloadEnglish <- downloadHandler(
    filename = function() {
      content <- editedContent()
      sanitized_title <- gsub("[^[:alnum:]]", "_", content$workshop_title)
      paste0("ZoomInvitation_", content$file_date, "_", sanitized_title, ".txt")
    },
    content = function(file) {
      cat(editedContent()$en, file = file)
    }
  )
}

# Start the Shiny app
shinyApp(ui = ui, server = server)