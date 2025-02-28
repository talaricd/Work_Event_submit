library(shiny)
library(aws.s3)
library(lubridate)
library(shinyjs)

#############################
# Pay Period Functions
#############################

# Generate pay periods based on a start date and number of periods
generate_pay_periods <- function(start_date, num_periods) {
  pay_periods <- data.frame(
    Start_Date = as.Date(character()),
    End_Date = as.Date(character()),
    Period = character(),
    stringsAsFactors = FALSE
  )
  
  current_start <- start_date
  for (i in 1:num_periods) {
    current_end <- current_start + days(13)
    period_text <- paste(current_start, "to", current_end)
    pay_periods <- rbind(
      pay_periods,
      data.frame(
        Start_Date = current_start,
        End_Date = current_end,
        Period = period_text,
        stringsAsFactors = FALSE
      )
    )
    current_start <- current_start + days(14)
  }
  return(pay_periods)
}

# Find the pay period for a given date using a provided pay period table
find_pay_period <- function(date, pay_periods) {
  for (i in 1:nrow(pay_periods)) {
    if (date >= pay_periods$Start_Date[i] && date <= pay_periods$End_Date[i]) {
      return(pay_periods$Period[i])
    }
  }
  return(NA)
}

# Set up pay periods (example: starting on Sunday 2025-02-16, with two pay periods per month for 24 months)
start_date <- as.Date("2025-02-16")  
num_periods <- ceiling(24 * 2)
pp_data <- generate_pay_periods(start_date, num_periods)

#############################
# AWS S3 Data Storage Functions
#############################

# These variables should be set in your environment or via your config method.
bucket <- Sys.getenv("AWS_BUCKET_NAME")      # e.g., "my-shiny-app-bucket"
object_key <- Sys.getenv("AWS_OBJECT_KEY")     # e.g., "data/my_events.csv"

# Function to load data from S3 (returns an empty data frame if not found)
load_data_from_s3 <- function(bucket, object_key) {
  tryCatch({
    obj <- aws.s3::get_object(object = object_key, bucket = bucket)
    # If object is empty or not found, return empty data frame
    if (length(obj) == 0) {
      return(data.frame(
        Event_Name = character(),
        Event_Date = character(),
        Event_Time = character(),
        Event_Duration = numeric(),
        Pay_Period = character(),
        Form_Submission_Timestamp = character(),
        stringsAsFactors = FALSE
      ))
    }
    df <- read.csv(text = rawToChar(obj), stringsAsFactors = FALSE)
    return(df)
  }, error = function(e) {
    # If error (e.g., object not found), return empty data frame
    return(data.frame(
      Event_Name = character(),
      Event_Date = character(),
      Event_Time = character(),
      Event_Duration = numeric(),
      Pay_Period = character(),
      Form_Submission_Timestamp = character(),
      stringsAsFactors = FALSE
    ))
  })
}

# Function to save data to S3
save_data_to_s3 <- function(data, bucket, object_key) {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data, file = tmp, row.names = FALSE)
  aws.s3::put_object(file = tmp, object = object_key, bucket = bucket)
}

#############################
# Shiny App: UI
#############################

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Event Input Form"),
  sidebarLayout(
    sidebarPanel(
      textInput("event_name", "Event Name"),
      dateInput("event_date", "Event Date"),
      textInput("event_time", "Event Time (HHMM)"),
      numericInput("event_duration", "Event Duration (minutes)", value = NA, min = 0),
      actionButton("submit", "Submit"),
      verbatimTextOutput("error_message")
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)

#############################
# Shiny App: Server
#############################

server <- function(input, output, session) {
  # Initialize shared data from S3 into a reactive value.
  shared_data <- reactiveVal(load_data_from_s3(bucket, object_key))
  
  observeEvent(input$submit, {
    # Validate required fields.
    if (input$event_name == "" || input$event_time == "" || is.na(input$event_duration)) {
      output$error_message <- renderText("Error: All fields must be filled out.")
    } else if (!grepl("^([01]\\d|2[0-3])[0-5]\\d$", input$event_time)) {
      output$error_message <- renderText("Error: Event Time must be in HHMM format (military time) without a colon.")
    } else {
      output$error_message <- renderText("")
      
      # Calculate the pay period for the event date.
      pp <- find_pay_period(as.Date(input$event_date), pp_data)
      
      new_entry <- data.frame(
        Event_Name = input$event_name,
        Event_Date = as.character(input$event_date),
        Event_Time = input$event_time,
        Event_Duration = input$event_duration,
        Pay_Period = pp,
        Form_Submission_Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      )
      
      # Append the new entry to the existing shared data.
      updated_data <- rbind(shared_data(), new_entry)
      shared_data(updated_data)
      
      # Save the updated data back to S3.
      save_data_to_s3(updated_data, bucket, object_key)
      
      # Clear input fields.
      updateTextInput(session, "event_name", value = "")
      updateDateInput(session, "event_date", value = Sys.Date())
      updateTextInput(session, "event_time", value = "")
      updateNumericInput(session, "event_duration", value = NA)
    }
  })
  
  output$data <- renderTable({
    shared_data()
  })
}

#############################
# Run the Shiny App
#############################

shinyApp(ui = ui, server = server)
