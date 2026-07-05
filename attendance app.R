library(shiny)
library(shinydashboard)
library(timeDate)
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)
library(stringi)
library(tidyverse)
library(tidyr)
library(DT)

# Define file paths for static data (modify these paths as needed)
#setwd("C:\\Users\\ADMIN\\Documents\\VF R\\payment")

EMPLOYEE_FILE <- "Employee.csv"
SHIFT_TYPE_FILE <- "Shift Type.csv"

ui <- dashboardPage(
  dashboardHeader(title = "Attendance Analysis System", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Management", tabName = "upload", icon = icon("database")),
      menuItem("Attendance Report", tabName = "attendance", icon = icon("clock")),
      menuItem("Attendance Summary", tabName = "Summary", icon = icon("chart-bar"),
               menuSubItem("Department Summary", tabName = "department"),
               menuSubItem("Employee Daily Summary", tabName = "employee_daily")),
      menuItem("Absentee Reports", tabName = "absent", icon = icon("user-slash"),
               menuSubItem("Detailed Absentee", tabName = "absent_detailed"),
               menuSubItem("Summary by Employee", tabName = "absent_employee"),
               menuSubItem("Summary by Department", tabName = "absent_department")),
      menuItem("Tardiness Reports", tabName = "tardy", icon = icon("clock"),
               menuSubItem("Detailed Tardiness", tabName = "tardy_detailed"),
               menuSubItem("Summary by Employee", tabName = "tardy_employee"),
               menuSubItem("Summary by Department", tabName = "tardy_department"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        .btn-success {
          background-color: #28a745;
          border-color: #28a745;
        }
        .btn-info {
          background-color: #17a2b8;
          border-color: #17a2b8;
        }
        .btn-primary {
          background-color: #007bff;
          border-color: #007bff;
        }
        .filter-info {
          background-color: #e9f7fe;
          padding: 10px;
          border-radius: 5px;
          margin-bottom: 10px;
          border-left: 4px solid #17a2b8;
        }
        .processing-spinner {
          color: #007bff;
          font-size: 18px;
        }
      "))
    ),
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                # Value Boxes
                valueBoxOutput("total_employees_box", width = 3),
                valueBoxOutput("total_regular_hours_box", width = 3),
                valueBoxOutput("total_overtime_hours_box", width = 3),
                valueBoxOutput("total_holiday_hours_box", width = 3)
              ),
              fluidRow(
                valueBoxOutput("total_absent_days_box", width = 3),
                valueBoxOutput("avg_absent_per_employee_box", width = 3),
                valueBoxOutput("attendance_rate_box", width = 3),
                valueBoxOutput("total_leave_hours_box", width = 3)
              ),
              fluidRow(
                box(width = 12, title = "Department Summary", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    dataTableOutput("dashboard_department_table")
                )
              )
      ),
      
      # Upload Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(width = 12, title = "Upload Required Files", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    p("Please upload required CSV files. Employee and Shift Type data are loaded automatically."),
                    
                    fluidRow(
                      column(3, fileInput("shift_file", "Shift Assignment.csv", accept = ".csv")),
                      column(3, fileInput("leave_file", "Leave Application.csv", accept = ".csv")),
                      column(3, fileInput("biometric_file", "AttendanceInOutExcel.csv", accept = ".csv")),
                      column(3, fileInput("weekly_file", "Weekly Off Assignment.csv", accept = ".csv"))
                    ),
                    fluidRow(
                      column(3, br(), 
                             actionButton("process", "Process Data", class = "btn-success", icon = icon("cogs")),
                             uiOutput("process_spinner")
                      ),
                      column(3, br(), actionButton("refresh_static", "Refresh Employee/Shift Data", class = "btn-info", icon = icon("sync")))
                    ),
                    fluidRow(
                      column(12, 
                             conditionalPanel(
                               condition = "input.process > 0",
                               box(width = 12, title = "File Upload Status", status = "info", solidHeader = TRUE,
                                   tableOutput("file_status_table")
                               )
                             )
                      )
                    )
                ),
                fluidRow(
                  box(width = 12, title = "Processing Status", status = "info",
                      solidHeader = TRUE, collapsible = TRUE,
                      verbatimTextOutput("process_status")
                  )
                )
              )),
      
      # [REST OF YOUR UI CODE REMAINS THE SAME]
      # Attendance Report Tab
      tabItem(tabName = "attendance",
              fluidRow(
                box(width = 12, title = "Filters", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    fluidRow(
                      column(3, selectizeInput("attendance_employee", "Employee", choices = NULL, multiple = TRUE)),
                      column(3, selectizeInput("attendance_department", "Department", choices = NULL, multiple = TRUE)),
                      column(3, dateRangeInput("attendance_date_range", "Date Range", start = Sys.Date() - 30, end = Sys.Date())),
                      column(3, br(), actionButton("attendance_apply_filters", "Apply Filters", class = "btn-primary"),
                             actionButton("attendance_clear_filters", "Clear Filters", class = "btn-default"))
                    )
                )
              ),
              fluidRow(
                uiOutput("attendance_filter_info")
              ),
              fluidRow(
                box(width = 12, title = "Detailed Attendance Report", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    downloadButton("download_attendance", "Download Attendance Report", class = "btn-success"),
                    br(), br(),
                    dataTableOutput("attendance_table")
                )
              )
      ),
      
      # Department Summary Tab
      tabItem(tabName = "department",
              fluidRow(
                box(width = 12, title = "Hours Summary by Department", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    downloadButton("download_department", "Download Department Summary", class = "btn-success"),
                    br(), br(),
                    dataTableOutput("department_table")
                )
              )
      ),
      
      # Employee Daily Summary Tab
      tabItem(tabName = "employee_daily",
              fluidRow(
                box(width = 12, title = "Filters", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    fluidRow(
                      column(3, selectizeInput("daily_employee", "Employee", choices = NULL, multiple = TRUE)),
                      column(3, selectizeInput("daily_department", "Department", choices = NULL, multiple = TRUE)),
                      column(3, dateRangeInput("daily_date_range", "Date Range", start = Sys.Date() - 30, end = Sys.Date())),
                      column(3, br(), actionButton("daily_apply_filters", "Apply Filters", class = "btn-primary"),
                             actionButton("daily_clear_filters", "Clear Filters", class = "btn-default"))
                    )
                )
              ),
              fluidRow(
                uiOutput("daily_filter_info")
              ),
              fluidRow(
                box(width = 12, title = "Employee Daily Summary", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    downloadButton("download_employee_daily", "Download Employee Daily Summary", class = "btn-success"),
                    br(), br(),
                    dataTableOutput("employee_daily_table")
                )
              )
      ),
      
      # Detailed Absentee Report Tab
      tabItem(tabName = "absent_detailed",
              fluidRow(
                box(width = 12, title = "Filters", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    fluidRow(
                      column(3, selectizeInput("absent_employee", "Employee", choices = NULL, multiple = TRUE)),
                      column(3, selectizeInput("absent_department", "Department", choices = NULL, multiple = TRUE)),
                      column(3, dateRangeInput("absent_date_range", "Date Range", start = Sys.Date() - 30, end = Sys.Date())),
                      column(3, br(), actionButton("absent_apply_filters", "Apply Filters", class = "btn-primary"),
                             actionButton("absent_clear_filters", "Clear Filters", class = "btn-default"))
                    )
                )
              ),
              fluidRow(
                uiOutput("absent_filter_info")
              ),
              fluidRow(
                box(width = 12, title = "Detailed Absentee Report", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    downloadButton("download_absent_detailed", "Download Detailed Absentee Report", class = "btn-success"),
                    br(), br(),
                    dataTableOutput("absent_detailed_table")
                )
              )
      ),
      
      # Absentee Summary by Employee Tab
      tabItem(tabName = "absent_employee",
              fluidRow(
                box(width = 12, title = "Filters", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    fluidRow(
                      column(3, selectizeInput("absent_emp_employee", "Employee", choices = NULL, multiple = TRUE)),
                      column(3, selectizeInput("absent_emp_department", "Department", choices = NULL, multiple = TRUE)),
                      column(3, numericInput("absent_emp_min_count", "Minimum Absences", value = 1, min = 1)),
                      column(3, br(), actionButton("absent_emp_apply_filters", "Apply Filters", class = "btn-primary"),
                             actionButton("absent_emp_clear_filters", "Clear Filters", class = "btn-default"))
                    )
                )
              ),
              fluidRow(
                uiOutput("absent_emp_filter_info")
              ),
              fluidRow(
                box(width = 12, title = "Absentee Summary by Employee", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    downloadButton("download_absent_employee", "Download Employee Summary", class = "btn-success"),
                    br(), br(),
                    dataTableOutput("absent_employee_table")
                )
              )
      ),
      
      # Absentee Summary by Department Tab
      tabItem(tabName = "absent_department",
              fluidRow(
                box(width = 12, title = "Absentee Summary by Department", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    downloadButton("download_absent_department", "Download Department Summary", class = "btn-success"),
                    br(), br(),
                    dataTableOutput("absent_department_table")
                )
              )
      ),
      
      # Detailed Tardiness Report Tab
      tabItem(tabName = "tardy_detailed",
              fluidRow(
                box(width = 12, title = "Filters", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    fluidRow(
                      column(3, selectizeInput("tardy_employee", "Employee", choices = NULL, multiple = TRUE)),
                      column(3, selectizeInput("tardy_department", "Department", choices = NULL, multiple = TRUE)),
                      column(3, dateRangeInput("tardy_date_range", "Date Range", start = Sys.Date() - 30, end = Sys.Date())),
                      column(3, numericInput("tardy_min_minutes", "Minimum Tardiness (minutes)", value = 1, min = 1)),
                      column(3, br(), actionButton("tardy_apply_filters", "Apply Filters", class = "btn-primary"),
                             actionButton("tardy_clear_filters", "Clear Filters", class = "btn-default"))
                    )
                )
              ),
              fluidRow(
                uiOutput("tardy_filter_info")
              ),
              fluidRow(
                box(width = 12, title = "Detailed Tardiness Report", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    downloadButton("download_tardy_detailed", "Download Detailed Tardiness Report", class = "btn-success"),
                    br(), br(),
                    dataTableOutput("tardy_detailed_table")
                )
              )
      ),
      
      # Tardiness Summary by Employee Tab
      tabItem(tabName = "tardy_employee",
              fluidRow(
                box(width = 12, title = "Filters", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    fluidRow(
                      column(3, selectizeInput("tardy_emp_employee", "Employee", choices = NULL, multiple = TRUE)),
                      column(3, selectizeInput("tardy_emp_department", "Department", choices = NULL, multiple = TRUE)),
                      column(3, numericInput("tardy_emp_min_instances", "Minimum Instances", value = 1, min = 1)),
                      column(3, br(), actionButton("tardy_emp_apply_filters", "Apply Filters", class = "btn-primary"),
                             actionButton("tardy_emp_clear_filters", "Clear Filters", class = "btn-default"))
                    )
                )
              ),
              fluidRow(
                uiOutput("tardy_emp_filter_info")
              ),
              fluidRow(
                box(width = 12, title = "Tardiness Summary by Employee", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    downloadButton("download_tardy_employee", "Download Employee Summary", class = "btn-success"),
                    br(), br(),
                    dataTableOutput("tardy_employee_table")
                )
              )
      ),
      
      # Tardiness Summary by Department Tab
      tabItem(tabName = "tardy_department",
              fluidRow(
                box(width = 12, title = "Tardiness Summary by Department", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    downloadButton("download_tardy_department", "Download Department Summary", class = "btn-success"),
                    br(), br(),
                    dataTableOutput("tardy_department_table")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store processed data
  values <- reactiveValues(
    timeAttendance = NULL,
    attendancedepartment = NULL,
    employee_daily_summary = NULL,
    absentees_detailed = NULL,
    absence_summary_employee = NULL,
    absence_summary_department = NULL,
    tardiness_detailed = NULL,
    tardiness_summary_employee = NULL,
    tardiness_summary_department = NULL,
    dashboard_summary = NULL,
    dashboard_department = NULL,
    processing_complete = FALSE,
    employees = NULL,
    active_shift = NULL,
    processing = FALSE,
    
    # Filtered datasets
    filtered_timeAttendance = NULL,
    filtered_employee_daily_summary = NULL,
    filtered_absentees_detailed = NULL,
    filtered_absence_summary_employee = NULL,
    filtered_tardiness_detailed = NULL,
    filtered_tardiness_summary_employee = NULL,
    
    # Filter states
    attendance_filters_applied = FALSE,
    daily_filters_applied = FALSE,
    absent_filters_applied = FALSE,
    absent_emp_filters_applied = FALSE,
    tardy_filters_applied = FALSE,
    tardy_emp_filters_applied = FALSE
  )
  
  # Process spinner UI
  output$process_spinner <- renderUI({
    if (values$processing) {
      div(class = "processing-spinner", 
          icon("spinner", class = "fa-spin"),
          " Processing...")
    }
  })
  
  # File status table
  output$file_status_table <- renderTable({
    files <- c("Shift Assignment", "Leave Application", "Attendance Data", "Weekly Off")
    status <- c(
      ifelse(!is.null(input$shift_file), "✅ Uploaded", "❌ Missing"),
      ifelse(!is.null(input$leave_file), "✅ Uploaded", "❌ Missing"),
      ifelse(!is.null(input$biometric_file), "✅ Uploaded", "❌ Missing"),
      ifelse(!is.null(input$weekly_file), "✅ Uploaded", "❌ Missing")
    )
    data.frame(File = files, Status = status)
  }, bordered = TRUE)
  
  # Load static data (employee and shift type) on app start
  observe({
    # Try to load employee data
    if (file.exists(EMPLOYEE_FILE)) {
      tryCatch({
        values$employees <- fread(EMPLOYEE_FILE)
        showNotification("✅ Employee data loaded successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Error loading employee data:", e$message), type = "error")
      })
    } else {
      showNotification("⚠️ Employee file not found. Please upload or check file path.", type = "warning")
    }
    
    # Try to load shift type data
    if (file.exists(SHIFT_TYPE_FILE)) {
      tryCatch({
        values$active_shift <- fread(SHIFT_TYPE_FILE)
        showNotification("✅ Shift type data loaded successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Error loading shift type data:", e$message), type = "error")
      })
    } else {
      showNotification("⚠️ Shift type file not found. Please upload or check file path.", type = "warning")
    }
  })
  
  # Process data when button is clicked - FIXED VERSION
  observeEvent(input$process, {
    # Validate that required files are uploaded and static data is loaded
    if (is.null(input$shift_file) || is.null(input$leave_file) || 
        is.null(input$biometric_file) || is.null(input$weekly_file)) {
      showNotification("❌ Please upload all required files before processing.", type = "error")
      return()
    }
    
    if (is.null(values$employees) || is.null(values$active_shift)) {
      showNotification("❌ Employee or Shift Type data not loaded. Please check file paths.", type = "error")
      return()
    }
    
    # Set processing flag
    values$processing <- TRUE
    
    tryCatch({
      # Show processing message
      output$process_status <- renderText("🔄 Processing data... Please wait. This may take a few moments.")
      
      # Read uploaded files with progress indication
      withProgress(message = 'Reading files...', value = 0.1, {
        employees <- values$employees
        incProgress(0.1, detail = "Reading shift data...")
        shift <- fread(input$shift_file$datapath)
        incProgress(0.1, detail = "Reading leave data...")
        leave <- fread(input$leave_file$datapath)
        incProgress(0.1, detail = "Reading biometric data...")
        biometric <- fread(input$biometric_file$datapath)
        incProgress(0.1, detail = "Reading weekly off data...")
        weekly <- fread(input$weekly_file$datapath)
        active_shift <- values$active_shift
      })
      
      # Process data with progress updates
      withProgress(message = 'Processing data...', value = 0.3, {
        
        # YOUR EXISTING PROCESSING CODE STARTS HERE
        #formats dates
        incProgress(0.1, detail = "Formatting dates...")
        shift[,`:=` (Date = as.Date(`Start Date`,format = "%d-%m-%Y"), 
                     shift_start_date = as.Date(`Start Date`,format = "%d-%m-%Y"),
                     shift_end_date = as.Date(`End Date`,format = "%d-%m-%Y"))]
        employees<-employees[grepl("Active",Status)] # to get active employees
        shift[, Date := as.IDate(`Start Date`,format = "%d-%m-%Y")]
        biometric[, Date := as.IDate(Date,format = "%d-%m-%Y")]
        
        leave[, `:=`(
          Start_Date = as.IDate(dmy(`From Date`)),
          End_Date = as.IDate(dmy(`To Date`))
        )]
        #leave[, `:=`(Start_Date = as.IDate(`From Date`,format = "%d-%m-%Y"),
        #  End_Date = as.IDate(`To Date`,format = "%d-%m-%Y"))]
        
        #format employee Id
        incProgress(0.1, detail = "Formatting employee IDs...")
        employees[, Employee_ID := str_pad(trimws(`Employee Number`), width = 4, side = "left", pad = "0")]
        shift[, Employee_ID := str_pad(trimws(Employee), width = 4, side = "left", pad = "0")]
        weekly[, Employee_ID := str_pad(trimws(`Employee (Employee Details)`), width = 4, side = "left", pad = "0")]
        leave[, Employee_ID := str_pad(trimws(Employee), width = 4, side = "left", pad = "0")]
        biometric[, Employee_ID := str_pad(trimws(BadgeNumber), width = 4, side = "left", pad = "0")]
        
        # data require
        employees1<-employees[grepl("Active",Status)] # to get active employees
        employees2<-employees[grepl("H|P",Grade)]# on hours
        leave<-leave[grepl("Approved",Status)] #to get only approved leaves
        
        #shift----------
        incProgress(0.1, detail = "Processing shift data...")
        #shift type and shift merge
        #format shift type
        active_shift<-active_shift %>% 
          mutate(
            `Shift Type`=ID,
            shift_start_time=`Start Time`,
            shift_end_time=`End Time`,
            `lunch break`=`Unpaid breaks (minutes)`/60) %>% 
          select(`Shift Type`,shift_start_time,shift_end_time,`lunch break`)
        
        #format shift assignment
        shift<-shift %>% 
          mutate(shift_end_date=as.Date(shift_end_date),
                 shift_end_date=
                   ifelse(
                     grepl("NIGHT",toupper(`Shift Type`))|`Shift Type`=="ROO SECURITY OFFSITE SHIFT",
                     ymd(paste(shift_end_date+days(1))),
                     ymd(shift_end_date)),
                 shift_end_date= as.Date(shift_end_date,format = "%d-%m-%Y")) %>% 
          select(Employee_ID,`Shift Type`,shift_start_date,shift_end_date,Date)
        
        #merge
        shift_combined<-active_shift[shift,on=.(`Shift Type`)]
        
        #combined data shift start and end
        shift_completed<-shift_combined %>% 
          mutate(shift_start_date_time=ymd_hms(paste(shift_start_date,shift_start_time)),
                 shift_end_date_time=ymd_hms(paste(shift_end_date,shift_end_time)))
        
        #biometric--------
        incProgress(0.1, detail = "Processing biometric data...")
        # Create master grid of all employees and all dates in shift period
        biometric_completed<-biometric %>% 
          mutate(clock_in=ifelse(is.na(`Check In`),NA_POSIXct_,ymd_hms(paste(Date,`Check In`))),
                 clock_out=ifelse(is.na(`Check Out`),NA_POSIXct_,ymd_hms(paste(Date,`Check Out`)))
          ) %>% 
          select(Employee_ID,Date,`Check In`,`Check Out`,clock_in,clock_out)
        
        #weekly off-------------
        weekly[, Date := as.IDate(`Weekly Off Date`,format = "%d-%m-%Y")]
        weekly_off_days <- unique(weekly[, .(Employee_ID, Date)])
        weekly_off_days[, Is_Weekly_Off := TRUE]
        
        #holiday list--------
        current_year <- as.numeric(format(Sys.Date(), "%Y"))
        year_holidays <-data.frame(Date1=c(
          # Fixed date holidays
          as.Date(paste0(current_year, "-01-01")),  # New Year's Day
          as.Date(paste0(current_year, "-04-18")),  # Good Friday
          as.Date(paste0(current_year, "-04-21")),  # Easter Monday
          as.Date(paste0(current_year, "-05-01")),  # Labour Day
          as.Date(paste0(current_year, "-06-01")),  # Madaraka Day
          as.Date(paste0(current_year, "-10-10")),  # Huduma Day
          as.Date(paste0(current_year, "-10-20")),  # Mashujaa Day
          as.Date(paste0(current_year, "-12-12")),  # Jamhuri Day
          as.Date(paste0(current_year, "-12-25")),  # Christmas Day
          as.Date(paste0(current_year, "-12-26"))   # Boxing Day
        ),
        holiday=c( "New Year's Day",  "Good Friday",  "Easter Monday"," Labour Day", "Madaraka Day",
                   "Huduma Day","Mashujaa Day","Jamhuri Day","Christmas Day","Boxing Day"))
        
        # More robust approach using case_when
        year_holidays <- year_holidays %>% 
          mutate(
            Date = case_when(
              wday(Date1) == 1 ~ Date1 + days(1),  # If Sunday, move to Monday
              TRUE ~ Date1  # Otherwise keep original date
            )
          ) %>% 
          # Ensure both columns are proper Date format
          mutate(
            Date1 = as.Date(Date1),
            Date = as.Date(Date)
          )
        
        #leave ---------
        incProgress(0.1, detail = "Processing leave data...")
        result<-as.data.table (leave %>% 
                                 rowwise()%>% 
                                 mutate(
                                   Date=list(seq(Start_Date,End_Date,by="day"))) %>% 
                                 unnest(Date) %>% 
                                 select(Employee_ID,Date,`Leave Type`))
        
        result[, On_Leave := TRUE]
        results<-unique(result)
        results<-results%>% 
          mutate(
            is_sunday=weekdays(Date)=="Sunday",
            is_holiday=Date %in% year_holidays, 
            "leave hours"= case_when(
              is_sunday|is_holiday~0,
              `Leave Type`=="Annual Leave"~9.00,
              `Leave Type`=="Maternity Leave"~9.00,
              `Leave Type`=="Paternity Leave"~9.00,
              `Leave Type`=="Sick Leave - Full Days"~9.00,
              `Leave Type`=="Sick Leave - Half Days"~4.50,
              `Leave Type`=="Unpaid Leave"~0.00,
              TRUE~NA_real_ #unmatch leavetype
            )) %>% 
          select(Employee_ID,Date,`Leave Type`,`leave hours`,On_Leave)
        
        #employee-----
        employees_completed<-employees2 %>% 
          select(Employee_ID,`Full Name`,Department)
        
        ## Create master grid of all employees and all dates in shift period
        date_range <- seq(biometric[, min(Date, na.rm = TRUE)], 
                          biometric[, max(Date, na.rm = TRUE)], 
                          by = "day")
        master_dt <- CJ(Employee_ID = employees_completed$Employee_ID, Date = date_range)
        master_emp <- employees_completed[master_dt, on = .(Employee_ID)]#merge on employee
        master_emp_shift <- shift_completed[master_emp, on = .(Employee_ID, Date)]# merge on shift
        master_emp_shft_we <- weekly_off_days[master_emp_shift, on = .(Employee_ID,Date)]#weekly off
        
        master1<-master_emp_shft_we %>% 
          mutate(`Shift Type`=ifelse(!(is.na(Is_Weekly_Off)),"WO",`Shift Type`),
                 Is_Weekly_Off=ifelse(is.na(Is_Weekly_Off),FALSE,Is_Weekly_Off))
        
        master2<-results[master1,on = .(Employee_ID, Date)]
        master3<-master2 %>% 
          mutate(`Shift Type`=ifelse(!(is.na(`Leave Type`)),`Leave Type`,`Shift Type`),
                 On_Leave=ifelse(is.na(On_Leave),FALSE,On_Leave))
        
        master_final1 <- biometric_completed[master3, on = .(Employee_ID, Date)]
        master_final <- master_final1 %>% 
          mutate(
            is_kenya_holiday = Date %in% year_holidays$Date,
            day_type = case_when(
              is_kenya_holiday ~ "Holiday",
              wday(Date) == 1 ~ "Sunday",
              TRUE ~ "Weekday"
            )
          )
        
        #analysis time
        incProgress(0.1, detail = "Analyzing attendance data...")
        masteranalysis<-master_final %>% 
          mutate(
            `Shift Type`=ifelse(is.na(`Shift Type`),"No Shift Allocated",`Shift Type`),
            clock_out=ifelse(grepl("NIGHT",toupper(`Shift Type`))|`Shift Type`=="ROO SECURITY OFFSITE SHIFT",
                             ymd_hms(paste(Date+days(1),lead(`Check Out`))),clock_out),
            clock_in=as.POSIXct(clock_in,"%d-%m-%Y %H:%M:%S"),
            clock_out=as.POSIXct(clock_out,"%d-%m-%Y %H:%M:%S"),
            shift_start_date_time=as.POSIXct(shift_start_date_time,"%d-%m-%Y %H:%M:%S"),
            shift_end_date_time=as.POSIXct(shift_end_date_time,"%d-%m-%Y %H:%M:%S"),
            adjusted_in=ifelse(is.na(clock_in),
                               shift_start_date_time,pmax(clock_in,shift_start_date_time,na.rm = TRUE)),
            adjusted_out=ifelse(is.na(clock_out),
                                shift_end_date_time,pmin(clock_out,shift_end_date_time,na.rm = TRUE)), 
            #hours worked
            hour_worked=ifelse(
              `Shift Type` %in% c("Annual Leave","Maternity Leave","Paternity Leave",
                                  "Sick Leave - Full Days","Sick Leave - Half Days","Unpaid Leave","WO","No Shift Allocated") | wday(Date)==1 | is_kenya_holiday==TRUE,NA,
              as.numeric(difftime(adjusted_out,adjusted_in,units = "hours"))-`lunch break`),
            #holiday hours
            hour_holiday=ifelse((wday(Date)==1 | is_kenya_holiday==TRUE)&
                                  !( `Shift Type` %in% c("Annual Leave","Maternity Leave","Paternity Leave",
                                                         "Sick Leave - Full Days","Sick Leave - Half Days","Unpaid Leave","WO","No Shift Allocated") ),
                                as.numeric(difftime(adjusted_out,adjusted_in,units = "hours")),NA),
            
            #overtime
            hour_overtime=ifelse(clock_out>shift_end_date_time &
                                   !(`Shift Type` %in% c("Annual Leave","Maternity Leave","Paternity Leave",
                                                         "Sick Leave - Full Days","Sick Leave - Half Days","Unpaid Leave","WO","No Shift Allocated")),
                                 as.numeric(difftime(clock_out,shift_end_date_time,units = "hours")),0.0),
            
            #late to work
            late_arrival=ifelse(
              clock_in>shift_start_date_time, as.numeric(difftime(clock_in,shift_start_date_time,units = "hours")),0.0),
            ##earier departure
            earier_departure=ifelse(
              clock_out<shift_end_date_time, as.numeric(difftime(shift_end_date_time,clock_out,units = "hours"),na.rm=TRUE),0.0),
            
            #convert to time zone
            adjusted_in=as.POSIXct(adjusted_in,"%d-%m-%Y %H:%M:%S"),
            adjusted_out=as.POSIXct(adjusted_out,"%d-%m-%Y %H:%M:%S")) %>% 
          mutate("check  in"= format(adjusted_in,"%H:%M:%S"),
                 "check  out"= format(adjusted_out,"%H:%M:%S")
          )
        
        # NEW: Employee Daily Summary - MOVED BEFORE OTHER REPORTS THAT MIGHT DEPEND ON IT
        incProgress(0.1, detail = "Generating employee daily summary...")
        employee_daily_summary <- masteranalysis %>%
          mutate(
            Date = as.Date(Date),
            # Convert to numeric safely, handling NA values
            hour_worked_num = as.numeric(hour_worked),
            leave_hours_num = as.numeric(`leave hours`),
            hour_holiday_num = as.numeric(hour_holiday),
            hour_overtime_num = as.numeric(hour_overtime),
            
            # Calculate hours with proper NA handling
            Worked_Hours = round(ifelse(is.na(hour_worked_num) & is.na(leave_hours_num), 
                                        0.0, 
                                        coalesce(hour_worked_num, 0) + coalesce(leave_hours_num, 0)), 2),
            
            Holiday_Hours = round(coalesce(hour_holiday_num, 0), 2),
            Overtime_Hours = round(coalesce(hour_overtime_num, 0), 2)
          ) %>%
          select(Employee_ID, `Full Name`, Department, Date, `Shift Type`, 
                 Worked_Hours, Holiday_Hours, Overtime_Hours) %>%
          arrange(Employee_ID, Date)
        
        #absent report------
        incProgress(0.1, detail = "Generating absentee reports...")
        absent1<-masteranalysis %>% 
          mutate(absent=ifelse(is.na(clock_in)&is.na(clock_out),TRUE,FALSE))
        
        absentees_detailed <- absent1 [
          !(`Shift Type` %in% c("WO","No Shift Allocated","ROO SECURITY FARM NIGHT SHIFT","SECURITY DAY SHIFT - Farm lake security")) & # Was scheduled to work
            !(Employee_ID %in% c("1352","1502","1830","0149","0154","0315","0259","0788",
                                 "0887","0805","1997"))&
            On_Leave == FALSE &       # Not on approved leave
            Is_Weekly_Off == FALSE &  # Not weekly off day
            absent == TRUE,         # No biometric login
          .(Employee_ID, `Full Name`,Department , `Shift Type`,Date,`Check In`,`Check Out`)
        ] 
        
        # Order the results
        setorder(absentees_detailed, Date, Department, Employee_ID)
        
        # Generate summary report by employee
        absence_summary_employee <- absentees_detailed[, .(Absence_Count = .N), 
                                                       by = .(Department, Employee_ID, `Full Name`)]
        setorder(absence_summary_employee, Department, -Absence_Count)
        
        # Generate summary report by department
        absence_summary_department <- absentees_detailed[, .(
          Total_Absences = .N,
          Unique_Employees = uniqueN(Employee_ID),
          Average_Absences_Per_Employee = round(.N/uniqueN(Employee_ID), 2)
        ), by = .(Department)]
        setorder(absence_summary_department, -Total_Absences)
        
        #report on attendance hours-------
        incProgress(0.1, detail = "Generating attendance reports...")
        timeAttendance<-masteranalysis %>% 
          select(Employee_ID,`Full Name`,Department,Date,`Shift Type`,shift_start_time,shift_end_time,`check  in`, `check  out`,late_arrival,earier_departure,`lunch break`,hour_overtime,hour_worked,hour_holiday,`leave hours`) %>% 
          mutate(Date= format(Date, "%Y-%m-%d")) %>% 
          group_by(Employee_ID) %>% 
          mutate(Department1 = case_when(
            Date == min(Date) ~ as.character(Department),
            TRUE ~ NA_character_),
            `Full Name1` = case_when(
              Date == min(Date) ~ as.character(`Full Name`),
              TRUE ~ NA_character_),
            Employee_ID1 = case_when(
              Date == min(Date) ~ as.character(Employee_ID),
              TRUE ~ NA_character_),
            total_hours_worked1=
              ifelse(Date==min(Date),
                     sum(hour_worked,na.rm = TRUE),NA_real_),
            total_hours_leave=
              ifelse(Date==min(Date),
                     sum(`leave hours`,na.rm = TRUE),NA_real_),
            total_hours_holiday=
              ifelse(Date==min(Date),
                     sum(hour_holiday,na.rm = TRUE),NA_real_)) %>% 
          mutate(total_hours_worked=ifelse(
            (total_hours_worked1+total_hours_leave)>225.0,
            225,total_hours_worked1+total_hours_leave),
            total_hours_overtime=
              ifelse(Date==min(Date),
                     ifelse(total_hours_worked==225,
                            (total_hours_worked1+total_hours_leave)-225,0.0 )
                     ,NA_real_)) %>% 
          ungroup() %>% 
          select(Employee_ID,Department,`Full Name`,Employee_ID1,`Full Name1`,Department1,Date,`Shift Type`,shift_start_time,shift_end_time,
                 `check  in`, `check  out`,late_arrival,earier_departure,hour_worked,hour_overtime,
                 hour_holiday,`lunch break`,`leave hours`,total_hours_worked,total_hours_holiday,total_hours_overtime) %>% 
          #format dates
          mutate(late_arrival=ifelse(!is.na(late_arrival),
                                     sprintf("%02d:%02d",floor(late_arrival),round((late_arrival %% 1)*60)),NA),
                 earier_departure=ifelse(!is.na(earier_departure),
                                         sprintf("%02d:%02d",floor(earier_departure),round((earier_departure %% 1)*60)),NA),
                 hour_overtime=ifelse(!is.na(hour_overtime),
                                      sprintf("%02d:%02d",floor(hour_overtime),round((hour_overtime %% 1)*60)),NA),
                 hour_worked=ifelse(!is.na(hour_worked),
                                    sprintf("%02d:%02d",floor(hour_worked),round((hour_worked %% 1)*60)),NA),
                 hour_holiday=ifelse(!is.na(hour_holiday),
                                     sprintf("%02d:%02d",floor(hour_holiday),round((hour_holiday %% 1)*60)),NA),
                 `leave hours`=ifelse(!is.na(`leave hours`),
                                      sprintf("%02d:%02d",floor(`leave hours`),round((`leave hours` %% 1)*60)),NA),
                 total_hours_worked=ifelse(!is.na(total_hours_worked),
                                           sprintf("%02d:%02d",floor(total_hours_worked),round((total_hours_worked %% 1)*60)),NA),
                 total_hours_holiday=ifelse(!is.na(total_hours_holiday),
                                            sprintf("%02d:%02d",floor(total_hours_holiday),round((total_hours_holiday %% 1)*60)),NA),
                 total_hours_overtime=ifelse(!is.na(total_hours_overtime),
                                             sprintf("%02d:%02d",floor(total_hours_overtime),round((total_hours_overtime %% 1)*60)),NA),
                 `lunch break`=ifelse(!is.na(`lunch break`),
                                      sprintf("%02d:%02d",floor(`lunch break`),round((`lunch break` %% 1)*60)),NA)
                 
          )
        
        #summary per department
        attendancedepartment<-masteranalysis %>% 
          select(Department,hour_worked,hour_holiday,hour_overtime) %>% 
          group_by(Department) %>% 
          mutate("regular hours"=round(sum(hour_worked,na.rm = TRUE),2),
                 "holiday hours"=round(sum(hour_holiday,na.rm = TRUE),2),
                 "overtime hours"=round(sum(hour_overtime,na.rm = TRUE),2) )%>% 
          select(Department,`regular hours`,`holiday hours`,`overtime hours`)
        attendancedepartment<-unique(attendancedepartment)  
        
        # DASHBOARD CALCULATIONS
        incProgress(0.1, detail = "Calculating dashboard metrics...")
        # Calculate dashboard metrics
        # Total employees
        total_employees <- length(unique(employees_completed$Employee_ID))
        
        # Total hours - ensure we're using numeric values
        total_regular_hours <- round(sum(as.numeric(masteranalysis$hour_worked), na.rm = TRUE), 2)
        total_holiday_hours <- round(sum(as.numeric(masteranalysis$hour_holiday), na.rm = TRUE), 2)
        total_overtime_hours <- round(sum(as.numeric(masteranalysis$hour_overtime), na.rm = TRUE), 2)
        total_leave_hours <- round(sum(as.numeric(masteranalysis$`leave hours`), na.rm = TRUE), 2)
        
        # Absentee metrics
        total_absent_days <- ifelse(!is.null(absentees_detailed), nrow(absentees_detailed), 0)
        avg_absent_per_employee <- ifelse(total_employees > 0, 
                                          round(total_absent_days / total_employees, 2), 0)
        
        # Calculate attendance rate (simplified)
        total_work_days <- nrow(masteranalysis[!(`Shift Type` %in% c("WO", "No Shift Allocated"))])
        attendance_rate <- ifelse(total_work_days > 0, 
                                  round((total_work_days - total_absent_days) / total_work_days * 100, 2), 0)
        
        # Create dashboard summary
        values$dashboard_summary <- list(
          total_employees = total_employees,
          total_regular_hours = total_regular_hours,
          total_holiday_hours = total_holiday_hours,
          total_overtime_hours = total_overtime_hours,
          total_leave_hours = total_leave_hours,
          total_absent_days = total_absent_days,
          avg_absent_per_employee = avg_absent_per_employee,
          attendance_rate = attendance_rate
        )
        
        # Department summary for dashboard
        if (!is.null(absentees_detailed) && nrow(absentees_detailed) > 0) {
          dashboard_dept <- masteranalysis %>%
            group_by(Department) %>%
            summarise(
              Employees = n_distinct(Employee_ID),
              Regular_Hours = round(sum(as.numeric(hour_worked), na.rm = TRUE), 2),
              Holiday_Hours = round(sum(as.numeric(hour_holiday), na.rm = TRUE), 2),
              Overtime_Hours = round(sum(as.numeric(hour_overtime), na.rm = TRUE), 2),
              .groups = 'drop'
            ) %>%
            left_join(
              absentees_detailed %>%
                group_by(Department) %>%
                summarise(Absent_Days = n(), .groups = 'drop'),
              by = "Department"
            )
        } else {
          dashboard_dept <- masteranalysis %>%
            group_by(Department) %>%
            summarise(
              Employees = n_distinct(Employee_ID),
              Regular_Hours = round(sum(as.numeric(hour_worked), na.rm = TRUE), 2),
              Holiday_Hours = round(sum(as.numeric(hour_holiday), na.rm = TRUE), 2),
              Overtime_Hours = round(sum(as.numeric(hour_overtime), na.rm = TRUE), 2),
              Absent_Days = 0,
              .groups = 'drop'
            )
        }
        
        values$dashboard_department <- dashboard_dept
        
        # TARDINESS REPORTS - UPDATED CODE WITH HH:MM:SS FORMAT
        incProgress(0.1, detail = "Generating tardiness reports...")
        # Detailed tardiness report - Filter for 1 minute or more and format as HH:MM:SS
        tardiness_detailed <- masteranalysis %>%
          filter(late_arrival >= 1/60) %>%  # 1 minute or more (1/60 hours)
          mutate(
            late_arrival_formatted = sprintf("%02d:%02d:%02d", 
                                             floor(late_arrival), 
                                             floor((late_arrival %% 1) * 60),
                                             round(((late_arrival %% 1) * 60 - floor((late_arrival %% 1) * 60)) * 60))
          ) %>%
          select(Employee_ID, `Full Name`, Department, Date, `Shift Type`, 
                 shift_start_time, `Check In`, late_arrival = late_arrival_formatted)
        
        # Tardiness summary by employee - Convert hours to HH:MM:SS format
        tardiness_summary_employee <- masteranalysis %>%
          filter(late_arrival >= 1/60) %>%  # 1 minute or more
          group_by(Department, Employee_ID, `Full Name`) %>%
          summarise(
            Total_Tardiness_Instances = n(),
            Total_Tardy_Hours = round(sum(late_arrival, na.rm = TRUE), 2),
            Average_Tardy_Hours = round(mean(late_arrival, na.rm = TRUE), 2),
            .groups = 'drop'
          ) %>%
          mutate(
            Total_Tardy_Hours_Formatted = sprintf("%02d:%02d:%02d", 
                                                  floor(Total_Tardy_Hours), 
                                                  floor((Total_Tardy_Hours %% 1) * 60),
                                                  round(((Total_Tardy_Hours %% 1) * 60 - floor((Total_Tardy_Hours %% 1) * 60)) * 60)),
            Average_Tardy_Hours_Formatted = sprintf("%02d:%02d:%02d", 
                                                    floor(Average_Tardy_Hours), 
                                                    floor((Average_Tardy_Hours %% 1) * 60),
                                                    round(((Average_Tardy_Hours %% 1) * 60 - floor((Average_Tardy_Hours %% 1) * 60)) * 60))
          ) %>%
          select(Department, Employee_ID, `Full Name`, Total_Tardiness_Instances, 
                 Total_Tardy_Hours = Total_Tardy_Hours_Formatted, 
                 Average_Tardy_Hours = Average_Tardy_Hours_Formatted) %>%
          arrange(Department, desc(Total_Tardiness_Instances))
        
        # Tardiness summary by department - Convert hours to HH:MM:SS format
        tardiness_summary_department <- masteranalysis %>%
          filter(late_arrival >= 1/60) %>%  # 1 minute or more
          group_by(Department) %>%
          summarise(
            Total_Tardiness_Instances = n(),
            Unique_Employees = n_distinct(Employee_ID),
            Total_Tardy_Hours = round(sum(late_arrival, na.rm = TRUE), 2),
            Average_Tardy_Hours_Per_Employee = round(Total_Tardy_Hours / Unique_Employees, 2),
            .groups = 'drop'
          ) %>%
          mutate(
            Total_Tardy_Hours_Formatted = sprintf("%02d:%02d:%02d", 
                                                  floor(Total_Tardy_Hours), 
                                                  floor((Total_Tardy_Hours %% 1) * 60),
                                                  round(((Total_Tardy_Hours %% 1) * 60 - floor((Total_Tardy_Hours %% 1) * 60)) * 60)),
            Average_Tardy_Hours_Per_Employee_Formatted = sprintf("%02d:%02d:%02d", 
                                                                 floor(Average_Tardy_Hours_Per_Employee), 
                                                                 floor((Average_Tardy_Hours_Per_Employee %% 1) * 60),
                                                                 round(((Average_Tardy_Hours_Per_Employee %% 1) * 60 - floor((Average_Tardy_Hours_Per_Employee %% 1) * 60)) * 60))
          ) %>%
          select(Department, Total_Tardiness_Instances, Unique_Employees, 
                 Total_Tardy_Hours = Total_Tardy_Hours_Formatted, 
                 Average_Tardy_Hours_Per_Employee = Average_Tardy_Hours_Per_Employee_Formatted) %>%
          arrange(desc(Total_Tardiness_Instances))
        
        # Store all processed data in reactive values
        values$timeAttendance <- timeAttendance
        values$attendancedepartment <- attendancedepartment
        values$employee_daily_summary <- employee_daily_summary
        values$absentees_detailed <- absentees_detailed
        values$absence_summary_employee <- absence_summary_employee
        values$absence_summary_department <- absence_summary_department
        values$tardiness_detailed <- tardiness_detailed
        values$tardiness_summary_employee <- tardiness_summary_employee
        values$tardiness_summary_department <- tardiness_summary_department
        values$processing_complete <- TRUE
        
        # Initialize filtered datasets
        values$filtered_timeAttendance <- timeAttendance
        values$filtered_employee_daily_summary <- employee_daily_summary
        values$filtered_absentees_detailed <- absentees_detailed
        values$filtered_absence_summary_employee <- absence_summary_employee
        values$filtered_tardiness_detailed <- tardiness_detailed
        values$filtered_tardiness_summary_employee <- tardiness_summary_employee
        
        # Reset filter states
        values$attendance_filters_applied <- FALSE
        values$daily_filters_applied <- FALSE
        values$absent_filters_applied <- FALSE
        values$absent_emp_filters_applied <- FALSE
        values$tardy_filters_applied <- FALSE
        values$tardy_emp_filters_applied <- FALSE
        
      }) # End of withProgress
      
      # Update process status
      output$process_status <- renderText("✅ Data processing completed successfully!")
      
      # Show comprehensive success notification
      showNotification(
        paste(
          "✅ Data processing completed successfully!",
          paste("📊 Total employees processed:", total_employees),
          paste("📅 Date range:", min(date_range), "to", max(date_range)),
          paste("📈 Total records:", nrow(masteranalysis)),
          sep = "\n"
        ), 
        type = "message", 
        duration = 10
      )
      
    }, error = function(e) {
      output$process_status <- renderText(paste("❌ Error processing data:", e$message))
      showNotification(paste("❌ Error processing data:", e$message), type = "error", duration = 10)
    })
    
    # Reset processing flag
    values$processing <- FALSE
  })
  
  # [REST OF YOUR SERVER CODE - Filter functions and output rendering remains the same]
  # ... [All your existing filter functions and output renderers] ...
  
  # Refresh static data when button is clicked
  observeEvent(input$refresh_static, {
    if (file.exists(EMPLOYEE_FILE)) {
      tryCatch({
        values$employees <- fread(EMPLOYEE_FILE)
        showNotification("✅ Employee data refreshed successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Error refreshing employee data:", e$message), type = "error")
      })
    }
    
    if (file.exists(SHIFT_TYPE_FILE)) {
      tryCatch({
        values$active_shift <- fread(SHIFT_TYPE_FILE)
        showNotification("✅ Shift type data refreshed successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Error refreshing shift type data:", e$message), type = "error")
      })
    }
  })
  
  # Update filter choices when data is processed
  observeEvent(values$processing_complete, {
    if (values$processing_complete) {
      # Update employee choices
      if (!is.null(values$timeAttendance)) {
        employees <- unique(values$timeAttendance$`Full Name`)
        updateSelectizeInput(session, "attendance_employee", choices = employees, server = TRUE)
        updateSelectizeInput(session, "daily_employee", choices = employees, server = TRUE)
        updateSelectizeInput(session, "absent_employee", choices = employees, server = TRUE)
        updateSelectizeInput(session, "absent_emp_employee", choices = employees, server = TRUE)
        updateSelectizeInput(session, "tardy_employee", choices = employees, server = TRUE)
        updateSelectizeInput(session, "tardy_emp_employee", choices = employees, server = TRUE)
      }
      
      # Update department choices
      if (!is.null(values$timeAttendance)) {
        departments <- unique(values$timeAttendance$Department)
        updateSelectizeInput(session, "attendance_department", choices = departments, server = TRUE)
        updateSelectizeInput(session, "daily_department", choices = departments, server = TRUE)
        updateSelectizeInput(session, "absent_department", choices = departments, server = TRUE)
        updateSelectizeInput(session, "absent_emp_department", choices = departments, server = TRUE)
        updateSelectizeInput(session, "tardy_department", choices = departments, server = TRUE)
        updateSelectizeInput(session, "tardy_emp_department", choices = departments, server = TRUE)
      }
    }
  })
  
  # [Include all your filter functions from the previous code here]
  # ... [All your filter_* functions] ...
  
  # Filter functions with enhanced notifications
  filter_attendance_data <- function() {
    req(values$timeAttendance)
    filtered <- values$timeAttendance
    original_count <- nrow(filtered)
    filter_details <- c()
    
    # Employee filter
    if (!is.null(input$attendance_employee) && length(input$attendance_employee) > 0) {
      filtered <- filtered %>% filter(`Full Name` %in% input$attendance_employee)
      filter_details <- c(filter_details, paste("Employees:", paste(input$attendance_employee, collapse = ", ")))
    }
    
    # Department filter
    if (!is.null(input$attendance_department) && length(input$attendance_department) > 0) {
      filtered <- filtered %>% filter(Department %in% input$attendance_department)
      filter_details <- c(filter_details, paste("Departments:", paste(input$attendance_department, collapse = ", ")))
    }
    
    # Date range filter
    if (!is.null(input$attendance_date_range)) {
      filtered <- filtered %>% 
        filter(Date >= input$attendance_date_range[1] & Date <= input$attendance_date_range[2])
      filter_details <- c(filter_details, paste("Date Range:", input$attendance_date_range[1], "to", input$attendance_date_range[2]))
    }
    
    values$filtered_timeAttendance <- filtered
    values$attendance_filters_applied <- TRUE
    
    # Create notification message
    filtered_count <- nrow(filtered)
    message <- paste(
      "Attendance filters applied successfully!",
      paste0("Records: ", filtered_count, " of ", original_count, " (", round(filtered_count/original_count*100, 1), "%)"),
      if(length(filter_details) > 0) paste("Filters:", paste(filter_details, collapse = " | ")) else "No filters applied - showing all records",
      sep = "\n"
    )
    
    list(data = filtered, message = message, original_count = original_count, filtered_count = filtered_count)
  }
  
  filter_daily_summary <- function() {
    req(values$employee_daily_summary)
    filtered <- values$employee_daily_summary
    original_count <- nrow(filtered)
    filter_details <- c()
    
    # Employee filter
    if (!is.null(input$daily_employee) && length(input$daily_employee) > 0) {
      filtered <- filtered %>% filter(`Full Name` %in% input$daily_employee)
      filter_details <- c(filter_details, paste("Employees:", paste(input$daily_employee, collapse = ", ")))
    }
    
    # Department filter
    if (!is.null(input$daily_department) && length(input$daily_department) > 0) {
      filtered <- filtered %>% filter(Department %in% input$daily_department)
      filter_details <- c(filter_details, paste("Departments:", paste(input$daily_department, collapse = ", ")))
    }
    
    # Date range filter
    if (!is.null(input$daily_date_range)) {
      filtered <- filtered %>% 
        filter(Date >= input$daily_date_range[1] & Date <= input$daily_date_range[2])
      filter_details <- c(filter_details, paste("Date Range:", input$daily_date_range[1], "to", input$daily_date_range[2]))
    }
    
    values$filtered_employee_daily_summary <- filtered
    values$daily_filters_applied <- TRUE
    
    # Create notification message
    filtered_count <- nrow(filtered)
    message <- paste(
      "Daily summary filters applied successfully!",
      paste0("Records: ", filtered_count, " of ", original_count, " (", round(filtered_count/original_count*100, 1), "%)"),
      if(length(filter_details) > 0) paste("Filters:", paste(filter_details, collapse = " | ")) else "No filters applied - showing all records",
      sep = "\n"
    )
    
    list(data = filtered, message = message, original_count = original_count, filtered_count = filtered_count)
  }
  
  filter_absent_detailed <- function() {
    req(values$absentees_detailed)
    filtered <- values$absentees_detailed
    original_count <- nrow(filtered)
    filter_details <- c()
    
    # Employee filter
    if (!is.null(input$absent_employee) && length(input$absent_employee) > 0) {
      filtered <- filtered %>% filter(`Full Name` %in% input$absent_employee)
      filter_details <- c(filter_details, paste("Employees:", paste(input$absent_employee, collapse = ", ")))
    }
    
    # Department filter
    if (!is.null(input$absent_department) && length(input$absent_department) > 0) {
      filtered <- filtered %>% filter(Department %in% input$absent_department)
      filter_details <- c(filter_details, paste("Departments:", paste(input$absent_department, collapse = ", ")))
    }
    
    # Date range filter
    if (!is.null(input$absent_date_range)) {
      filtered <- filtered %>% 
        filter(Date >= input$absent_date_range[1] & Date <= input$absent_date_range[2])
      filter_details <- c(filter_details, paste("Date Range:", input$absent_date_range[1], "to", input$absent_date_range[2]))
    }
    
    values$filtered_absentees_detailed <- filtered
    values$absent_filters_applied <- TRUE
    
    # Create notification message
    filtered_count <- nrow(filtered)
    message <- paste(
      "Absentee filters applied successfully!",
      paste0("Records: ", filtered_count, " of ", original_count, " (", round(filtered_count/original_count*100, 1), "%)"),
      if(length(filter_details) > 0) paste("Filters:", paste(filter_details, collapse = " | ")) else "No filters applied - showing all records",
      sep = "\n"
    )
    
    list(data = filtered, message = message, original_count = original_count, filtered_count = filtered_count)
  }
  
  filter_absent_employee <- function() {
    req(values$absence_summary_employee)
    filtered <- values$absence_summary_employee
    original_count <- nrow(filtered)
    filter_details <- c()
    
    # Employee filter
    if (!is.null(input$absent_emp_employee) && length(input$absent_emp_employee) > 0) {
      filtered <- filtered %>% filter(`Full Name` %in% input$absent_emp_employee)
      filter_details <- c(filter_details, paste("Employees:", paste(input$absent_emp_employee, collapse = ", ")))
    }
    
    # Department filter
    if (!is.null(input$absent_emp_department) && length(input$absent_emp_department) > 0) {
      filtered <- filtered %>% filter(Department %in% input$absent_emp_department)
      filter_details <- c(filter_details, paste("Departments:", paste(input$absent_emp_department, collapse = ", ")))
    }
    
    # Minimum absences filter
    if (!is.null(input$absent_emp_min_count) && input$absent_emp_min_count > 1) {
      filtered <- filtered %>% filter(Absence_Count >= input$absent_emp_min_count)
      filter_details <- c(filter_details, paste("Min Absences:", input$absent_emp_min_count))
    }
    
    values$filtered_absence_summary_employee <- filtered
    values$absent_emp_filters_applied <- TRUE
    
    # Create notification message
    filtered_count <- nrow(filtered)
    message <- paste(
      "Absentee employee filters applied successfully!",
      paste0("Records: ", filtered_count, " of ", original_count, " (", round(filtered_count/original_count*100, 1), "%)"),
      if(length(filter_details) > 0) paste("Filters:", paste(filter_details, collapse = " | ")) else "No filters applied - showing all records",
      sep = "\n"
    )
    
    list(data = filtered, message = message, original_count = original_count, filtered_count = filtered_count)
  }
  
  filter_tardy_detailed <- function() {
    req(values$tardiness_detailed)
    filtered <- values$tardiness_detailed
    original_count <- nrow(filtered)
    filter_details <- c()
    
    # Employee filter
    if (!is.null(input$tardy_employee) && length(input$tardy_employee) > 0) {
      filtered <- filtered %>% filter(`Full Name` %in% input$tardy_employee)
      filter_details <- c(filter_details, paste("Employees:", paste(input$tardy_employee, collapse = ", ")))
    }
    
    # Department filter
    if (!is.null(input$tardy_department) && length(input$tardy_department) > 0) {
      filtered <- filtered %>% filter(Department %in% input$tardy_department)
      filter_details <- c(filter_details, paste("Departments:", paste(input$tardy_department, collapse = ", ")))
    }
    
    # Date range filter
    if (!is.null(input$tardy_date_range)) {
      filtered <- filtered %>% 
        filter(Date >= input$tardy_date_range[1] & Date <= input$tardy_date_range[2])
      filter_details <- c(filter_details, paste("Date Range:", input$tardy_date_range[1], "to", input$tardy_date_range[2]))
    }
    
    # Minimum tardiness filter
    if (!is.null(input$tardy_min_minutes) && input$tardy_min_minutes > 1) {
      min_hours <- input$tardy_min_minutes / 60
      filtered <- filtered %>%
        mutate(late_hours = as.numeric(substr(late_arrival, 1, 2)) + 
                 as.numeric(substr(late_arrival, 4, 5))/60) %>%
        filter(late_hours >= min_hours) %>%
        select(-late_hours)
      filter_details <- c(filter_details, paste("Min Tardiness:", input$tardy_min_minutes, "minutes"))
    }
    
    values$filtered_tardiness_detailed <- filtered
    values$tardy_filters_applied <- TRUE
    
    # Create notification message
    filtered_count <- nrow(filtered)
    message <- paste(
      "Tardiness filters applied successfully!",
      paste0("Records: ", filtered_count, " of ", original_count, " (", round(filtered_count/original_count*100, 1), "%)"),
      if(length(filter_details) > 0) paste("Filters:", paste(filter_details, collapse = " | ")) else "No filters applied - showing all records",
      sep = "\n"
    )
    
    list(data = filtered, message = message, original_count = original_count, filtered_count = filtered_count)
  }
  
  filter_tardy_employee <- function() {
    req(values$tardiness_summary_employee)
    filtered <- values$tardiness_summary_employee
    original_count <- nrow(filtered)
    filter_details <- c()
    
    # Employee filter
    if (!is.null(input$tardy_emp_employee) && length(input$tardy_emp_employee) > 0) {
      filtered <- filtered %>% filter(`Full Name` %in% input$tardy_emp_employee)
      filter_details <- c(filter_details, paste("Employees:", paste(input$tardy_emp_employee, collapse = ", ")))
    }
    
    # Department filter
    if (!is.null(input$tardy_emp_department) && length(input$tardy_emp_department) > 0) {
      filtered <- filtered %>% filter(Department %in% input$tardy_emp_department)
      filter_details <- c(filter_details, paste("Departments:", paste(input$tardy_emp_department, collapse = ", ")))
    }
    
    # Minimum instances filter
    if (!is.null(input$tardy_emp_min_instances) && input$tardy_emp_min_instances > 1) {
      filtered <- filtered %>% filter(Total_Tardiness_Instances >= input$tardy_emp_min_instances)
      filter_details <- c(filter_details, paste("Min Instances:", input$tardy_emp_min_instances))
    }
    
    values$filtered_tardiness_summary_employee <- filtered
    values$tardy_emp_filters_applied <- TRUE
    
    # Create notification message
    filtered_count <- nrow(filtered)
    message <- paste(
      "Tardiness employee filters applied successfully!",
      paste0("Records: ", filtered_count, " of ", original_count, " (", round(filtered_count/original_count*100, 1), "%)"),
      if(length(filter_details) > 0) paste("Filters:", paste(filter_details, collapse = " | ")) else "No filters applied - showing all records",
      sep = "\n"
    )
    
    list(data = filtered, message = message, original_count = original_count, filtered_count = filtered_count)
  }
  
  # Clear filter functions
  clear_attendance_filters <- function() {
    updateSelectizeInput(session, "attendance_employee", selected = character(0))
    updateSelectizeInput(session, "attendance_department", selected = character(0))
    updateDateRangeInput(session, "attendance_date_range", 
                         start = Sys.Date() - 30, end = Sys.Date())
    values$filtered_timeAttendance <- values$timeAttendance
    values$attendance_filters_applied <- FALSE
    showNotification("Attendance filters cleared", type = "message")
  }
  
  clear_daily_filters <- function() {
    updateSelectizeInput(session, "daily_employee", selected = character(0))
    updateSelectizeInput(session, "daily_department", selected = character(0))
    updateDateRangeInput(session, "daily_date_range", 
                         start = Sys.Date() - 30, end = Sys.Date())
    values$filtered_employee_daily_summary <- values$employee_daily_summary
    values$daily_filters_applied <- FALSE
    showNotification("Daily summary filters cleared", type = "message")
  }
  
  clear_absent_filters <- function() {
    updateSelectizeInput(session, "absent_employee", selected = character(0))
    updateSelectizeInput(session, "absent_department", selected = character(0))
    updateDateRangeInput(session, "absent_date_range", 
                         start = Sys.Date() - 30, end = Sys.Date())
    values$filtered_absentees_detailed <- values$absentees_detailed
    values$absent_filters_applied <- FALSE
    showNotification("Absentee filters cleared", type = "message")
  }
  
  clear_absent_emp_filters <- function() {
    updateSelectizeInput(session, "absent_emp_employee", selected = character(0))
    updateSelectizeInput(session, "absent_emp_department", selected = character(0))
    updateNumericInput(session, "absent_emp_min_count", value = 1)
    values$filtered_absence_summary_employee <- values$absence_summary_employee
    values$absent_emp_filters_applied <- FALSE
    showNotification("Absentee employee filters cleared", type = "message")
  }
  
  clear_tardy_filters <- function() {
    updateSelectizeInput(session, "tardy_employee", selected = character(0))
    updateSelectizeInput(session, "tardy_department", selected = character(0))
    updateDateRangeInput(session, "tardy_date_range", 
                         start = Sys.Date() - 30, end = Sys.Date())
    updateNumericInput(session, "tardy_min_minutes", value = 1)
    values$filtered_tardiness_detailed <- values$tardiness_detailed
    values$tardy_filters_applied <- FALSE
    showNotification("Tardiness filters cleared", type = "message")
  }
  
  clear_tardy_emp_filters <- function() {
    updateSelectizeInput(session, "tardy_emp_employee", selected = character(0))
    updateSelectizeInput(session, "tardy_emp_department", selected = character(0))
    updateNumericInput(session, "tardy_emp_min_instances", value = 1)
    values$filtered_tardiness_summary_employee <- values$tardiness_summary_employee
    values$tardy_emp_filters_applied <- FALSE
    showNotification("Tardiness employee filters cleared", type = "message")
  }
  
  # Apply filter events with enhanced notifications
  observeEvent(input$attendance_apply_filters, {
    result <- filter_attendance_data()
    showNotification(result$message, type = "message", duration = 5)
  })
  
  observeEvent(input$daily_apply_filters, {
    result <- filter_daily_summary()
    showNotification(result$message, type = "message", duration = 5)
  })
  
  observeEvent(input$absent_apply_filters, {
    result <- filter_absent_detailed()
    showNotification(result$message, type = "message", duration = 5)
  })
  
  observeEvent(input$absent_emp_apply_filters, {
    result <- filter_absent_employee()
    showNotification(result$message, type = "message", duration = 5)
  })
  
  observeEvent(input$tardy_apply_filters, {
    result <- filter_tardy_detailed()
    showNotification(result$message, type = "message", duration = 5)
  })
  
  observeEvent(input$tardy_emp_apply_filters, {
    result <- filter_tardy_employee()
    showNotification(result$message, type = "message", duration = 5)
  })
  
  # Clear filter events
  observeEvent(input$attendance_clear_filters, {
    clear_attendance_filters()
  })
  
  observeEvent(input$daily_clear_filters, {
    clear_daily_filters()
  })
  
  observeEvent(input$absent_clear_filters, {
    clear_absent_filters()
  })
  
  observeEvent(input$absent_emp_clear_filters, {
    clear_absent_emp_filters()
  })
  
  observeEvent(input$tardy_clear_filters, {
    clear_tardy_filters()
  })
  
  observeEvent(input$tardy_emp_clear_filters, {
    clear_tardy_emp_filters()
  })
  
  # Filter info displays
  output$attendance_filter_info <- renderUI({
    if (values$attendance_filters_applied && !is.null(values$filtered_timeAttendance)) {
      total <- nrow(values$timeAttendance)
      filtered <- nrow(values$filtered_timeAttendance)
      div(class = "filter-info",
          tags$strong("Active Filters:"),
          paste("Showing", filtered, "of", total, "records", 
                paste0("(", round(filtered/total*100, 1), "%)")))
    }
  })
  
  output$daily_filter_info <- renderUI({
    if (values$daily_filters_applied && !is.null(values$filtered_employee_daily_summary)) {
      total <- nrow(values$employee_daily_summary)
      filtered <- nrow(values$filtered_employee_daily_summary)
      div(class = "filter-info",
          tags$strong("Active Filters:"),
          paste("Showing", filtered, "of", total, "records", 
                paste0("(", round(filtered/total*100, 1), "%)")))
    }
  })
  
  output$absent_filter_info <- renderUI({
    if (values$absent_filters_applied && !is.null(values$filtered_absentees_detailed)) {
      total <- nrow(values$absentees_detailed)
      filtered <- nrow(values$filtered_absentees_detailed)
      div(class = "filter-info",
          tags$strong("Active Filters:"),
          paste("Showing", filtered, "of", total, "records", 
                paste0("(", round(filtered/total*100, 1), "%)")))
    }
  })
  
  output$absent_emp_filter_info <- renderUI({
    if (values$absent_emp_filters_applied && !is.null(values$filtered_absence_summary_employee)) {
      total <- nrow(values$absence_summary_employee)
      filtered <- nrow(values$filtered_absence_summary_employee)
      div(class = "filter-info",
          tags$strong("Active Filters:"),
          paste("Showing", filtered, "of", total, "records", 
                paste0("(", round(filtered/total*100, 1), "%)")))
    }
  })
  
  output$tardy_filter_info <- renderUI({
    if (values$tardy_filters_applied && !is.null(values$filtered_tardiness_detailed)) {
      total <- nrow(values$tardiness_detailed)
      filtered <- nrow(values$filtered_tardiness_detailed)
      div(class = "filter-info",
          tags$strong("Active Filters:"),
          paste("Showing", filtered, "of", total, "records", 
                paste0("(", round(filtered/total*100, 1), "%)")))
    }
  })
  
  output$tardy_emp_filter_info <- renderUI({
    if (values$tardy_emp_filters_applied && !is.null(values$filtered_tardiness_summary_employee)) {
      total <- nrow(values$tardiness_summary_employee)
      filtered <- nrow(values$filtered_tardiness_summary_employee)
      div(class = "filter-info",
          tags$strong("Active Filters:"),
          paste("Showing", filtered, "of", total, "records", 
                paste0("(", round(filtered/total*100, 1), "%)")))
    }
  })
  
  # [Include all your output rendering functions from the previous code here]
  # ... [All your output$*_table and output$*_box functions] ...
  
  # Dashboard value boxes
  output$total_employees_box <- renderValueBox({
    req(values$dashboard_summary)
    valueBox(
      values$dashboard_summary$total_employees,
      "Total Employees",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$total_regular_hours_box <- renderValueBox({
    req(values$dashboard_summary)
    valueBox(
      paste0(values$dashboard_summary$total_regular_hours, " hrs"),
      "Total Regular Hours",
      icon = icon("clock"),
      color = "green"
    )
  })
  
  output$total_overtime_hours_box <- renderValueBox({
    req(values$dashboard_summary)
    valueBox(
      paste0(values$dashboard_summary$total_overtime_hours, " hrs"),
      "Total Overtime Hours",
      icon = icon("business-time"),
      color = "orange"
    )
  })
  
  output$total_holiday_hours_box <- renderValueBox({
    req(values$dashboard_summary)
    valueBox(
      paste0(values$dashboard_summary$total_holiday_hours, " hrs"),
      "Total Holiday Hours",
      icon = icon("calendar"),
      color = "red"
    )
  })
  
  output$total_absent_days_box <- renderValueBox({
    req(values$dashboard_summary)
    valueBox(
      values$dashboard_summary$total_absent_days,
      "Total Absent Days",
      icon = icon("user-slash"),
      color = "red"
    )
  })
  
  output$avg_absent_per_employee_box <- renderValueBox({
    req(values$dashboard_summary)
    valueBox(
      values$dashboard_summary$avg_absent_per_employee,
      "Avg Absences per Employee",
      icon = icon("chart-bar"),
      color = "yellow"
    )
  })
  
  output$attendance_rate_box <- renderValueBox({
    req(values$dashboard_summary)
    valueBox(
      paste0(values$dashboard_summary$attendance_rate, "%"),
      "Attendance Rate",
      icon = icon("percent"),
      color = "green"
    )
  })
  
  output$total_leave_hours_box <- renderValueBox({
    req(values$dashboard_summary)
    valueBox(
      paste0(values$dashboard_summary$total_leave_hours, " hrs"),
      "Total Leave Hours",
      icon = icon("umbrella-beach"),
      color = "teal"
    )
  })
  
  # Dashboard department table
  output$dashboard_department_table <- renderDataTable({
    req(values$dashboard_department)
    datatable(values$dashboard_department, 
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Blfrtip'),
              rownames = FALSE) %>%
      formatStyle(names(values$dashboard_department), fontSize = '12px')
  })
  
  # Attendance Report Table (with filters)
  output$attendance_table <- renderDataTable({
    data_to_show <- if (values$attendance_filters_applied && !is.null(values$filtered_timeAttendance)) {
      values$filtered_timeAttendance
    } else {
      values$timeAttendance
    }
    req(data_to_show)
    datatable(data_to_show, 
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Blfrtip'),
              rownames = FALSE) %>%
      formatStyle(names(data_to_show), fontSize = '12px')
  })
  
  # Department Summary Table
  output$department_table <- renderDataTable({
    req(values$attendancedepartment)
    datatable(values$attendancedepartment, 
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Blfrtip'),
              rownames = FALSE) %>%
      formatStyle(names(values$attendancedepartment), fontSize = '12px')
  })
  
  # Employee Daily Summary Table (with filters)
  output$employee_daily_table <- renderDataTable({
    data_to_show <- if (values$daily_filters_applied && !is.null(values$filtered_employee_daily_summary)) {
      values$filtered_employee_daily_summary
    } else {
      values$employee_daily_summary
    }
    req(data_to_show)
    datatable(data_to_show, 
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Blfrtip'),
              rownames = FALSE) %>%
      formatStyle(names(data_to_show), fontSize = '12px')
  })
  
  # Detailed Absentee Report Table (with filters)
  output$absent_detailed_table <- renderDataTable({
    data_to_show <- if (values$absent_filters_applied && !is.null(values$filtered_absentees_detailed)) {
      values$filtered_absentees_detailed
    } else {
      values$absentees_detailed
    }
    req(data_to_show)
    datatable(data_to_show, 
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Blfrtip'),
              rownames = FALSE) %>%
      formatStyle(names(data_to_show), fontSize = '12px')
  })
  
  # Absentee Summary by Employee Table (with filters)
  output$absent_employee_table <- renderDataTable({
    data_to_show <- if (values$absent_emp_filters_applied && !is.null(values$filtered_absence_summary_employee)) {
      values$filtered_absence_summary_employee
    } else {
      values$absence_summary_employee
    }
    req(data_to_show)
    datatable(data_to_show, 
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Blfrtip'),
              rownames = FALSE) %>%
      formatStyle(names(data_to_show), fontSize = '12px')
  })
  
  # Absentee Summary by Department Table
  output$absent_department_table <- renderDataTable({
    req(values$absence_summary_department)
    datatable(values$absence_summary_department, 
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Blfrtip'),
              rownames = FALSE) %>%
      formatStyle(names(values$absence_summary_department), fontSize = '12px')
  })
  
  # Detailed Tardiness Report Table (with filters)
  output$tardy_detailed_table <- renderDataTable({
    data_to_show <- if (values$tardy_filters_applied && !is.null(values$filtered_tardiness_detailed)) {
      values$filtered_tardiness_detailed
    } else {
      values$tardiness_detailed
    }
    req(data_to_show)
    datatable(data_to_show, 
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Blfrtip'),
              rownames = FALSE) %>%
      formatStyle(names(data_to_show), fontSize = '12px')
  })
  
  # Tardiness Summary by Employee Table (with filters)
  output$tardy_employee_table <- renderDataTable({
    data_to_show <- if (values$tardy_emp_filters_applied && !is.null(values$filtered_tardiness_summary_employee)) {
      values$filtered_tardiness_summary_employee
    } else {
      values$tardiness_summary_employee
    }
    req(data_to_show)
    datatable(data_to_show, 
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Blfrtip'),
              rownames = FALSE) %>%
      formatStyle(names(data_to_show), fontSize = '12px')
  })
  
  # Tardiness Summary by Department Table
  output$tardy_department_table <- renderDataTable({
    req(values$tardiness_summary_department)
    datatable(values$tardiness_summary_department, 
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Blfrtip'),
              rownames = FALSE) %>%
      formatStyle(names(values$tardiness_summary_department), fontSize = '12px')
  })
  
  # Download handlers for all reports (updated to use filtered data where applicable)
  output$download_attendance <- downloadHandler(
    filename = function() { "attendance_report.csv" },
    content = function(file) { 
      data_to_export <- if (values$attendance_filters_applied && !is.null(values$filtered_timeAttendance)) {
        values$filtered_timeAttendance
      } else {
        values$timeAttendance
      }
      fwrite(data_to_export, file) 
    }
  )
  
  output$download_department <- downloadHandler(
    filename = function() { "department_summary.csv" },
    content = function(file) { fwrite(values$attendancedepartment, file) }
  )
  
  output$download_employee_daily <- downloadHandler(
    filename = function() { "employee_daily_summary.csv" },
    content = function(file) { 
      data_to_export <- if (values$daily_filters_applied && !is.null(values$filtered_employee_daily_summary)) {
        values$filtered_employee_daily_summary
      } else {
        values$employee_daily_summary
      }
      fwrite(data_to_export, file) 
    }
  )
  
  output$download_absent_detailed <- downloadHandler(
    filename = function() { "absentee_detailed.csv" },
    content = function(file) { 
      data_to_export <- if (values$absent_filters_applied && !is.null(values$filtered_absentees_detailed)) {
        values$filtered_absentees_detailed
      } else {
        values$absentees_detailed
      }
      fwrite(data_to_export, file) 
    }
  )
  
  output$download_absent_employee <- downloadHandler(
    filename = function() { "absentee_employee_summary.csv" },
    content = function(file) { 
      data_to_export <- if (values$absent_emp_filters_applied && !is.null(values$filtered_absence_summary_employee)) {
        values$filtered_absence_summary_employee
      } else {
        values$absence_summary_employee
      }
      fwrite(data_to_export, file) 
    }
  )
  
  output$download_absent_department <- downloadHandler(
    filename = function() { "absentee_department_summary.csv" },
    content = function(file) { fwrite(values$absence_summary_department, file) }
  )
  
  output$download_tardy_detailed <- downloadHandler(
    filename = function() { "tardiness_detailed.csv" },
    content = function(file) { 
      data_to_export <- if (values$tardy_filters_applied && !is.null(values$filtered_tardiness_detailed)) {
        values$filtered_tardiness_detailed
      } else {
        values$tardiness_detailed
      }
      fwrite(data_to_export, file) 
    }
  )
  
  output$download_tardy_employee <- downloadHandler(
    filename = function() { "tardiness_employee_summary.csv" },
    content = function(file) { 
      data_to_export <- if (values$tardy_emp_filters_applied && !is.null(values$filtered_tardiness_summary_employee)) {
        values$filtered_tardiness_summary_employee
      } else {
        values$tardiness_summary_employee
      }
      fwrite(data_to_export, file) 
    }
  )
  
  output$download_tardy_department <- downloadHandler(
    filename = function() { "tardiness_department_summary.csv" },
    content = function(file) { fwrite(values$tardiness_summary_department, file) }
  )
}

shinyApp(ui, server)
