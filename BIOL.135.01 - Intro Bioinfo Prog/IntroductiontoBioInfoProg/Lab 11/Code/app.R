library(shiny)
library(seqinr)
library(stringr)
library(ggplot2)

ui <- fluidPage(
#Title
  titlePanel("FASTA Dinucleotide and GC Content Analysis"),
#File input and sliding window slider
  sidebarLayout(
    sidebarPanel(
      fileInput("fastaFile", "Choose FASTA file"),
      uiOutput("WindowSlider")
    ),
#Contains two parts, one for the # of dinucleotides and one for the GC content in the window size as it slides across the sequence
    mainPanel(
      tabsetPanel(
        tabPanel("Dinucleotide Frequency", dataTableOutput("info"), plotOutput("dinucleotidePlot")),
        tabPanel("GC Content", plotOutput("gcContentPlot"))
        
      )
    )
  )
)

server <- function(input, output, session) { 
#Dinucleotide data calculation after file input
  dinucleotideData <- reactive({
    req(input$fastaFile)
#Read the file and collapse the sequence into a single string    
    fasta <- read.fasta(input$fastaFile$datapath)
    sequence <- unlist(fasta[[1]])
    sequence <- paste(sequence, collapse = "")
    sequence <- toupper(sequence)
#Creating a list of all possible dinucleotides    
    dinucleotides <- c("AA", "AT", "AC", "AG", "TA", "TT", "TC", "TG", 
                       "CA", "CT", "CC", "CG", "GA", "GT", "GC", "GG")
#counting the total count of each dinucleotide
    DiCount <- sapply(dinucleotides, function(dinuc) str_count(sequence, dinuc))
    total_count <- sum(DiCount)
#creating a dataframe that contains the dinucleotide, the count, and the relative frequency    
    Didf <- data.frame(
      dinucleotide = names(DiCount),
      count = DiCount,
      frequency = DiCount / total_count
    )
#seperates the dinucleotide pairs into its first and second base    
    DiSplit <- strsplit(Didf$dinucleotide, "")
    Split <- Reduce(rbind, DiSplit)
    Didf$first <- Split[, 1]
    Didf$second <- Split[, 2]
#display the final data frame    
    Didf
  })
  
# Render dinucleotide frequency table
  output$info <- renderDataTable({
    dinucleotideData()
  })
  
#Render the dinucleotide frequency plot
  output$dinucleotidePlot <- renderPlot({
    data <- dinucleotideData()
#creating a bar plot that contains the different dinucleotide pairs and their respective frequency with a line indicating 1/16 frequency    
    ggplot(data, aes(x = first, y = frequency, fill = second)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_hline(yintercept = 0.0625, linetype = "dashed", color = "black") +  
      labs(title = "Dinucleotide Frequency Analysis", x = "Starting Nucleotide", y = "Relative Frequency") 
  })
  
#Dynamic slider input based on the sequence length from input of file
  output$WindowSlider <- renderUI({
    req(input$fastaFile)
#read sequence from file and creating a string for the sequence as well as calculating sequence length 
    fasta <- read.fasta(input$fastaFile$datapath)
    sequence <- unlist(fasta[[1]])
    seq_length <- length(sequence)
#creating sliding window slider from 1 to the length of the sequence    
    sliderInput("window_size", "GC Content Window Size", 
                min = 1, max = seq_length, value = min(10, seq_length))
  })
  
#GC Content calculation for the sliding window, requiring input of file and specifying window size
  gcContentData <- reactive({
    req(input$fastaFile, input$window_size)
#Making the sequence into a string from the file, and caluclating length    
    fasta <- read.fasta(input$fastaFile$datapath)
    sequence <- unlist(fasta[[1]])
    sequence <- paste(sequence, collapse = "")
    sequence <- toupper(sequence)
    seq_length <- nchar(sequence)
#Specifying window size as input from slider
    window_size <- input$window_size
    
#Initial GC count for the first window
#Subsetting from base 1 to size of window
    initial_window <- substr(sequence, 1, window_size)
#Calculating G and C content from within the substring
    gc_count <- sum(str_count(initial_window, "G") + str_count(initial_window, "C"))
#Calculating GC content
    gc_content <- numeric(seq_length - window_size + 1)
    gc_content[1] <- (gc_count / window_size) * 100
    
#Slide the window across the sequence
    for (i in 2:(seq_length - window_size + 1)) {
#Subtracting GC count of the base leaving the window
      if (substr(sequence, i - 1, i - 1) %in% c("G", "C")) {
        gc_count <- gc_count - 1
      }
#Add GC count of the base entering the window
      if (substr(sequence, i + window_size - 1, i + window_size - 1) %in% c("G", "C")) {
        gc_count <- gc_count + 1
      }
#Calculating GC content for current position
      gc_content[i] <- (gc_count / window_size) * 100
    }
#Creating a data frame for plotting
    data.frame(Position = 1:(seq_length - window_size + 1), GC_Content = gc_content)
  })
#Rendering GC content plot
  output$gcContentPlot <- renderPlot({
    data <- gcContentData()
#Creating line plot based on size of window as it slides across the sequence and GC content
    ggplot(data, aes(x = Position, y = GC_Content)) +
      geom_line(color = "blue") +
      labs(title = "GC Content for Sequence", x = "Position", y = "GC Content (%)") 
  })
}

shinyApp(ui = ui, server = server)
