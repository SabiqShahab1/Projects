library(shiny)
library(seqinr)
library(stringr)
library(ggplot2)

ui <- fluidPage(

    # Application title
    titlePanel("FASTA Dinucleotide Frequency Analysis"),
    #Creating input
    fileInput("fastaFile", "Choose fasta file"),
    #displaying frequency table
    dataTableOutput("info"),
    #displaying dinucelotide plot
    plotOutput("dinucleotidePlot")
)

server <- function(input, output) { 
  #creating a data table that changes when file input
  dinucleotideData <- reactive({
  #requiring input before displaying
    req(input$fastaFile)
  #reading fasta file from input
    fasta <- read.fasta(input$fastaFile$datapath)
  #creating character vector
    sequence <- unlist(fasta[[1]])
  #collapsing character vector
    sequence <- paste(sequence, collapse = "")
  #making sure characters are uppercase
    sequence <- toupper(sequence)
  #creating a vector containing all dinucleotide pairs  
    dinucleotides <- c("AA", "AT", "AC", "AG", "TA", "TT", "TC", "TG", "CA", "CT", "CC", "CG", "GA", "GT", "GC", "GG")
  #counting the dinucleotides from the vector in the sequence using str_count and storing
    DiCount <- sapply(dinucleotides, function(dinuc) str_count(sequence, dinuc))
  #calculating total count
    total_count <- sum(DiCount)
  #creating a data frame  
    Didf <- data.frame(
  #creating a column with each dinucleotide pair    
      dinucleotide = names(DiCount),
  #creating a column containing the counts of the pairs    
      count = DiCount,
  #calculating relative frequency of the pairs    
      frequency = DiCount / total_count
    )
  #splitting the pairs into individual bases  
    DiSplit <- strsplit(Didf$dinucleotide, "")
  #creating columns containing the bases   
    Split <- Reduce(rbind, DiSplit)
  #defining the columns in the dataframe  
    Didf$first <- Split[, 1]
    Didf$second <- Split[, 2]
  #displaying data table  
    Didf
  })
 #defining output for table 
  output$info <- renderDataTable({
    dinucleotideData()
  })
 #defining plot to display 
  output$dinucleotidePlot <- renderPlot({
 #grabbing data from data table   
    data <- dinucleotideData()
 #creating plot using ggplot, first base as the x axis, frequency as the y axis, and the bars being the second base    
    ggplot(data, aes(x = first, y = frequency, fill = second)) +
 #creating bar plot, identity for using raw data from table and dodge for spacing     
      geom_bar(stat = "identity", position = "dodge") +
 #creating line to represent expected frequency of each dinucleotide assuming random transitions     
      geom_hline(yintercept = 0.0625, linetype = "dashed", color = "black") +  
 #creating corresponding labels    
      labs(
        title = "Dinucleotide Frequency Analysis",
        x = "Starting Nucleotide",
        y = "Relative Frequency"
      ) 
  })
  
  }
  
# Run the application 
shinyApp(ui = ui, server = server)

