# Load necessary libraries
library(readxl)
library(WaveletComp)
library(ggplot2)
# Define function to perform wavelet coherence analysis
wavelet_coherence_analysis <- function(sheet_name) {
  # Read the data from Excel
  df <- read_excel("C:\\Users\\MAITHILI\\Downloads\\COM_WORK (2).xlsx", sheet = sheet_name)
  
  # Rename columns correctly
  colnames(df) <- c("Date", "Price", "fsi")  # Ensure correct mapping
  
  # Convert Date column to Date format
  df$Date <- as.Date(df$Date)  # Keep as Date
  
  # Ensure chronological order
  df <- df[order(df$Date), ]
  
  # Extract the year from the Date column
  df$Year <- format(df$Date, "%Y")
  
  # Perform wavelet coherence analysis
  result <- analyze.coherency(df, my.pair = c("fsi", "Price"), 
                              loess.span = 0, make.pval = TRUE, 
                              n.sim = 100, dj = 0.125)  # Adjusts frequency resolution
  
  # Custom function to plot wavelet coherence with yearly formatting
  wc.image_custom <- function(result, df) {
    # Extract unique years
    unique_years <- unique(df$Year)
    
    # Define tick positions at the start of each year
    tick_positions <- match(unique_years, df$Year)
    
    # Ensure 2023 is included in the labels
    if (!"2023" %in% unique_years) {
      unique_years <- c(unique_years, "2023")
      tick_positions <- c(tick_positions, nrow(df))  # Place 2023 at the last position
    }
    
    # Generate wavelet coherence plot
    wc.image(result, 
             main = paste("Wavelet Coherence: FSI vs", sheet_name), 
             plot.coi = TRUE,
             plot.contour = TRUE,
             siglvl.contour = 0.1,
             plot.ridge = FALSE,
             plot.arrow = TRUE,
             use.sAngle = FALSE,
             color.key = "quantile",
             plot.legend = TRUE,
             label.time.axis = TRUE,
             spec.time.axis = list(at = tick_positions, labels = unique_years),  
             label.period.axis = TRUE,
             periodlab = "Period (Months)",  
             periodtck = 0.02,
             periodtcl = 0.5,
             lwd = 2, lwd.axis = 1, 
             graphics.reset = TRUE,
             verbose = FALSE)
  }
  
  # Call the custom function to plot
  wc.image_custom(result, df)
}

# List of commodity sheets
commodity_sheets <- c("Gold", "Silver", "Copper", "Crude Oil", "Natural Gas")

# Run wavelet coherence analysis for each sheet
for (sheet in commodity_sheets) {
  wavelet_coherence_analysis(sheet)
}
  ``
