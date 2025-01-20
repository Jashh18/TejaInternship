install.packages("dplyr")

# Load necessary libraries
library(dplyr)

# Read the CSV file
tracks <- read.csv("C:/Users/tunte/Desktop/tracks_features.csv")

# Define the exact row ranges from the image
row_ranges <- list(
  c(1, 250000),           # First section
  c(250001, 500000),      # Second section
  c(500001, 750000),      # Third section
  c(750001, 1000000),     # Fourth section
  c(1000001, 1204025)     # Fifth section (to the end)
)

# Define the column ranges (3 columns per file as shown in image)
col_ranges <- list()
for(i in 1:8) {  # 8 groups of columns
  start_col <- (i-1)*3 + 1
  end_col <- min(i*3, ncol(tracks))
  col_ranges[[i]] <- c(start_col, end_col)
}

# Function to split and save the data
split_and_save <- function(data, row_start, row_end, col_start, col_end, filename) {
  subset <- data[row_start:row_end, col_start:col_end]
  write.csv(subset, filename, row.names = FALSE)
}

# Process the splitting
for(section in 1:5) {
  row_range <- row_ranges[[section]]
  
  for(col_group in 1:8) {
    col_range <- col_ranges[[col_group]]
    file_num <- (section-1)*8 + col_group
    filename <- sprintf("spotify_%02d.csv", file_num)
    
    split_and_save(
      data = tracks,
      row_start = row_range[1],
      row_end = row_range[2],
      col_start = col_range[1],
      col_end = col_range[2],
      filename = filename
    )
  }
}

# Function to combine all files back
combine_files <- function() {
  complete <- data.frame()
  
  for(section in 1:5) {
    section_data <- NULL
    
    # Combine horizontally within each section
    for(col_group in 1:8) {
      file_num <- (section-1)*8 + col_group
      filename <- sprintf("spotify_%02d.csv", file_num)
      current_data <- read.csv(filename)
      
      if(is.null(section_data)) {
        section_data <- current_data
      } else {
        section_data <- cbind(section_data, current_data)
      }
    }
    
    # Combine vertically between sections
    if(is.null(complete)) {
      complete <- section_data
    } else {
      complete <- rbind(complete, section_data)
    }
  }
  
  return(complete)
}

# Create the complete dataframe
complete <- combine_files()

# Verify the splitting
# Print the files created in each section
for(section in 1:5) {
  cat("\nSection", section, "files:\n")
  for(col_group in 1:8) {
    file_num <- (section-1)*8 + col_group
    filename <- sprintf("spotify_%02d.csv", file_num)
    
    # Read the file and print its dimensions
    df <- read.csv(filename)
    cat(sprintf("%s: Rows=%d, Cols=%d\n", 
                filename, nrow(df), ncol(df)))
  }
}

# Print final dimensions of combined data
cat("\nFinal combined dimensions:", dim(complete), "\n")


# List all created files
files <- list.files(pattern = "spotify_[0-9]{2}\\.csv")
print(length(files))  # Should be 40

# Check a few key files
print(nrow(read.csv("spotify_01.csv")))  # Should be 250000
print(nrow(read.csv("spotify_09.csv")))  # Should be 250000
print(nrow(read.csv("spotify_33.csv")))  # Should handle the remainder