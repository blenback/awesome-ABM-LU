split_markdown <- function(input_file, output_dir) {
  # Check if the input file exists
  if (!file.exists(input_file)) {
    stop("The input file does not exist.")
  }
  
  # Read the Markdown file into a character vector
  lines <- readLines(input_file)
  
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Initialize variables
  current_file <- NULL
  file_connection <- NULL
  
  # Process each line
  for (line in lines) {
    # Check for level 2 headings
    if (grepl("^## ", line)) {
      # Close the previous file connection, if any
      if (!is.null(file_connection)) {
        close(file_connection)
      }
      
      # Extract the heading text, sanitize it for a filename, and create a new file
      heading <- sub("^## ", "", line)
      filename <- paste0(gsub("[^a-zA-Z0-9]", "_", heading), ".qmd")
      current_file <- file.path(output_dir, filename)
      
      # Open a new file connection
      file_connection <- file(current_file, open = "w")
    }
    
    # Write the line to the current file
    if (!is.null(file_connection)) {
      writeLines(line, file_connection)
    }
  }
  
  # Close the last file connection, if any
  if (!is.null(file_connection)) {
    close(file_connection)
  }
  
  cat("Markdown file has been split into separate files in the", output_dir, "directory.\n")
}

# Example usage
split_markdown("README.md", "sections")

