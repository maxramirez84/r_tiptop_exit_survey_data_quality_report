library(redcapAPI)

# Auxiliar functions -----------------------------------------------------------

# Data timestamp
DataTimestamp <- function(data.retrieval.mode, file.date = "", file.time = "") {
  # Returns the file timestamp if we are retrieving the data from a file and the
  # current datetime if we are extrating the data through the REDCap API.
  #
  # Args:
  #   data.retrieval.mode: String {file, api} indicating how we're retrieving 
  #                        the data.
  #   file.date: String indicating the date of the data file in yyyy-mm-dd 
  #              format.
  #   file.time: String indicating the time of the data file in hh:mm (24 hours)
  #              format.
  # 
  # Returns:
  #   The timestamp of the retrieved data.
  if (data.retrieval.mode == "file") {
    data.timestamp <- paste(file.date, file.time)
  } 
  else if (data.retrieval.mode == "api") {
    data.timestamp <- Sys.time()
  }
  else {
    message("data.retrieval.mode must be either file or api. No more modes 
            available.")
    return(NULL)
  }
  
  return(data.timestamp)
}