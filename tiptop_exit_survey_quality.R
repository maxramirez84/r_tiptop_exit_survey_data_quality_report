library(redcapAPI)

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


ReadData <- function(data.retrieval.mode, file.prefix = "", 
                     file.content = "_DATA_", file.date = "", file.time = "", 
                     api.url = "", api.token = "", 
                     non.retrieved.variables = "") {
  # Retrieve data from a CSV file (downloaded from REDCap) or directly from 
  # REDCap through the API.
  #
  # Args:
  #   data.retrieval.mode: String {file, api} indicating how we're retrieving 
  #                        the data.
  #   file.prefix: String idicating the path to the data file. Usually 
  #                DATA/DATA_WITH_NO_DUPS/TIPTOPExitSurvey<Country>
  #   file.content: String indicating the middle part of the file name which
  #                 indicates if the data contains duplicates or not. Usually
  #                 either _DATA_ or _DATA_WITH_NO_DUPS_ or other 
  #                 data transformation.
  #   file.date: String indicating the date of the data in the file.
  #   file.time: String indicating the time of the data in the file.
  #   api.url: String indicating the URL of the REDCap API where the data is.
  #   api.token: String indicating the token for accessing the REDCap API.
  #   non.retireved.variables: Vector containing the list of fields or variables
  #                            to be excluded during the data export.
  # Returns:
  #   Data frame with the dataset.
  if (data.retrieval.mode == "file") {
    data.filename <- paste0(file.prefix, file.content, file.date, "_", 
                            gsub(":", "", file.time), ".csv")
    data <- read.csv(data.filename, stringsAsFactors = FALSE)
  } 
  else if (data.retrieval.mode == "api") {
    rcon <- redcapConnection(api.url, api.token)
    field.names <- exportFieldNames(rcon)
    fields <- unique(
      field.names$original_field_name[! field.names$original_field_name %in% 
                                        non.retrieved.variables])
    data <- exportRecords(rcon, factors = F, fields = fields)
  }
  else {
    message("data.retrieval.mode must be either file or api. No more modes 
            available.")
    return(NULL)
  }
  
  return(data)
}


# Get the timestamp of the last collected record
LastRecordDate <- function(data) {
  # Get the timestamp of the last collected record.
  #
  # Args:
  #   data: Data frame containing the study data set.
  #
  # Returns:
  #   String indicating the date of the last interview done.
  return(max(as.character(data$interview_date), na.rm = T))
}


NumberOfRecords <- function(data) {
  # Get the number of records uploaded to REDCap.
  #
  # Args:
  #   data: Data frame containing the study data set.
  #
  # Returns:
  #   Int indicating how many records/interviews have been already done.
  return(nrow(data))
}