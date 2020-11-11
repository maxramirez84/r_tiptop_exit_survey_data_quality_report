library(redcapAPI)

kColorPalette <- c("gray8", "gray35", "gray90")

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


NumberOfParticipantsWhoConsented <- function(data) {
  # Compute number of participants who consented the interview.
  #
  # Args:
  #   data: Data frame containing the study data set.
  #
  # Returns:
  #   Int indicating how many women signed the informed consent.
  consent.table <- table(data$consent)
  
  if (length(consent.table) > 0){
    consented <- consent.table[1] # consent = yes
  } else {
    consented <- 0
  }
  
  return(consented)
}


RecruitmentRate <- function(data, sample.size) {
  # Compute the recruitment rate.
  #
  # Args:
  #   data: Data frame containing the study data set.
  #   sample.size: Int indicating the expected number of women to be 
  #                interviewed.
  #
  # Returns:
  #   Double indicating the rate of recruitment
  consented <- NumberOfParticipantsWhoConsented(data)
  
  if (is.na(consented)) {
    recruitment.rate <- 0 
  } else {
    recruitment.rate <- floor((consented / sample.size) * 100)
  } 
    
  return(recruitment.rate)
}


Union <- function(...) {
  # Behaves as MySQL UNION statement. Appends a list just below the other.
  #
  # Args:
  #   ...: Lists to be appended.
  #
  # Returns:
  #   Data frame containing the lists appended.
  aux <- list(...)
  dat <- data.frame()
  for (i in seq(along = aux)) {
    if (length(aux[[i]]) == 0) {
      dat[i, ] <- rep(0, ncol(dat))
    } else {
      for (j in names(aux[[i]]))
        dat[i, j] <- aux[[i]][j] 
    }
  }
  dat <- rapply(dat, f = function(x) ifelse(is.na(x), 0, x), how = "replace")
  return(dat)
}


ProgressOfArea <- function(data, study.area.column, study.area.label, interval, 
                           approaches.mean, lang) {
  # Plot a bar graph indicating number of approached and interviewed women by 
  # health facility in a concrete study area.
  #
  # Args:
  #   data: Data frame containing the study data set.
  #   study.area.column: String indicating the column name in the data frame 
  #                      storing the study area.
  #   study.area.label: String containing the name of the study area.
  #   interval: Int indicating the y-axis interval.
  #   approaches.mean: Int indicating the expected number of women to be 
  #                    approached when exiting ANC for getting the required 
  #                    sample size.
  #   lang: List of strings in the plot which are language-specific.
  #
  # Returns:
  #   None
  column <- paste0("facility_", study.area.column)
  
  approaches.number <- table(data[column])
  if (length(approaches.number) > 0) {
    max.y.axis <- max(approaches.number) + interval * 3
    consented.number <- table(data[data$consent == 1, column])
    
    dat <- Union(approaches.number, consented.number)
    par(cex.lab = 1, cex.main = 1.4, cex.axis = 1, mar = c(5, 5, 4, 0))
    progress <- barplot(
      height = matrix(c(dat[2, ], dat[1, ] - dat[2, ]), nrow = 2, byrow = T),
      main   = sprintf(lang$progress.plot.title, study.area.label),
      xlab   = sprintf(lang$progress.plot.x, study.area.label),
      ylab   = lang$progress.plot.y,
      ylim   = c(0, max.y.axis),
      axes   = F,
      col    = kColorPalette[2:3],
      mgp    = c(4, 1, 0)
    )
    axis(1, progress, 
         sprintf(lang$progress.plot.hf, rownames(approaches.number)), las = 2)
    axis(2, seq(0, max.y.axis, interval))
    abline(h = approaches.mean, lwd = 1, lty = 2, col = "red")
    legend("topright", legend = c(lang$progress.plot.s1, lang$progress.plot.s2),
           fill = kColorPalette[2:3], cex = 1)
    text(x = progress, y = dat[2, ], labels = dat[2, ], pos = 3, 
         col = kColorPalette[1])
  } else {
    print(lang$progress.no.data) 
  }
}