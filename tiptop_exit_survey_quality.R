library(redcapAPI)
library(kableExtra)

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
  
  if (length(dat) > 1) {
    dat <- dat[, order(names(dat))]
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


SurveyProfileOfHealthFacility <- function(exit.survey.data, health.system.data,
                                          study.area.column, facility, lang) {
  # Produce and print a Kable with the survey profile of a concrete health 
  # facility in which inconsistencies are identified and displyed in red with a 
  # tooltip describing the problem.
  #
  # Args:
  #   exit.survey.data: Data frame containing the exit survey data set.
  #   health.system.data: Data frame containing the health system data set.
  #   study.area.column: String indicating the column name in the data frame 
  #                      storing the study area.
  #   facility: String indicating the health facility.
  #   lang: List of strings in the plot which are language-specific.
  #
  # Returns:
  #   None
  kMaxNumberColumns <- 29
  kFontSize <- 10
  
  column <- paste0("facility_", study.area.column)
  hf.data <- exit.survey.data[which(exit.survey.data[column] == facility), ]
  hf.data$interview_only_date <- as.Date(hf.data$interview_date, "%Y-%m-%d")
  
  hs.data <- health.system.data[which(health.system.data[column] == facility), ]
  hs.data$interview_only_date <- as.Date(hs.data$interview_date, "%Y-%m-%d")
  
  # Row two of survey profile - Women reached for an interview
  reached.women.per.day <- table(hf.data$interview_only_date)
  
  # Row three of survey profile - Women reached that meet criteria 1, where
  # criteria 1 is pregnant women when leaving an antenatal care visit
  reached.pregnant.women.per.day <- table(hf.data[
    which(hf.data$pregnant_woman == 1 & hf.data$anc_visit == 1), 
    'interview_only_date'
  ])
  
  # Row four of survey profile - Women reached that meet criteria 2, where
  # criteria 2 is living in the districts were TIPTOP is being implemented, for
  # a period of 6 months before the interview (at least)
  reached.resident.women.per.day <- table(hf.data[
    which(hf.data$resident_during_pregnancy == 1), 
    'interview_only_date'
  ])
  
  # Row five of survey profile - Eligible women (that meet criteria 1 and 2)
  # selected for an interview
  eligible.women <- table(hf.data[
    which(hf.data$pregnant_woman == 1 & hf.data$anc_visit == 1 & 
            hf.data$resident_during_pregnancy == 1), 
    'interview_only_date'
  ])
  
  # Row six of survey profile - Women that sign the informed consent 
  # (women interviewed)
  interviewed.women <- table(hf.data[
    which(hf.data$consent == 1), 
    'interview_only_date'
  ])
  
  # Row seven of survey profile - Women that do not sign the informed consent
  # (women NON interviewed)
  non.interviewed.women <- table(hf.data[
    which(hf.data$consent == 0), 
    'interview_only_date'
  ])
  
  # Row eight of survey profile - Women that refused to participate in the study
  women.who.refused <- table(hf.data[
    which(hf.data$why_not_consent == 0), 
    'interview_only_date'
  ])
  
  # Row nine of survey profile - Women that are not able to respond
  women.not.able <- table(hf.data[
    which(hf.data$why_not_consent == 1), 
    'interview_only_date'
  ])
  
  # TODO(maxramirez84): Include row in the profile and number it here 
  #                     accrodingly
  # Row ?? (not in current version) of survey profile - Women that are not 
  # available taking into account the time required to complete the interview
  women.not.available <- table(hf.data[
    which(hf.data$why_not_consent == 2), 
    'interview_only_date'
  ])
  
  # Row 10 of survey profile - Women that do not sign IC due to other reason
  women.non.ic.other <- table(hf.data[
    which(hf.data$why_not_consent == 88), 
    'interview_only_date'
  ])
  
  # Row 11 of survey profile - Women that sign the informed consent but 
  # interrupt the interview at some point later on
  women.interrupt <- table(hf.data[
    # sp_community_doses is the last question of the interview (q30)
    which(hf.data$consent == 1 & is.na(hf.data$sp_community_doses)),
    'interview_only_date'
  ])
  
  # Row 12 of survey profile - Interviewed women that had and episode of malaria
  # during their current pregnancy
  interviewed.mip <- table(hf.data[
    which(hf.data$consent == 1 & hf.data$mip %in% c(1, 2)),
    'interview_only_date'
  ])
  
  # Row 13 of survey profile - Interviewed women suffering an episode of malaria
  # (in current pregnancy) that requires hospitalization
  interviewed.mip.hosp <- table(hf.data[
    which(hf.data$consent == 1 & hf.data$mip_hosp == 1),
    'interview_only_date'
  ])
  
  # Row 14 of survey profile - Interviewed health workers
  interviewed.hw <- table(hs.data$interview_only_date)
  
  profile <- Union(
    # Row 1 not available from REDCap data = Number of women visited at ANC                              
    reached.women.per.day,          # Row 2 = Women reached for an interview
    reached.pregnant.women.per.day, # Row 3 = Women reached that meet criteria 1
    reached.resident.women.per.day, # Row 4 = Women reached that meet criteria 2
    eligible.women,                 # Row 5 = Eligible women (meet all criteria)
    interviewed.women,              # Row 6 = Women that sign IC - interviewed
    non.interviewed.women,          # Row 7 = Women that don't sign IC
    women.who.refused,              # Row 8 = Women that refused to participate
    women.not.able,                 # Row 9 = Women that aren't able to respond
    # TODO(maxramirez84): Include row in the profile and number it here 
    #                     accrodingly
    women.not.available,            # Row ? = Women that aren't available
    women.non.ic.other,             # Row 10 = Women that don't sign (others)
    women.interrupt,                # Row 11 = Women that interrupt interview
    interviewed.mip,                # Row 12 = Women that had mip
    interviewed.mip.hosp,           # Row 13 = Women that had complicated mip
    interviewed.hw                  # Row 14 = Interviewed health workers
  )
  row.names(profile) <- c(
    lang$profile.row2, 
    paste0(lang$profile.row3, footnote_marker_symbol(1, "html")), 
    paste0(lang$profile.row4, footnote_marker_symbol(2, "html")),
    paste0(lang$profile.row5, footnote_marker_symbol(3, "html")),
    lang$profile.row6, 
    lang$profile.row7,
    lang$profile.row8, 
    lang$profile.row9,
    lang$profile.rowX, 
    lang$profile.row10,
    lang$profile.row11, 
    lang$profile.row12,
    lang$profile.row13,
    lang$profile.row14
  )
  profile$total <- rowSums(profile)
  
  # Consistency checks
  profile.checked <- profile
  for (i in colnames(profile)) {
    # Check 1: non.interviewed.women = women.who.refused + women.not.able + 
    #                                  women.not.available + women.non.ic.other
    check1 <- profile[7, i] + profile[8, i] + profile[9, i] + 
      profile[10, i] != profile[6, i]
    
    profile.checked[c(6, 7, 8, 9, 10), i] = cell_spec(
      x        = profile[c(6, 7, 8, 9, 10), i],
      format   = "html",
      color    = ifelse(check1, "red", ""),
      tooltip  = ifelse(check1, lang$profile.check1, "")
    )
    
    # TODO(maxramirez84): Identify and implement more checks in the profile
  }
  
  # TODO(maxramirez84): Check if profile has more columns than kMaxNumberColumns
  #                     and create one or more kables accordingly
  
  print(kable(profile.checked, "html", escape = F) %>% 
          kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                        font_size = kFontSize) %>%
          row_spec(0, bold = T, color = "white", background = "#494949") %>%
          row_spec(c(1, 4, 5, 14), bold = T) %>%
          add_indent(c(5, 6, 11, 12, 13), level_of_indent = 1) %>% 
          add_indent(c(7, 8, 9, 10), level_of_indent = 2) %>%
          footnote(
            general_title = paste0(lang$profile.notes.title, ":"),
            general       = lang$profile.notes.desc,
            symbol        = c(
              lang$profile.note2, 
              lang$profile.note3,
              lang$profile.note1
            )
          )
  )
}


GetDuplicatedWomen <- function(data, study.area.column, study.area.label, 
                               lang) {
  # Compute a data frame containing all the records that are duplicated on the
  # woman ID variable, considering that IDs are unique by interviewer and
  # day.
  #
  # Args:
  #   data: Data frame containing the study data set.
  #   study.area.column: String indicating the column name in the data frame 
  #                      storing the study area.
  #   study.area.label: String containing the name of the study area.
  #   lang: List of strings in the plot which are language-specific.
  #
  # Returns:
  #   Data frame with records in which woman ID is being reused by the same
  #   interviewer and day.
  data$interview_only_date <- as.Date(data$interview_date, "%Y-%m-%d")
  
  column.facility <- paste0("facility_", study.area.column)
  column.residence <- paste0("residence_", study.area.column)
  
  # All variables are the same except record id
  x <- duplicated(data[2:ncol(data)])
  y <- duplicated(data[2:ncol(data)], fromLast = T)
  dup.records <- data[x | y, ]
  
  # ID variables are the same
  key.columns <- c(column.facility, "woman_id", "interviewer_id", 
                   "interview_only_date")
  id.columns <- data[key.columns]
  x <- duplicated(id.columns)
  y <- duplicated(id.columns, fromLast = T)
  dup.women <- data[x | y, ]
  
  # Exclude from reused women IDs the duplicated records
  x <- !(dup.women$record_id %in% dup.records$record_id)
  reused.woman.ids <- dup.women[x, ]
  
  # Check if there is reused womam IDs which are also duplicated records
  reused.and.duplicated <- intersect(reused.woman.ids[key.columns], 
                                     dup.records[key.columns])
  
  if (nrow(reused.and.duplicated) > 0) {
    for (i in 1:nrow(reused.and.duplicated)) {
      if (!is.na(reused.and.duplicated[i, column.facility])) {
        a <- dup.records[column.facility] == 
          reused.and.duplicated[i, column.facility]
        b <- dup.records$woman_id == reused.and.duplicated$woman_id[i]
        x <- a & b
          
        reused.woman.ids <- rbind(reused.woman.ids, dup.records[x, ][1, ])
      }
    }
  }
  
  if (nrow(reused.woman.ids) == 0) {
    return(NULL)
  }
  
  columns <- c("record_id", "district", column.facility, "woman_id", "consent",
               column.residence, "reported_age", "interviewer_id", 
               "interview_date")
  reused.woman.ids.sum <- reused.woman.ids[
    order(reused.woman.ids[column.facility], reused.woman.ids$woman_id, 
          reused.woman.ids$interviewer_id, reused.woman.ids$interview_date), 
    columns]
  
  # Remove deleted records
  reused.woman.ids.sum <- 
    reused.woman.ids.sum[!is.na(reused.woman.ids.sum$district), ]
  
  # Place labels
  reused.woman.ids.sum$consent[is.na(reused.woman.ids.sum$consent)] <- 
    lang$dups.label1
  reused.woman.ids.sum$consent[reused.woman.ids.sum$consent == 0] <- lang$no
  reused.woman.ids.sum$consent[reused.woman.ids.sum$consent == 1] <- lang$yes
  
  reused.woman.ids.sum$district <- study.area.label
  
  return(reused.woman.ids.sum)
}

DuplicatedWomen <- function(data, study.area.column, study.area.label, lang) {
  # Create and print a Kable containing all the records that are duplicated on 
  # the woman ID variable, considering that IDs are unique by interviewer and
  # day.
  #
  # Args:
  #   data: Data frame containing the study data set.
  #   study.area.column: String indicating the column name in the data frame 
  #                      storing the study area.
  #   study.area.label: String containing the name of the study area.
  #   lang: List of strings in the plot which are language-specific.
  #
  # Returns:
  #   None
  reused.women.ids.sum <- GetDuplicatedWomen(data, study.area.column,
                                             study.area.label, lang)
  
  if (!is.null(reused.women.ids.sum)) {
    if (nrow(reused.women.ids.sum) > 0) {
      colnames(reused.women.ids.sum) <- c(
        lang$dups.tab.header1, lang$dups.tab.header2, lang$dups.tab.header3, 
        lang$dups.tab.header4, lang$dups.tab.header5, lang$dups.tab.header6,
        lang$dups.tab.header7, lang$dups.tab.header8, lang$dups.tab.header9
      )
      
      kable(reused.women.ids.sum, "html", row.names = F, escape = F) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                      font_size = 12) %>%
        row_spec(0, bold = T, color = "white", background = "#494949") %>%
        scroll_box(height = "250px")
    } else {
      print(lang$dups.no.reuse)
    }
  } else {
    print(lang$dups.no.reuse)
  }
}


GetDuplicatedRecords <- function(data, study.area.column, study.area.label, lang) {
  # Compute a data frame containing all the records that are duplicated, i.e.
  # all variables are equal except record id.
  #
  # Args:
  #   data: Data frame containing the study data set.
  #   study.area.column: String indicating the column name in the data frame 
  #                      storing the study area.
  #   study.area.label: String containing the name of the study area.
  #   lang: List of strings in the plot which are language-specific.
  #
  # Returns:
  #   Data frame with records in which all variables are the equal.
  column.facility <- paste0("facility_", study.area.column)
  column.residence <- paste0("residence_", study.area.column)
  
  # All variables are the same except the record id
  x <- duplicated(data[2:ncol(data)])
  y <- duplicated(data[2:ncol(data)], fromLast = T)
  dup.records <- data[x | y, ]
  
  columns <- c("record_id", "district", column.facility, "woman_id", "consent",
               column.residence, "reported_age", "interviewer_id", 
               "interview_date")
  
  dup.records.sum <- dup.records[order(dup.records[column.facility], 
                                       dup.records$woman_id), columns]
  
  if (nrow(dup.records.sum) > 0) {
    dup.records.sum$consent[is.na(dup.records.sum$consent)] <- lang$dups.label1
    dup.records.sum$consent[dup.records.sum$consent == 0] <- lang$no
    dup.records.sum$consent[dup.records.sum$consent == 1] <- lang$yes
    
    dup.records.sum$district <- study.area.label
  }
  
  return(dup.records.sum)
}


DuplicatedRecords <- function(data, study.area.column, study.area.label, lang) {
  # Create and print a Kable containing all the records that are duplicated, 
  # i.e. all variables are equal except record id.
  #
  # Args:
  #   data: Data frame containing the study data set.
  #   study.area.column: String indicating the column name in the data frame 
  #                      storing the study area.
  #   study.area.label: String containing the name of the study area.
  #   lang: List of strings in the plot which are language-specific.
  #
  # Returns:
  #   None
  dup.records.sum <- GetDuplicatedRecords(data, study.area.column, 
                                          study.area.label, lang)
  
  if (nrow(dup.records.sum) > 0) {
    colnames(dup.records.sum) <- c(
      lang$dups.tab.header1, lang$dups.tab.header2, lang$dups.tab.header3, 
      lang$dups.tab.header4, lang$dups.tab.header5, lang$dups.tab.header6,
      lang$dups.tab.header7, lang$dups.tab.header8, lang$dups.tab.header9
    )
    
    kable(dup.records.sum, "html", row.names = F, escape = F) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                    font_size = 12) %>%
      row_spec(0, bold = T, color = "white", background = "#494949") %>%
      scroll_box(height = "250px")
  } else {
    print(lang$dups.no.records)
  }
}


MiPRate <- function(data) {
  # Compute the percentage of women having malaria in pregnancy.
  #
  # Args:
  #   data: Data frame containing the study data set.
  #
  # Returns:
  #   Int indicating percentage of MiP
  consented <- NumberOfParticipantsWhoConsented(data)
  
  mip <- table(data$mip)
  
  if (is.na(mip[2])) {
    mip.rate <- 0 
  } else { 
    mip.rate <- floor(mip[2] / consented * 100)
  }
  
  return(mip.rate)
}


HospitalizedMiPRate <- function(data) {
  # Compute the percentage of women having malaria in pregnancy and who got
  # hospitalized due to it.
  #
  # Args:
  #   data: Data frame containing the study data set.
  #
  # Returns:
  #   Int indicating percentage of hospitalized MiP
  consented <- NumberOfParticipantsWhoConsented(data)
  
  mip <- table(data$mip)
  mip.hosp <- table(data$mip_hosp)
  
  if (is.na(mip.hosp[2])) {
    mip.hosp.rate <- 0 
  } else { 
    mip.hosp.rate <- floor(mip.hosp[2] / mip[2] * 100)
  }
  
  return(mip.hosp.rate)
}