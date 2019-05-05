rm(list=ls());cat("\014");gc();
packs<- list("dplyr", "data.table", "pdftools", "stringr", "lubridate")
sapply(setNames(packs,packs), require, character.only = T)
###############

view <- function(df, n=5){
  tib <- as_tibble(df)
  print(head(tib, n), n=n)
  print(tib[sort(sample(1:nrow(tib),n)),], n=n)
  print(tail(tib, n), n=n)}

text <- pdf_text("../Times.pdf")
times <- data.table(text = unlist(strsplit(text, "\n")))
times[, I := .I]
names <- c("Claire Day", "Steve Day", "David Deininger")
times[text %in% names, author := text]
times[str_sub(text,1,1) == " ", author := 'Matthew Deininger']
times[, end := str_sub(text, nchar(text)-1, nchar(text))]
times[end %in% c("AM", "PM"), author := "Date"]
times[author == "Date", date := str_match(text, ".{1,2}/.{1,2}/.{1,2}")]
times[!is.na(date), year := as.numeric(paste0("20", str_sub(str_match(date, "/.{1,2}$"), 2,3)))]
times[!is.na(date), day := as.numeric(str_remove_all(str_match(date, "/.{1,2}/"), "/"))]
times[!is.na(date), month := as.numeric(str_sub(str_match(date, "^.{1,2}/"), 1,2))]
times[!is.na(date), date2 := as_date(paste(year, month, day, " "))]
times[str_detect(text, "Today") & author == "Date", date2 := as_date(paste(2019, 4, 21))]
times[str_detect(text, "Yesterday") & author == "Date", date2 := as_date(paste(2019, 4, 20))]

times <- times[-1,]
times[,date := NULL]
times[, date := zoo::na.locf(date2)]
times <- times[author != "Date" | is.na(author),]
times[, author := zoo::na.locf(author)]
times[, c("date2", "end", "year", "month", "day") := NULL]
view(times)

others <- c("Deborah", "Debbie", "Eli", "Chris", "Rachel")
times[ , who := str_extract(text, paste(others, collapse = "|"))]
times[who == "Debbie", who := "Deborah"]
times[is.na(who), who := author]
times[, puzzle_time := str_extract(text, "\\d{1,2}\\.\\d{2}")]

times <- times[!is.na(puzzle_time),]
times[, c("text", "I") := NULL]

times[,seconds := str_sub(str_extract(puzzle_time, "\\.\\d{2}"),2,3)]
times[,minutes := str_remove(str_extract(puzzle_time, "\\d{1,2}\\."), "\\.")]
times[,puzzle_time_num := as.numeric(seconds) + as.numeric(minutes)*60]

times[,day_of_week := weekdays(date)]
times <- times[(day_of_week != "Saturday" & puzzle_time_num < 5*60) | day_of_week == "Saturday"]
saveRDS(times, "./times_data/puzzle_times_4_21_2019.RDS")

        