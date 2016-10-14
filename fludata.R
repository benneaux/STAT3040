require(tm)
require(pdftools)
require(stringi)

setwd("C:/Users/50026161/Downloads/www.flutracking.net/Info/Reports/")
files <- list.files(pattern = "pdf$")
data <- matrix(nrow=length(files),ncol = 12)
val1 <- vector(length=length(files))
val2 <- vector(length=length(files))
val3 <- vector(length=length(files))
val4 <- vector(length=length(files))
val5 <- vector(length=length(files))
val6 <- vector(length=length(files))
val7 <- vector(length=length(files))
val8 <- vector(length=length(files))
val9 <- vector(length=length(files))
val10 <- vector(length=length(files))
val11 <- vector(length=length(files))
val12 <- vector(length=length(files))

for(i in 1:length(files)){
  tryCatch({
  txt <- pdf_text(files[i])[1]
  fludate <- if(
    is.na(unlist(stri_extract_first_regex(as.list(txt),"[0-9]{2} [A-Z][a-z]{1,10} [0-9]{4}")))) {
      unlist(stri_extract_first_regex(as.list(txt),"[0-9]{1} [A-Z][a-z]{1,10} [0-9]{4}"))
    } else {
      unlist(stri_extract_first_regex(as.list(txt),"[0-9]{2} [A-Z][a-z]{1,10} [0-9]{4}"))
    }
   val2[i] <- unlist(stri_extract_all_charclass(stri_extract_all_regex(as.list(txt),"(received [0-9]*|[0-9]* responses)"),"\\p{N}"))
   val3[i] <- unlist(stri_extract_all_charclass(unlist(stri_extract_all_regex(as.list(txt),"[0-9]{3,} people|from [0-9]{3,}")),"\\p{N}"))
   val4[i] <- unlist(stri_extract_all_charclass(unlist(stri_extract_all_regex(as.list(txt),"[0-9]{3,} household|[0-9]{3,}\r\nhousehold")),"\\p{N}"))
   val5[i] <- unlist(stri_extract_all_charclass(unlist(stri_extract_all_regex(as.list(txt),"[0-9]{3,}+/")),"\\p{N}"))
   val6[i] <- unlist(stri_extract_all_charclass(unlist(stri_extract_all_regex(as.list(txt),"/+[0-9]{3,}")),"\\p{N}"))
   val7[i] <- unlist(stri_extract_all_charclass(unlist(stri_extract_all_regex(as.list(txt),"[0-9]{3,} participants|vaccine so far. Of the [0-9]{3,}")),"\\p{N}"))
   val8[i] <- unlist(stri_extract_all_charclass(unlist(stri_extract_all_regex(as.list(txt),"patients, [0-9]{3,}")),"\\p{N}"))
   val9[i] <- unlist(stri_extract_all_regex(unlist(stri_extract_all_regex(as.list(txt),"\\d[.]+\\d% of vaccinated|\\d% of vaccinated|reported by \\d[.]+\\d%")),"\\d[.]+\\d%|\\d%"))[1]
   val10[i] <- unlist(stri_extract_all_regex(unlist(stri_extract_all_regex(as.list(txt),"\\d{0,}[.]{0,}\\d% of unvaccinated|\\d% of unvaccinated|vaccinated participants and \\d{0,}[.]{0,}+\\d%")),"\\d[.]+\\d%|\\d%"))[1]
   val11[i] <- unlist(stri_extract_all_regex(unlist(stri_extract_all_regex(as.list(txt),"\\d{0,}[.]{0,}\\d% of vaccinated|\\d% of vaccinated|reported by \\d[.]+\\d%")),"\\d[.]+\\d%|\\d%"))[2]
   val12[i] <- unlist(stri_extract_all_regex(unlist(stri_extract_all_regex(as.list(txt),"\\d{0,}[.]{0,}\\d% of unvaccinated|\\d% of unvaccinated|vaccinated participants and \\d{0,}[.]{0,}+\\d%")),"\\d[.]+\\d%|\\d%"))[2]
   vals=cbind(val2,val3,val4,val5,val6,val7,val8,val9,val10,val11,val12)
   data[,2:12] <- vals
   data[i,1] <- fludate
  },
  error=function(e){})
  }

colnames(data) <- c("Week_end",
                    "Responses",
                    "Self_Report",
                    "Other_Report",
                    "Vaccinated_Respondents",
                    "Total_Respondents",
                    "Clinical_Staff",
                    "Clinical_Staff_Vaccinated",
                    "ILI_Vaccinated",
                    "ILI_Unvaccinated",
                    "ILI_wAbsence_Vaccinated",
                    "ILI_wAbsence_Unvaccinated")
write.csv(data,file="fludata.csv")
