
# Com base no pacote "lettercase"
str_title_case <- function(x){
    x <- sub("^\\s+", "", x, perl = TRUE)
    x <- sub("\\s+$", "", x, perl = TRUE)
    x <- gsub( "[\\s_]+", " ", x, perl = TRUE)  # whitespace to single space
    x <- strsplit(x, " ")
    title_case <- function(a){
        a <- tolower(a)
        a <- sub("^(.)", "\\U\\1", a, perl = TRUE)
        a
    }
    x <- lapply(x, title_case)
    x <- sapply(x, paste, collapse = " ")
    x <- gsub(" Iii", " III", x)
    x <- gsub(" Iv", " IV", x)
    x <- gsub(" Ii", " II", x)
    for(w in c(" E ",  " Da ", " De ", " Em ", " Of ", " An ", " On ", " In ",
               " The ", " And "))
        x <- gsub(w, tolower(w), x)
    return(x)
}
