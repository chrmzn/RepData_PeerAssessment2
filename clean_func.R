
load_data <- function () {
    download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', destfile = 'StormData.csv.bz2', method = 'curl')
    system('bunzip2 StormData.csv.bz2')
    df <- read.csv('StormData.csv')
}

inital_filtering <- function(df){
    human_impact <- df %>% filter(FATALITIES > 0 | INJURIES > 0)
}

build_event_to_pattern_map <- function (eventTypeFile){
    eventTypeValues <- readLines(eventTypeFile)
    eventType <- list()
    for (event in eventTypeValues){
        spaceSplit <- strsplit(event, ' ')[[1]]
        lastElement <- length(spaceSplit)
        eventName <- paste(spaceSplit[-lastElement], collapse = ' ')
        typeSplit <- strsplit(eventName, '/')[[1]]
        regexpV <- vector('character', length = length(typeSplit))
        for( i in seq_along(typeSplit) ){
            if(i == 1){
                regexpV[i] <- paste('(?=.*', typeSplit[i], '){1}', sep = '')
            } else {
                regexpV[i] <- paste('(?=.*', typeSplit[i], '){0,1}', sep = '')
            }
        }
        if(grepl('Hurricane', eventName)){
            regexpV <- c('(?=.*Hurricane)|(?=.*Typhoon)')
        }
        eventType[[eventName]] <- paste(regexpV, collapse ='')
    }
    eventType
}

apply_event_map <- function (df, eventMap) {
    df$ProcessedEventType <- 'Unknown'
    for (i in seq_along(eventMap)) {
        eventName <- names(eventMap[i])
        eventMatch <- eventMap[[eventName]]
        indexMatch <- grep(eventMatch,df$EVTYPE, ignore.case=T, perl=T)
        df[indexMatch,]$ProcessedEventType <- eventName
    }
    df
}

apply_cost_modifier <- function ( cost, modifier ){
    if(modifier == 'b'){
        cost <- cost * 100000000
    } else if (modifier == 'm' ) {
        cost <- cost * 1000000
    } else if (modifier == 'k' ) {
        cost <- cost * 1000
    }
    cost
}






