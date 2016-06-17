## library to read the sdmx's
library(rsdmx)

## libraries for data manipulation
library(dplyr)
library(tidyr)

## libraries for interactive plot, table and map
library(DT)
library(rCharts)
library(leaflet)
library(rgdal)




## Read system tables, always offline
tabIndicators <-
    read.csv2("backupData/tabIndicatori.csv", stringsAsFactors = F)
tabNUTS <- read.csv2("backupData/tabNUTS.csv", stringsAsFactors = F)
tabConcepts <-
    read.csv2("backupData/tabConcepts.csv", stringsAsFactors = F)
tabSettori <-
    read.csv2("backupData/tabSettori.csv", stringsAsFactors = F)

#Map data
NUTS2 <- readOGR("Shapefile/NUTS_RG_10M_2013.shp", 
                 layer = "NUTS_RG_10M_2013", 
                 verbose = F, 
                 stringsAsFactors = F)
NUTS2_ALL <- subset(NUTS2, NUTS2@data$STAT_LEVL_ == 2)
NUTS2_SP <- subset(NUTS2, NUTS2@data$NUTS_ID %in% tabNUTS$id)

## Read the NUTS list
getGEOFilters <- function() {
    filters <- select(tabNUTS, id) %>% 
        unlist()
    return(filters)
}

## All possbile NUTS combinations
totalCombOfGEO <- unlist(sapply(length(getGEOFilters()):1, function(i) combn(length(getGEOFilters()), i, simplify = F)), recursive = F)

## Get the ind key, from ind id
getKey <- function(id) {
    key <- filter(tabIndicators, idDataFlow == id) %>%
        select(nome) %>%
        as.character()
    return(key)
}


## Get the concepts (filters) relative to the key DataFlow from EUROSTAT
downloadConcepts <- function(id) {
    BaseUrl <-
        "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_"
    key <- getKey(id)
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    concepts <- as.data.frame(DSD@concepts,
                              conceptSchemeId = paste0("CS_DSD_", key))
    return(concepts)
}

## Get the code list relative to the concept from EUROSTAT
downloadCodeList <- function(id, concept) {
    BaseUrl <-
        "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_"
    key <- getKey(id)
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    codelist <- as.data.frame(DSD@codelists,
                              codelistId = paste0("CL_", concept))
    return(codelist)
}

## Download the data, given the filter
downloadData <- function(id, geoF) {
    BaseUrl <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data"
    key <- getKey(id)
    geoFilter <- paste0(getGEOFilters()[geoF[[1]]], collapse = '+')
    filter <- paste0(getFilters(id), ".", geoFilter, ".")
    dataUrl <- paste(BaseUrl,
                     key,
                     filter,
                     sep = "/")
    tryCatch(
        {
            dataz <- as.data.frame(readSDMX(dataUrl))
            print(dataUrl)
            write.csv2(dataz, 
                       paste0("backupData/", 
                              key, "-", 
                              id, ".csv"), 
                       row.names = F)
        },
        warning = function(w) {
            print("warning")
            dataz <- as.data.frame(readSDMX(dataUrl))
            write.csv2(dataz, 
                       paste0("backupData/", 
                              key, "-", 
                              id, ".csv"), 
                       row.names = F)
            
        },
        error = function(e) {
            print("GEO filter not right, retrying next one")
            geoF[[1]] <- NULL
            downloadData(id, geoF)
        }
    )
    
    #return(dataz)
}

## Read the indicator file
getDataOffline <- function(id) {
    key <- getKey(id)
    dataz <- read.csv2(
        paste0("backupData/", key, "-", id, ".csv"),
        stringsAsFactors = F,
        quote = "\""
    )
    # read.csv2 trasforma le colonne "T" in booleane
    # quindi li ritrasformiamo in character
    # (non che quelle colonne siano utilizzate)
    dataz[,sapply(dataz,class) == "logical"] <-
        sapply(dataz[,sapply(dataz,class) == "logical"],
               function(i)
                   substr(as.character(i),1,1))
    
    return(dataz)
}

## Wrapper, read from offline or download if older than 10 days
getData <- function(id) {
    key <- getKey(id)
    file <- paste0("backupData/", key, "-", id, ".csv")
    #geoFilter <- getGEOFilters()
    if (file.exists(file)) {
        old <-
            difftime(Sys.time(), file.info(file)$mtime, units = "days") > 10
        if (!old)
            dataz <- getDataOffline(id)
        else dataz <- downloadData(id, totalCombOfGEO)
    } else dataz <- downloadData(id, totalCombOfGEO)
    
    lastYear <- dataz %>% 
        filter(GEO == "ITH2" & obsValue != "NA") %>%
        summarise(year = max(obsTime)) %>%
        as.numeric()
    direction <- filter(tabIndicators, idDataFlow == id) %>%
        select(direction) %>%
        as.character()
    dataz <- dataz %>% 
        filter(obsTime <= lastYear & obsValue != "NA") %>%
        select(obsTime, GEO, obsValue) %>%
        group_by(obsTime, GEO) %>%
        summarise(obsValue = sum(obsValue)) %>%
        ungroup()
    
    rank <- dataz %>%
        group_by(obsTime) %>% 
        mutate(dir = direction) %>%
        mutate(obsValue = as.integer(
            ifelse(dir == "-", 
                   rank(obsValue), 
                   rank(desc(obsValue))
            )
        ) 
        ) %>% 
        select(-dir) %>%
        ungroup()
    return(list(value = dataz,
                rank = rank,
                lastYear,
                direction
    )
    )
}

## Transform the data to have vertical year and horizontal Geo
pivotData <- function(x) {
    
    dataz <- x[[1]]
    dir <- x[[4]]
    flag <- "<img src=\"FLAG.svg\" height=\"16\" width = \"16\"></img>"
    img <- "<img src=\"arrow_DIR.svg\" height=\"16\" width = \"16\"></img>"
    
    if (all(c("GEO", "obsTime", "obsValue") %in% names(dataz))) {
        res <- dataz %>%
            select(GEO, obsTime, obsValue) %>%
            spread(obsTime, obsValue) %>%
            full_join(select(tabNUTS, id, stato), 
                      by = c("GEO" = "id")) %>%
            .[, c(1, 
                  (ncol(.) - ifelse(ncol(.)>8, 
                                    8, ncol(.)-2)):ncol(.)
            )] %>%
            mutate(
                img = 
                    ifelse(
                        .[, ncol(.)-1] > .[, ncol(.)-2],
                        ifelse(
                            dir == "+",
                            sub("DIR", "up_green", img),
                            sub("DIR", "up_red", img)
                        ),
                        ifelse(
                            .[, ncol(.)-1] == .[, ncol(.)-2],
                            sub("DIR", "equal", img),
                            ifelse(
                                dir == "+",
                                sub("DIR", "down_red", img),
                                sub("DIR", "down_green", img)
                            )
                        )
                    )
            ) %>%
            mutate(
                stato = paste0(
                    "<img src=\"",
                    stato,
                    ".png\" height=\"18\" width = \"18\" alt = \"",
                    stato,
                    "\"></img>"
                )
            ) %>%
            select(GEO, stato, starts_with("20"), img) %>%
            arrange(GEO)
        
        return(res)
    } else
        cat("c'è stato qualche errore")
}

rankData <- function(x) {
    dataz <- x[[2]]
    img <- "<img src=\"arrow_DIR.svg\" height=\"16\" width = \"16\"></img>"
    
    if (all(c("GEO", "obsTime", "obsValue") %in% names(dataz))) {
        res <- dataz %>%
            select(GEO, obsTime, obsValue) %>%
            spread(obsTime, obsValue) %>%
            full_join(select(tabNUTS, id, stato), 
                      by = c("GEO" = "id")) %>%
            .[, c(1, (ncol(.) - ifelse(ncol(.)>8, 8, ncol(.)-2)):ncol(.))] %>%
            mutate(
                img = ifelse(
                    .[, ncol(.)-1] == .[, ncol(.)-2],
                    sub("DIR", "equal", img),
                    ifelse(
                        .[, ncol(.)-1] > .[, ncol(.)-2],
                        sub("DIR", "down_red", img),
                        sub("DIR", "up_green", img)
                    )
                )
            )  %>%
            mutate(
                stato = paste0(
                    "<img src=\"",
                    stato,
                    ".png\" height=\"18\" width = \"18\" alt = \"",
                    stato,
                    "\"></img>"
                )
            ) %>%
            select(GEO, stato, starts_with("20"), img) %>%
            arrange(GEO)
        
        return(res)
    }
}

## Helper to obtain the string to insert (manually) in the DB
getConceptsForSQL <- function(id) {
    lev <- levels(downloadConcepts(id)$id)
    a <- head(lev,-6) %>%
        paste(collapse = ".")
    return(a)
}

## Retrive the id starting from the name
getId <- function(key) {
    id <- as.integer(filter(tabIndicators, nome == key) %>%
                         select(idDataFlow))
    return(id)
}

## Retrive the filter values
getFilters <- function(id) {
    concepts <- filter(tabConcepts, idDataFlow == id) %>%
        select(value) %>%
        unlist() %>%
        paste0(collapse = ".")
    return(concepts)
}

## Retive sector (fixed)
getSectors <- function() {
    sectors <- tabSettori
    options <- list()
    for (i in 1:nrow(sectors)) {
        options[[as.character(sectors$descriz[i])]] <- sectors$idSettore[i]
    }
    
    return(options)
}

getSectorFromId <- function(id) {
    sector <- tabIndicators %>%
        filter(idDataFlow == id)
    return(sector$idSettore)
}


## Read the NUTS for the table in sidebar
getGEO <- function() {
    filters <- tabNUTS %>%
        transmute(
            id,Stato = paste0(
                "<img src=\"",
                stato,
                ".png\" height=\"24\" width = \"24\" alt = \"",
                stato,
                "\"></img>"
            ),
            Regione = descriz,
            link = wiki
            
        )
    return(filters)
}

## Get indicators
getIndicators <- function(idSector = NULL) {
    indicators <- tabIndicators
    if (!is.null(idSector))
        indicators <- filter(indicators, idSettore == idSector)
    
    return(indicators)
}

## List of indicators per sector to populate the dropdown
getIndicatorsList <- function(idSector = NULL) {
    indicators <- getIndicators(idSector)
    options <- list()
    for (i in 1:nrow(indicators)) {
        options[[as.character(indicators$descriz[i])]] <- indicators$idDataFlow[i]
    }
    return(options)
}

##
getIndName <- function(key) {
    if (!is.na(as.numeric(key))) {
        indicator <- tabIndicators %>%
            filter(idDataFlow == key) %>%
            select(descriz) %>%
            as.character()
    } else {
        indicator <- tabIndicators %>%
            filter(nome == key) %>%
            select(descriz) %>%
            as.character()
    }
    return(indicator)
}

## Build the plot
valuePlot <- function(data, selected) {
    dataz <- select(data[[1]], GEO, obsValue, obsTime) %>%
        mutate(obsTime = as.numeric(obsTime))
    
    colors <- c("#297957", "#77A337", "#CECE00", "#BF3400",
                "#242479", "#FF0000", "#0385BD", "#9AAD35",
                "#C3629D", "#FF8700", "#923262", "#DA7000")
    
    p <- hPlot(
        y = "obsValue",
        x = "obsTime",
        data = dataz,
        type = "line",
        group = "GEO",
        radius = 0
    )
    p$chart(zoomType = "xy")
    p$xAxis(title = list(text = "Anno"),
            categories = dataz$obsTime,
            tickInterval = 2
    )
    p$yAxis(title = list(text = "Valore"))
    p$colors(colors)
    
    selected <- pivotData(data)[selected, 1]
    
    # Black Magic
    p$params$series = lapply(seq_along(p$params$series), 
                             function(i) {
                                 x = p$params$series[[i]]
                                 x$visible = x$name %in% selected$GEO
                                 return(x)
                             })
    return(p)
}

rankPlot <- function(data, selected) {
    dataz <- select(data[[2]], GEO, obsValue, obsTime) %>%
        mutate(obsTime = as.numeric(obsTime))
    
    colors <- c("#297957", "#77A337", "#CECE00", "#BF3400",
                "#242479", "#FF0000", "#0385BD", "#9AAD35",
                "#C3629D", "#FF8700", "#923262", "#DA7000")
    
    p <- hPlot(
        y = "obsValue",
        x = "obsTime",
        data = dataz,
        type = "line",
        group = "GEO",
        radius = 0
    )
    p$chart(zoomType = "xy")
    p$xAxis(title = list(text = "Anno"),
            categories = dataz$obsTime,
            tickInterval = 2
    )
    p$yAxis(title = list(text = "Rank"),
            reversed = "true",
            min = 1,
            max = 12,
            startOnTick = "false",
            endOnTick = "false",
            tickInterval = 1,
            gridLineWidth = 0.5)
    p$colors(colors)
    
    selected <- pivotData(data)[selected, 1]
    
    # Black Magic
    p$params$series = lapply(seq_along(p$params$series), function(i) {
        
        x = p$params$series[[i]]
        x$visible = x$name %in% selected$GEO
        return(x)
    })
    return(p)
}

getWholeData <- function() {
    indicators <- getIndicators()
    
    df <- sapply(
        indicators$idDataFlow, FUN = function(x)
            x =  getData(x)[[1]]
    )
    names(df) <- indicators$nome
    df <-
        Reduce(function(...)
            merge(..., by = c("obsTime", "GEO"), all = T), df)
    colnames(df) <- c("obsTime", "GEO", indicators)
    return(df)
}

getRank <- function(id, year = "") {
    
    data <- getData(id)
    direction <- data[[4]]
    year <- ifelse(year == "Ultimo anno disponibile", 
                   data[[3]], 
                   year)
    
    df <- getData(id)[[1]] %>%
        filter(obsTime == year) %>%
        mutate(dir = direction) %>%
        mutate(rank = as.integer(
            ifelse(dir == "-", 
                   rank(obsValue), 
                   rank(desc(obsValue))
            )
        ) 
        )
    df
}

getBestTN <- function(year = "") {
    indicators <- getIndicators()
    df <- do.call(rbind, 
                  apply(
                      indicators, 
                      MARGIN = 1,
                      FUN = function(x){
                          d <- getRank(as.integer(x["idDataFlow"]),
                                       year) %>%
                              mutate(ind = x["descriz"]) %>%
                              filter(GEO == "ITH2" & rank<=3) 
                      }
                  )
    )
    df
}

getWorstTN <- function(year = "") {
    indicators <- getIndicators()

    df <- do.call(rbind, 
                  apply(
                      indicators, 
                      MARGIN = 1,
                      FUN = function(x){
                          d <- getRank(as.integer(x["idDataFlow"]),
                                       year) %>%
                              mutate(ind = x["descriz"]) %>%
                              filter(GEO == "ITH2" & rank>=10)
                      }
                  )
    )
    df
}

getComparison <- function(id, year) {
    df <-  getRank(id,year) %>%
        mutate(ind = id) %>%
        filter(GEO == "ITH2" | rank %in% c(1, 2, nrow(.)-1, nrow(.))) %>%
        inner_join(tabNUTS, by = c("GEO" = "id")) %>%
        select(rank, descriz, obsValue) %>%
        arrange(rank)
    df
}

## Start Comparison Module
## Comparison UI
comparisonUi <- function(id, ind) {
    options(ns.sep = "_")
    ns <- NS(id)
    
    nome <- tabIndicators %>% 
        filter(idDataFlow == ind) %>%
        select(descriz) %>%
        as.character(.)
    
    box(id = id,
        title = nome,
        width = 6,
        solidHeader = T,
        textOutput(ns("year")),
        tableOutput(ns("table")),
        actionButton(ns("sec"), "approfondisci")
    )
}

## Comparison Server
comparison <- function(input, output, session, ind) {
    
    output$year <- renderText({
        lastYear <- getData(ind)[[3]]
        paste0("Anno di riferimento: ", lastYear)
    })
    
    output$table <- renderTable({
        getComparison(ind, year = getData(ind)[[3]]) %>%
            select(Posizione = rank, Regione = descriz, Valore = obsValue)
    },
    include.rownames = F
    )
}
## End Comparison Module

### Mappe

makeMap <- function() {
    info <- getGEO()
    
    NUTS2_SP@data <- NUTS2_SP@data %>%
        full_join(info, by = c("NUTS_ID" = "id"))
    
    leaflet() %>% 
        fitBounds( -2, 35, 12, 55) %>%
        addPolygons(data = NUTS2_ALL, 
                    weight = 1, 
                    color = "grey"
        ) %>%
        addPolygons(data = NUTS2_SP,
                    weight = 1,
                    color = "black",
                    fillColor = "#8B1F3F",
                    fillOpacity = 0.8,
                    popup = ~paste0("<b>", Regione, "</b>",
                                    "<br>",
                                    NUTS_ID, "\t", Stato,
                                    "<br>",
                                    "<a href=\"", link, "\">Link Wikipedia</a>")
        )
    
    
}
