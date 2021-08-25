###author: Praminda Mahesh Imaduwa-Gamage (Mahesh Imaduwa)

###This code pulls molecules data from CDD Vault###
##library(RCurl)  
##library(jsonlite)


buildSynonymsList <- function(object) {
  obj = object
  synList = rep(0, length(obj$name))
  
  for (nameIndex in 1:length(obj$name)) {
    synList[nameIndex] =
      toString(obj$synonyms[[nameIndex]][!grepl(obj$name[[nameIndex]],
                                                obj$synonyms[[nameIndex]])])
  }
  synList
}

verifySSL <- function() {
  opts <- list(
    capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
    ssl.verifypeer = FALSE,
    httpheader = httpHeader
  )
  
  options(RCurlOptions = opts)
  
}


getCddCSVobj <- function(vaultid, token, projectid) {
  pageSize = 1000
  offset = 0
  httpHeader = paste0("X-CDD-Token:", token)
  url <- "https://app.collaborativedrug.com/api/v1/vaults"
  query = paste0("molecules?page_size=",
                 pageSize,
                 "&projects=",
                 projectid,
                 "&offset=",
                 offset)
  
  path <- paste0(url, "/", vaultid, "/", query)
  errMsg = ""
  
  tryCatch({
    response = fromJSON(getURL(path, httpheader = httpHeader))
    
    if (is.null(response$objects)) {
      errMsg = "Error occured in processing"
      
      if (response$error != "") {
        errMsg = response$error
      }
      else if (is.null(response$error)) {
        errMsg = "Null return"
      }
      
    }
    else{
      return(response$objects)
    }
  }
  , error = function(e) {
    errMsg = "Error occured in processing"
  })
  errMsg
  
}



getMolDf <- function(cddobject){
  
  obj <- cddobject
  df <- data.frame()
  
  if(!is.null(obj) && (is.data.frame(obj) | is.list(obj))){
    
    df <- data.frame(
      
      id = obj$id
      , name = obj$name
      , synonyms = buildSynonymsList(obj)
      , cdd_registry_number = obj$cdd_registry_number
      , smiles = obj$smiles
      , cxsmiles = obj$cxsmiles
      , iupac_name = obj$iupac_name
      , molecular_weight = obj$molecular_weight
      , formula = obj$formula
      , log_p = obj$log_p
      , log_d = obj$log_d
      , log_s = obj$log_s
      , psa = obj$topological_polar_surface_area
      , h_bond_donors = obj$num_h_bond_donors
      , h_bond_acceptors = obj$num_h_bond_acceptors
      , rotational_bonds = obj$num_rotatable_bonds
      , p_k_a = obj$p_k_a
      , composition = obj$composition
      , rule_of_five_violations = obj$num_rule_of_5_violations
      , stringsAsFactors = FALSE
    )
  } 
  
  df
}


createBlankDf <- function(colName){
  
  col = colName
  emptydf <- data.frame(matrix(ncol = length(col), nrow = 10))
  colnames(emptydf) <- col
  emptydf
  
}

getMoleculesCSV <- function(cddobject) {
  
  fields = fromJSON("tablefields.json")
  colName <- fields$basicfields
  df <- data.frame(matrix(ncol = length(colName), nrow = 10))
  df <- getMolDf(cddobject)
  colnames(df) <- colName
  df
  
}

getMoleculesCSVwithUDFS <- function(cddobject) {
  
  fields = fromJSON("tablefields.json")
  colName <- fields$basicfields
  
  basicDf <- data.frame(matrix(ncol = length(colName), nrow = 10))
  basicDf <- getMolDf(cddobject)
  colnames(basicDf) <- colName
  
  colName_udfs <- fields$udfs
  preferred_colname = c(colName_udfs[1], paste0("Collaborators_", colName_udfs[-1]))
  #extract the available user defined fields from the original
  #table from CDD
  udfs_index = sapply(
    names(cddobject$udfs),
    FUN = function(pattern) {
      pattern = paste0(unlist(strsplit(pattern, " ")), collapse = "")
      x = sapply(
        colName_udfs,
        FUN = function(x) {
          paste0(unlist(strsplit(x, " ")), collapse = "")
        },
        USE.NAMES = F
      )
      value = grep(pattern, x , ignore.case = TRUE, value = F)
      
      if (length(value) > 0) {
        return(value)
      }
      
    },
    USE.NAMES = T
  )
  
  userDf <- data.frame(matrix(ncol = length(preferred_colname), nrow = 10))
  userDf <- cddobject$udfs
  colnames(userDf) <- preferred_colname[udfs_index]
  cbind(basicDf, userDf)
  
}


getCSVtable <- function(vaultid, token, projectid) {
  cddobject =  getCddCSVobj(vaultid, token, projectid) #datframe, errMsg, or null
  if (!is.null(cddobject) &&
      is.data.frame(cddobject) && nrow(cddobject) > 0) {
    if (sum(names(cddobject) == "udfs") == 1) {
      return(getMoleculesCSVwithUDFS(cddobject))
    }
    if (sum(names(cddobject) == "udfs") == 0) {
      return(getMoleculesCSV(cddobject))
    }
    
  }
  cddobject
  
}
