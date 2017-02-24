path_source <- 'Outcome of Care Measures.csv'
path_cleaned <- 'outcome.cleaned.RData'


tidy_data <- function() {
  
  csv_df <- read.csv(path_source, colClasses = "character")

  for (column in length(csv_df):8) {
    
    if (sum(grepl('[0-9]', csv_df[,column])) == 0) {
      print(paste("column", colnames(csv_df[column]), "deleted"))
      csv_df[,column] <- NULL
    }
  }
  
 
  names(csv_df) <- gsub('\\.', ' ', names(csv_df))

  names(csv_df) <- gsub('30 Day', '30-Day', names(csv_df))

  names(csv_df) <- gsub('   ', ' - ', names(csv_df))
 
  names(csv_df) <- gsub('Death  Mortality  Rates', 'Mortality Rates', names(csv_df))
  
  hnames <- csv_df$'Hospital Name'
  hnames <- gsub('MED CENTER', 'M.C.', hnames)
  hnames <- gsub('MEDICAL CENTER', 'M.C.', hnames)
  hnames <- gsub('MEDICAL CTR', 'M.C.', hnames)
  hnames <- gsub('MED CENTER', 'M.C.', hnames)
  hnames <- gsub('MEMORIAL HOSPITAL', 'M.H.', hnames)
  hnames <- gsub('HOSPITALS CENTER', 'H.C.', hnames)
  hnames <- gsub('HOSPITAL CENTER', 'H.C.', hnames)
  hnames <- gsub('HOSPITALS', 'H.', hnames)
  hnames <- gsub('HOSPITAL', 'H.', hnames)
  hnames <- gsub('HEALTH SYSTEM', 'H.S.', hnames)
  hnames <- gsub(', THE$', '', hnames)
  hnames <- gsub('HEALTHCARE', 'HC.', hnames)
  hnames <- gsub('UNIVERSITY', 'UNIV.', hnames)
  hnames <- gsub('SYSTEMS', 'S.', hnames)
  hnames <- gsub('SYSTEM', 'S.', hnames)
  hnames <- gsub('CENTERS', 'C.', hnames)
  hnames <- gsub('CENTER', 'C.', hnames)
  csv_df$Hospital <- hnames
  csv_df
}

save_cleaned_data <- function() {
  csv_df <- tidy_data()
  save(csv_df, file=path_cleaned)
}

load_cleaned_data <- function() {

  if (!file.exists(path_cleaned)) {
    parent <- dirname(path_cleaned)
    if (!file.exists(parent)) {
  
      dir.create(parent)
    }
    save_cleaned_data()
  }

  load(file=path_cleaned)
  csv_df
}



csv_df <- load_cleaned_data()


states <- unique(csv_df$State)


columns <- colnames(csv_df[11:(length(csv_df)-1)])


outcomes <- colnames(csv_df[11:(length(csv_df)-1)])

mid <- function(csv_df, nmin, nmax) {
  tail(head(csv_df, nmax), nmax - nmin + 1)
}



get_col_index <- function(csv_df, name) {
  which(colnames(csv_df) == name)
}


