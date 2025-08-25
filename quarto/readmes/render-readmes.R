library(yaml)

# NOTE: for this script to work, the project file readmes.Rproj must be loaded

quarto_files <- list.files(pattern = "\\.qmd$", full.names = TRUE)

for (qmd in quarto_files) {
  # render the .qmd to markdown in place
  system2("quarto", c("render", qmd, "--to", "gfm"))
  
  # work out filenames
  base <- tools::file_path_sans_ext(basename(qmd))  # e.g. "workflow"
  md_file <- paste0(base, ".md")                    # output from quarto
  
  # destination: root README vs subfolder README
  if (base == "main") {
    dest_file <- file.path("../..", "README.md")
  } else {
    dest_file <- file.path("../..", base, "README.md")
  }
  
  # copy + rename
  file.copy(md_file, dest_file, overwrite = TRUE)
}