# Load all R files from tools directory
tools_files <- list.files("tools", pattern = "\\.R$", full.names = TRUE)
sapply(tools_files, source)
