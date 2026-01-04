dir.create("C:/Temp/NBA_Slate_Test", recursive = TRUE, showWarnings = FALSE)

out_file <- "C:/Temp/NBA_Slate_Test/PROOF.txt"

writeLines(paste("R ran OK at", Sys.time()), out_file)

message("WROTE: ", normalizePath(out_file))
