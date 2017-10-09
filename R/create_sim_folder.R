create.sim.folder <- function (from, to) {
    files <- list.files(from)
    dir.create(to, showWarnings = FALSE, recursive=T)
    for (id in 1:length(files)) {
        from_temp <- paste(from, files[id], sep="\\")
        file.copy(from_temp, to, recursive=T)
    }
}
