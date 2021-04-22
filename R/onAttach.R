
.onAttach <- function(...) {
    attached_packages <- search()
    dembase_not_attached <- !("package:dembase" %in% attached_packages)
    if (dembase_not_attached)
        packageStartupMessage(paste("Package 'dembase' is no longer loaded automatically",
                                    "when package 'demlife' loads. You may need to",
                                    "add 'library(dembase)' to existing code",
                                    "for the code to work properly."))
}


