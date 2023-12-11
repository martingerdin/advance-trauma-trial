create_data_section <- function() {
    variables.data <- read.csv("variables.csv")
    variables.data[] <- lapply(variables.data, trimws)
    variables.list <- split(variables.data, variables.data$type)
    variables.list <- variables.list[unique(variables.data$type)]
    data.section <- paste0(unlist(lapply(names(variables.list), function(variable.type) {
        type.data <- variables.list[[variable.type]]
        type.list <- split(type.data, type.data$variable)
        type.list <- type.list[unique(type.data$variable)]
        paste0(
            "## ", stringr::str_to_title(variable.type), "\n\n",
            paste0(unlist(lapply(type.list, function(variable) {
                paste0(
                    "- **", variable$variable, "**",
                    if (variable$definition != "") {
                        paste0(" (", variable$definition, ")")
                    } else {
                        ""
                    },
                    ", *Source:* ", variable$source
                )
            })), collapse = "\n")
        )
    })), collapse = "\n\n")
    return(data.section)
}
