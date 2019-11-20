# .onAttach <- function(libname = find.package("furniture"), pkgname = "furniture") {
#   
#   ## get potential conflicts and filter out unncessary ones
#   
#   confs <- search_conflicts() %>%
#     do.call(rbind, .) %>%
#     .[(!grepl("^%", rownames(.))) &
#         (!grepl("::", rownames(.))) &
#         (!grepl("group_by", rownames(.))) &
#         (!grepl("pt", rownames(.))) &
#         (!grepl("\\?", rownames(.))) &
#         (! .$rowname %in% c("package:utils", "package:stats", "package:base")) &
#         (grepl("package:", .$rowname)),] %>%
#     data.frame
#   
#   confs$objects <- gsub("\\.[0-9]*$", "", rownames(confs))
#   if (nrow(confs) > 0){
#     conflict_type <- sapply(paste0(gsub("package:", "", confs$rowname), "::", confs$objects), 
#                             function(x) class(eval(parse(text = x)))[1])
#     confs <- confs[conflict_type == "function", ]
#   }
#   
#   if (nrow(confs) == 0){
#     confs_msg <- text_col(paste0(crayon::green(cli::symbol$tick), " No potential conflicts found"))
#     helper_msg <- ""
#   } else {
#     confs_msg <- text_col(paste0(crayon::yellow(cli::symbol$cross),
#                                  " The furniture::", confs$objects, "() function has the same name as ", 
#                                  gsub("package:", "", confs$rowname), "::", confs$objects, " (", 
#                                  sapply(paste0(gsub("package:", "", confs$rowname), "::", confs$objects), 
#                                         function(x) class(eval(parse(text = x)))[1]), " object)\n"))
#     helper_msg <- text_col(crayon::italic(" Consider using `furniture::` for each function call."))
#   }
#   
#   packageStartupMessage(text_col(cli::rule(left = paste0("furniture ", furniture_version("furniture")), 
#                                            right = "learn more at tysonbarrett.com")),
#                         text_col(paste0("\n", crayon::green(cli::symbol$tick), " furniture attached\n")),
#                         confs_msg,
#                         helper_msg)
#   
# }

.onLoad <- function(libname = find.package("furniture"), pkgname = "furniture"){
  if(getRversion() >= "2.15.1") {
    utils::globalVariables(c(".", ".rows", ":="))
  }
  invisible()
}
