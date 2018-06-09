.onAttach <- function(libname = find.package("furniture"), pkgname = "furniture") {
  
  confs <- search_conflicts() %>%
    do.call("rbind", .) %>%
    .[(!grepl("%>%", rownames(.))) &
        (!grepl("group_by", rownames(.))),]
  if (length(confs) == 0){
    confs_msg <- text_col(paste0(crayon::green(cli::symbol$tick), " No conflicts found."))
    helper_msg <- ""
  } else {
    confs_msg <- text_col(paste0(crayon::yellow(cli::symbol$cross),
                                 " The furniture::", rownames(confs), "() function has the same name as ", 
                                 gsub("package:", "", confs$rowname), "::", rownames(confs), " (", 
                                 sapply(paste(gsub("package:", "", confs$rowname), "::", rownames(confs)), 
                                        function(x) class(eval(parse(text = x)))[1]), ")\n"))
    helper_msg <- text_col(crayon::italic("   Consider using `furniture::` for each function call.\n"))
  }
  
  packageStartupMessage(text_col(cli::cat_rule(left = paste0("furniture ", furniture_version("furniture")), 
                                               right = "learn more at tysonbarrett.com")),
                        confs_msg,
                        helper_msg)
  
}

.onLoad <- function(libname = find.package("furniture"), pkgname = "furniture"){
  if(getRversion() >= "2.15.1") {
    utils::globalVariables(".")
  }
  invisible()
}