# ---------------------------Basic Plotting --------------------------------- #

# Function for stacked bar plot
#--------------------------------#
# df: dataframe
# group_var: Variable to group by (ie. x axis of the plot)
# group_var_stack: Variable to stack by
# order_by_count: Whether to order bars by counts
# string_wrap: Whether to perform string wrapping for long group_var names
# large_fig: Whether to make a large figure
stacked_bar <- function(df,
                        group_var,
                        group_var_stack,
                        order_by_count = TRUE,
                        string_wrap = NA,
                        large_fig = FALSE){

  # Split the label text if it is too long.
  if (!is.na(string_wrap)) {
    df[, group_var] <- str_wrap(pull(df[, group_var]),
                                width = string_wrap)
  }

  # Create two new variables: (i) n: count for a column and
  # (ii) plot_n: column height to be used in the stacked bar
  # plot.  The plot_n variable deals with the case where the count number
  # in the column is too large to display in a stacked bar chart.
  temp_df <- df %>%
    group_by(!!sym(group_var), !!sym(group_var_stack)) %>%
    summarise(Count = n()) %>%
    mutate(plot_n = Count + 3)

  # Order the columns by count, if desired.
  if (order_by_count) {

    p <- ggplot(data = temp_df,
                aes(x = reorder(!!sym(group_var), -plot_n, sum),
                    y = plot_n,
                    fill = !!sym(group_var_stack)))

  } else {

    p <- ggplot(data = temp_df,
                aes(x = !!sym(group_var),
                    y = plot_n,
                    fill = !!sym(group_var_stack)))
  }

  # Adjust font size.
  text_size <- if_else(large_fig, 8, 3)
  axis_text_size <- if_else(large_fig, 20, 9)
  axis_title_size <- if_else(large_fig, 15, 7)

  p + geom_bar(stat = "identity", position = "stack") +
    scale_fill_jama() +
    labs(x = "", y = "Number of articles") +
    guides(fill = guide_legend("")) +
    geom_text(aes(label = Count),
              size = text_size,
              color = "white",
              position = position_stack(vjust = 0.5)) +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(face = "bold", size = axis_text_size),
          axis.text = element_text(size = axis_text_size, colour = "black"),
          axis.title = element_text(size = axis_text_size),
          legend.title = element_text(size = axis_text_size),
          legend.text = element_text(size = axis_title_size))
}


# Function for horizontal stacked bar plot
#----------------------------------------#
# df: dataframe
# group_var: Variable to group by (ie. x axis of the plot)
# group_var_stack: Variable to stack by
# restrict_count: Whether to restrict group_var to be at least 2
# top_count: Maximum count of group_var
# y_lim: Restriction for the y limit
# label_text: Whether to label bars with the count
# title: Title name for the plot
# ylab: y_axis label
# string_wrap: Whether to perform string wrapping for long group_var names
# color_grid: Option grid of colors to use
# large_fig: Whether to make a large figure
# legend_caption: Caption for the legend
horizontal_bar <- function(df,
                           group_var,
                           group_var_stack = NA,
                           restrict_count = FALSE,
                           top_count = 999,
                           ylim = NA,
                           label_text = TRUE,
                           title = NULL,
                           ylab = NA,
                           string_wrap = NA,
                           color_grid = NA,
                           large_fig = FALSE,
                           legend_cap = "Unsupervised phenotype category"){

  # Split the label text if it is too long.
  if (!is.na(string_wrap)) {

    df[, group_var] <- str_wrap(pull(df[, group_var]), width = string_wrap)

  }

  # Group by stacked variable, if any.
  if (is.na(group_var_stack)) {

    temp_df <- df %>%
      group_by(!!sym(group_var)) %>%
      summarise(Count = n()) %>%
      mutate(n = Count)

  } else {

    df[, legend_cap] <- df[, group_var_stack]

    temp_df <- df %>%
      group_by(!!sym(group_var)) %>%
      mutate(n = n()) %>%
      group_by(!!sym(group_var), !!sym(legend_cap)) %>%
      summarise(n = mean(n), Count = n())

  }

  # Restict `group_var` to be > 1, if desired.
  if (restrict_count) {

    temp_df <- temp_df %>% filter(n > 1)

  }

  # Keep the top n bars.
  temp_df <- temp_df %>% arrange(-n) %>% top_n(top_count, n)

  # Set up plots.
  if (is.na(group_var_stack)) {

    p <- ggplot(data = temp_df,
                aes(x = reorder(!!sym(group_var), -Count, sum),
                    y = Count))

    if (any(!is.na(color_grid))) {

      p <- p + geom_bar(aes(fill = !!sym(group_var)), stat = "identity")

    } else {

      p <- p + geom_bar(stat = "identity")

    }

  } else {

    p <- ggplot(data = temp_df,
                aes(x = reorder(!!sym(group_var), -Count, sum),
                    y = Count,
                    alpha = !!sym(legend_cap))) +
      geom_bar(stat="identity")
  }

  # Label the number on top of the bars, if desired.
  if (label_text) {

    p <- p + geom_text(aes(label = Count),
                       position = position_dodge(width = 0.9),
                       hjust = -0.5,
                       size = 3)
  }

  # Restrict the y limit, if desired.
  if (!is.na(ylim))  p <- p + ylim(0, ylim)

  # Make the plot.
  if (!is.na(group_var_stack)) {
    p <- p + geom_col_pattern(aes(pattern    = !!sym(legend_cap),
                                  pattern_angle   = !!sym(legend_cap),
                                  pattern_spacing = !!sym(legend_cap)),
                              fill            = 'white',
                              colour          = 'black',
                              pattern_density = 0.05,
                              pattern_fill    = 'black',
                              pattern_colour  = 'black') +
      theme_classic() +
      coord_flip() +
      labs(title = title, y = "", x = "") +
      theme(legend.position = "right")

  } else {

    p <- p + scale_fill_manual(values = color_grid) +
      theme_classic() +
      coord_flip() +
      labs(title = title, y = "", x = "") +
      theme(legend.position = "none")

  }

  text_size = if_else(large_fig, 15, 7)
  title_size = if_else(large_fig, 20, 12)

  p <- p +
    theme(plot.title = element_text(face = "bold", size = title_size),
          axis.text = element_text(size = text_size),
          legend.title = element_text(size = title_size),
          legend.text = element_text(size = text_size))

  if (!is.na(ylab)) p <- p + ylab(ylab)

  p +
    theme(axis.text.y = element_text(colour = "black"),
          axis.text.x = element_text(colour = "black"))

}


# Function to plot top phenotypes.
#--------------------------------#
# df: dataframe
# group_var: Variable to group by (ie. x axis of the plot)
# restrict_count: Whether to restrict group_var to be at least 2
# top_count: Maximum count of group_var
# title: Title name for the plot
# y_lim: Restriction for the y limit
# label_text: Whether to label bars with the count
# string_wrap: Whether to perform string wrapping for long group_var names
# color_grid: Option grid of colors to use
# large_fig: Whether to make a large figure
plot_top_pheno <- function(df,
                           group_var_stack = NA,
                           restrict_count = TRUE,
                           top_count = 5,
                           title,
                           ylim = 5,
                           label_text = TRUE,
                           string_wrap = 25,
                           color_grid = NA,
                           large_fig = FALSE) {

  df_wo_comp <- df %>% filter(Competition_data_name == '')
  phenotype_unnested <- unnest_string_var(df_wo_comp, "Phenotype")

  horizontal_bar(phenotype_unnested,
                 "Phenotype_unnested",
                 group_var_stack = group_var_stack,
                 restrict_count = restrict_count,
                 top_count = top_count,
                 ylim = ylim,
                 label_text = label_text,
                 title = title,
                 string_wrap = string_wrap,
                 color_grid = color_grid,
                 large_fig = large_fig)
}


# Function to plot top phenotypes for all methods.
#------------------------------------------------#
# traditional_supervised: dataframe with articles using traditional supervised
# deep_supervised: dataframe with articles using traditional supervised
# semi_supervised: dataframe with articles using traditional supervised
# weakly_supervised: dataframe with articles using weakly supervised
# un_supervised: dataframe with articles using unsupervised
plot_all_top_pheno <- function(traditional_supervised,
                               deep_supervised,
                               semi_supervised,
                               weakly_supervised,
                               un_supervised) {

  df_all <- rbind(traditional_supervised,
                  deep_supervised,
                  semi_supervised,
                  weakly_supervised,
                  un_supervised)

  # Remove competition data since these have set phenotypes.
  df_wo_comp <- df_all %>% filter(Competition_data_name == '')

  phenotype_unnested <- unnest_string_var(df_wo_comp, "Phenotype")

  phenotype_common <- phenotype_unnested %>%
    group_by(Phenotype_unnested, ML_type, Traditional) %>%
    summarise(n = n()) %>%
    filter(n > 1) %>%
    arrange(-n) %>%
    group_by(ML_type, Traditional) %>%
    top_n(5)

  phenotype_unique <- phenotype_common %>%
    group_by(Phenotype_unnested) %>%
    summarise(n = n()) %>%
    filter(n > 1) %>%
    select(Phenotype_unnested) %>%
    pull()

  phenotype_common <- phenotype_common %>%
    select(Phenotype_unnested) %>%
    pull()

  # Set the color.
  color_grid <- rep("#000003", length(phenotype_common))
  color_panel <- pal_d3()(9)
  for (i in c(1:length(phenotype_unique))) {
    color_grid[which(phenotype_common == phenotype_unique[i])] <- color_panel[i]
  }

  names(color_grid) <- phenotype_common

  color_grid["Chronic obstructive\npulmonary disease"] <- "#000003"
  color_grid["Non-alcoholic fatty liver\ndisease"] <- "#000003"
  color_grid["Type 2 diabetes mellitus"] <- "#6fd5db"

  p1 <- plot_top_pheno(traditional_supervised,
                       label_text = FALSE,
                       top_count = 5,
                       title = "Traditional supervised",
                       color_grid = color_grid)

  p2 <- plot_top_pheno(deep_supervised,
                       label_text = FALSE,
                       top_count = 5,
                       title = "Deep supervised",
                       color_grid = color_grid)

  p3 <- plot_top_pheno(semi_supervised,
                       label_text = FALSE,
                       top_count = 5,
                       title = "Semi-supervised",
                       color_grid = color_grid)

  p4 <- plot_top_pheno(weakly_supervised,
                       label_text = FALSE,
                       top_count = 5,
                       title = "Weakly-supervised",
                       color_grid = color_grid)
  p5 <- plot_top_pheno(un_supervised,
                       label_text = FALSE,
                       top_count = 5,
                       title = "Unsupervised",
                       color_grid = color_grid)

  plot_grid(p1, p2, p3, p4, p5,
            ncol = 2,
            nrow = 3,
            align = "v",
            axis = "l",
            labels = c('(a)', '(b)', '(c)', '(d)', '(e)'),
            label_size = 12)
}

# ------------------------------------- Tables --------------------------------#

# Function to print a summary table with grouped information.
# ----------------------------------------------------------#
# df: dataframe.
# group_var: Variable to group by.
# group_var_2: Additional variable to group by, optional.
# group_var_3: Additional variable to group by, optional.
# title: Title of plot, optional.
# top_count: Largest number of rows to display for each groupby variable,
#   default is 5.
# restric_count: Whether to restrict to articles with a value of at least one
#   in the grouped variables
# col_width: Column width for latex tabels, default is 5.
# hold_pos: Whether to scale latex tables, default is FALSE.
print_tables <- function(df,
                         group_var,
                         group_var_2 = NA,
                         group_var_3 = NA,
                         title = NA,
                         top_count = 5,
                         restric_count = TRUE,
                         col_width = 5,
                         hold_pos = FALSE) {

  # Group data based on grouping variables.
  if (is.na(group_var_2)) {

    df <- df %>%
      group_by(!!sym(group_var)) %>%
      summarise(Count = n()) %>%
      slice_max(order_by = Count, n = top_count) %>%
      arrange(-Count)

    if (restric_count) df <- df %>% filter(Count > 1)

  } else if (is.na(group_var_3)) {

    df <- df %>%
      group_by(!!sym(group_var), !!sym(group_var_2)) %>%
      summarise(Count = n()) %>%
      slice_max(order_by = Count, n = top_count)

    if (restric_count) df <- df %>% filter(Count > 1)

  } else {

    df1 <- df %>%
      group_by(!!sym(group_var)) %>%
      summarise(Count = n()) %>%
      slice_max(order_by = Count, n = top_count)

    if (restric_count) df1 <- df1 %>% filter(Count > 1)

    df <- df %>%
      group_by(!!sym(group_var), !!sym(group_var_2), !!sym(group_var_3)) %>%
      summarise(Count = n()) %>%
      pivot_wider(names_from = c("ML_type", "Traditional"),
                  values_from = "Count",
                  names_sep = " ",
                  values_fill = 0) %>%
      inner_join(df1) %>%
      arrange(-Count)

  }

  new_name <- strsplit(colnames(df)[1], "_")
  new_name <- new_name[[1]]

  colnames(df)[1] <- str_c(new_name[-length(new_name)], collapse = " ")

  if (!is.na(group_var_2) & is.na(group_var_3)) {

    new_name <- strsplit(colnames(df)[2], "_")
    new_name <- new_name[[1]]

    colnames(df)[2] <- str_c(new_name[-length(new_name)], collapse = " ")

  }

  # To split the label text if it is too long.
  if (!is.na(group_var_3)) {

    num_col <- ncol(df)

    # Latex table options, whether to scale down table size.

    if (hold_pos) {
      df %>%
        kbl(booktabs = T, caption = title) %>%
        kable_paper("striped") %>%
        kable_styling(position = "center", latex_options = "HOLD_position") %>%
        column_spec(c(1:num_col), width = paste0(col_width, "em"))

    } else {

      df %>%
        kbl(booktabs = T, caption = title) %>%
        kable_paper("striped") %>%
        kable_styling(position = "center", latex_options = "scale_down") %>%
        column_spec(c(1:num_col), width = paste0(col_width, "em"))

    }

  } else {

    df %>%
      kbl(booktabs = T, caption = title) %>%
      kable_paper("striped") %>%
      kable_styling(position = "center", latex_options = "HOLD_position")

  }

}


# Function to print number of papers using multiple types of a single variable.
# ---------------------------------------------------------------------------- #
# df: dataframe.
# item: variable to count number of papers using multiples.
print_multiple_items <- function(df, item) {

  n_multiple <- df %>%
    group_by(PMID) %>%
    summarise(n = n()) %>%
    filter(n > 1) %>%
    nrow()

  print(paste('There are', n_multiple, 'papers using multiple', item))

}


# Function to print overall summary table for key variables.
# -------------------------------------------------------- #
# df: dataframe.
print_summary_table <- function(df) {

  res <- c()

  for (dataframe in df) {

    total <- nrow(dataframe)

    freetext <- dataframe %>%
      filter(Unstructured == TRUE) %>%
      filter(Unstructured_data_language != "Non-language") %>%
      nrow()

    nlp <- dataframe %>%
      filter(Unstructured == TRUE & NLP_software != "") %>%
      nrow()

    comp_data <- dataframe %>%
      filter(Competition_data_name != '') %>%
      nrow()

    private_multisite <- dataframe %>%
      filter(Multi_sites_data == 1 & Openly_available_data == 0) %>%
      nrow()

    open_data <-  dataframe %>%
      filter(Openly_available_data == 1 & Competition_data_name == '') %>%
      nrow()

    private_single <- dataframe %>%
      filter(Multi_sites_data == 0 & Openly_available_data == 0) %>%
      nrow()

    compare_rule <- dataframe %>%
      filter(Compare_with_rule_based != "") %>%
      nrow()

    compare_ml <- dataframe %>%
      filter(Compare_with_traditional_ML != "") %>%
      nrow()

    demographics <- dataframe %>%
      filter(Reported_demographics == 1 & Openly_available_data == 0) %>%
      nrow()

    open_code <- dataframe %>%
      filter(Open_code == 1) %>%
      nrow()

    tmp <- c(total,
             freetext,
             nlp,
             comp_data,
             private_multisite,
             open_data,
             private_single,
             compare_rule,
             compare_ml,
             demographics,
             open_code)

    res <- rbind(res, tmp)

  }

  colnames(res) <- c("Total number of papers",
                     "Used free-text",
                     "Used NLP software",
                     "Used competition data",
                     "Used multisite data",
                     "Used open data",
                     "Used private single-site data",
                     "Compared to rule-based algorithms",
                     "Comapred to traditional ML",
                     "Reported patient demographic",
                     "Released open code")

  row_names <-c("TSL", "DSL", "SSL",
                "WSL", "USL", "Total")

  as.data.frame(res, row.names = row_names) %>%
    kbl(booktabs = T) %>%
    kable_paper("striped") %>%
    kable_styling(position = "center", latex_options = "scale_down") %>%
    column_spec(c(1:12), width = paste0(4, "em"))

}


# ------------------------ Functions to unnest strings ----------------------- #

# Function to unnest a column with strings separated by a semi-colon.
# ----------------------------------------------------------------- #
# df: dataframe
# var: variable name to unnest
unnest_string_var <- function(df, var) {

  df %>% mutate(var_unnested = (strsplit(as.character(!!sym(var)), ";"))) %>%
    unnest(var_unnested) %>%
    mutate_if(is.character, trimws) %>%
    rename_with(stringr::str_replace,
                pattern = "var_unnested",
                replacement = paste0(var, "_unnested"),
                matches("var_unnested"))

}


# Function to separate phenotype and validation metrics by pair.
# ------------------------------------------------------------ #
# df: dataframe
# vars: vectors of length 2 with variable names to unnest;
#   "phenotype" is the first argument
# utility: whether to split by validation metric or phenotype
unnest_two_string <- function(df, vars, utility = "validation") {

  # Check if there is a paper with record of multiple values of
  # a single phenotype. eg. PMID 29447188 has 10 phenotypes + 10 sensitivities.

  pheno_view <- unnest_string_var(df, "Phenotype") %>%
    group_by(PMID) %>%
    summarise(n_pheno = n())

  metric_view <- unnest_string_var(df, vars[2]) %>%
    group_by(PMID) %>%
    summarise(n_metric = n())

  overview <- merge(pheno_view, metric_view, all = TRUE)

  # Split multiple values and match them with the corresponding phenotype.
  pmid_split <- overview %>% filter(n_metric > 1) %>% select(PMID) %>% pull()

  df_split <- df %>% filter(PMID %in% pmid_split)
  df_remain <- subset(df, !(PMID %in% pmid_split))

  if (utility == "validation") {

    # If there is no record with multiple values for a single metric,
    # return itself.

    if (nrow(df_split) == 0) {

      res <- df_remain %>% select(PMID,
                                  Best_performing_model,
                                  Ref,
                                  Phenotype,
                                  !!sym(vars[2])) %>% na.omit()

      # Otherwise, split the validation metrics by phenotypes.

    } else {

      for (var in vars) {

        # From library splitstackshape.
        df_split <- cSplit(df_split, c(var), sep = ";",  type.convert = FALSE)

      }

      n_pheno <- dim(df_split %>%
                       select(starts_with(paste0(vars[1], "_"))))[2]
      n_metric <- dim(df_split %>%
                        select(starts_with(paste0(vars[2], "_"))))[2]

      end_pname <- paste0("_0", c(1:9))
      end_pname <- c(end_pname, paste0("_", c(10:100)))
      end_mname <- paste0("_", c(1:100))

      res <- c()

      for (i in c(1:n_pheno)) {
        tmp <- df_split %>% select(PMID,
                                   Best_performing_model,
                                   Ref,
                                   ends_with(end_pname[i]),
                                   ends_with(end_mname[i]))

        # Needed to unnest weakly supervised metrics.
        if (dim(tmp)[2] == 5) {
          colnames(tmp) <- c("PMID", "Best_performing_model", "Ref", vars)
          res <- rbind(res, tmp)
        }

      }

      out <- df_remain %>% select(PMID,
                                  Best_performing_model,
                                  Phenotype,
                                  Ref,
                                  !!sym(vars[2]))

      res <- res %>% filter(!is.na(Phenotype))
      res[, 5] <- as.numeric(unlist(res[, 5]))
      out[, 5] <- as.numeric(unlist(out[, 5]))

      res <- out %>%
        bind_rows(res) %>%
        na.omit()
    }

  } else {

    # If there is no record with multiple values for a single, return itself.

    if (nrow(df_split) == 0) {
      res <- df_remain %>% select(PMID, Phenotype, !!sym(vars[2])) %>% na.omit()

      # Otherwise, split the by phenotypes.

    } else {

      # From library splitstackshape.\

      for (var in vars) {

        # From library splitstackshape.
        df_split <- cSplit(df_split, c(var), sep = ";",  type.convert = FALSE)

      }

      n_pheno <- dim(df_split %>% select(starts_with(paste0(vars[1], "_"))))[2]
      n_metric <- dim(df_split %>% select(starts_with(paste0(vars[2], "_"))))[2]

      end_pname <- paste0("_0", c(1:9))
      end_pname <- c(end_pname, paste0("_", c(10:25)))
      end_mname <- paste0("_", c(1:25))

      res <- c()
      for (i in c(1:n_pheno)) {
        tmp <- df_split %>% select(PMID,
                                   ends_with(end_pname[i]),
                                   ends_with(end_mname[i]))

        colnames(tmp) <- c("PMID", vars)
        res <- rbind(res, tmp)
      }

      res2 <- unnest_string_var(df_remain, vars[1]) %>%
        select(PMID, Phenotype_unnested, !!sym(vars[2]))

      colnames(res2) <- c("PMID", vars)

      res <- res %>% bind_rows(res2) %>% na.omit()

    }
  }

  return(res)

}

# Function to unnest two column with strings separated by a semi-colon.
#---------------------------------------------------------------------#
# df: dataframe
# comparator: comparator approach
unnest_validate_string <- function(df, comparator = "deep") {

  # Used to unnest validation metrics recorded in paired columns separated by
  # semi-colons, eg.
  # Input:  col 1: phenotypes hypertension;obesity  col 2: specificity 0.99;0.22
  # Output: hypertension_specificity: 0.99          obesity_specificity: 0.22

  if (comparator == "deep") {

    # No deep learning papers reported NPV.
    for (metric in c("Sensitivity", "Specificity", "PPV", "AUROC")) {

      metric_best <- paste0("Best_performing_", metric)
      metric_ml <- paste0("Best_comparator_traditional_", metric)
      metric_deep <- paste0("Best_comparator_DL_", metric)

      best <- unnest_two_string(df, vars = c("Phenotype", metric_best))
      ml <- unnest_two_string(df, vars = c("Phenotype", metric_ml))
      deep <- unnest_two_string(df, vars = c("Phenotype", metric_deep))

      mfile <- merge(best, ml, all.x = TRUE)
      mfile <- merge(mfile, deep, all.x = TRUE)

      if (metric == "Sensitivity") {
        res <- mfile
      } else {
        res <- merge(res, mfile, all = TRUE)
      }
    }

  } else {

    for (metric in c("Sensitivity", "Specificity", "PPV", "NPV", "AUROC")) {

      metric_best <- paste0("Best_performing_", metric)
      metric_ml <- paste0("Best_comparator_traditional_", metric)
      metric_rule <- paste0("Best_comparator_Rule_", metric)

      best <- unnest_two_string(df, vars = c("Phenotype", metric_best))
      ml <- unnest_two_string(df, vars = c("Phenotype", metric_ml))
      rule <- unnest_two_string(df, vars = c("Phenotype", metric_rule))

      mfile <- merge(best, ml, all.x = TRUE)
      mfile <- merge(mfile, rule, all.x = TRUE)

      if (metric == "Sensitivity") {
        res <- mfile
      } else {
        res <- merge(res, mfile, all = TRUE)
      }
    }
  }

  res$Phenotype_ref <- paste0(res$Phenotype, " [", res$Ref, "]")
  return(res)

}


# --------------------------- Validation Metrics ----------------------------- #

# Function to get the validation metrics formatted for plotting.
#--------------------------------------------------------------#
# df: Dataframe.
# comparator: String for what method to compare to, either "rule"
#   or "traditional".
get_metrics <- function(df, comparator = "rule"){

  if (comparator == "rule") {

    df <- df %>% filter(Compare_with_rule_based != "")
    df <- unnest_validate_string(df, comparator = "not deep")


  } else if (comparator == "traditional") {

    df <- df %>% filter(Best_comparator_traditional != "")
    df <- unnest_validate_string(df, comparator = "deep")
  }

  df %>%
    unite("Study1", c(Phenotype, PMID), sep = "\n", remove = FALSE) %>%
    unite("Study2", c(Phenotype, PMID), sep = "\n", remove = FALSE)

}


# Function to determine best performing model for plotting.
#---------------------------------------------------------#
# df: Dataframe.
# baseline_method: String for column name of baseline method.
# comparison_method: String ofr column name of comparison method.
# metrics: Whether to consider all ROC metrics.
best_performing_model <- function(df, baseline_method,
                                  comparison_method,
                                  metrics = "ALL"){

  df <- df %>% mutate_all(na_if, "")

  if(metrics == "ALL"){

    df$baseline_sens_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_Sensitivity"))) >
                                    df %>% select(!!sym(paste0(comparison_method, "_Sensitivity")))) * 1

    df$baseline_ppv_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_PPV"))) >
                                   df %>% select(!!sym(paste0(comparison_method, "_PPV")))) * 1

    df$baseline_spec_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_Specificity"))) >
                                    df %>% select(!!sym(paste0(comparison_method, "_Specificity")))) * 1

    df$baseline_npv_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_NPV"))) >
                                   df %>% select(!!sym(paste0(comparison_method, "_NPV")))) * 1

    df$baseline_auc_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_AUROC"))) >
                                   df %>% select(!!sym(paste0(comparison_method, "_AUROC")))) * 1

    df$baseline_all_better <- apply(df %>% select(baseline_sens_better,
                                                  baseline_ppv_better,
                                                  baseline_spec_better,
                                                  baseline_npv_better,
                                                  baseline_auc_better),
                                    1, min,
                                    na.rm = TRUE)
  } else if (metrics == "deep"){

    df$baseline_sens_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_Sensitivity"))) >
                                    df %>% select(!!sym(paste0(comparison_method, "_Sensitivity")))) * 1

    df$baseline_ppv_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_PPV"))) >
                                   df %>% select(!!sym(paste0(comparison_method, "_PPV")))) * 1

    df$baseline_spec_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_Specificity"))) >
                                    df %>% select(!!sym(paste0(comparison_method, "_Specificity")))) * 1

    df$baseline_auc_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_AUROC"))) >
                                   df %>% select(!!sym(paste0(comparison_method, "_AUROC")))) * 1

    df$baseline_all_better <- apply(df %>% select(baseline_sens_better,
                                                  baseline_ppv_better,
                                                  baseline_spec_better,
                                                  baseline_auc_better),
                                    1, min,
                                    na.rm = TRUE)
  }


  return(df)
}

# Function to plot validation metrics.
#------------------------------------#
# df: Dataframe.
# comparator: Comparator method.
# baseline: Baseline method.
# large_fig: Whether to produce large figure.
plot_validation_metrics <- function(df,
                                  comparator = "rule",
                                  baseline = "traditional",
                                  large_fig = FALSE) {

  if (comparator == "rule" & baseline == "traditional") {

    # Get traditional ML models that compare to rule-based.
    df_formatted <- get_metrics(df, "rule")

    df_ML_better <- best_performing_model(df_formatted %>%
                                            filter(Best_performing_model == "Traditional ML"),
                                          "Best_performing", "Best_comparator_Rule")

    df_rule_better <- best_performing_model(df_formatted %>%
                                              filter(Best_performing_model == "Rule-based"),
                                            "Best_comparator_traditional",
                                            "Best_performing")

    df_rule_better$Phenotype_ref <- c("Obesity and other comorbidities [156]",
                                        "Marital Status [157]")

    g1_df <- df_ML_better %>% filter(baseline_all_better == 1)
    g1 <- plot_rule_compare(g1_df,
                            study = "Phenotype_ref",
                            large_fig = large_fig, legend = TRUE)

    g2_df <- df_ML_better %>% filter(baseline_all_better == 0,
                                     baseline_sens_better == 0,
                                     (baseline_ppv_better == 1 | is.na(baseline_ppv_better)))
    g2 <- plot_rule_compare(g2_df,
                            study = "Phenotype_ref",
                            large_fig = large_fig, legend = TRUE)


    g3_df <- df_ML_better %>% filter(!(PMID %in% c(g2_df$PMID, g1_df$PMID, df_rule_better$PMID)))

    g3 <- plot_rule_compare(g3_df,
                            study = "Phenotype_ref",
                            large_fig = large_fig, legend = TRUE)

    g4_df <- df_rule_better
    g4 <- plot_rule_compare(g4_df, study = "Phenotype_ref",
                            large_fig = large_fig, legend = TRUE)

    plot_grid(g1, g2, g3, g4,
              nrow = 4,
              align = "v",
              rel_heights = c(5, 5, 5, 5))

  } else if (baseline == "weakly" & comparator == "rule") {

      # Get traditional ML models that compare to rule-based.
      df_formatted <- get_metrics(df, "rule")

      df_ML_better <- best_performing_model(df_formatted %>%
                                              filter(Best_performing_model == "Traditional ML"),
                                            "Best_performing", "Best_comparator_Rule")

      g1_df <- df_ML_better %>% filter(baseline_all_better == 1)
      g1 <- plot_rule_compare(g1_df,
                              study = "Phenotype_ref",
                              large_fig = large_fig, legend = TRUE,
                              weakly = TRUE)

      g2_df <- df_ML_better %>% filter((baseline_all_better == 0 | is.na(baseline_all_better)) &
                                       (baseline_sens_better == 0 | is.na(baseline_sens_better)) &
                                       baseline_ppv_better == 1)
      g2 <- plot_rule_compare(g2_df,
                              study = "Phenotype_ref",
                              large_fig = large_fig, legend = TRUE,
                              weakly = TRUE)


      g3_df <- df_ML_better %>% filter((baseline_all_better == 0 | is.na(baseline_all_better)) &
                                       baseline_sens_better == 1,
                                       (baseline_ppv_better == 0 | is.na(baseline_ppv_better)))
      g3 <- plot_rule_compare(g3_df,
                              study = "Phenotype_ref",
                              large_fig = large_fig, legend = TRUE,
                              weakly = TRUE)

      plot_grid(g1, g2, g3,
                nrow = 3,
                align = "v",
                rel_heights = c(6, 6, 6))

  } else if(baseline == "weakly" & comparator == "traditional"){

      df <- get_metrics(df, "traditional")
      df <- df %>% filter(Best_performing_AUROC != "")
      df$Phenotype_ref <- paste0(df$Phenotype, " [", df$Ref, "]")

      plot_metrics(df,
                   comparison = "weakly_traditional",
                   study = "Phenotype_ref",
                   metric = "AUROC",
                   large_fig,
                   xlim = 0.5) +
        theme(legend.position = "bottom")

    } else if (baseline == "deep" & comparator == "traditional") {

    # Fix spacing.
    df$Best_performing_model <- gsub(" ", "",
                                     df$Best_performing_model,
                                     fixed = TRUE)

    # Get traditional ML models that compare to rule-based.
    df_formatted <- get_metrics(df, "traditional")

    # Replace the long phenotype description.
    df_formatted$Phenotype_ref[df_formatted$PMID == 34514351] <-
      "Social determinants of health [56]"
    df_formatted$Phenotype_ref[df_formatted$PMID == 35007754] <-
      "Social determinants of health [50]"
    df_formatted$Phenotype_ref[df_formatted$PMID == 32548622] <-
      "Acute care conditions [60]"

    df_ML_better_1 <- best_performing_model(df_formatted %>%
                                              filter(Best_performing_model == "DL"),
                                            "Best_performing",
                                            "Best_comparator_traditional",
                                            metrics = "deep")

    df_ML_better_2 <- best_performing_model(df_formatted %>%
                                              filter(Best_performing_model == "Rule-based"),
                                            "Best_comparator_traditional",
                                            "Best_comparator_DL",
                                            metrics = "deep")

    df_ML_better_2$Best_performing_model <- "DL"
    df_ML_better_2$Best_performing_Sensitivity <-
      df_ML_better_2$Best_comparator_DL_Sensitivity
    df_ML_better_2$Best_performing_PPV <-
      df_ML_better_2$Best_comparator_DL_PPV

    df_formatted_3 <- df_formatted %>% filter(Ref == 61) %>%
      filter(Phenotype_ref != "Fall [61]") %>%
      filter(Best_performing_Sensitivity != "")

    df_formatted_3$Best_performing_model <- "DL"
    df_formatted_3$Best_comparator_traditional_PPV <-
      df_formatted_3$Best_performing_PPV
    df_formatted_3$Best_comparator_traditional_Sensitivity <-
      df_formatted_3$Best_performing_Sensitivity
    df_formatted_3$Best_performing_PPV <-
      df_formatted_3$Best_comparator_DL_PPV

    df_ML_better_3 <- best_performing_model(df_formatted_3,
                                            "Best_performing",
                                            "Best_comparator_traditional",
                                            metrics = "deep")

    df_ML_better <- rbind(df_ML_better_1, df_ML_better_2, df_ML_better_3)

    df_traditional_better <- best_performing_model(df_formatted %>%
                                              filter(Best_performing_model == "TraditionalML"),
                                            "Best_comparator_DL",
                                            "Best_performing",
                                            metrics = "deep")

    g1_df <- df_ML_better %>% filter(baseline_all_better == 1)
    g1 <- plot_deep_compare(g1_df,
                            study = "Phenotype_ref",
                            large_fig = large_fig, legend = TRUE)

    g2_df <- df_ML_better %>%  filter((baseline_all_better == 0 | is.na(baseline_all_better)) &
                                        (baseline_sens_better == 0 | is.na(baseline_sens_better)) &
                                        baseline_ppv_better == 1)
    g2 <- plot_deep_compare(g2_df,
                            study = "Phenotype_ref",
                            large_fig = large_fig, legend = TRUE)


    g3_df <- df_ML_better %>% filter((baseline_all_better == 0 | is.na(baseline_all_better)),
                                       baseline_sens_better == 1,
                                     (baseline_ppv_better == 0 | is.na(baseline_ppv_better)))

    g4_df <- df_traditional_better %>% filter(baseline_all_better != "Inf")
    g3_df <- g3_df %>% rbind(g4_df) %>% filter(Ref != 61)
    g3 <- plot_deep_compare(g3_df,
                            study = "Phenotype_ref",
                            large_fig = large_fig, legend = TRUE)

    g4_df <- g4_df %>% filter(Phenotype_ref == "Fall [61]")
    g4_df$Best_performing_model <- "DL"
    g4_df$Best_comparator_traditional_PPV <- g4_df$Best_performing_PPV
    g4_df$Best_performing_PPV <- g4_df$Best_comparator_DL_PPV

    g4 <- plot_deep_compare(g4_df, study = "Phenotype_ref",
                            large_fig = large_fig, legend = TRUE)

    plot_grid(g1, g2, g3, g4,
              nrow = 4,
              align = "v",
              rel_heights = c(7, 3.5, 4, 3))

  }

}

# Figure to plot ML vs rule across all metrics.
#------------------------------------#
# df: Dataframe.
# comparator: Comparator method.
# study: The column to be printed for the x-axis text.
# large_fig: Whether to produce large figure.
# legend: Whether to plot legend.
# weakly: Indicator if baseline is weakly-supervised learning.
plot_rule_compare <- function(df,
                              comparison = "rule",
                              study = "Phenotype_ref",
                              large_fig = FALSE,
                              legend = FALSE,
                              weakly = FALSE) {

  # Remove y-axis label and text for graph other than sensitivity.
  theme_type <- theme(axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank())

  legend_pos = if_else(legend, "bottom", "none")

  p1 <- plot_metrics(df, comparison, study, metric = "Sensitivity", large_fig)

  p2 <- plot_metrics(df, comparison, study, metric = "Specificity", large_fig) +
    theme_type

  if(!weakly){

    p3 <- plot_metrics(df, comparison, study, metric = "PPV", large_fig) +
      theme(legend.position = legend_pos) +
      theme_type

  } else {

    p3 <- plot_metrics(df, comparison, study, metric = "PPV", large_fig) +
      theme(legend.position = legend_pos) +
      theme_type +
      scale_color_jama(labels=c('Rule-based', 'Weakly-supervised learning'))
  }

  p4 <- plot_metrics(df, comparison, study, metric = "NPV", large_fig) +
    theme_type

  p5 <- plot_metrics(df, comparison, study, metric = "AUROC", large_fig) +
    theme_type

  egg::ggarrange(p1,
                 p2,
                 p3,
                 p4,
                 p5,
                 nrow = 1,
                 draw = FALSE)

}

# Figure to plot ML vs DL across all metrics.
#------------------------------------#
# df: Dataframe.
# comparator: Comparator method.
# study: The column to be printed for the x-axis text.
# large_fig: Whether to produce large figure.
# legend: Whether to plot legend.
plot_deep_compare <- function(df,
                              comparison = "deep",
                              study = "Phenotype_ref",
                              large_fig = FALSE,
                              legend = FALSE) {

  # Remove y-axis label and text for graph other than sensitivity.
  theme_type <- theme(axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank())

  legend_pos = if_else(legend, "bottom", "none")

  p1 <- plot_metrics(df, comparison, study, "Sensitivity",
                     large_fig = large_fig)

  p2 <- plot_metrics(df, comparison, study, "Specificity",
                     large_fig = large_fig) +
    theme(legend.position = legend_pos) +
    theme_type

  p3 <- plot_metrics(df, comparison, study, "PPV",
                     large_fig = large_fig) + theme_type

  p4 <- plot_metrics(df, comparison, study, "AUROC",
                     large_fig = large_fig) + theme_type



  egg::ggarrange(p1,
                 p2,
                 p3,
                 p4,
                 nrow = 1,
                 draw = FALSE)

}

# Function to plot sub-figure for a single metric.
# ---------------------------------------------- #
# df: Dataframe.
# comparator: Comparator method.
# study: The column to be printed for the x-axis text.
# metric: Accuracy metric to consider.
# large_fig: Whether to produce large figure.
# xlim: The minimum metric range on the figure, [xlim, 100].
# legend: Whether to plot legend.
plot_metrics <- function(df,
                         comparison = "rule",
                         study = "Phenotype",
                         metric = "Sensitivity",
                         large_fig = FALSE,
                         xlim = 0.01) {

  if (comparison == "rule") {

    df <- ml_rule_metrics(df, metric)

  } else if (comparison == "weakly") {

    df <- weakly_rule_metrics(df, metric)

  } else if (comparison == "deep") {

    df <- ml_deep_metrics(df, metric)
    df$Method <- factor(df$Method, levels = c("Traditional supervised learning",
                                              "Deep supervised learning"))

  } else {

    df <- weakly_trad_metrics(df, metric)

  }


  title_size <- if_else(large_fig, 18, 8)
  text_size <- if_else(large_fig, 10, 5)
  legend_size <- if_else(large_fig, 15, 7)
  point_size <- if_else(large_fig, 3, 1)
  vjust_size <- if_else(large_fig, 0.5, 0.25)

  p <- df %>%
      ggplot(aes(x = !!sym(study),
                 y = as.numeric(!!sym(metric)),
                 color = Method)) +
      scale_x_discrete(labels = function(x) str_extract(x, ".+"))

  p <- p + scale_y_continuous(limits = c(xlim, 1),
                              labels = function(y) label_parsed(paste0(y*100))) +
    scale_color_jama() +
    coord_flip() +
    labs(x = "", y = "", title = metric) +
    theme_bw() +
    geom_point(size = point_size) +
    theme(legend.position = "none",
          plot.title = element_text(size = title_size, hjust = 0.5, face="bold"),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = text_size, color = "black"),
          axis.text.y = element_text(size = legend_size, vjust = vjust_size),
          legend.title = element_blank(),
          legend.text = element_text(size = legend_size))

}

# Utility function to plot supervised ML vs rule for one metric.
# ------------------------------------------------------------ #
# df: Dataframe.
# metric: Evaluation metric.
ml_rule_metrics <- function(df, metric = "Sensitivity") {

  metric_best <- paste0("Best_performing_", metric)
  metric_ml <- paste0("Best_comparator_traditional_", metric)
  metric_rule <- paste0("Best_comparator_Rule_", metric)

  df %>%
    mutate(`Traditional supervised learning` = case_when(
      str_detect(Best_performing_model, "Traditional ML") ~
        as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_ml)))) %>%
    mutate(`Rule-based` = case_when(
      str_detect(Best_performing_model, "Rule-based") ~
        as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_rule)))) %>%
    mutate(diff = `Traditional supervised learning` - `Rule-based`) %>%
    mutate(ML_better = diff > 0) %>%
    mutate(rule = `Rule-based`) %>%
    pivot_longer(cols = c(`Traditional supervised learning`, `Rule-based`),
                 names_to = "Method",
                 values_to = metric) %>%
    dplyr::select(Study1,
                  Study2,
                  PMID,
                  Phenotype,
                  Phenotype_ref,
                  rule,
                  diff,
                  ML_better,
                  !!sym(metric),
                  Method) %>%
    mutate_if(~ all(. %in% c(0, NA)), ~ replace(., is.na(.), 0))

}

# Utility function to plot supervised ML vs rule for one metric.
# ------------------------------------------------------------ #
# df: Dataframe.
# metric: Evaluation metric.
weakly_rule_metrics <- function(df, metric = "Sensitivity") {

  metric_best <- paste0("Best_performing_", metric)
  metric_ml <- paste0("Best_comparator_traditional_", metric)
  metric_rule <- paste0("Best_comparator_Rule_", metric)

  df %>%
    filter(Best_performing_model != "") %>%
    mutate(`Weakly-supervised learning` = case_when(
      str_detect(Best_performing_model, "Traditional ML") ~
        as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_ml)))) %>%
    mutate(`Rule-based` = case_when(str_detect(
      Best_performing_model, "Rule-based") ~ as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_rule)))) %>%
    mutate(diff = `Weakly-supervised learning` - `Rule-based`) %>%
    mutate(ML_better = diff > 0) %>%
    mutate(rule = `Rule-based`) %>%
    pivot_longer(cols = c(`Weakly-supervised learning`, `Rule-based`),
                 names_to = "Method",
                 values_to = metric) %>%
    dplyr::select(Study1,
                  Study2,
                  PMID,
                  Phenotype_ref,
                  Phenotype,
                  rule,
                  diff,
                  ML_better,
                  !!sym(metric),
                  Method) %>%
    mutate_if(~ all(. %in% c(0, NA)), ~ replace(., is.na(.), 0))

}

# Utility function to plot weakly-supervised ML vs supervised ML for one metric.
# ---------------------------------------------------------------------------- #
# df: Dataframe.
# metric: Evaluation metric.
weakly_trad_metrics <- function(df, metric = "Sensitivity") {

  metric_best <- paste0("Best_performing_", metric)
  metric_ml <- paste0("Best_comparator_traditional_", metric)

  df %>%
    mutate(`Weakly-supervised learning` = as.numeric(!!sym(metric_best))) %>%
    mutate(`Traditional supervised learning` = as.numeric(!!sym(metric_ml))) %>%
    mutate(diff = `Weakly-supervised learning`-`Traditional supervised learning`) %>%
    mutate(ML_better = diff > 0) %>%
    pivot_longer(cols = c(`Weakly-supervised learning`, `Traditional supervised learning`),
                 names_to = "Method",
                 values_to = metric) %>%
    dplyr::select(Phenotype,
                  Phenotype_ref,
                  Study2,
                  diff,
                  ML_better,
                  !!sym(metric),
                  Method) %>%
    mutate_if(~ all(. %in% c(0, NA)), ~ replace(., is.na(.), 0))

}

# Utility function to plot traditional supervised ML vs DL for one metric.
# ---------------------------------------------------------------------- #
# df: Dataframe.
# metric: Evaluation metric.
ml_deep_metrics <- function(df, metric = "Sensitivity") {

  metric_best <- paste0("Best_performing_", metric)
  metric_ml <- paste0("Best_comparator_traditional_", metric)
  metric_deep <- paste0("Best_comparator_DL_", metric)

  df %>%
    filter(Best_performing_model != "") %>%
    mutate(`Deep supervised learning` = case_when(
      str_detect(Best_performing_model, "DL") ~
        as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_deep)))) %>%
    mutate(`Traditional supervised learning` = case_when(
      str_detect(Best_performing_model, "Traditional ML") ~
        as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_ml)))) %>%
    mutate(diff = `Deep supervised learning`-`Traditional supervised learning`) %>%
    mutate(DL_better = diff > 0) %>%
    pivot_longer(cols = c(`Traditional supervised learning`,
                          `Deep supervised learning`),
                 names_to = "Method",
                 values_to = metric) %>%
    dplyr::select(Study1,
                  Study2,
                  Phenotype_ref,
                  Phenotype,
                  diff,
                  DL_better,
                  !!sym(metric),
                  Method) %>%
    mutate_if(~ all(. %in% c(0, NA)), ~ replace(., is.na(.), 0))

}

