get_message_data = function(
  dir = ".",
  custom_translation_functions = list(R = NULL, src = NULL),
  style = NULL,
  verbose = !is_testing()
) {
  package = get_desc_data(dir, 'Package')
  is_base = package == 'base'

  # If style not specified, read from DESCRIPTION
  if (is.null(style)) {
    style <- get_desc_data(dir, "Config/potools/style")
    if (is.na(style)) {
      style <- "base"
    }
  }

  if (verbose && dir.exists(file.path(dir, "R"))) message('Getting R-level messages...')
  r_message_data = get_r_messages(
    dir,
    custom_translation_functions = custom_translation_functions$R,
    style = style,
    is_base = is_base
  )

  if (verbose && dir.exists(file.path(dir, "src"))) message('Getting src-level messages...')
  src_message_data = get_src_messages(
    dir,
    custom_translation_functions = custom_translation_functions$src,
    is_base
  )

  rbind(
    R = r_message_data,
    src = src_message_data,
    fill = TRUE,
    idcol = "message_source"
  )
}
