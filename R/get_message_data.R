get_message_data = function(
  dir = ".", src_translation_macros = c("_", "N_"),
  use_base_rules = package %chin% .potools$base_package_names,
  verbose = FALSE
) {
  package = get_desc_data(dir)['Package']
  is_base = package == 'base'

  if (verbose) message('Getting R-level messages...')
  r_message_data = get_r_messages(dir, is_base)

  if (verbose) message('Getting src-level messages...')
  src_message_data = get_src_messages(dir, src_translation_macros, use_base_rules, is_base)

  rbind(
    R = r_message_data,
    src = src_message_data,
    fill = TRUE,
    idcol = "message_source"
  )
}
