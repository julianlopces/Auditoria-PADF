### Exportar alertas

# Verificar si las credenciales están disponibles
if (!exists("temp_creds_file") || !file.exists(temp_creds_file)) {
  stop("No se encontraron las credenciales de Google Sheets. Asegúrate de cargarlas desde el script maestro.")
}

sheet <- tryCatch({
  gs4_get(id_alertas)
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

# Helper genérico para exportar a Google Sheets con manejo de errores y pausa opcional
export_sheet <- function(df, ss, sheet_name, label = sheet_name, pause = 0) {
  message(sprintf("Exportando %s...", label))
  tryCatch({
    sheet_write(df, ss = ss, sheet = sheet_name)
    message(sprintf("Datos de %s exportados correctamente.", label))
  }, error = function(e) {
    stop(sprintf("Error al exportar %s: %s", label, conditionMessage(e)))
  })
  if (pause > 0) Sys.sleep(pause)
}

# Llamadas usando el wrapper
export_sheet(alertas,             sheet, "alertas",            label = "alertas",                 pause = 5)
export_sheet(data,              sheet, "data_raw",           label = "datos crudos",            pause = 5)
export_sheet(tracking,              sheet, "tracking",           label = "tracking",            pause = 5)
export_sheet(progreso_trat_enc, sheet, "avance_tratamiento", label = "progreso_trat", pause = 5)
message("✅ Todos los datos fueron exportados exitosamente.")


