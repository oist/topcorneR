exampleEcho <- planTransfers(
  source = examplePlate,
  destination = Plate("96", deadVolume = 1e4, maxVolume = 1e5),
  model="525",
  plan = Plate("96") |>
    setWell(Well("A01"), "dNTP",   50) |>
    setWell(Well("A02"), "dNTP",  100) |>
    setWell(Well("A01"), "buffer", 50)
)

usethis::use_data(exampleEcho, overwrite = TRUE)
