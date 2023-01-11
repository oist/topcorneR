exampleTransducer <- Transducer( source      = Well("A01")
                               , destination = Well("A01", plateFormat = "96"))

usethis::use_data(exampleTransducer, overwrite = TRUE)
