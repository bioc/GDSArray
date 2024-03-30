test_that("filename getter and setter works", {
    file <- gdsExampleFileName("snpgds")
    ga <- GDSArray(file, "genotype")

    gf <- GDSFile(file)
    expect_equal(normalizePath(gdsfn(ga)), normalizePath(file))
    expect_identical(gdsfn(ga), gdsfn(seed(ga)))
    expect_identical(gdsfn(ga), gdsfn(gf))

    ## setter only for GDSFile class
    file1 <- gdsExampleFileName("seqgds")
    gdsfn(gf) <- file1
    expect_true(validObject(gf))

    ## error when setting filename for GDSArray(Seed)
    expect_error(gdsfn(ga) <- file1)
})

