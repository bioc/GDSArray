test_that("filename getter and setter works", {
    file <- gdsExampleFileName("snpgds")
    ga <- GDSArray(file, "genotype")

    gf <- GDSFile(file)
    expect_equal(normalizePath(gdsfile(ga)), normalizePath(file))
    expect_identical(gdsfile(ga), gdsfile(seed(ga)))
    expect_identical(gdsfile(ga), gdsfile(gf))

    ## setter only for GDSFile class
    file1 <- gdsExampleFileName("seqgds")
    gdsfile(gf) <- file1
    expect_true(validObject(gf))

    ## error when setting filename for GDSArray(Seed)
    expect_error(gdsfile(ga) <- file1)
})

