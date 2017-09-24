test_that('Numeric encoder is fit properly', {
  y <- matrix(c('red', 'red', 'blue', 'red'), nrow=4, ncol=1)
  encoder <- fitNumericEncoder(y)
  expectedEncoder <- list(red = 1, blue = 2)
  expect_equal(encoder, expectedEncoder)
})


test_that('Numeric encoding is done right', {
  y <- matrix(c('red', 'red', 'blue', 'red'), nrow=4, ncol=1)
  encoder <- fitNumericEncoder(y)
  yExpected <- matrix(c(1, 1, 2, 1), nrow=4, ncol=1)
  yEncoded <- numericEncode(encoder, y)
  expect_equal(yEncoded, yExpected)
})


test_that('Numeric decoder is fit properly', {
  encoder <- list(red = 1, blue = 2)
  expectedDecoder <- list('red', 'blue')
  decoder <- getNumericDecoder(encoder)
  expect_equal(decoder, expectedDecoder)
})


test_that('Numeric decoding is done right', {
  yExpected <- matrix(c('red', 'red', 'blue', 'red'), nrow=4, ncol=1)
  encoder <- fitNumericEncoder(yExpected)
  yEncoded <- numericEncode(encoder, yExpected)
  yDecoded <- numericDecode(encoder, yEncoded)
  expect_equal(yDecoded, yExpected)
})


test_that('One hot encoder dummifies a numeric encoded vector', {
  y <- matrix(c(2, 3, 4, 1), nrow=4, ncol=1)
  yExpected <- rbind(c(0, 1, 0, 0), c(0, 0, 1, 0), c(0, 0, 0, 1), c(1, 0, 0, 0))
  expect_equal(oneHotEncode(y), yExpected)
})


test_that('One hot decoder undummifies a dummy matrix', {
  dummyMatrix <- rbind(c(0, 1, 0, 0), c(0, 0, 1, 0), c(0, 0, 0, 1), c(1, 0, 0, 0))
  yExpected <- matrix(c(2, 3, 4, 1), nrow=4, ncol=1)
  expect_equal(oneHotDecode(dummyMatrix), yExpected)
})