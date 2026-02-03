library(testthat)
devtools::load_all(".", quiet = TRUE)

test_that("dist_mirai rejects quoted expressions for url/remote", {
  expect_error(
    dist_mirai(remote = quote(mirai::ssh_config("ssh://node1"))),
    "no longer accepts quoted expressions"
  )

  expect_error(
    dist_mirai(n = 1, url = quote(mirai::local_url(tcp = TRUE))),
    "no longer accepts quoted expressions"
  )
})

