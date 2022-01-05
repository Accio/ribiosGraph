library(ribiosGraph)
library(testthat)

myIncMat <- matrix(c(0, 0, 1,
  0, 1, 0,
  1, 0, 0,
  0, 1, 1,
  1, 1, 1),
  ncol=3, byrow=TRUE, dimnames=list(LETTERS[1:5], letters[1:3]))
myGraph <- incidence2bipartite(myIncMat,
  size=c(18, 12),
  color=c("red", "yellow"),
  label.cex=c(1.5, 0.8),
  label.color=c("red3", "black"),
  V=list(shape=c("rectangle", "circle"),
         font=c(2,1),
         frame.color="lightgray",
         label.dist=0.5),
  E=list(color="lightblue", width=2, arrow.size=1.5, arrow.width=1.25, lty=2))

rcv <- c(5,3)

context("indidence2bipartite")

test_that("incidence2bipartite works", {
  expect_identical(as_incidence_matrix(myGraph), myIncMat)
  expect_true(is_bipartite(myGraph))
  expect_identical(V(myGraph)$name,
                   c(LETTERS[1:5], letters[1:3]))
  
  expect_identical(V(myGraph)$size,
                   rep(c(18, 12), rcv))
  expect_identical(V(myGraph)$color,
                   rep(c("red", "yellow"), rcv))
  expect_identical(V(myGraph)$label.cex,
                   rep(c(1.5, 0.8), rcv))
  expect_identical(V(myGraph)$label.color,
                   rep(c("red3", "black"), rcv))
  
  expect_identical(V(myGraph)$shape,
                   rep(c("rectangle", "circle"), rcv))
  expect_identical(V(myGraph)$font,
                   rep(c(2, 1), rcv))
  expect_identical(V(myGraph)$frame.color,
                   rep("lightgray", 8))
  expect_identical(V(myGraph)$label.dist,
                   rep(0.5, 8))
  
  expect_identical(E(myGraph)$color,
                   rep("lightblue", 8))
  expect_identical(E(myGraph)$width,
                   rep(2, 8))
  expect_identical(E(myGraph)$arrow.size,
                   rep(1.5, 8))
  expect_identical(E(myGraph)$arrow.width,
                   rep(1.25, 8))
  expect_identical(E(myGraph)$lty,
                   rep(2, 8))
})
