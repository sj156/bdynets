test_that('source adapters align IDs and preserve missing and zero observations', {
  b <- network_example('beijing'); l <- network_example('largest')
  expect_identical(b$unit_type,'edge'); expect_identical(l$unit_type,'node')
  expect_identical(l$unit_ids,c('s2','s1'))
  expect_equal(unname(l$values[,1]),c(0,8,6))
  expect_true(is.na(l$values[2,2])); expect_equal(unname(b$values[1,1]),0)
  tab <- panel_long(b)
  expect_equal(nrow(tab),6); expect_equal(sum(!tab$observed),1)
  expect_true(is.na(tab$value[!tab$observed]))
  expect_equal(network_view_data(b)$limits,c(0,7))
  expect_equal(network_view_data(l)$limits,c(0,8))
  expect_equal(nrow(l$network$edges),0)
})

test_that('model adapters preserve numerical inputs and reject ambiguous conversions', {
  b <- network_example('beijing')
  expect_error(as_gmde_inputs(b,NULL),'complete')
  b$values[2,2] <- 5
  g <- gmde_graph(matrix(c(0,1,1,0),2),b$unit_ids)
  expect_identical(as_gmde_inputs(b,g)$Y,t(b$values))
  expert <- gmde_expert('gaussian',matrix(1,3,1),prior=list(m0=0,C0=matrix(1),variance=list(shape=2,scale=1)))
  control <- gmde_mcmc(iterations=4,warmup=2,seed=123)
  direct <- gmde_fit(t(b$values),g,1,expert,mcmc=control)
  adapted <- do.call(gmde_fit,c(as_gmde_inputs(b,g),list(K=1,expert=expert,mcmc=control)))
  expect_equal(adapted$draws,direct$draws)
  graph <- list(edges=data.frame(name=b$unit_ids),A=diag(2),W=diag(2))
  expect_identical(as_bdcn_inputs(b,graph,b$unit_ids)$Y,b$values)
  expect_error(as_bdcn_inputs(b,graph,rev(b$unit_ids)),'IDs')
  expect_error(as_gmde_inputs(b,gmde_graph(matrix(c(0,1,1,0),2),rev(b$unit_ids))),'IDs')
  b$exposure[,] <- 2
  expect_error(as_gmde_inputs(b,g),'exposure')
  d <- as_lpr_data(b,1)
  expect_equal(d$y,c(0,2)); expect_equal(d$exposure,c(2,2))
  expect_equal(d$edges,matrix(c(1,2,2,3),2))
  expect_identical(d$node_ids,c('a','b','c'))
  b$values[1,2] <- NA
  expect_equal(as_lpr_data(b,1)$y,0)
  expect_equal(as_lpr_data(b,1)$n,3L)
})

test_that('constructors reject invalid identities, time and observation contracts', {
  b <- network_example('beijing'); l <- network_example('largest')
  expect_error(network_data(data.frame(node_id=c('a','a'))),'unique')
  expect_error(network_data(data.frame(node_id='a'),data.frame(edge_id='e',from='a',to='b')),'endpoints')
  expect_error(network_panel(b$values,rev(b$time),b$unit_ids,b$network,'edge'),'increasing')
  expect_error(network_panel(b$values,b$time,rev(b$unit_ids),b$network,'edge'),'Column names')
  expect_error(network_panel(b$values,b$time,b$unit_ids,b$network,'edge',exposure=c(1,2)),'exposure')
  expect_error(largest_panel(data.frame(ID='absent',Lat=1,Lng=2),matrix(1,1,1),'s',1),'missing')
  expect_error(as_lpr_data(l,1),'edge panel')
  expect_error(as_lpr_data(b,0),'time_index')
  restored <- readRDS(local({ f<-tempfile(); saveRDS(b,f); f }))
  expect_identical(restored,b)
})

test_that('Beijing sparse export semantics are explicit', {
  nodes <- data.frame(node_id=c('a','b'),x_84=c(1,2),y_84=c(2,3))
  links <- data.frame(edge_index='e',from_node_id='a',to_node_id='b')
  obs <- data.frame(edge_index='e',time_bin=0,x_ijt=3)
  a <- beijing_panel(nodes,links,obs,0:1,0:1)
  z <- beijing_panel(nodes,links,obs,0:1,0:1,absent='zero')
  expect_true(is.na(a$values[2,1])); expect_equal(unname(z$values[2,1]),0)
  expect_error(beijing_panel(nodes,links,rbind(obs,obs),0:1,0:1),'duplicate')
})

test_that('both sources render through the same plot method', {
  f <- tempfile(fileext='.pdf'); grDevices::pdf(f)
  tryCatch({
    for(source in c('beijing','largest')) {
      x <- network_example(source)
      expect_equal(plot(x,time_index=2),network_view_data(x))
    }
  },finally=grDevices::dev.off())
  expect_gt(file.info(f)$size,0); unlink(f)
})
