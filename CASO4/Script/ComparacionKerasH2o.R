library(h2o)

h2o.init()
#Define Dataset 
ais <- ais 
head(ais)
# standardise
minmax <- function(x) (x - min(x))/(max(x) - min(x))
x_train <- apply(ais[,1:11], 2, minmax)

x_train_h2o<- as.h2o(x_train)
# PCA

model<-h2o.deeplearning(1:11,
                        training_frame = x_train_h2o,
                        hidden = c(6,2,6),
                        autoencoder = T,
                        activation = "tanh",
                        epochs = 500
)

summary(model)

features<-h2o.deepfeatures(model,
                           x_train_h2o,
                           layer=2)
features<-features%>%
  as.data.frame()

d <- as.matrix(features[1:10,])
d
labels= as.vector(rownames(ais)[1:10])
labels

plot(d,pch=17)
text(d,labels,pos=3)

p_h2o<-ggplot(data.frame(PC1 = features[,1], PC2 = features[,2],sex=ais$sex), aes(x = PC1, y = PC2, col = sex)) + geom_point()

h2o.performance(model)
################################################################
library(keras)
# set training data
x_train <- as.matrix(x_train)
# set model
model_keras <- keras_model_sequential()
model_keras %>%
  layer_dense(units = 6, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dense(units = 2, activation = "tanh", name = "bottleneck") %>%
  layer_dense(units = 6, activation = "tanh") %>%
  layer_dense(units = ncol(x_train))
# view model layers
summary(model_keras)

# compile model
model_keras %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam"
)
# fit model
model_keras %>% fit(
  x = x_train, 
  y = x_train, 
  verbose = 0,
  epochs = 500
)
# evaluate the performance of the model
mse.ae2 <- evaluate(model_keras, x_train, x_train)
mse.ae2

# extract the bottleneck layer
intermediate_layer_model <- keras_model(inputs = model_keras$input, outputs = get_layer(model_keras, "bottleneck")$output)
intermediate_output <- predict(intermediate_layer_model, x_train)
p_keras<-ggplot(data.frame(PC1 = intermediate_output[,1], PC2 = intermediate_output[,2]), aes(x = PC1, y = PC2, col = ais$sex)) + geom_point()

gridExtra::grid.arrange(p_h2o,p_keras)
### Comparacion 
cbind(features[1:10,],
      intermediate_output[1:10,])
