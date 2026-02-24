library(ISLR2)
# EX7 --------------------------------------------------------------------------
#NN with 1 hidden layer with 10 units and dropout reg 
 data(Default)
 Default$default <- as.numeric(Default$default)-1

n <- nrow(Default)
set.seed(13)
ntest <- trunc(n / 3)
testid <- sample(1:n, ntest)

# Logistic regression ----------------------------------------------------------
mod_fit <- glm(default~.,data=Default[-testid,],family = binomial)
mpred <- predict(mod_fit,Default[testid,])
with(Default[testid,],mean(abs(mpred-default)))

# NN Fit --------------------------------------------------------------------
library(keras3)
x <- scale(model.matrix(default ~ . - 1, data = Default))
y <- Default$default

modnn <- keras_model_sequential(input_shape = ncol(x)) |>
  layer_dense(units = 10, activation = "relu") |> # 10 hidden units
  layer_dropout(rate = 0.4) |> 
  layer_dense(units = 1) # 1 hidden layer

compile(modnn, loss = "mse", optimizer = optimizer_rmsprop(),
        metrics = list("mean_absolute_error"))
# 500 epochs to be slightly faster
history <- fit(modnn, x[-testid, ], y[-testid], epochs = 500, 
               batch_size=32, validation_data=list(x[testid, ], y[testid]))

evaluate(modnn, x[testid, ], y[testid]) #NN MAE-> 0.049

with(Default[testid,],mean(abs(mpred-default))) #GLM MAE -> 6.108


# EX 8 -------------------------------------------------------------------------
# 10 images and CNN  to predict group with top 5 likely groups
# picking some random pics of objects ------------------------------------------
library(magick)
urls1 <- paste0("https://loremflickr.com/200/200/piano?lock=", 1:3)
urls2 <- paste0("https://loremflickr.com/200/200/car?lock=", 1:4)
urls3 <- paste0("https://loremflickr.com/200/200/dog?lock=", 1:3)
urls <- c(urls1,urls2,urls3)
num_images <- length(urls)
path="C:/Users/yuriv/Box/MPVM (Yuri Calvo)/winter 2026/ECL-298/photos"
for (a in 1:num_images){
  img <- image_read(urls[a])
  image_write(img,paste(path,"/",a,".jpg",sep=""))
}
# CNN --------------------------------------------------------------------------

image_files <- list.files(path, pattern=".jpg$", full.names = TRUE)
x <- array(dim = c(num_images, 224, 224, 3))
for (i in 1:num_images) {
  img <- image_load(image_files[i], target_size = c(224, 224))
  x[i,,, ] <- image_to_array(img)
}

x <- imagenet_preprocess_input(x)
model <- application_resnet50(weights = "imagenet")
summary(model)

pred6 <- predict(model, x) |>
  imagenet_decode_predictions(top = 5)
names(pred6) <- basename(image_files)
print(pred6)


