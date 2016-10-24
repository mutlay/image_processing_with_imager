# https://dahtah.wordpress.com/2015/06/05/new-package-for-image-processing-in-r/
# http://dahtah.github.io/imager/foreground_background.html
library(imager)
im <- load.image("1362481787.jpg")
layout(t(1:3))
plot(im, main="Original Image")
grad <- grayscale(im) %>% get_gradient("xy")
names(grad) <- paste("Gradient along", c("x", "y"))
l_ply(names(grad), function(n) plot(grad[[n]],main = n))

###
grad.sq <- grad %>% llply(function(v) v^2)
grad.sq <- add(grad.sq) #Add (d/dx)^2 and (d/dy)^2
edges <- imsplit(grad.sq,"c") %>% add
plot(sqrt(edges), main = "Detected edges")

###

# We have too many edges: for example, the edges around the eyes are spurious (they’re not true object boundaries). A way of mitigating the problem is to operate at a lower resolution. I’ll wrap everything into a function

#Sigma is the size of the blur window.

detect.edges <- function(im,sigma=1)
{
  isoblur(im,sigma) %>% imgradient("xy") %>% llply(function(v) v^2) %>% add %>% imsplit("c") %>% add
}

detect.edges(im, 5) %>% sqrt %>% plot
