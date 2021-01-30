colpal <- function (colmin = "#e7e9ea", colmax = "#707982", 
                    N = 10, bias = 0.8) 
{
  degrade <- colorRampPalette(c(colmin, colmax), bias = bias)
  mypal <- c(degrade(N))
  return(mypal)
}

im <- c("#100D26", "#1B1640",  "#022873", "#0444BF", "#0656BF",  "#0174D1",
        "#0486DA", "#0584F2", "#0798F2", "#0788D9", "#0AAFF1", "#0CC8F2")

im1 <- colpal(colmin = "#0CC8F2", colmax = "#100D26", N = 20 )


im_pal <- function (nb_col = 10) {
  manual_pal(im)(nb_col)
}

im1_pal <- function (nb_col = 10) {
  manual_pal(im1)(nb_col)
}


sk <- c("#040C0E", "#0C1416", "#132326", "#132226",
        "#41443A", "#515954", "#525B56", "#646354",
        "#BF8F65", "#BF8665",  "#DAB183", "#A69992")

sk1 <- colpal(colmin = "#BF8665", colmax = "#040C0E", N = 20 )



sk_pal <- function (nb_col = 10) {
  manual_pal(sk)(nb_col)
}

sk1_pal <- function (nb_col = 10) {
  manual_pal(sk1)(nb_col)
}




ca <- c("#BF4A06", "#F27507","#F18904", "#D08406",
        "#F2B90C", "#F2A20C", 
        "#FE9B00", 
        "#F49F05", "#FFBB06", "#F3CD05", "#FEDE3D",
        "#FFD95C")

ca1 <- colpal(colmin = "#FFD95C", colmax = "#BF4A06", N = 20 )


ca_pal <- function (nb_col = 10) {
  manual_pal(ca)(nb_col)
}


ca1_pal <- function (nb_col = 10) {
  manual_pal(ca1)(nb_col)
}







# 
# ta_pal <- function (nb_col = 10) {
#   manual_pal(ta)(nb_col)
# }
# 
# te_pal <- function (nb_col = 10) {
#   manual_pal(te)(nb_col)
# }
# 
# mo_pal <- function (nb_col = 10) {
#   manual_pal(mo)(nb_col)
# }
# 
# 
# pa_pal <- function (nb_col = 10) {
#   manual_pal(pa)(nb_col)
# }
# 
# ma_pal <- function (nb_col = 10) {
#   manual_pal(ma)(nb_col)
# }

colpal <- function (colmin = "#e7e9ea", colmax = "#707982", 
                    N = 10, bias = 0.8) 
{
  degrade <- colorRampPalette(c(colmin, colmax), bias = bias)
  mypal <- c(degrade(N))
  return(mypal)
}

im <- c("#100D26", "#1B1640",  "#022873", "#0444BF", "#0656BF",  "#0174D1",
        "#0486DA", "#0584F2", "#0798F2", "#0788D9", "#0AAFF1", "#0CC8F2")

im1 <- colpal(colmin = "#0CC8F2", colmax = "#100D26", N = 20 )


im_pal <- function (nb_col = 10) {
  manual_pal(im)(nb_col)
}

im1_pal <- function (nb_col = 10) {
  manual_pal(im1)(nb_col)
}


sk <- c("#040C0E", "#0C1416", "#132326", "#132226",
        "#41443A", "#515954", "#525B56", "#646354",
        "#BF8F65", "#BF8665",  "#DAB183", "#A69992")

sk1 <- colpal(colmin = "#BF8665", colmax = "#040C0E", N = 20 )



sk_pal <- function (nb_col = 10) {
  manual_pal(sk)(nb_col)
}

sk1_pal <- function (nb_col = 10) {
  manual_pal(sk1)(nb_col)
}




ca <- c("#BF4A06", "#F27507","#F18904", "#D08406",
        "#F2B90C", "#F2A20C", 
        "#FE9B00", 
        "#F49F05", "#FFBB06", "#F3CD05", "#FEDE3D",
        "#FFD95C")

ca1 <- colpal(colmin = "#FFD95C", colmax = "#BF4A06", N = 20 )


ca_pal <- function (nb_col = 10) {
  manual_pal(ca)(nb_col)
}


ca1_pal <- function (nb_col = 10) {
  manual_pal(ca1)(nb_col)
}







# 
# ta_pal <- function (nb_col = 10) {
#   manual_pal(ta)(nb_col)
# }
# 
# te_pal <- function (nb_col = 10) {
#   manual_pal(te)(nb_col)
# }
# 
# mo_pal <- function (nb_col = 10) {
#   manual_pal(mo)(nb_col)
# }
# 
# 
# pa_pal <- function (nb_col = 10) {
#   manual_pal(pa)(nb_col)
# }
# 
# ma_pal <- function (nb_col = 10) {
#   manual_pal(ma)(nb_col)
# }