from skimage.filters import gabor_kernel
def gabor_filter_bank_fun(frequency, theta=0, bandwidth=1, sigma_x=None, sigma_y=None, n_stds=3, offset=0):
  kernel = gabor_kernel(frequency, theta, bandwidth, sigma_x, sigma_y, n_stds, offset)
 
  return(kernel)
