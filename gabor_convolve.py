import tensorflow as tf
import numpy as np
def gabor_convolve(input,gabor_kernels,strides_var):
  im_convolved_real = tf.nn.convolution(
    input,  np.real(gabor_kernels), strides_var, padding='VALID', 
    data_format=None, dilations=None,
    name=None)
  im_convolved_im = tf.nn.convolution(
    input,  np.imag(gabor_kernels), strides_var, padding='VALID', 
    data_format=None, dilations=None,
    name=None)
  return(np.sqrt(im_convolved_real**2 + im_convolved_im**2))

