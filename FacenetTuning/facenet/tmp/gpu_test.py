import tensorflow as tf
import os
os.environ['TF_FORCE_GPU_ALLOW_GROWTH'] = 'true'
#os.environ['TF_CPP_MIN_LOG_LEVEL'] = '0'  # or any {'0', '1', '2', '3'}
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'  # or any {'0', '1', '2', '3'}

#tf.test.is_gpu_available() #should return True 
tf.config.list_physical_devices('GPU')
#tf.compat.v1.disable_v2_behavior()  #disable for tensorFlow V2
#physical_devices = tf.config.list_physical_devices('GPU')
#tf.config.experimental.set_memory_growth(physical_devices[0], True)

print("Num GPUs Available: ", len(tf.config.list_physical_devices('GPU')))