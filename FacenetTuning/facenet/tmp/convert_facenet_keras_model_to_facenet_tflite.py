import tensorflow.lite as lite

def main():
    input_file = "./models/hiroki/facenet_keras.h5"
    output_file = "./models/hiroki/facenet.tflite"

    # Converts the Keras model to TensorFlow Lite
    converter = lite.TocoConverter.from_keras_model_file(input_file)
    converter.post_training_quantize = True
    tflite_model = converter.convert()
    open(output_file, "wb").write(tflite_model)

if __name__ == '__main__':
    main()
