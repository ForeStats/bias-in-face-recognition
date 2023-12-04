#! encoding: utf-8

import os
import numpy as np
import cv2

class ResizeImages:

    def __init__(self, data_dir, resized_data_dir, img_ext):
        """
        Parameter data_dir, is your data directory.
        Parameter resized_data_dir, is your output data directory.
        Parameter img_ext, is the image data extension for all of your image data.
        """
        self.data_dir = data_dir
        self.resized_data_dir = resized_data_dir
        self.img_ext = img_ext


    
	
    def ResizeImage(self):
        self.resize_image_data_dir()
		

    def resize_image_data_dir(self):
        """
        resize image in data dir
        """
        folderIndex = 0
        for name in os.listdir(self.data_dir):
            folderIndex += 1
            # printing name
            print(name + " = Name")
	        
            fileIndex = 0
            for inputfile in os.listdir(self.data_dir + name):
                fileIndex += 1
                tempfilename = inputfile.rsplit( ".", 1 )[ 0 ]
                temp = tempfilename.split("_") # This line may vary depending on how your images are named.
                otherImage = ""
                if int(temp[2])>int(0):
                    otherImage = "_" + str(temp[2])

                outfilename = os.path.join(self.resized_data_dir, os.path.join("M"+ str(folderIndex).zfill(6), "M"+ str(folderIndex).zfill(6)+ "_"+str(fileIndex).zfill(4) + str(otherImage) + ".jpg"))
                #print(inputfile)
                print(outfilename)

                os.makedirs(os.path.dirname(outfilename),exist_ok=True)
                with open(inputfile, "w") as f:
					# Save image in set directory
					# Read RGB image
                    #images = []
                    img = cv2.imread(os.path.join(self.data_dir, os.path.join(name, inputfile)))
                    # get dimensions of image
                    dimensions = img.shape
		
		            # height, width, number of channels in image
                    h = img.shape[0]
                    w = img.shape[1]
                    c = img.shape[2] if len(img.shape)>2 else 1

                    #h, w = img.shape[:2]
                    #c = img.shape[2] if len(img.shape)>2 else 1
                    #size=(160,160)
                    size=(182,182)
                    if h == w: 
                        outImg=cv2.resize(img, size, cv2.INTER_AREA)
                        cv2.imwrite(outfilename, outImg)
                        continue

                    dif = h if h > w else w

                    interpolation = cv2.INTER_CUBIC
                    if dif > (size[0]+size[1])//2:
                        interpolation = cv2.INTER_AREA

                    x_pos = (dif - w)//2
                    y_pos = (dif - h)//2

                    if len(img.shape) == 2:
                        mask = np.zeros((dif, dif), dtype=img.dtype)
                        mask[y_pos:y_pos+h, x_pos:x_pos+w] = img[:h, :w]
                    else:
                        mask = np.zeros((dif, dif, c), dtype=img.dtype)
                        mask[y_pos:y_pos+h, x_pos:x_pos+w, :] = img[:h, :w, :]

                    outImg = cv2.resize(mask, size, interpolation)
                    cv2.imwrite(outfilename, outImg)



if __name__ == '__main__':
    data_dir = "e:\\Project\\Software\\Data\\MS-Celeb-1M\\data\\aligned_face_images\\FaceImageCroppedWithAlignment\\"
    resized_data_dir = "e:\\Project\\Software\\Data\\MS-Celeb-1M\\data\\aligned_face_images\\FaceImageCroppedWithAlignment_Resized\\"
    img_ext = ".jpg"
    resizeImages = ResizeImages(data_dir, resized_data_dir, img_ext)
    resizeImages.ResizeImage()