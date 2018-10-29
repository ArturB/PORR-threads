import csv
from pylab import imshow, show, get_cmap
import numpy as np

def hex_to_int(value):
    return int.from_bytes(value, byteorder='big')

def hex_to_int_list(value):
    return [val for val in value]

def get_hex_list(file_input, start, length):
    file_input.seek(start)
    return file_input.read(length)

def read_label(label_file):
    with open(label_file, 'rb') as file_hex:
        magic_number = hex_to_int(get_hex_list(file_hex, 0, 4))
        no_item = hex_to_int(get_hex_list(file_hex, 4, 4))
        label_list = hex_to_int_list(get_hex_list(file_hex, 8, no_item))
        return label_list

def read_image(image_file):
    with open(image_file, 'rb') as file_hex:
        magic_number = hex_to_int(get_hex_list(file_hex, 0, 4))
        no_item = hex_to_int(get_hex_list(file_hex, 4, 4))
        no_row = hex_to_int(get_hex_list(file_hex, 8, 4))
        no_column = hex_to_int(get_hex_list(file_hex, 12, 4))
        elements_in_image = no_row * no_column
        image_list = [hex_to_int_list(get_hex_list(file_hex, 16 + (no_image*elements_in_image), elements_in_image)) for no_image in range(no_item)]

        return image_list

def write_to_csv(filename, file_list):
    with open(filename, 'w', newline='') as csv_file:
        csv_file = csv.writer(csv_file, delimiter=',')
        for value in file_list:
            csv_file.writerow(value)

def write_to_txt(filename, file_list):
    with open(filename, 'w') as txt_file:
        for value in file_list:
            txt_file.write(str(value) + '\n')

def show_image(pixel_list, row, column):
    image_list = []
    tmp_list = []
    for index, value in enumerate(pixel_list):
        if index % row == 0 and index != 0:
            image_list.append(tmp_list)
            tmp_list = []
        tmp_list.append(float(value)/256)
    return image_list


label_10k = read_label('t10k-labels.idx1-ubyte')
label_60k = read_label('train-labels.idx1-ubyte')

image_10k = read_image('t10k-images.idx3-ubyte')
image_60k = read_image('train-images.idx3-ubyte')

write_to_txt('label_10k.txt', label_10k)
write_to_txt('label_60k.txt', label_60k)
write_to_csv('image_10k.csv', image_10k)
write_to_csv('image_60k.csv', image_60k)

image_list = show_image(image_10k[2], 28, 28)
Z = np.random.random((28,28))
imshow(image_list, cmap='gray', interpolation='nearest')
show()





    


            





