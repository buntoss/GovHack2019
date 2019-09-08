import cv2
import pdf2image
import pytesseract
import numpy as np
from PIL import Image

pages = pdf2image.convert_from_path(pdf_path='./Statement20190630.pdf',
                                    dpi=500)

for i, page in enumerate(pages):
    page = np.array(page)
    gray = cv2.cvtColor(page, cv2.COLOR_BGR2GRAY)
    gray = cv2.threshold(gray, 0, 255, cv2.THRESH_BINARY | cv2.THRESH_OTSU)[1]
    filename = './temp.png'
    cv2.imwrite(filename, gray)
    text = pytesseract.image_to_string(Image.open(filename))
    print(text)
#     page.save(f'out_{i}.jpg', 'JPEG')

print('done')

