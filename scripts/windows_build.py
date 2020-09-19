import sys
import os
import colorama,os,subprocess

current_dir = os.path.dirname(os.path.abspath(__file__))

colorama.init()

while True:
    rc = subprocess.call(["make","-f","makefile"],cwd=os.path.join(current_dir,"..","src"))

    if rc:
        print(colorama.Back.RED+"Build Failed"+colorama.Back.RESET)
        input("Press return to retry")
    else:
        print(colorama.Back.GREEN+"Build Succeeded")
        input("Press return")
        break