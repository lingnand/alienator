#!/usr/bin/python
# build_native.py
# Build native codes
#
# Please use cocos console instead


import sys
import os, os.path
import shutil
from optparse import OptionParser

def build(android_platform, build_mode):


    current_dir = os.path.dirname(os.path.realpath(__file__))
    cocos_root = os.path.join(current_dir, "../cocos2d")

    app_android_root = os.path.join(current_dir, "../")

    if build_mode is None:
    	  build_mode = 'debug'
    elif build_mode != 'release':
        build_mode = 'debug'

    if android_platform is None:
        android_platform_args = ""
    else:
        android_platform_args = "--ap %s" % android_platform

    command = 'cocos compile -p android %s --android-studio -s %s -m %s' % (android_platform_args, app_android_root, build_mode)

    print "Running" , command
    if os.system(command) != 0:
        raise Exception("Build dynamic library for project [ " + app_android_root + " ] fails!")

# -------------- main --------------
if __name__ == '__main__':

    parser = OptionParser()
    parser.add_option("-n", "--ndk", dest="ndk_build_param", help='it is not used', action="append")
    parser.add_option("-p", "--platform", dest="android_platform", default="android-22")
    parser.add_option("-b", "--build", dest="build_mode",
    help='the build mode for java project,debug[default] or release.Get more information,please refer to http://developer.android.com/tools/building/building-cmdline.html')
    (opts, args) = parser.parse_args()

    print "Please use cocos console instead.\n"

    build(opts.android_platform, opts.build_mode)
