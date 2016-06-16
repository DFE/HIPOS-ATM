# common stuff for all HIPOS images

require image-common.inc

IMAGE_INSTALL_append_hikirk += " \
  hipos-watchdog \
"

IMAGE_INSTALL_append_himx0294 += " \
  hipos-watchdog \
  hipos-lte-gps \
  libdrtp \
  libdrtp-demo \
"

