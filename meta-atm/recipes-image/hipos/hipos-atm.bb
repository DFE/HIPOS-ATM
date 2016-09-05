require hipos-image.inc

export IMAGE_BASENAME = "hipos-atm"

IMAGE_FSTYPES = "tar.bz2"

IMAGE_INSTALL += " \
	sqlite3 \
	poppler \
	atm \
        qtbase-fonts \
        qtbase-plugins \
        qtbase-tools \
	qttools \
        icu \
  gstreamer \
  gst-plugins-base-app \
  gst-plugins-base-meta \
  gst-plugins-good-meta \
  gst-plugins-ugly-meta \
  gst-meta-audio \
  gst-meta-video \
  gst-plugins-base-tcp \
  gst-plugins-good-meta \
  gst-plugins-good-udp \
  gst-plugins-good-rtp \
  gst-plugins-good-rtpmanager \
  gst-plugins-good-rtsp \
  v4l-utils \
  live555 \
  qtimageformats-plugins \
  hydraudp \
  packagegroup-fonts-truetype \
  liberation-fonts \
  psplash \
"
