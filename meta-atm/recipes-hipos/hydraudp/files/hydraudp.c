/* Editor hints for vim
 * vim:set ts=4 sw=4 noexpandtab:  */
/**\file
 * \brief  Video access to remote recorder devices
 * \author Ralf Schröder
 *
 * (C) 2016,2017 DResearch Fahrzeugelektronik GmbH
 *
 * This program is
 * - a video server connecting to remote systems and
 *   display streams on a display.
 * - a simple adapter layer to allow the communication
 *   with the continues running server
 *
 * Normally the server is started as systemd service.
 * However the adapter creates a missing server instance
 * automatically.
 *
 * $Id$
 *
 */
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <glib.h>
#include <time.h>
#include <sys/time.h>
#include <gst/gst.h>
#include <sys/ioctl.h>
#include <linux/fb.h>

#define SD_JOURNAL_SUPPRESS_LOCATION /* otherwise compile problems not understand yet (RSR) */
#include <systemd/sd-journal.h>

#include <drtp.h>

#define MIN_NET_DELAY_MS 1000             /**< network stream lost detection in ms, start reconnecting after that period */
#define BLACK_SCREEN_DELAY 750            /**< display progress fail detection time in ms, draw black after that period */
#define MAX_QUADRANT 4                    /**< maximum supported quadrants (by this application) */
#define FRAMEBUFFER "/dev/fb1"            /**< Frame buffer device of the display, the physical connector depends on the used hardware */
#define RUN_NAME "/var/run/hydraudp"      /**< Name base of the FIFO and PID file */


/** Helper macros to simplify the code */
#define CHECK_PTR(P)  check_true((P) !=0, __func__, __LINE__, #P, "NULL")
#define CHECK_TRUE(B) check_true((B), __func__, __LINE__, #B, "FALSE")

static inline gboolean check_true(gboolean b, const char* func, unsigned line, const char* expr, const char *what)
{
	if (b)
	{
		g_debug("%s line %u '%s' ok", __func__, line, expr);
	} else {
		g_error("%s line %u '%s' is %s", __func__, line, expr, what);
	}
	return b;
}


/** Return value distinguish adapter/server layer */
typedef enum
{
	E_FAILED = 0,   /**< Fatal problem */
	E_SERVER,       /**< background video server */
	E_ADAPTER       /**< adapter layer */
} appl_t;


/** Connection object holding all relevant information */
typedef struct /* inherits struct drtp_stream_handle_t */
{
	struct drtp_stream_handle_t sh;           /**< librtp stream handle, first member for cast to stream_t in rtp callbacks */
	unsigned vin;                             /**< vin id for tracing */
	gchar* ipaddr;                            /**< IP address for the remote connection */
	unsigned x,y,w,h;                         /**< quadrant layout */

	GstElement *pipeline;                     /**< pipeline container */
	GstElement *appsrc;                       /**< application source */
	GstElement *vpudec;                       /**< decoder */
	GstElement *display;                      /**< display sink */
	gulong signalErrorID;                     /**< id of the error signal handler */

	unsigned q;                               /**< rtp subheader q */
	unsigned height;                          /**< rtp subheader height */
	unsigned width;                           /**< rtp subheader width */
	unsigned type;                            /**< rtp subheader type */
	unsigned header_offset;                   /**< reserved header offet for local header generation */

	unsigned char framebuf[150 * 1024];       /**< rtp frame buffer memory */

	unsigned frame_count;                     /**< frame counter (statistics only) */
	unsigned net_errors;                      /**< detected net errors (statistics only) */
	gint64  start_ts;                         /**< simple start timestamp (statistics only) */
	gint64 display_ts;                        /**< timestamp set on display sink to observe the progress */
	gint64 frame_ts;                          /**< timestamp set on rtp callback to observe received frames */
	gint64 black_ts;                          /**< timestamp of the last back drawing */
	GMutex mutex_ts;                          /**< timestamp mutex because setting is not atomic */

	int reconnect_delay;                      /** reconnect delay in sec to avoid overloading the recorder */
	gboolean running;                         /**< TRUE if the stream is actively displayed */

	/**< Set TRUE if the source is an hipox device.
	 * An hipox device can scale without problems but has a bottle neck with the encoder cpu. So it's good to request
	 * CIF only on quad views.
	 *
	 * New hipos device have a bottle neck mit IPU function, i,e, scaling. Here it's better to encode 4 pal streams instead
	 * of scaling it.
	 *
	 * We can set the field by observing the jpeg header information. That's ok for most applications. If not sufficient,
	 * api/system/info.json must be accessed with correct credentials to set the field on other, more earlier ways.
	 */
	gboolean is_hipox;
} stream_t;


static gboolean s_version = FALSE;            /**< TRUE for version printing */
static gboolean s_server = FALSE;             /**< TRUE as server */
static gboolean s_stdout = FALSE;             /**< debug all to stdout (do no deamonize if the server is started) */
static gint s_debug = 4;                      /**< debug level */
static gint s_stream_trace;                   /**< server stream trace (needs level 5 debug if used, developer option only) */
static int s_fifo;                            /**< fifo descriptor (adapter layer only) */



static GMainLoop *s_pGMainloop;               /**< server mainloop object (server only) */
static int fb_handle = -1;                    /**< frame buffer file descriptor (server only, lazy init) */
static struct fb_var_screeninfo fb_vinfo;     /**< variable screen info (server only) */
static struct fb_fix_screeninfo fb_finfo;     /**< fix screen info (server only) */

static stream_t* s_streams[MAX_QUADRANT];     /**< active stream objects per quadrant if not 0 (server only) */
static gboolean trace_fames[DRTP_MAX_CAMS];   /**< stream trace flag (server only) */

#define FIX_VIDEO_AREA
/** Support changes for other display resolution later. See code inf server_main to fix these constants... */
#ifdef FIX_VIDEO_AREA
#define HEIGHT 768                            /**< video area height */
#define WIDTH  960                            /**< video area width */
#else
static unsigned HEIGHT = 768;                 /**< video area height */
static unsigned WIDTH = 960;                  /**< video area width */
#endif /* FIX_VIDEO_AREA */


// Forwards...
static gboolean connect_fifo(void* ign);
static gboolean check_network(gpointer _thread);




#if 0
/* Code to activate the vout connectors on non monitor hardware (himx0294 ivap/dvrec, hipox,...).
 * A monitor is activated with the default startup
 *
 * I do not remove that code to allow tests with other hardware... (RSR).
 */

#define MAX_VOUT 1                        /**< variant 1 display, no other supported vout connectors */
#define MAX_VIN 0                         /**< no vin connectors with that hardware */

/** Simple file write helper */
static void write_file(const char* filename, const char* what, unsigned size)
{
	int fd = open(filename, O_WRONLY);
	int ret;
	if (fd < 0) {
		printf("open %s failed: %s\n", filename, strerror(errno));
		return;
	}
	if ((int) size != (ret = write(fd, what, size))) {
		printf("write %s to %s failed (ret:%d): %s\n", what, filename, ret, strerror(errno));
	}
	close(fd);
	printf("%s:  %s to %s\n", __func__, what, filename);
}

/** Switch on/off the physical connector of the given frame buffer (off means no sync signal's are generated) */
static void fb_sync(unsigned fb, gboolean state)
{
	char buf[64];
	sprintf(buf, "/sys/class/graphics/fb%u/blank", fb);
	if (g_file_test(buf, G_FILE_TEST_EXISTS)) {
		printf("%s: fb%u %s\n", __func__, fb, (state ? "on" : "off"));
		write_file(buf, (state ? "0" : "1"), 1);
	} else {
		printf("%s: %s not supported\n", __func__, buf);
	}
}

/** Connector activation. This process is highly hardware dependent. */
static int vout_activation(unsigned vout, int cam)
{
	vout--;
	assert(vout < MAX_VOUT);
	assert(cam <= MAX_VIN);
	if (cam > 0) {
		printf("bypassc setting  vout/%u to %u not supported by hardware\n", vout + 1, cam);
		return -1;
	} else {
		printf("%s:  set vout/%u to %d\n", __func__, vout + 1, cam);
		if (0 == cam) {
			fb_sync(2 * vout, TRUE);
		} else {
			fb_sync(2 * vout, FALSE);
		}
		return 0;
	}
}

#endif


/** Helper to improve tracing of debug levels */
static const gchar * log_level_to_string (GLogLevelFlags level)
{
	switch (level)
	{
		case G_LOG_LEVEL_ERROR: return "ERROR";
		case G_LOG_LEVEL_CRITICAL: return "CRITICAL";
		case G_LOG_LEVEL_WARNING: return "WARNING";
		case G_LOG_LEVEL_MESSAGE: return "MESSAGE";
		case G_LOG_LEVEL_INFO: return "INFO";
		case G_LOG_LEVEL_DEBUG: return "DEBUG";
		default: return "UNKNOWN";
	}
}

/** G-log handler for journal support (very simple, could be improved to work with the gstreamer trace facility) */
static void log_handler_cb (const gchar* log_domain, GLogLevelFlags log_level, const gchar *message, gpointer user_data)
{
	const gchar *log_level_str;
	log_level &= G_LOG_LEVEL_MASK;
	if ((G_LOG_LEVEL_ERROR << s_debug) < log_level) { return; }

	log_level_str = log_level_to_string (log_level);
	if (!s_stdout)
	{
		sd_journal_print(((log_level <= G_LOG_LEVEL_WARNING) ? LOG_WARNING: LOG_NOTICE), "%s(%u) %s", log_level_str, log_level & G_LOG_LEVEL_MASK, message);
	} else {
		g_print ("%s(%u): %s\n", log_level_str, log_level, message);
	}
}


/** Set a blank quadrant using the frame buffer device directly. */
static void blank_area(unsigned x, unsigned y, unsigned width, unsigned height, int diff)
{
	g_message("%s: [%u,%u,%u,%u] diff:%dms", __func__, x, y, width, height, diff);
	gchar* blank_line = g_new0(char, fb_finfo.line_length);
	int line_len = width * fb_vinfo.bits_per_pixel / 8;
	unsigned h;
	for(h = 0; h < height; h++)
	{
		long location = (x+fb_vinfo.xoffset) * (fb_vinfo.bits_per_pixel/8) + (y+fb_vinfo.yoffset+h) * fb_finfo.line_length;
		if (location != lseek(fb_handle, location, SEEK_SET))
		{
			g_critical("%s: seek failed: %s", __func__, strerror(errno));
			break;
		}
		if (line_len != write(fb_handle, blank_line, line_len))
		{
			g_critical("%s: write framebuffer failed: %s", __func__, strerror(errno));
			break;
		}
	}
}



/*** RTP standard stuff taken directly from RFC
 * This code is used to generate the jpeg header from rtp context if not available.
 * Note: its legal to cut the header information!
***/

/*
 * Table K.1 from JPEG spec.
 */
static const int jpeg_luma_quantizer[64] =
{
	16, 11, 10, 16, 24, 40, 51, 61,
	12, 12, 14, 19, 26, 58, 60, 55,
	14, 13, 16, 24, 40, 57, 69, 56,
	14, 17, 22, 29, 51, 87, 80, 62,
	18, 22, 37, 56, 68, 109, 103, 77,
	24, 35, 55, 64, 81, 104, 113, 92,
	49, 64, 78, 87, 103, 121, 120, 101,
	72, 92, 95, 98, 112, 100, 103, 99
};

/*
 * Table K.2 from JPEG spec.
 */
static const int jpeg_chroma_quantizer[64] =
{
	17, 18, 24, 47, 99, 99, 99, 99,
	18, 21, 26, 66, 99, 99, 99, 99,
	24, 26, 56, 99, 99, 99, 99, 99,
	47, 66, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99
};

/*
 * Call MakeTables with the Q factor and two u_char[64] return arrays
 */
static void MakeTables(int q, u_char *lqt, u_char *cqt)
{
	int i;
	int factor = q;

	if (q < 1) factor = 1;
	if (q > 99) factor = 99;
	if (q < 50)
		q = 5000 / factor;
	else
		q = 200 - factor * 2;

	for (i = 0; i < 64; i++) {
		int lq = (jpeg_luma_quantizer[i] * q + 50) / 100;
		int cq = (jpeg_chroma_quantizer[i] * q + 50) / 100;

		/* Limit the quantizers to 1 <= q <= 255 */
		if (lq < 1) lq = 1;
		else if (lq > 255) lq = 255;
		lqt[i] = lq;

		if (cq < 1) cq = 1;
		else if (cq > 255) cq = 255;
		cqt[i] = cq;
	}
}

/*
   The following routines can be used to create the JPEG marker segments
   corresponding to the table-specification data that is absent from the
   RTP/JPEG body.
 */

static u_char lum_dc_codelens[] =
{
	0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
};

static u_char lum_dc_symbols[] =
{
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
};

static u_char lum_ac_codelens[] =
{
	0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 0x7d,
};

static u_char lum_ac_symbols[] =
{
	0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12,
	0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07,
	0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xa1, 0x08,
	0x23, 0x42, 0xb1, 0xc1, 0x15, 0x52, 0xd1, 0xf0,
	0x24, 0x33, 0x62, 0x72, 0x82, 0x09, 0x0a, 0x16,
	0x17, 0x18, 0x19, 0x1a, 0x25, 0x26, 0x27, 0x28,
	0x29, 0x2a, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
	0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
	0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
	0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
	0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
	0x7a, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
	0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98,
	0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
	0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6,
	0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3, 0xc4, 0xc5,
	0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4,
	0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xe1, 0xe2,
	0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea,
	0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
	0xf9, 0xfa,
};

static u_char chm_dc_codelens[] =
{
	0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
};

static u_char chm_dc_symbols[] =
{
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
};

static u_char chm_ac_codelens[] =
{
	0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 0x77,
};

static u_char chm_ac_symbols[] =
{
	0x00, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21,
	0x31, 0x06, 0x12, 0x41, 0x51, 0x07, 0x61, 0x71,
	0x13, 0x22, 0x32, 0x81, 0x08, 0x14, 0x42, 0x91,
	0xa1, 0xb1, 0xc1, 0x09, 0x23, 0x33, 0x52, 0xf0,
	0x15, 0x62, 0x72, 0xd1, 0x0a, 0x16, 0x24, 0x34,
	0xe1, 0x25, 0xf1, 0x17, 0x18, 0x19, 0x1a, 0x26,
	0x27, 0x28, 0x29, 0x2a, 0x35, 0x36, 0x37, 0x38,
	0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
	0x49, 0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
	0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
	0x69, 0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
	0x79, 0x7a, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
	0x88, 0x89, 0x8a, 0x92, 0x93, 0x94, 0x95, 0x96,
	0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5,
	0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4,
	0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3,
	0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2,
	0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda,
	0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9,
	0xea, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
	0xf9, 0xfa,
};

static u_char * MakeQuantHeader(u_char *p, u_char *qt, int tableNo)
{
	*p++ = 0xff;
	*p++ = 0xdb; /* DQT */
	*p++ = 0; /* length msb */
	*p++ = 67; /* length lsb */
	*p++ = tableNo;
#if 0
	memcpy(p, qt, 64);
	return (p + 64);
#else  /* We need the reorder, somehow the code from standard is incorrect */
	int z;
	/* jpeg_natural_order[i] is the index (in natural order) of 8x8 table entry (in zig-zag order) */
	static const int jpeg_natural_order[] ={
		0, 1, 8, 16, 9, 2, 3, 10,
		17, 24, 32, 25, 18, 11, 4, 5,
		12, 19, 26, 33, 40, 48, 41, 34,
		27, 20, 13, 6, 7, 14, 21, 28,
		35, 42, 49, 56, 57, 50, 43, 36,
		29, 22, 15, 23, 30, 37, 44, 51,
		58, 59, 52, 45, 38, 31, 39, 46,
		53, 60, 61, 54, 47, 55, 62, 63,
	};

	for (z = 0; z < 64; z++) {
		int i = jpeg_natural_order[z];
		*p++ = qt[i];
	}
	return p;
#endif
}

static u_char * MakeHuffmanHeader(u_char *p, u_char *codelens, int ncodes, u_char *symbols, int nsymbols, int tableNo, int tableClass)
{
	*p++ = 0xff;
	*p++ = 0xc4; /* DHT */
	*p++ = 0; /* length msb */
	*p++ = 3 + ncodes + nsymbols; /* length lsb */
	*p++ = (tableClass << 4) | tableNo;
	memcpy(p, codelens, ncodes);
	p += ncodes;
	memcpy(p, symbols, nsymbols);
	p += nsymbols;
	return (p);
}

static u_char *
MakeDRIHeader(u_char *p, u_short dri)
{
	*p++ = 0xff;
	*p++ = 0xdd; /* DRI */
	*p++ = 0x0; /* length msb */
	*p++ = 4; /* length lsb */
	*p++ = dri >> 8; /* dri msb */
	*p++ = dri & 0xff; /* dri lsb */
	return (p);
}

/*
 *  Arguments:
 *    type, width, height: as supplied in RTP/JPEG header
 *    lqt, cqt: quantization tables as either derived from
 *         the Q field using MakeTables() or as specified
 *         in section 4.2.
 *    dri: restart interval in MCUs, or 0 if no restarts.
 *
 *    p: pointer to return area
 *
 *  Return value:
 *    The length of the generated headers.
 *
 *    Generate a frame and scan headers that can be prepended to the
 *    RTP/JPEG data payload to produce a JPEG compressed image in
 *    interchange format (except for possible trailing garbage and
 *    absence of an EOI marker to terminate the scan).
 */
static int MakeHeaders(u_char *p, int type, int w, int h, u_char *lqt, u_char *cqt, u_short dri)
{
	u_char *start = p;

	/* convert from blocks to pixels */
	w <<= 3;
	h <<= 3;

	*p++ = 0xff;
	*p++ = 0xd8; /* SOI */

	p = MakeQuantHeader(p, lqt, 0);
	p = MakeQuantHeader(p, cqt, 1);

	if (dri != 0)
		p = MakeDRIHeader(p, dri);

	*p++ = 0xff;
	*p++ = 0xc0; /* SOF */
	*p++ = 0; /* length msb */
	*p++ = 17; /* length lsb */
	*p++ = 8; /* 8-bit precision */
	*p++ = h >> 8; /* height msb */
	*p++ = h; /* height lsb */
	*p++ = w >> 8; /* width msb */
	*p++ = w; /* wudth lsb */
	*p++ = 3; /* number of components */
	*p++ = 0; /* comp 0 */
	if (type == 0)
		*p++ = 0x21; /* hsamp = 2, vsamp = 1 */
	else
		*p++ = 0x22; /* hsamp = 2, vsamp = 2 */
	*p++ = 0; /* quant table 0 */
	*p++ = 1; /* comp 1 */
	*p++ = 0x11; /* hsamp = 1, vsamp = 1 */
	*p++ = 1; /* quant table 1 */
	*p++ = 2; /* comp 2 */
	*p++ = 0x11; /* hsamp = 1, vsamp = 1 */
	*p++ = 1; /* quant table 1 */
	p = MakeHuffmanHeader(p, lum_dc_codelens,
			sizeof (lum_dc_codelens),
			lum_dc_symbols,
			sizeof (lum_dc_symbols), 0, 0);
	p = MakeHuffmanHeader(p, lum_ac_codelens,
			sizeof (lum_ac_codelens),
			lum_ac_symbols,
			sizeof (lum_ac_symbols), 0, 1);
	p = MakeHuffmanHeader(p, chm_dc_codelens,
			sizeof (chm_dc_codelens),
			chm_dc_symbols,
			sizeof (chm_dc_symbols), 1, 0);
	p = MakeHuffmanHeader(p, chm_ac_codelens,
			sizeof (chm_ac_codelens),
			chm_ac_symbols,
			sizeof (chm_ac_symbols), 1, 1);

	*p++ = 0xff;
	*p++ = 0xda; /* SOS */
	*p++ = 0; /* length msb */
	*p++ = 12; /* length lsb */
	*p++ = 3; /* 3 components */
	*p++ = 0; /* comp 0 */
	*p++ = 0; /* huffman table 0 */
	*p++ = 1; /* comp 1 */
	*p++ = 0x11; /* huffman table 1 */
	*p++ = 2; /* comp 2 */
	*p++ = 0x11; /* huffman table 1 */
	*p++ = 0; /* first DCT coeff */
	*p++ = 63; /* last DCT coeff */
	*p++ = 0; /* sucessive approx. */

	return (p - start);
};


/*** end of RTP standard stuff ***/


/*** handle RTP connection  ***/

/** network check main thread.
 * We send pings with rtp-fragment size and check the loss report.
 */
static void* check_network_main(void *_ip)
{
	GError *gerr = 0;
	gchar *ip = _ip, *out = 0, *err = 0;
	gint ret;
	gchar *cmd = g_strdup_printf("/bin/ping -c 30 -s 1400 %s", ip);
	g_debug("%s: %", __func__, cmd);
	if (g_spawn_command_line_sync(cmd, &out, &err, &ret, &gerr))
	{
		//g_message("%s '%s' ret: %d out:'%s' err:'%s' ", __func__, cmd, ret, out, err);
		gchar* loss = g_strstr_len(out, -1, "received, ");
		if (loss) { loss = strchr(loss, ' '); }
		if (loss) 
		{ 
			int percent = atoi(loss+1);
			if (percent) { g_critical("network to %s is instable %d%% package loss on ping", ip, percent); }
			else { g_message("network to %s currently stable (ping no loss)", ip); }
		} else { /* ping output changed...*/
			g_warning("network %s pinged without evaluable results (implementation problem)", ip);
		}
	} else {
		g_warning("%s ping failed: %s", __func__, gerr->message); /* upsi? */
		g_error_free(gerr);
	}
	g_free(out); g_free(err); g_free(cmd);
	g_idle_add(check_network, _ip);
	return 0;
}

/** Simple network checker to help finding instable network connections.
 *  The function must be called with 0. The network script calls it with the checked ip.
 *  We check the ip of quadrant 0.
 */
static gboolean check_network(gpointer ip)
{
	static GThread *t = 0;
	if (ip) 
	{
		g_debug("%s join ping to %s...", __func__, ip);
		g_free(ip);
		g_thread_join(t); t = 0;
	} else {
		if (!t && s_streams[0]) 
		{ 
			g_debug("%s starting ping...", __func__);
			t = g_thread_new("netcheck", check_network_main, g_strdup(s_streams[0]->ipaddr));
		}
	}
}

/** libdrtp error callback installed at start time. */
static void rtp_error_cb(const char *msg)
{
	if (msg)
	{
		unsigned len = strlen(msg);
		if (msg[len - 1] == '\n') { len -= 1; }
		g_warning("libdrtp: %.*s", len, msg);
		if (g_strstr_len(msg, len, "Expected seq")) { g_idle_add(check_network, 0); }
	}
}

/** Called before receiving a new frame to provide a frame buffer. We use a simple static one (per stream) here and skip the generated header. */
static void * rtp_memory_cb(struct drtp_stream_handle_t* sh, uint32_t *len)
{
	stream_t *stream = (stream_t*) sh;
	void * data = stream->framebuf + stream->header_offset;
	*len = sizeof (stream->framebuf) - stream->header_offset;
	return data;
}

/** Called for each read frame, note: the requested resolution can differ with the request in case of shared vout-streams with multiple display applications (especially web service tool).
 *  Note: The callback supports buffer queuing. Returning failed indicates, that the buffer can be reused. Because we use a non allocated buffer, we always return DRTP_SUCCESS here. 
 */
static enum drtp_status rtp_frame_cb(struct drtp_stream_handle_t* sh, uint32_t flags, uint32_t cam, uint32_t width, uint32_t height, uint64_t ts, void* data, uint32_t len, struct rtp_with_subheaders_t * header)
{
	stream_t* stream = (stream_t*) sh;

	if (!stream->running) { return DRTP_SUCCESS; }

	if (trace_fames[stream->vin-1]) { g_debug("%s: %p cam:%u %ux%u flags:%02X size:%u ts:%llu", __func__, data, cam, width, height, flags, len, ts); }
	if (stream->framebuf + stream->header_offset != data) { g_error("%s illegal frame pointer", __func__); } /* impl sanity */
	if (sizeof (stream->framebuf) < stream->header_offset + len) { g_error("%s frame buffer overrun (%u/%u)", __func__, stream->header_offset + len, sizeof (stream->framebuf)); } /* impl sanity */
	if (header->rtp.bits.bit.type != RTP_TYPE_JPEG)
	{
		g_warning("%s: only jpeg supported: %d\n", __func__, header->rtp.bits.bit.type);
	} else if (
			(header->sub.jpeg.bit.type != stream->type) || (header->sub.jpeg.bit.width != stream->width) ||
			(header->sub.jpeg.bit.height != stream->height) || (header->sub.jpeg.bit.q != stream->q)
			) {
		stream->type = header->sub.jpeg.bit.type;
		stream->width = header->sub.jpeg.bit.width;
		stream->height = header->sub.jpeg.bit.height;
		stream->q = header->sub.jpeg.bit.q;
		if ((((uint8_t*)data)[0] != 0xFF) && (((uint8_t*)data)[1] != 0xD8))
		{
			/* The received stream does not include a jpeg header, we have to reconstruct it as proposed in the rtp rfc. It's unchanged, so we reuse it
			 * and put it on start of out buffer. To simplify the procedure, we do not display the first frame.
			 * This happens on hipox systems, all hipos systems send this header.
			 */
			u_char lqt[64], cqt[64];
			MakeTables(header->sub.jpeg.bit.q, lqt, cqt);
			stream->header_offset = MakeHeaders(stream->framebuf, header->sub.jpeg.bit.type, header->sub.jpeg.bit.width, header->sub.jpeg.bit.height, lqt, cqt, 0);
			g_debug("%s generate header with q:%u type:%u w:%u h:%u header:%u (hipox seen)",
					__func__, header->sub.jpeg.bit.q, header->sub.jpeg.bit.type, header->sub.jpeg.bit.width, header->sub.jpeg.bit.height, stream->header_offset);
			stream->is_hipox = TRUE; /* this is a hipox property */
		} else {
			g_debug("%s header with q:%u type:%u w:%u h:%u (non hipox systems)",
					__func__, header->sub.jpeg.bit.q, header->sub.jpeg.bit.type, 8*header->sub.jpeg.bit.width, 8*header->sub.jpeg.bit.height);
			stream->header_offset = 0;
			stream->is_hipox = FALSE; /* gstreamer encoder includes the header */
		}
	} else {
		if (flags & DRTP_PACKET_LOST)
		{
			stream->net_errors++; g_idle_add(check_network, 0);
			g_debug("%s q:%u type:%u w:%u h:%u header:%u data:%u flags:%02X (incomplete)",
					__func__, header->sub.jpeg.bit.q, header->sub.jpeg.bit.type, header->sub.jpeg.bit.width, header->sub.jpeg.bit.height, stream->header_offset, len, flags);
			if (stream->net_errors % 32 == 31)
			{
				g_warning("package loss counter is %u. Check the network connection or network overload!", stream->net_errors);
			}
#ifndef FORWARD_INCLOMPLETE_FRAMES  /* Warning: Using incomplete frames (green areas within the picture but progress) could crash the jpeg decoder and imply a server restart */
			return DRTP_SUCCESS;
#endif
		}
		GstBuffer *buf = gst_buffer_new_allocate(0, stream->header_offset + len, 0);
		gsize s = gst_buffer_fill(buf, 0, stream->framebuf, stream->header_offset + len);
		if (trace_fames[stream->vin-1])
		{
			g_debug("%s q:%u type:%u w:%u h:%u header:%u data:%u size:%u",
					__func__, header->sub.jpeg.bit.q, header->sub.jpeg.bit.type, header->sub.jpeg.bit.width, header->sub.jpeg.bit.height, stream->header_offset, len, s);
		}
		GstFlowReturn fret;
		g_signal_emit_by_name(stream->appsrc, "push-buffer", buf, &fret);
		if (fret != GST_FLOW_OK) {
			g_critical("%s: buffer not pushed", __func__);
		} else {
			g_mutex_lock(&stream->mutex_ts);
			stream->frame_ts = g_get_monotonic_time();
			stream->reconnect_delay = 0;
			g_mutex_unlock(&stream->mutex_ts);
		}
		gst_buffer_unref(buf); /* Free the buffer now that we are done with it */
	}
	return DRTP_SUCCESS;
}

/** Called for each frame on display sinks to update the progress timestamp */
static GstPadProbeReturn display_progress_cb(GstPad *pad, GstPadProbeInfo *info, gpointer _stream)
{
	(void) pad; (void) info;
	stream_t *stream = _stream;
	if (stream->running)
	{
		g_mutex_lock(&stream->mutex_ts);
		stream->display_ts = g_get_monotonic_time();
		stream->frame_count++;
		g_mutex_unlock(&stream->mutex_ts);
		return GST_PAD_PROBE_OK;
	}
	return GST_PAD_PROBE_REMOVE; /* no frame display an inactive set streams */
}


/** gstreamer error callback. Normally we never should see a problem, so the best reaction
 * is an abort (g_error). Main reasons are frozen hardware (mainly decoder on bit errors with
 * network problems) and memory problems.
 *
 * In most cases restarting the pipeline won't fix the problem so a complete restart is
 * the best way here.
 */
static void on_error(GstBus *bus, GstMessage *message, gpointer _stream)
{
	stream_t *stream = _stream;
	(void)bus;
	if (GST_MESSAGE_ERROR == GST_MESSAGE_TYPE(message))
	{
		GError *err = NULL;
		gchar *dbg_info = NULL;
		gst_message_parse_error (message, &err, &dbg_info);
		g_error("gst error message: %s:vin/%u element %s: %s (%s)\n", stream->ipaddr, stream->vin, GST_OBJECT_NAME(message->src), err->message, (dbg_info ? dbg_info : ""));
		g_error_free (err);
		g_free (dbg_info);
	} else {
		g_critical("%s %s:vin/%u called with type %d\n", __func__, stream->ipaddr, stream->vin, GST_MESSAGE_TYPE(message));
	}
}


/** Create the stream object for the given quadrant (starting from 0).
 *   pipeline: librtp -> appsrc ! imxvpudec ! imxg2dvideosink
 *   no status change
 */
static stream_t* quadrant_factory(unsigned quadrant, const char* ipaddr, unsigned vin)
{
	if (quadrant >= MAX_QUADRANT) { g_error("%s illegal quadrant %u (max:%u)", __func__, quadrant, MAX_QUADRANT); return 0; } /* impl. sanity */
	stream_t *stream;
	if (!s_streams[quadrant])
	{
		stream = s_streams[quadrant] = g_new0(stream_t, 1);
		g_mutex_init(&stream->mutex_ts);
		CHECK_PTR(stream->pipeline = gst_pipeline_new(NULL));

		CHECK_PTR(stream->appsrc = gst_element_factory_make("appsrc", NULL));
		CHECK_TRUE(gst_bin_add(GST_BIN(stream->pipeline), stream->appsrc));

		GstCaps *caps = gst_caps_new_empty_simple("image/jpeg");
		g_object_set(stream->appsrc, "caps", caps, NULL);
		gst_caps_unref(caps);

		CHECK_PTR(stream->vpudec = gst_element_factory_make("imxvpudec", NULL));
		CHECK_TRUE(gst_bin_add(GST_BIN(stream->pipeline), stream->vpudec));

		CHECK_PTR(stream->display = gst_element_factory_make("imxg2dvideosink", NULL));
		//CHECK_PTR(stream->display = gst_element_factory_make("imxipuvideosink", NULL)); /* could be an alternative in case of problems with scaling */
		CHECK_TRUE(gst_bin_add(GST_BIN(stream->pipeline), stream->display));
		g_object_set(stream->display, "framebuffer", FRAMEBUFFER, "force-aspect-ratio", FALSE, NULL);
		GstPad* pad;
		CHECK_PTR(pad = gst_element_get_static_pad(stream->display, "sink"));
		gst_pad_add_probe(pad, GST_PAD_PROBE_TYPE_BUFFER, display_progress_cb, stream, NULL);
		gst_object_unref(pad);
		pad = 0;

		CHECK_TRUE(gst_element_link_many(stream->appsrc, stream->vpudec, stream->display, NULL));
		GstBus *bus = gst_pipeline_get_bus(GST_PIPELINE (stream->pipeline));

		gst_bus_add_signal_watch(bus);
		stream->signalErrorID = g_signal_connect (bus, "message::error", (GCallback)on_error, stream);
		g_debug("%s stream quadrant %u %s:vin/%u created", __func__, quadrant, ipaddr, vin);
	} else {
		g_debug("%s stream quadrant %u %s:vin/%u", __func__, quadrant, ipaddr, vin);
		stream = s_streams[quadrant];
	}
	stream->vin = vin;
	if (0 != g_strcmp0(stream->ipaddr, ipaddr))
	{
		g_free(stream->ipaddr);
		stream->ipaddr = g_strdup(ipaddr);
		stream->header_offset = stream->height = stream->width = stream->type = stream->q = 0;
		if (quadrant != 0 && s_streams[0] && (0 == g_strcmp0(s_streams[0]->ipaddr, ipaddr)))
		{
			stream->is_hipox = s_streams[0]->is_hipox;
		} else {
			stream->is_hipox = FALSE; // try that first
		}
		if (0 == quadrant) { check_network(0); }
	}
	return stream;
}

/** Factory destroy function. Remove the streaming object from factory array. */
static void destroy_stream(unsigned quadrant)
{
	if (quadrant >= MAX_QUADRANT) { g_error("%s illegal quadrant %u", __func__, quadrant); return; } /* impl. sanity */
	if (s_streams[quadrant])
	{
		stream_t *stream = s_streams[quadrant]; s_streams[quadrant] = 0;
		CHECK_TRUE(GST_STATE_CHANGE_FAILURE != gst_element_set_state(stream->pipeline, GST_STATE_NULL));
		GstBus *bus = gst_pipeline_get_bus(GST_PIPELINE (stream->pipeline));
		g_signal_handler_disconnect (bus, stream->signalErrorID);
		gst_bus_remove_signal_watch(bus);

		gst_object_unref(stream->pipeline); stream->pipeline = 0;
		g_mutex_clear(&stream->mutex_ts);

		g_debug("stream quadrant %u %s:vin/%u deleted", quadrant, stream->ipaddr, stream->vin);
		g_free(stream->ipaddr); stream->ipaddr = 0;
		g_free(stream);
	}
}


/** Stop the rtp streaming of the quadrant pipeline */
static void remote_media_stop(unsigned quadrant)
{
	if (quadrant >= MAX_QUADRANT) { g_error("%s illegal quadrant %u", __func__, quadrant); return; } /* impl. sanity */
    stream_t *stream = s_streams[quadrant];
	if (stream && stream->running)
	{
		enum drtp_status ret = drtp_stream_stop(&stream->sh);
		if (DRTP_SUCCESS != ret) {
			g_warning("%s quadrant %u stopping rtp stream vin/%u failed: %s", __func__, quadrant, stream->vin, drtp_strerror(ret));
		} else {
			int diff = (int)((g_get_monotonic_time() - stream->start_ts) / 1000000ll);
			if (!diff) { diff = 1; }
			float fps = (float)stream->frame_count / diff;
			g_message("%s quadrant %u stopped rtp stream vin/%u after frames:%u sec:%u fps:%.1f udp errors:%u", __func__, quadrant, stream->vin, stream->frame_count, diff, fps, stream->net_errors);
		}
		memset(&stream->sh, 0, sizeof(stream->sh)); /* bug in librtp, need clean handle here */
		/* Using PAUSE implies sometimes, that frames remain in the pipeline diplay after the next start. Here we need some research to use the g-framework more efficient */
		CHECK_TRUE(GST_STATE_CHANGE_FAILURE != gst_element_set_state(stream->pipeline, GST_STATE_NULL));
		stream->running = FALSE;
	} else {
		g_debug("%s quadrant %u not active", __func__, quadrant);
	}
}

/** Activate the given vin-connector of a stream device in display coordinates x,y with given dimension */
static void remote_media_start(unsigned quadrant, const char* ipaddr, unsigned vin, unsigned x, unsigned y, unsigned width, unsigned height)
{
	if (vin-1 >= DRTP_MAX_CAMS) { g_error("%s illegal vin/%u", __func__, vin); } /* impl. sanity */
	stream_t *stream = quadrant_factory(quadrant, ipaddr, vin);
	if (stream->running)
	{ /* sanity */
		g_warning("%s: quadrant %u running with %s:vin/%u %ux%u %ux%u -> stop", __func__, quadrant, stream->ipaddr, stream->vin, stream->x, stream->y, stream->width, stream->height);
		remote_media_stop(quadrant);
	}

	g_object_set(stream->display, "window-x-coord", x, "window-y-coord", y, "window-width", width, "window-height", height, NULL);
	stream->frame_count = 0;
	stream->net_errors = 0;
	stream->start_ts = stream->frame_ts = stream->display_ts = g_get_monotonic_time();
	stream->x = x; stream->y = y; stream->h = height; stream->w = width;
	stream->running = TRUE;
	CHECK_TRUE(GST_STATE_CHANGE_FAILURE != gst_element_set_state(stream->pipeline, GST_STATE_PLAYING));

	enum drtp_status librtp_ret;
	if (!stream->is_hipox) /* this field is set with the first read frames, I think thats ok */
	{  /* on gstreamer based systems is better to get the full pal resolution because scaling is here the bottle nack */
		g_message("%s quadrant %u %s vin/%u on display [%u,%u] width %u height %u (704x576)", __func__, quadrant, stream->ipaddr, stream->vin, x, y, width, height);
		librtp_ret = drtp_udp_stream_start(&stream->sh, rtp_frame_cb, rtp_memory_cb, stream->ipaddr, stream->vin, 704, 576);
	} else { /* reduce the resolution with old hipox devices, here the encoder is the bottle neck */
		if (width == WIDTH)
		{
			g_message("%s(hipox) quadrant %u %s vin/%u on display [%u,%u] width %u height %u (704x288)", __func__, quadrant, stream->ipaddr, stream->vin, x, y, width, height);
			librtp_ret = drtp_udp_stream_start(&stream->sh, rtp_frame_cb, rtp_memory_cb, stream->ipaddr, stream->vin, 704, 288);
		} else { // quad
			g_message("%s(hipox) quadrant %u %s vin/%u on display [%u,%u] width %u height %u (352x288)", __func__, quadrant, stream->ipaddr, stream->vin, x, y, width, height);
			librtp_ret = drtp_udp_stream_start(&stream->sh, rtp_frame_cb, rtp_memory_cb, stream->ipaddr, stream->vin, 352, 288);
		}
	}
	if (DRTP_SUCCESS != librtp_ret)
	{
		g_critical("%s rtp start of quadrant %u %s vin/%u on display [%u,%u] width %u height %u failed: %s",
				__func__, quadrant, stream->ipaddr, stream->vin, x, y, width, height, drtp_strerror(librtp_ret));
		remote_media_stop(quadrant);
		blank_area(x, y, width, height, 0);
	}
}

/** Timer controlled check function for active streams.
 * - Set a black area on missing stream progress and
 * - try to reconnect a failed stream.
 */
static void remote_vin_check(stream_t* stream)
{
	gint64 now = g_get_monotonic_time();
	g_mutex_lock(&stream->mutex_ts);
	int display_progess_ms = (int)((now - stream->display_ts) / 1000);
	int src_progess_ms = (int)((now - stream->frame_ts) / 1000);
	g_mutex_unlock(&stream->mutex_ts);
#if BLACK_SCREEN_DELAY  /* switch off black drawing with a set 0 */
	if (display_progess_ms > BLACK_SCREEN_DELAY)
	{
		if (stream->black_ts + 1000000 < now)  /* keep 1 second for blanks to avoid a busy CPU */
		{
			blank_area(stream->x, stream->y, stream->w, stream->h, display_progess_ms);
			stream->black_ts = now;
		}
	} else { stream->black_ts = 0llu; }
#endif /* BLACK_SCREEN_DELAY */
	if (src_progess_ms > MIN_NET_DELAY_MS + 1000 * stream->reconnect_delay)
	{
		g_warning("rtp stream failure (%dms), try to reconnect %d", src_progess_ms, stream->reconnect_delay);
		enum drtp_status ret = drtp_stream_restart(&stream->sh); /* this call reconnects the UDP rtp stream, works well after server problems */
		if (ret != DRTP_SUCCESS)
	   	{
			g_critical("%s restart rtp stream failed: %s", __func__, drtp_strerror(ret));
		} else {
			if (stream->reconnect_delay++ > 20) { stream->reconnect_delay = 20; }
			g_mutex_lock(&stream->mutex_ts);
			stream->frame_ts = now; /* restart the timer */
			g_mutex_unlock(&stream->mutex_ts);
		}
	}
}


/** Timer callback function installed at start time */
static gboolean cam_check_timer(gpointer ign)
{
	(void)ign;
	int i;
	for (i = 0; i < MAX_QUADRANT; i++)
	{
		if (!s_streams[i] || !s_streams[i]->running) { continue; }
		remote_vin_check(s_streams[i]);
	}
	return TRUE; /* periodic timer */
}


/** Stop any running view without drawing a black screen. Normally a new view is activated directly after that call. */
static void view_0()
{
	unsigned i;
	for(i = 0; i < MAX_QUADRANT; i++)
	{
		if (!s_streams[i] || !s_streams[i]->running) { continue; }
		remote_media_stop(i);
	}
}

/** Single view implementation */
static void view_1(const char* ipaddr, unsigned vin)
{
	view_0(); /* in case it's running */
	if (vin-1 >= DRTP_MAX_CAMS) { g_critical("%s illegal vin/%u requested - black screen", __func__, vin); blank_area(0, 0, WIDTH, HEIGHT, 0); }
	else { remote_media_start(0, ipaddr, vin, 0, 0, WIDTH, HEIGHT); }
}

/* Dual view was not requested */
//static void view_2(const char* ipaddr, unsigned vin1, unsigned vin2);

/** Quad view implementation */
static void view_4(const char* ipaddr, unsigned vin1, unsigned vin2, unsigned vin3, unsigned vin4)
{
	unsigned w = WIDTH / 2;
	unsigned h = HEIGHT / 2;
	view_0(); /* in case it's running */
	if (vin1-1 >= DRTP_MAX_CAMS)
	{
		g_critical("%s illegal vin/%u requested - black quadrant 0", __func__, vin1); blank_area(0, 0, w, h, 0);
	} else {
		remote_media_start(0, ipaddr, vin1, 0, 0, w, h);
	}

	if ((vin2-1 >= DRTP_MAX_CAMS) || (vin2 == vin1))
	{
		g_critical("%s illegal vin/%u requested - black quadrant 1", __func__, vin2); blank_area(w, 0, w, h, 0);
	} else {
		remote_media_start(1, ipaddr, vin2, w, 0, w, h);
	}

	if ((vin3-1 >= DRTP_MAX_CAMS) || (vin3 == vin1) || (vin3 == vin2))
	{
		g_critical("%s illegal vin/%u requested - black quadrant 2", __func__, vin3); blank_area(0, h, w, h, 0);
	} else {
		remote_media_start(2, ipaddr, vin3, 0, h, w, h);
	}

	if ((vin4-1 >= DRTP_MAX_CAMS) || (vin4 == vin1) || (vin4 == vin2) || (vin4 == vin3))
	{
		g_critical("%s illegal vin/%u requested - black quadrant 2", __func__, vin4); blank_area(w, h, w, h, 0);
	} else {
		remote_media_start(3, ipaddr, vin4, w, h, w, h);
	}
}

/* Other views was not requested */

/** Stop any running view and draw a black screen */
static void view_black()
{
	view_0();
	blank_area(0, 0, WIDTH, HEIGHT, 0);
}

/** Server command processor:
 *  cmd:
 *  - 1<ip>#cam single view
 *  - 4<ip>#cam#cam#cam#cam quad view
 *  - 0 simple of without black screen
 *  - S server down
 *  - D<cam> stream debug
 *  - B black screen
 *  - d<level> debug level
 *
 *  Note: Currently streaming from one recorder IP is supported. This could be changed using "<ip>:camX" notations...
 */
static gboolean server_cmd(gchar* cmd)
{
	if (!fb_handle <= 0)
	{

		fb_handle = open(FRAMEBUFFER, O_RDWR, 0);
		if (fb_handle >= 0)
		{
			if (0 != ioctl(fb_handle, FBIOGET_VSCREENINFO, &fb_vinfo))
			{
				g_critical("%s handle %d ioctl(FBIOGET_VSCREENINFO) failed: %s(%d)", FRAMEBUFFER, fb_handle, strerror(errno), errno);
				close(fb_handle); fb_handle = -1;
			}
			if (0 != ioctl(fb_handle, FBIOGET_FSCREENINFO, &fb_finfo))
			{
				g_critical("%s handle %d ioctl(FBIOGET_VSCREENINFO) failed: %s(%d)", FRAMEBUFFER, fb_handle, strerror(errno), errno);
				close(fb_handle); fb_handle = -1;
			}
		} else {
			g_critical("frame buffer %s open failed: %s(%d)", FRAMEBUFFER, strerror(errno), errno);
		}

#ifndef FIX_VIDEO_AREA
		/* possible adaptations for other display resolutions possible here */
		if (WIDTH > fb_vinfo.xres) { WIDTH = fb_vinfo.xres; }
		if (HEIGHT > fb_vinfo.yres) { HEIGHT = fb_vinfo.yres; }
#endif
		g_message("display resolution %ux%u video area [0, 0, %u, %u]", fb_vinfo.xres, fb_vinfo.yres, WIDTH, HEIGHT);
	}

	switch(cmd[0])
	{
		case '1':
			{
				const char* ipaddr = cmd+1;
				gchar* args = strchr(cmd, '#');
				if (!args) { g_warning("illegal server command '%s' ignored", cmd); break; }
				*args++ = 0; /* terminate the ip with 0 */
				unsigned cam;
				if (1 != sscanf(args, "%u", &cam)) { g_warning("illegal server command args '%s' for single view", args); break; }
				view_1(ipaddr, cam);
			} break;
		case '4':
			{
				const char* ipaddr = cmd+1;
				gchar* args = strchr(cmd, '#');
				if (!args) { g_warning("illegal server command '%s' ignored", cmd); break; }
				*args++ = 0; /* terminate the ip with 0 */
				unsigned cam[4] = {0};
				if (4 != sscanf(args, "%u#%u#%u#%u", &cam[0], &cam[1], &cam[2], &cam[3])) { g_warning("illegal server command args '%s' for quad view", args); break; }
				view_4(ipaddr, cam[0], cam[1], cam[2], cam[3]);
			} break;
		case '0':
			view_0();
			break;
		case 'B':
			view_black();
			break;
		case 'D':
			{
				unsigned cam;
				if ((1 != sscanf(cmd+1, "%u", &cam)) || (cam-1 >= DRTP_MAX_CAMS)) { g_warning("illegal server command args '%s' for stream debugging", cmd+1); break; }
				trace_fames[cam-1] = !trace_fames[cam-1];
				g_info("stream debugging vin/%u: %d", cam, trace_fames[cam-1]);
			} break;
		case 'd':
			{
				if (1 != sscanf(cmd+1, "%u", &s_debug)) { g_warning("illegal server command args '%s' for debug level", cmd+1); break; }
				g_info("stream debug level set to %s (%d)", log_level_to_string(G_LOG_LEVEL_ERROR << s_debug), s_debug);
			} break;
		case 'S':
			return FALSE;
		default:
			g_warning("cmd '%s' not supported", cmd);
			break;
	}
	return TRUE;
}

/** FIFO read handler of the server. This function runs in mainloop context as reaction to any fifo changes. */
static gboolean fifo_read_handler(GIOChannel *source, GIOCondition condition, gpointer _ign)
{
	(void)_ign;
	if (condition & G_IO_IN)
	{
		GIOStatus status;
		gchar *cmd = 0;
		gsize length;
		int args;

		status = g_io_channel_read_line (source, &cmd, &length, NULL, NULL);
		g_debug("%s status:%d len:%u cmd:%.*s", __func__, status, length, length-1, cmd);

		switch(status)
		{
			case G_IO_STATUS_NORMAL:
				if (cmd && !server_cmd(cmd))
				{
					g_debug("%s shutdown forced", __func__);
					view_black();
					int i;
					for(i = 0; i < MAX_QUADRANT; i++) { destroy_stream(i); }
					g_main_loop_quit(s_pGMainloop);
					return FALSE;  /* close channel */
				}
				break;
			case G_IO_STATUS_AGAIN:
				g_debug("%s: AGAIN", __func__);
				break;
				/* from here this should not happen */
			case G_IO_STATUS_ERROR:
				g_warning("%s: ERROR", __func__);
			case G_IO_STATUS_EOF:
				g_debug("%s: EOF", __func__);
			default:
				g_warning("%s: status ??? (0x%X)", __func__, status);
				break;
		}
		if (cmd) { g_free (cmd); cmd = 0; }
		condition &= ~G_IO_IN;
	}
	if (condition & (G_IO_HUP))
	{
		condition &= ~(G_IO_HUP);
		g_debug("%s: G_IO_HUP", __func__);
		g_idle_add(connect_fifo, 0); /* reopen fifo */
		return FALSE; /* close channel */
	}
	if (condition & (G_IO_ERR))
	{
		condition &= ~(G_IO_ERR);
		g_warning("%s: G_IO_ERR", __func__);
		g_idle_add(connect_fifo, 0); /* reopen fifo */
		return FALSE; /* close channel */
	}
	if (condition)
	{ /* implementation bug, incomplete handling */
		g_critical("%s unknown condition 0x%X", __func__, condition);
	}
	return TRUE; /* continue reading */
}


/** Open a GIO channel for the FIFO an connect it with the mainloop.
 *  Opening can take time until the next adapter process is started, which
 *  opens the FIFO for writing.
 *
 *  This function runs in mainloop (idle) context, but it could be moved into a thread
 *  if pipelines has to be manipulated in background. Currently there should
 *  be no active pipeline of the adapter layer is down. So this simple
 *  implementation is fine.
 */
static gboolean connect_fifo(void* ign)
{
	(void)ign;
	GError *gerr = 0;
	g_debug("%s open fifo %s for reading", __func__, RUN_NAME ".fifo");
	GIOChannel *gchannel= g_io_channel_new_file(RUN_NAME ".fifo", "r", &gerr);
	if (!gchannel)
	{
		g_critical("open %s failed: %s", RUN_NAME ".fifo", gerr->message);
		g_error_free(gerr); gerr = 0;
		g_main_loop_quit(s_pGMainloop);
	} else {
		g_debug("%s watching fifo %s", __func__, RUN_NAME ".fifo");
		g_io_add_watch(gchannel, G_IO_IN | G_IO_ERR  | G_IO_HUP, (GIOFunc)fifo_read_handler, 0);
		g_io_channel_unref(gchannel);
	}
	return FALSE;
}

/** Ignore the signals send from parent (a killed creator) */
void server_sighandler(int signum)
{
	g_info("Caught signal %d (ign)", signum);
}


/** Main function of the vout server. This is a threaded glib application with mainloop.
 *  The server ends on request only to avoid gstreamer pipeline reconstruction (critical
 *  timing for changing videos).
 */
static int server_main(GOptionContext *context, int argc, char**argv)
{
	g_debug("%s...", __func__);

	g_option_context_free(context);
	CHECK_TRUE(SIG_ERR != signal(SIGTERM, server_sighandler));
	CHECK_TRUE(SIG_ERR != signal(SIGINT, server_sighandler));

	gst_init(0, 0);
	CHECK_TRUE(DRTP_SUCCESS == drtp_init());
	drtp_set_error_logger(rtp_error_cb, 0);

	g_debug("%s: starting server mainloop", argv[0]);
	g_idle_add(connect_fifo, 0);
	g_timeout_add(100, cam_check_timer, 0);
	s_pGMainloop = g_main_loop_new (0, FALSE);
	g_main_loop_run(s_pGMainloop);
	g_debug("%s: stopped server mainloop", argv[0]);
	g_main_loop_unref (s_pGMainloop); s_pGMainloop = 0;

	unlink(RUN_NAME ".pid");
	close(fb_handle);
	return 0;
}


/** Write to server fifo with error handling */
static int write_fifo(gchar* buf)
{
	int ret;
	int len = strlen(buf), l;
	if (len != (l = write(s_fifo, buf, len)))
	{
		g_critical("%s: write(%.*s (%u)) to %s failed: %s(%d/%d)", __func__, len-1, buf, len, RUN_NAME ".fifo", strerror(errno), errno, l);
		close(s_fifo); s_fifo = -1;
		ret = -1;
	} else {
		g_message("%s: write(%.*s (%u)) to %s", __func__, len-1, buf, len, RUN_NAME ".fifo");
		ret = 0;
	}
	g_free(buf);
	return ret;
}

/** Killing the adapter layer sends an "off" command to the server.
 *  Hard killing work too using the HUP signal via named fifo.
 */
void adapter_sighandler(int signum)
{
	g_info("Caught signal %d switch off", signum);
	write_fifo(g_strdup("0\n"));
	close(s_fifo); s_fifo = -1;
	exit(0);
}

/** Main function of the adapter layer between application and vout server */
static int adapter_main(GOptionContext *context, int argc, char**argv)
{
	int ret;
	s_fifo = open(RUN_NAME ".fifo", O_WRONLY);
	if (s_fifo < 0)
	{
		g_critical("%s: open(%s) failed: %s(%d)", __func__, RUN_NAME ".fifo", strerror(errno), errno);
		return -1;
	}

	if (s_stream_trace > 0) { write_fifo(g_strdup_printf("D%u\n", s_stream_trace)); }
	gboolean do_wait = TRUE;
	switch (argc)
	{
		case 2: ret = write_fifo(g_strdup_printf("0\n", argv[1])); do_wait = FALSE; break;
		case 3: ret = write_fifo(g_strdup_printf("1%s#%s\n", argv[1], argv[2])); break;
		case 6: ret = write_fifo(g_strdup_printf("4%s#%s#%s#%s#%s\n", argv[1], argv[2], argv[3], argv[4], argv[5])); break;
		case 0:
				g_critical("%s: missing program name, IP and quadrant camera(s) - black screen", __func__);
				ret = write_fifo(g_strdup("B\n"));
				break;
		case 1:
				g_info("%s: shutdown server", argv[0]);
				ret = write_fifo(g_strdup("S\n"));
				do_wait = FALSE;
				break;
		default:
				g_critical("%s ip '%s': illegal number of requested quadrants (%u), allowed are 1 or 4 - black screen", argv[0], argv[1], argc - 2);
				write_fifo(g_strdup("B\n"));
				do_wait = FALSE;
				ret = -1;
				break;
	}
	g_option_context_free(context);

	CHECK_TRUE(SIG_ERR != signal(SIGTERM, adapter_sighandler));
	CHECK_TRUE(SIG_ERR != signal(SIGINT, adapter_sighandler));

	while (do_wait && (0 == ret))
	{
		g_debug("%s: alive - wait for signal to stop video output", argv[0]);
		sleep(60);
	}
	return ret;
}



/** Checks that the adapter/service role.
 *  Server PID is located in RUN_NAME.pid written by the server.
 *  Communication fifo is located in RUN_NAME.fifo created by the first runner.
 *  A server is started (forked) implicitly if not available.
 */
static appl_t hydra_application()
{
	GError *gerr = 0;
	appl_t ret = (s_server ? E_SERVER : E_ADAPTER);
	unsigned server_pid;
	gchar* spid = 0;
	gchar* proc = 0;
	if (g_file_get_contents( RUN_NAME ".pid", &spid, 0, 0) && (1 == sscanf(spid, "%u", &server_pid)))
	{
		if (g_file_test((proc = g_strdup_printf("/proc/%u", server_pid)), G_FILE_TEST_IS_DIR)) /* check running */
		{
			if (s_server)
			{
				g_critical("hydra vout service with PID:%u active - kill", server_pid);
				kill(server_pid, SIGKILL);
				server_pid = 0;
			} else {
				g_debug("hydra vout service with PID:%u active", server_pid);
			}
		} else {
			g_debug("hydra vout old service PID:%u not active", server_pid);
			server_pid = 0;
		}
	} else {
		g_debug("hydra vout no service active");
		server_pid = 0;
	}

	if (!server_pid || s_server)
	{   /* we are the first: (re-)create fifo */
		unlink( RUN_NAME ".fifo"); /* just to be save here */
		if (0 != mkfifo( RUN_NAME ".fifo", 0600)) /* create the communication fifo */
		{
			g_critical("%s mkfifo %s failed: %s (%u)", __func__, RUN_NAME ".fifo", strerror(errno), errno);
			ret = E_FAILED;
		} else {
			g_debug("%s FIFO %s created", __func__, RUN_NAME ".fifo");
		}
	}

	if ((E_FAILED != ret) && !server_pid && !s_server)
	{ /* we are adapter without server, start it */
		switch((server_pid = fork()))
		{
			case -1:
				g_critical("%s fork failed: %s (%u)", __func__, strerror(errno), errno);
				ret = E_FAILED;
				break;
			case 0:
				s_server = TRUE;
				ret = E_SERVER;
				server_pid = 0; /* force writing the pid file */
				if (!s_stdout) { daemon(0, 0); }
				g_debug("%s running as server", __func__);
				break;
			default:
				g_message("hydra vout service started pid:%u", server_pid);
				break;
		}
	}

	if (!server_pid && s_server)
	{
		g_free(spid); spid = g_strdup_printf("%u", getpid());
		if (!g_file_set_contents( RUN_NAME ".pid", spid, -1, &gerr))
		{
			g_critical("%s write PID to %s failed: %s", __func__, RUN_NAME ".fifo", gerr->message);
			ret = E_FAILED;
		}
	}
	g_free(spid);
	g_free(proc);
	return ret;
}


/** Supported options... */
static GOptionEntry s_main_opts[] =
{
	{ "debug",          'd', 0, G_OPTION_ARG_INT,  &s_debug,        "set debug level ", "0..5" },
	{ "stream-debug",   'D', 0, G_OPTION_ARG_INT,  &s_stream_trace, "switch stream debug of the given vin", "1..8" },
	{ "stderr",         's', 0, G_OPTION_ARG_NONE, &s_stdout,       "all traces to stderr", NULL},
	{ "server",         'S', 0, G_OPTION_ARG_NONE, &s_server,       "start in server mode ", NULL },
	{ "version",        'v', 0, G_OPTION_ARG_NONE, &s_version,      "version string", NULL },
	{ NULL }
};

int main (int argc, char *argv[])
{
	GError *gerr = 0;
	g_log_set_handler(0, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION, log_handler_cb, NULL);

	GOptionContext *context = g_option_context_new ("[ IPv4  (vin | vin1...vin4) ] - HydraIP vout adapter");
	g_option_context_add_main_entries (context, s_main_opts, 0);
	g_option_context_set_help_enabled(context, TRUE);
	if (!g_option_context_parse (context, &argc, &argv, &gerr))
	{
		g_critical ("option parsing failed: %s", gerr->message);
		g_error_free(gerr); gerr = 0;
		return -1;
	}
	if (s_version)
	{
		gint debug = s_debug; s_debug = 5;
		g_info("%s version %s (%s)", argv[0], VERSION_STRING, "$Id$");
		s_debug = debug;
	}

	switch (hydra_application())
	{
		case E_FAILED:
			g_critical("%s exit(-1)", argv[0]);
			return -1;
		case E_SERVER:
			g_debug("%s server PID %u", argv[0], getpid());
			return server_main(context, argc, argv);
		case E_ADAPTER:
			g_debug("%s adapter PID %u", argv[0], getpid());
			return adapter_main(context, argc, argv);
	}
}

/* Editor hints for emacs
 *
 * Local Variables:
 * mode:c
 * c-basic-offset:4
 * indent-tabs-mode:t
 * tab-width:4
 * End:
 *
 * NO CODE BELOW THIS! */
