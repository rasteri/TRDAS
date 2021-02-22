#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <unistd.h>

#include <ogcsys.h>
#include <gccore.h>
#include <ogc/si.h>
#include <ogc/machine/processor.h>

#include "video.h"

static void *xfb = NULL;
static GXRModeObj *rmode = NULL;

// Loop indefinitely until a controller is connected on the specified channel.
#define TIMEOUT_FRAMES	60 * 30
static u32 SI_CMD_TYPE = 0x00000000;
static u32 chn0_type = 0x00000008;
int wait_for_controller(u32 chn) {
	u32 frame = 0;
	u32 type = 0;
	printf("[!] Waiting for a controller on port %d ...\n", chn);
	while (1) {
		VIDEO_WaitVSync();
		type = SI_GetType(chn);
		if ((type & 0xffff0000) == 0x09000000) {
			printf("Connected on port 0 (type=%08x)\n", type);
			return 0;
		}
		if (frame >= TIMEOUT_FRAMES) 
			return -1;
		else
			frame += 1;
	}
}

int main(int argc, char **argv) {
	xfb = video_init(xfb, rmode);

	// Use the libogc implementation of PAD here: the majority of GC games 
	// are probably configured to behave something like this, but writing
	// tests with these mostly abstracts away all the details of doing 
	// all of the underlying serial transfers.
	//
	// Start when controller is connected on port 0.
	// If we wait for more than 30 seconds, just reset.
	//
	// The loop in wait_for_controller() is aligned to vertical retrace,
	// meaning that we have one frame to waste before expecting inputs.

	if (wait_for_controller(0) != 0) {
		printf("[!] Timeout waiting for controller, resetting ...\n");
		sleep(5);
		return -1;
	} else {

		PAD_Init();
		SI_SetSamplingRate(0);
		VIDEO_WaitVSync();

		pad_test(0);
		printf("[!] Test completed, press START to reset ...\n");
		while (1) {
			VIDEO_WaitVSync();
			PAD_ScanPads();
			int p0_input = PAD_ButtonsDown(0);
			if (p0_input & PAD_BUTTON_START)
				break;
		}
	}
	return 0;
}

static u32 frame_ctr = 0;
static u32 error_ctr = 0;
static u32 ng_ctr = 0;
static u32 ok_ctr = 0;
static PADStatus status[4];

void pad_test(u32 chn) {

	// For now, let's just test the button inputs.
	// The bottom 13 bits (0x1fff) are mapped to valid buttons.

	u16 button_next = 0x0000;
	while (button_next < 0x2000) {

		// Wait until the next vertical retrace
		VIDEO_WaitVSync();

		// Read the status on all channels
		PAD_Read(status);

		// TODO: These don't reflect underlying SI errors; they're 
		// all typically abstracted away by the PAD libraries
		if (status[chn].err < 0) {
			error_ctr += 1;
		}

		if (status[chn].button == button_next) {
			ok_ctr += 1;
		} else {
			ng_ctr += 1;
		}

		printf("\x1b[10;0H");
		printf("Current frame:     %08d", frame_ctr);
		printf("\x1b[11;0H");
		printf("Expected input:    0x%04x/0x1fff", button_next);
		printf("\x1b[12;0H");
		printf("Recieved input:    0x%04x/0x1fff", status[chn].button);
		printf("\x1b[13;0H");
		printf("Correct inputs:    %08d", ok_ctr);
		printf("\x1b[14;0H");
		printf("Incorrect inputs:  %08d", ng_ctr);
		printf("\x1b[15;0H");
		printf("PAD error count:   %08d", error_ctr);

		button_next += 1;
		frame_ctr += 1;
	}
	printf("\n\n");
}

