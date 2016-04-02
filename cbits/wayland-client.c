/*
 * Copyright © 2008-2012 Kristian Høgsberg
 * Copyright © 2010-2012 Intel Corporation
 * Copyright © 2013 Jason Ekstrand
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <errno.h>
#include <poll.h>

#include <wayland-client.h>

#define WL_CLOSURE_MAX_ARGS 20

struct argument_details {
	char type;
	int nullable;
};

const char *
get_next_argument(const char *signature, struct argument_details *details)
{
	details->nullable = 0;
	for(; *signature; ++signature) {
		switch(*signature) {
		case 'i':
		case 'u':
		case 'f':
		case 's':
		case 'o':
		case 'n':
		case 'a':
		case 'h':
			details->type = *signature;
			return signature + 1;
		case '?':
			details->nullable = 1;
		}
	}
	details->type = '\0';
	return signature;
}


void
wl_argument_from_va_list(const char *signature, union wl_argument *args,
			 int count, va_list ap)
{
	int i;
	const char *sig_iter;
	struct argument_details arg;

	sig_iter = signature;
	for (i = 0; i < count; i++) {
		sig_iter = get_next_argument(sig_iter, &arg);

		switch(arg.type) {
		case 'i':
			args[i].i = va_arg(ap, int32_t);
			break;
		case 'u':
			args[i].u = va_arg(ap, uint32_t);
			break;
		case 'f':
			args[i].f = va_arg(ap, wl_fixed_t);
			break;
		case 's':
			args[i].s = va_arg(ap, const char *);
			break;
		case 'o':
			args[i].o = va_arg(ap, struct wl_object *);
			break;
		case 'n':
			args[i].o = va_arg(ap, struct wl_object *);
			break;
		case 'a':
			args[i].a = va_arg(ap, struct wl_array *);
			break;
		case 'h':
			args[i].h = va_arg(ap, int32_t);
			break;
		case '\0':
			return;
		}
	}
}

char *
proxy_msg_get_signature(struct wl_proxy*, uint32_t);
WL_EXPORT void
wl_proxy_marshal(struct wl_proxy *proxy, uint32_t opcode, ...)
{
	union wl_argument args[WL_CLOSURE_MAX_ARGS];
	va_list ap;

	va_start(ap, opcode);
	wl_argument_from_va_list(proxy_msg_get_signature(proxy, opcode),
				 args, WL_CLOSURE_MAX_ARGS, ap);
	va_end(ap);

	wl_proxy_marshal_array_constructor(proxy, opcode, args, NULL);
}
WL_EXPORT struct wl_proxy *
wl_proxy_marshal_constructor(struct wl_proxy *proxy, uint32_t opcode,
			     const struct wl_interface *interface, ...)
{
	union wl_argument args[WL_CLOSURE_MAX_ARGS];
	va_list ap;

	va_start(ap, interface);
	wl_argument_from_va_list(proxy_msg_get_signature(proxy, opcode),
				 args, WL_CLOSURE_MAX_ARGS, ap);
	va_end(ap);

	return wl_proxy_marshal_array_constructor(proxy, opcode,
						  args, interface);
}

WL_EXPORT struct wl_proxy *
wl_proxy_marshal_constructor_versioned(struct wl_proxy *proxy, uint32_t opcode,
				       const struct wl_interface *interface,
				       uint32_t version, ...)
{
	union wl_argument args[WL_CLOSURE_MAX_ARGS];
	va_list ap;

	va_start(ap, version);
	wl_argument_from_va_list(proxy_msg_get_signature(proxy, opcode),
				 args, WL_CLOSURE_MAX_ARGS, ap);
	va_end(ap);

	return wl_proxy_marshal_array_constructor_versioned(proxy, opcode,
							    args, interface,
							    version);
}

// this should really be haskell code
static void
sync_callback(void *data, struct wl_callback *callback, uint32_t serial)
{
	int *done = data;

	*done = 1;
	wl_callback_destroy(callback);
}

static const struct wl_callback_listener sync_listener = {
	sync_callback
};
WL_EXPORT int
wl_display_roundtrip_queue(struct wl_display *display, struct wl_event_queue *queue)
{
	struct wl_callback *callback;
	int done, ret = 0;

	done = 0;
	callback = wl_display_sync(display);
	if (callback == NULL)
		return -1;
	wl_proxy_set_queue((struct wl_proxy *) callback, queue);
	wl_callback_add_listener(callback, &sync_listener, &done);
	while (!done && ret >= 0)
		ret = wl_display_dispatch_queue(display, queue);

	if (ret == -1 && !done)
		wl_callback_destroy(callback);

	return ret;
}
static int
wl_display_poll(struct wl_display *display, short int events)
{
	int ret;
	struct pollfd pfd[1];

	pfd[0].fd = wl_display_get_fd(display);
	pfd[0].events = events;
	do {
		ret = poll(pfd, 1, -1);
	} while (ret == -1 && errno == EINTR);

	return ret;
}
WL_EXPORT int
wl_display_dispatch_queue(struct wl_display *display,
			  struct wl_event_queue *queue)
{
	int ret;

	if (wl_display_prepare_read_queue(display, queue) == -1)
		return wl_display_dispatch_queue_pending(display, queue);

	while (1) {
		ret = wl_display_flush(display);

		if (ret != -1 || errno != EAGAIN)
			break;

		if (wl_display_poll(display, POLLOUT) == -1) {
			wl_display_cancel_read(display);
			return -1;
		}
	}

	/* Don't stop if flushing hits an EPIPE; continue so we can read any
	 * protocol error that may have triggered it. */
	if (ret < 0 && errno != EPIPE) {
		wl_display_cancel_read(display);
		return -1;
	}

	if (wl_display_poll(display, POLLIN) == -1) {
		wl_display_cancel_read(display);
		return -1;
	}

	if (wl_display_read_events(display) == -1)
		return -1;

	return wl_display_dispatch_queue_pending(display, queue);
}
